{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- todo: hide ColumnDef ctor
module HaGrid
  ( MaybeSort (..),
    haGrid,
    textColumn,
    showOrdColumn,
    customColumn
  )
where

import Control.Lens (abbreviatedFields, makeLensesWith, (%~), (.~), (<>~), (?~), (^.))
import Control.Lens.Combinators (non)
import Control.Lens.Lens ((&))
import Control.Monad as X (forM_, void)
import Data.Default.Class as X (def)
import Data.Foldable (foldl')
import qualified Data.List as List
import Data.List.Extra as X ((!?))
import Data.List.Index (izipWith, setAt)
import Data.Maybe (fromJust)
import Data.Maybe as X (fromMaybe)
import Data.Ord (Down (Down))
import Data.Sequence (Seq, ViewR ((:>)))
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Monomer
import qualified Monomer.Lens as L
import Monomer.Widgets.Container
import Monomer.Widgets.Single

data HaGridEvent ep
  = OrderByColumn Int
  | ResizeColumn Int Int
  | ParentEvent ep

data ColumnDef e a where
  ColumnDef ::
    { _cdName :: Text,
      _cdWidget :: a -> WidgetNode (HaGridModel a) (HaGridEvent e),
      _cdSortKey :: MaybeSort a,
      _cdInitialWidth :: Int,
      _cdMinWidth :: Int
    } ->
    ColumnDef e a

data MaybeSort a where
  DontSort :: MaybeSort a
  SortWith :: Ord b => (a -> b) -> MaybeSort a

data HaGridModel a = HaGridModel
  { _mSortedItems :: [a],
    _mColumnWidths :: [Int],
    _mSortColumn :: Maybe Int,
    _mSortReverse :: Bool
  }
  deriving (Eq, Show)

makeLensesWith abbreviatedFields ''HaGridModel

data HeaderDragHandleState = HeaderDragHandleState
  { _hdhsDragStartMouseX :: Double,
    _hdhsDragStartColumnW :: Int
  }
  deriving (Eq, Show)

-- todo: allow padding to be configurable (per cell? for custom cells?)
haGrid :: forall a s e. (Typeable a, Eq a, WidgetModel s, WidgetEvent e) => [ColumnDef e a] -> [a] -> WidgetNode s e
haGrid columnDefs items = widget
  where
    widget =
      compositeD_
        "HaGrid.Root"
        (WidgetValue (initialModel columnDefs items))
        buildUI
        (handleEvent columnDefs)
        []

    buildUI :: UIBuilder (HaGridModel a) (HaGridEvent e)
    buildUI _wenv HaGridModel {..} = tree
      where
        tree =
          hscroll $
            vstack
              [ headerPane `nodeKey` headerPaneKey,
                vscroll (contentPane `nodeKey` contentPaneKey)
              ]

        headerPane =
          defaultWidgetNode "HaGrid.HeaderPane" headerPaneContainer
            & L.children .~ S.fromList headerWidgets

        contentPane =
          defaultWidgetNode "HaGrid.ContentPane" contentPaneContainer
            & L.children .~ S.fromList (mconcat childWidgetRows)

        headerWidgets =
          foldMap (\(a, b) -> [a, b]) $ izipWith headerWidgetPair columnDefs _mColumnWidths
        headerWidgetPair i columnDef columnWidth = (btn, handle)
          where
            btn = headerButton i columnDef
            handle = headerDragHandle i columnDef columnWidth
        childWidgetRows =
          [[_cdWidget item | ColumnDef{_cdWidget} <- columnDefs] | item <- _mSortedItems]

        nRows = length items
        nCols = length columnDefs

        dragHandleWidth = 4
        dragHandleHeight = 32
        paddingW = 10
        paddingH = 10
        paddingTotalH = fromIntegral nRows * paddingH * 2

        wReqs = S.fromList (fixedSize . fromIntegral <$> _mColumnWidths)

        contentPaneContainer =
          createContainer
            _mColumnWidths
            def
              { containerGetSizeReq = contentGetSizeReq,
                containerResize = contentResize,
                containerRenderAfter = contentRenderAfter
              }

        contentGetSizeReq _wenv _node children = (w, h)
          where
            hReqs = toRowHeights children nCols
            w = foldl' sizeReqMergeSum (fixedSize 0) wReqs
            h = foldl' sizeReqMergeSum (fixedSize 0) hReqs & L.fixed %~ (+ paddingTotalH)

        contentResize wenv node viewport children = (resultNode node, assignedAreas)
          where
            style = currentStyle wenv node
            Rect l t _w h = fromMaybe def (removeOuterBounds style viewport)

            hReqs = toRowHeights children nCols
            colXs = sizesToPositions (S.fromList (fromIntegral <$> _mColumnWidths))
            rowYs = sizesToPositions (cellSizes hReqs (h - paddingTotalH))

            assignedAreas = S.fromList $ do
              row <- [0 .. nRows - 1]
              col <- [0 .. nCols - 1]
              pure (assignArea col row)

            assignArea col row = Rect chX chY chW chH
              where
                chX = l + S.index colXs col + paddingW
                chY = t + S.index rowYs row + paddingH * (fromIntegral row * 2 + 1)
                chW = S.index colXs (col + 1) - S.index colXs col - paddingW * 2
                chH = S.index rowYs (row + 1) - S.index rowYs row

        contentRenderAfter wenv node renderer = do
          setStrokeColor renderer (accentColor wenv)
          setStrokeWidth renderer 1

          forM_ (S.drop 1 colXs) $ \colX -> do
            beginPath renderer
            renderLine renderer (Point (l + colX) t) (Point (l + colX) (t + lastRowY))
            stroke renderer

          void $
            flip S.traverseWithIndex (S.drop 1 rowYs) $ \row rowY -> do
              beginPath renderer
              let y = t + rowY + paddingH * (fromIntegral (row + 1) * 2)
              renderLine renderer (Point l y) (Point (l + lastColX) y)
              stroke renderer
          where
            hReqs = toRowHeights (node ^. L.children) nCols
            colXs = sizesToPositions (S.fromList (fromIntegral <$> _mColumnWidths))
            rowYs = sizesToPositions (cellSizes hReqs (h - paddingTotalH))
            lastColX
              | _ :> a <- S.viewr colXs = a
              | otherwise = 0
            lastRowY
              | _ :> a <- S.viewr rowYs = a + paddingTotalH
              | otherwise = 0
            Rect l t _w h = node ^. L.info . L.viewport

        headerPaneContainer =
          createContainer
            _mColumnWidths
            def
              { containerGetSizeReq = headerGetSizeReq,
                containerResize = headerResize,
                containerRenderAfter = headerRenderAfter
              }

        headerGetSizeReq _wenv _node _children = (w, h)
          where
            w = fixedSize (fromIntegral (sum _mColumnWidths))
            h = fixedSize dragHandleHeight

        headerResize _wenv node viewport _children = (resultNode node, assignedAreas)
          where
            Rect l t _w h = viewport
            widgetWidths = do
              w <- _mColumnWidths
              [w - dragHandleWidth, dragHandleWidth]
            (assignedAreas, _) = foldl' assignArea (mempty, l) widgetWidths
            assignArea (areas, colX) columnWidth =
              (areas S.:|> Rect colX t (fromIntegral columnWidth) h, colX + fromIntegral columnWidth)

        headerRenderAfter wenv node renderer =
          forM_ _mSortColumn (renderSortIndicator wenv node renderer)

        renderSortIndicator wenv node renderer sortCol = do
          drawSortIndicator renderer indRect (Just (accentColor wenv)) _mSortReverse
          where
            style = wenv ^. L.theme . L.basic . L.btnStyle
            Rect l t _w h = node ^. L.info . L.viewport
            size = style ^. L.text . non def . L.fontSize . non def
            colOffset = fromIntegral (sum (take (sortCol + 1) _mColumnWidths) - dragHandleWidth)
            indW = unFontSize size * 2 / 3
            pad = indW / 3
            indT
              | _mSortReverse = t + pad
              | otherwise = h - pad - indW
            indL = l + colOffset - indW - pad
            indRect = Rect indL indT indW indW

    handleEvent :: [ColumnDef ep a] -> EventHandler (HaGridModel a) (HaGridEvent ep) sp ep
    handleEvent columnDefs wenv _node model = \case
      OrderByColumn colIndex
        | Just ColumnDef{_cdSortKey = DontSort} <- columnDefs !? colIndex ->
            []
        | Just c <- (model ^. sortColumn),
          c == colIndex ->
            [Model (sortItems columnDefs (model & sortReverse %~ not))]
        | otherwise ->
            [Model (sortItems columnDefs (model & sortColumn ?~ colIndex & sortReverse .~ False))]
      ResizeColumn colIndex width ->
        [ Model (model & columnWidths %~ setAt colIndex width),
          Request (ResizeWidgets headerPaneId),
          Request (ResizeWidgets contentPaneId)
        ]
      ParentEvent e ->
        [Report e]
      where
        headerPaneId = fromJust (widgetIdFromKey wenv (WidgetKey headerPaneKey))
        contentPaneId = fromJust (widgetIdFromKey wenv (WidgetKey contentPaneKey))

-- todo: use triangle fn from latest monomer
drawSortIndicator :: Renderer -> Rect -> Maybe Color -> Bool -> IO ()
drawSortIndicator _renderer _rect Nothing _reverse = pure ()
drawSortIndicator renderer rect (Just color) reverse = do
  beginPath renderer
  setFillColor renderer color
  moveTo renderer a
  void (renderLineTo renderer b)
  void (renderLineTo renderer c)
  void (renderLineTo renderer a)
  fill renderer
  where
    (a, b, c)
      | reverse = (p1, p2, p4)
      | otherwise = (p2, p4, p3)
    Rect x y w h = rect
    p1 = Point x y
    p2 = Point (x + w) y
    p3 = Point x (y + h)
    p4 = Point (x + w) (y + h)

accentColor :: WidgetEnv s e -> Color
accentColor wenv = transColor
  where
    style = wenv ^. L.theme . L.basic . L.btnStyle
    color = fromMaybe (rgb 255 255 255) (_sstText style >>= _txsFontColor)
    transColor = color {_colorA = 0.7}

textColumn :: Text -> (a -> Text) -> Int -> ColumnDef e a
textColumn _cdName get _cdInitialWidth =
  ColumnDef
    { _cdName,
      _cdWidget = \item -> label_ (get item) [ellipsis],
      _cdInitialWidth,
      _cdSortKey = SortWith get,
      _cdMinWidth = defaultMinColumnWidth
    }

showOrdColumn :: (Show b, Ord b) => Text -> (a -> b) -> Int -> ColumnDef e a
showOrdColumn _cdName get _cdInitialWidth =
  ColumnDef
    { _cdName,
      _cdWidget = \item -> label_ ((T.pack . show . get) item) [ellipsis],
      _cdInitialWidth,
      _cdSortKey = SortWith get,
      _cdMinWidth = defaultMinColumnWidth
    }

customColumn :: (Typeable a, Eq a, WidgetEvent e) => Text -> (forall s. a -> WidgetNode s e) -> Int -> MaybeSort a -> ColumnDef e a
customColumn _cdName get _cdInitialWidth _cdSortKey =
  ColumnDef
    { _cdName,
      _cdWidget = customColumnWidget get,
      _cdInitialWidth,
      _cdSortKey,
      _cdMinWidth = defaultMinColumnWidth
    }

defaultMinColumnWidth :: Int
defaultMinColumnWidth = 60

customColumnWidget ::
  forall ep a.
  (WidgetModel ep, Typeable a, Eq a) =>
  (a -> forall s. WidgetNode s ep) ->
  a ->
  WidgetNode (HaGridModel a) (HaGridEvent ep)
customColumnWidget get item =
  compositeD_ "HaGrid.Cell" (WidgetValue item) buildUI handleEvent []
  where
    buildUI :: forall s. UIBuilder s ep
    buildUI _wenv _model = get item

    handleEvent :: forall s. EventHandler s ep (HaGridModel a) (HaGridEvent ep)
    handleEvent _wenv _node _model e =
      [Report (ParentEvent e)]

headerButton :: WidgetEvent ep => Int -> ColumnDef ep a -> WidgetNode s (HaGridEvent ep)
headerButton colIndex ColumnDef {_cdName, _cdSortKey, _cdMinWidth} =
  button_ _cdName (OrderByColumn colIndex) [ellipsis]
    `styleBasic` [{- todo: textFont Font.bold, -} radius 0]

headerDragHandle :: WidgetEvent ep => Int -> ColumnDef ep a -> Int -> WidgetNode s (HaGridEvent ep)
headerDragHandle colIndex ColumnDef {_cdName, _cdSortKey, _cdMinWidth} columnWidth = tree
  where
    tree = defaultWidgetNode "HaGrid.HeaderDragHandle" (headerDragHandleWidget Nothing)

    headerDragHandleWidget state = single
      where
        single =
          createSingle
            state
            def
              { singleGetSizeReq = getSizeReq,
                singleGetBaseStyle = getBaseStyle,
                singleMerge = merge,
                singleHandleEvent = handleEvent,
                singleRender = render
              }

        getSizeReq _wenv _node =
          (fixedSize 6, SizeReq 24 0 1 1)

        getBaseStyle _wenv _node =
          Just def {_styleBasic = Just def {_sstCursorIcon = Just CursorSizeH}}

        merge _wenv newNode _oldNode oldState =
          -- preserve the drag state (this will be called continually as the column resizes)
          resultNode $ newNode & L.widget .~ headerDragHandleWidget oldState

        handleEvent wenv node _target = \case
          ButtonAction (Point _pX _pY) _btn BtnPressed _clicks -> Just result
            where
              -- todo: only if not focussed? set focus?
              result = resultReqs newNode [] -- todo: render? (to show now dragging?)
              newNode = node & L.widget .~ headerDragHandleWidget newState
              newState = Just (HeaderDragHandleState _pX columnWidth)
          ButtonAction _point _btn BtnReleased _clicks -> Just result
            where
              result = resultReqs newNode [] -- todo: render? (to show no-longer dragging?)
              newNode = node & L.widget .~ headerDragHandleWidget Nothing
          Move (Point _pX _pY) -> Just result
            where
              result
                | Just nw <- newColumnW =
                    resizeRequest & L.requests
                      <>~ S.fromList
                        [RaiseEvent (ResizeColumn colIndex nw)]
                | otherwise =
                    resultReqs node []
              newColumnW = do
                HeaderDragHandleState clickX columnW <- state
                pure (max _cdMinWidth (columnW + fromFractional (_pX - clickX)))
          _ -> Nothing
          where
            resizeRequest = widgetResize (node ^. L.widget) wenv node vp (const True)
            vp = node ^. L.info . L.viewport

        render :: WidgetEnv s e -> WidgetNode s e -> Renderer -> IO ()
        render wenv node renderer = do
          drawRect renderer vp (Just (accentColor wenv)) Nothing
          where
            vp = node ^. L.info . L.viewport

initialModel :: [ColumnDef ep a] -> [a] -> HaGridModel a
initialModel columnDefs items =
  sortItems columnDefs $
    HaGridModel
      { _mSortedItems = items,
        _mColumnWidths = _cdInitialWidth <$> columnDefs,
        _mSortColumn = Nothing,
        _mSortReverse = False
      }

headerPaneKey :: Text
headerPaneKey = "HaGrid.headerPane"

contentPaneKey :: Text
contentPaneKey = "HaGrid.contentPane"

sortItems :: [ColumnDef ep a] -> HaGridModel a -> HaGridModel a
sortItems columnDefs model
  | Just sc <- model ^. sortColumn,
    Just ColumnDef {_cdSortKey = SortWith f} <- columnDefs !? sc =
     if model ^. sortReverse
        then model & sortedItems %~ List.sortOn (Down . f)
        else model & sortedItems %~ List.sortOn f
  | otherwise =
     model

cellSizes :: Seq SizeReq -> Double -> Seq Double
cellSizes reqs available = reqResult <$> reqs
  where
    totalFixed = sum $ _szrFixed <$> reqs
    totalFlex = sum $ _szrFlex <$> reqs
    totalWeightedFlex = sum $ (\r -> _szrFlex r * _szrFactor r) <$> reqs
    totalWeightedExtra = sum $ (\r -> _szrExtra r * _szrFactor r) <$> reqs

    availableFlex = max 0 $ min (available - totalFixed) totalFlex
    availableExtra = available - totalFixed - availableFlex

    reqResult r
      | availableFlex >= totalFlex =
          if availableExtra > 0 && totalWeightedExtra > 0
            then
              let extraProp = _szrExtra r * _szrFactor r / totalWeightedExtra
               in _szrFixed r + _szrFlex r + availableExtra * extraProp
            else _szrFixed r + _szrFlex r
      | totalWeightedFlex > 0 =
          let flexProp = _szrFlex r * _szrFactor r / totalWeightedFlex
           in _szrFixed r + availableFlex * flexProp
      | otherwise =
          _szrFixed r

sizesToPositions :: Seq Double -> Seq Double
sizesToPositions = S.scanl (+) 0

toRowHeights :: Seq (WidgetNode s e) -> Int -> Seq SizeReq
toRowHeights children nCols = mergeHeights <$> S.chunksOf nCols children
  where
    mergeHeights widgets =
      foldl' sizeReqMergeMax (fixedSize 0) (_wniSizeReqH . _wnInfo <$> widgets)
