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

module HaGrid
  ( ColumnDef,
    ColumnSortKey (..),
    haGrid,
    textColumn,
    showOrdColumn,
    widgetColumn,
    columnInitialWidth,
    columnMinWidth,
    columnSortKey,
    columnPadding,
    columnPaddingW,
    columnPaddingH,
  )
where

import Control.Lens (abbreviatedFields, makeLensesWith, (%~), (.~), (<>~), (?~), (^.))
import Control.Lens.Combinators (non)
import Control.Lens.Lens ((&))
import Control.Monad as X (forM_)
import Data.Default.Class as X (def)
import Data.Foldable (foldl')
import qualified Data.List as List
import Data.List.Extra as X ((!?))
import Data.List.Index (indexed, izipWith, setAt)
import Data.Maybe (fromJust)
import Data.Maybe as X (fromMaybe)
import Data.Ord (Down (Down))
import Data.Sequence (Seq ((:<|), (:|>)))
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

data ColumnDef e a = ColumnDef
  { _cdName :: Text,
    _cdWidget :: a -> WidgetNode (HaGridModel a) (HaGridEvent e),
    _cdSortKey :: ColumnSortKey a,
    _cdInitialWidth :: Int,
    _cdMinWidth :: Int,
    _cdPaddingW :: Double,
    _cdPaddingH :: Double
  }

data ColumnSortKey a where
  DontSort :: ColumnSortKey a
  SortWith :: Ord b => (a -> b) -> ColumnSortKey a

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
    buildUI _wenv model@HaGridModel {..} = tree
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

        headerWidgets =
          mconcat (izipWith headerWidgetPair columnDefs _mColumnWidths)
        headerWidgetPair i columnDef columnWidth = [btn, handle]
          where
            btn = headerButton i columnDef
            handle = headerDragHandle i columnDef columnWidth
        childWidgetRows =
          [[_cdWidget item | ColumnDef {_cdWidget} <- columnDefs] | item <- _mSortedItems]

        nRows = length items
        columnDefsSeq = S.fromList columnDefs

        dragHandleWidth = 4
        dragHandleHeight = 40

        contentPaneContainer =
          createContainer
            model
            def
              { containerInit = contentInitHandler,
                containerMerge = contentMerge,
                containerGetSizeReq = contentGetSizeReq,
                containerResize = contentResize,
                containerRender = contentRender,
                containerHandleEvent = contentHandleEvent
              }
        
        contentInitHandler _wenv node = resultNode newNode where
          newNode = node
            & L.children .~ S.fromList (mconcat childWidgetRows)
          
        contentMerge _wenv node oldNode HaGridModel{_mSortedItems = oldSortedItems} = resultNode newNode where
          newNode = node & L.children .~ nodeChildren
          -- re-use the old children if possible, since creating all
          -- those labels does lots of expensive font-layout stuff
          nodeChildren
            | _mSortedItems == oldSortedItems = oldNode ^. L.children
            | otherwise = S.fromList (mconcat childWidgetRows)
        
        contentGetSizeReq _wenv _node children = (w, h)
          where
            w = fixedSize (fromIntegral (sum _mColumnWidths))
            h = fixedSize (sum (toRowHeights children columnDefsSeq))

        contentResize wenv node viewport children = (resultNode node, assignedAreas)
          where
            style = currentStyle wenv node
            Rect l t _w _h = fromMaybe def (removeOuterBounds style viewport)

            colXs = sizesToPositions (S.fromList (fromIntegral <$> _mColumnWidths))
            rowYs = sizesToPositions (toRowHeights children columnDefsSeq)

            assignedAreas = S.fromList $ do
              row <- [0 .. nRows - 1]
              (col, columnDef) <- indexed columnDefs
              pure (assignArea col columnDef row)

            assignArea col ColumnDef {_cdPaddingW, _cdPaddingH} row = Rect chX chY chW chH
              where
                chX = l + S.index colXs col + _cdPaddingW
                chY = t + S.index rowYs row + _cdPaddingH
                chW = S.index colXs (col + 1) - S.index colXs col - _cdPaddingW * 2
                chH = S.index rowYs (row + 1) - S.index rowYs row - _cdPaddingH * 2

        contentRender wenv node renderer = do
          forM_ (neighbours rowYs) $ \(y1, y2, even) -> do
            let color
                  | mouseover && _pY mouse >= (t + y1) && _pY mouse < (t + y2) = Just mouseOverColor
                  | not even = Just oddRowBgColor
                  | otherwise = Nothing
            drawRect renderer (Rect l (t + y1) lastColX (y2 - y1)) color Nothing

          forM_ (S.drop 1 colXs) $ \colX -> do
            drawLine renderer (Point (l + colX) t) (Point (l + colX) (t + lastRowY)) 1 (Just lineColor)

          forM_ (S.drop 1 rowYs) $ \rowY -> do
            drawLine renderer (Point l (t + rowY)) (Point (l + lastColX) (t + rowY)) 1 (Just lineColor)
          where
            colXs = sizesToPositions (S.fromList (fromIntegral <$> _mColumnWidths))
            rowYs = sizesToPositions (toRowHeights (node ^. L.children) columnDefsSeq)
            lastColX
              | _ :|> a <- colXs = a
              | otherwise = 0
            lastRowY
              | _ :|> a <- rowYs = a
              | otherwise = 0
            vp = node ^. L.info . L.viewport
            Rect l t _w _h = vp
            mouseover = pointInRect mouse vp
            mouse = wenv ^. L.inputStatus . L.mousePos
            mouseOverColor = (accentColor wenv) {_colorA = 0.3}
            oddRowBgColor = (accentColor wenv) {_colorA = 0.1}
            lineColor = accentColor wenv

        contentHandleEvent _wenv node _path = \case
          Move (Point _pX _pY) ->
            -- refresh which row shows as hovered
            Just (resultReqs node [RenderOnce])
          _ -> Nothing

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
        | Just ColumnDef {_cdSortKey = DontSort} <- columnDefs !? colIndex ->
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

drawSortIndicator :: Renderer -> Rect -> Maybe Color -> Bool -> IO ()
drawSortIndicator renderer rect color reverse = drawCmd
  where
    drawCmd
      | reverse = drawTriangle renderer p1 p2 p4 color
      | otherwise = drawTriangle renderer p2 p4 p3 color
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

textColumn :: Text -> (a -> Text) -> ColumnDef e a
textColumn _cdName get =
  ColumnDef
    { _cdName,
      _cdWidget = \item -> label_ (get item) [ellipsis],
      _cdInitialWidth = defaultColumnInitialWidth,
      _cdSortKey = SortWith get,
      _cdMinWidth = defaultColumnMinWidth,
      _cdPaddingW = defaultColumnPadding,
      _cdPaddingH = defaultColumnPadding
    }

showOrdColumn :: (Show b, Ord b) => Text -> (a -> b) -> ColumnDef e a
showOrdColumn _cdName get =
  ColumnDef
    { _cdName,
      _cdWidget = \item -> label_ ((T.pack . show . get) item) [ellipsis],
      _cdInitialWidth = defaultColumnInitialWidth,
      _cdSortKey = SortWith get,
      _cdMinWidth = defaultColumnMinWidth,
      _cdPaddingW = defaultColumnPadding,
      _cdPaddingH = defaultColumnPadding
    }

widgetColumn :: (Typeable a, Eq a, WidgetEvent e) => Text -> (forall s. a -> WidgetNode s e) -> ColumnDef e a
widgetColumn _cdName get =
  ColumnDef
    { _cdName,
      _cdWidget = customColumnWidget get,
      _cdInitialWidth = defaultColumnInitialWidth,
      _cdSortKey = DontSort,
      _cdMinWidth = defaultColumnMinWidth,
      _cdPaddingW = defaultColumnPadding,
      _cdPaddingH = defaultColumnPadding
    }

columnInitialWidth :: ColumnDef e a -> Int -> ColumnDef e a
columnInitialWidth c w = c {_cdInitialWidth = w}

columnMinWidth :: ColumnDef e a -> Int -> ColumnDef e a
columnMinWidth c w = c {_cdMinWidth = w}

columnSortKey :: ColumnDef e a -> ColumnSortKey a -> ColumnDef e a
columnSortKey c k = c {_cdSortKey = k}

columnPadding :: ColumnDef e a -> Double -> ColumnDef e a
columnPadding c p = columnPaddingH (columnPaddingW c p) p

columnPaddingW :: ColumnDef e a -> Double -> ColumnDef e a
columnPaddingW c p = c {_cdPaddingW = p}

columnPaddingH :: ColumnDef e a -> Double -> ColumnDef e a
columnPaddingH c p = c {_cdPaddingH = p}

defaultColumnInitialWidth :: Int
defaultColumnInitialWidth = 100

defaultColumnMinWidth :: Int
defaultColumnMinWidth = 60

defaultColumnPadding :: Double
defaultColumnPadding = 10

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
              { singleGetBaseStyle = getBaseStyle,
                singleMerge = merge,
                singleHandleEvent = handleEvent,
                singleRender = render
              }

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

sizesToPositions :: Seq Double -> Seq Double
sizesToPositions = S.scanl (+) 0

toRowHeights :: Seq (WidgetNode s e1) -> Seq (ColumnDef e2 a) -> Seq Double
toRowHeights children columnDefs = mergeHeights <$> S.chunksOf (length columnDefs) children
  where
    mergeHeights rowWidgets =
      foldl' max 0 (S.zipWith widgetHeight columnDefs rowWidgets)

    widgetHeight ColumnDef {_cdPaddingH} widget =
      widget
        & _wnInfo
        & _wniSizeReqH
        & _szrFixed
        & (+ (_cdPaddingH * 2))

neighbours :: Seq a -> Seq (a, a, Bool)
neighbours = \case
  a :<| b :<| c :<| rest -> (a, b, False) :<| (b, c, True) :<| neighbours (c :<| rest)
  a :<| b :<| S.Empty -> S.singleton (a, b, False)
  _ -> S.empty
