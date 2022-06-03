{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- A datagrid widget for the Monomer UI library.
module Monomer.Hagrid
  ( -- * Types
    Column (..),
    ColumnWidget (..),
    ColumnSortKey (..),
    SortDirection (..),

    -- * Hagrid constructor
    hagrid,

    -- * Column constructors
    textColumn,
    showOrdColumn,
    widgetColumn,
  )
where

import Control.Lens ((.~), (<>~), (^.))
import Control.Lens.Combinators (non)
import Control.Lens.Lens ((&))
import Control.Monad as X (forM_)
import Data.Default.Class as X (def)
import Data.Foldable (foldl')
import qualified Data.List as List
import Data.List.Index (indexed, izipWith, modifyAt)
import Data.Maybe (fromJust, maybeToList)
import Data.Maybe as X (fromMaybe)
import Data.Ord (Down (Down))
import Data.Sequence (Seq ((:<|), (:|>)))
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import Monomer
import qualified Monomer.Lens as L
import Monomer.Widgets.Container
import Monomer.Widgets.Single

data HagridEvent ep
  = OrderByColumn Int
  | ResizeColumn Int Int
  | ResizeColumnFinished Int
  | ParentEvent ep

-- | A column definition.
data Column e a = Column
  { -- | The name of the column, displayed in the column header.
    name :: Text,
    -- | Creates the widget for each cell in the column.
    widget :: ColumnWidget e a,
    -- | Determines if and how the column can be sorted by clicking the column header.
    sortKey :: ColumnSortKey a,
    -- | The initial width of the column, in pixels. The user can then change the
    -- width by dragging the edge of the column header.
    initialWidth :: Int,
    -- | The minimum allowed width of the column, in pixels.
    minWidth :: Int,
    -- | The padding to the left and right of the widget in each cell of the column, in pixels.
    paddingW :: Double,
    -- | The padding above and below the widget in each cell in the column, in pixels.
    paddingH :: Double,
    -- | An optional event to emit when a user has finished resizing the column. The function receives the new width in pixels.
    resizeHandler :: Maybe (Int -> e),
    -- | An optional event to emit when a user has sorted the column by clicking the header. The function receives the new sort direction.
    sortHandler :: Maybe (SortDirection -> e)
  }

-- | How to create the widget that displays each cell in a column.
data ColumnWidget e a where
  -- | Create a label widget.
  LabelWidget :: (a -> Text) -> ColumnWidget e a
  -- | Create a widget of arbitrary type.
  CustomWidget :: (forall s. a -> WidgetNode s e) -> ColumnWidget e a

-- | Whether a column can be sorted by the user clicking the column header, and if so, how.
data ColumnSortKey a where
  -- | Means that a column can't be sorted.
  DontSort :: ColumnSortKey a
  -- | Means that a column can be sorted, using the specified sort key function.
  SortWith :: Ord b => (a -> b) -> ColumnSortKey a

-- | Whether a column is being sorted in ascending or descending order.
data SortDirection
  = SortAscending
  | SortDescending
  deriving (Eq, Show)

data HagridModel a = HagridModel
  { sortedItems :: [a],
    columns :: [ModelColumn],
    sortColumn :: Maybe Int,
    sortDirection :: SortDirection
  }
  deriving (Eq, Show)

data ModelColumn = ModelColumn
  { currentWidth :: Int,
    name :: Text
  }
  deriving (Eq, Show)

data HeaderDragHandleState = HeaderDragHandleState
  { dragStartMouseX :: Double,
    dragStartColumnW :: Int
  }
  deriving (Eq, Show)

-- | Creates a hagrid widget.
hagrid ::
  forall a s e.
  (CompositeModel a, WidgetModel s, WidgetEvent e) =>
  -- | The definitions for each column in the grid.
  [Column e a] ->
  -- | The items for each row in the grid.
  [a] ->
  WidgetNode s e
hagrid columnDefs items = widget
  where
    -- todo: accept lens ?

    widget =
      compositeD_
        "Hagrid.Root"
        (WidgetValue (initialModel columnDefs items))
        buildUI
        handleEvent
        [compositeMergeModel mergeModel]

    buildUI :: UIBuilder (HagridModel a) (HagridEvent e)
    buildUI _wenv model = tree
      where
        tree =
          hscroll $
            vstack
              [ headerPane columnDefs model `nodeKey` headerPaneKey,
                vscroll (contentPane columnDefs model `nodeKey` contentPaneKey)
              ]

    handleEvent :: EventHandler (HagridModel a) (HagridEvent e) sp e
    handleEvent wenv _node model = \case
      OrderByColumn colIndex -> result
        where
          Column {sortHandler} = columnDefs !! colIndex
          newModel
            | Just c <- model.sortColumn,
              c == colIndex =
                model {sortDirection = flipSortDirection model.sortDirection}
            | otherwise =
                model {sortColumn = Just colIndex, sortDirection = SortAscending}
          result =
            Model (sortItems columnDefs newModel) : handler
          handler =
            Report <$> maybeToList (sortHandler <*> Just newModel.sortDirection)
      ResizeColumn colIndex newWidth ->
        [ Model (model {columns = modifyAt colIndex (\c -> c {currentWidth = newWidth}) model.columns}),
          Request (ResizeWidgets headerPaneId),
          Request (ResizeWidgets contentPaneId)
        ]
      ResizeColumnFinished colIndex -> result
        where
          ModelColumn {currentWidth} = model.columns !! colIndex
          Column {resizeHandler} = columnDefs !! colIndex
          result =
            Report <$> maybeToList (resizeHandler <*> Just currentWidth)
      ParentEvent e ->
        [Report e]
      where
        headerPaneId = fromJust (widgetIdFromKey wenv (WidgetKey headerPaneKey))
        contentPaneId = fromJust (widgetIdFromKey wenv (WidgetKey contentPaneKey))

    -- If the column names have not changed then preseve the column widths too,
    -- otherwise item/theme/etc changes will reset the column widths
    mergeModel :: MergeModelHandler (HagridModel a) (HagridEvent e) s
    mergeModel _wenv _parentModel oldModel newModel = resultModel
      where
        resultModel
          | columnNames oldModel == columnNames newModel =
              newModel {columns = oldModel.columns}
          | otherwise =
              newModel
        columnNames m =
          (.name) <$> columns m

drawSortIndicator :: Renderer -> Rect -> Maybe Color -> SortDirection -> IO ()
drawSortIndicator renderer rect color dir = drawCmd
  where
    drawCmd = case dir of
      SortAscending -> drawTriangle renderer p2 p4 p3 color
      SortDescending -> drawTriangle renderer p1 p2 p4 color
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

headerPane :: WidgetEvent ep => [Column ep a] -> HagridModel a -> WidgetNode s (HagridEvent ep)
headerPane columnDefs model = node
  where
    node =
      defaultWidgetNode "Hagrid.HeaderPane" headerPaneContainer
        & L.children .~ S.fromList headerWidgets

    dragHandleWidth = 4
    dragHandleHeight = 40

    headerWidgets =
      mconcat (izipWith headerWidgetPair columnDefs model.columns)

    headerWidgetPair i columnDef column = [btn, handle]
      where
        btn = headerButton i columnDef
        handle = headerDragHandle i columnDef column

    headerPaneContainer =
      createContainer
        model.columns
        def
          { containerGetSizeReq = headerGetSizeReq,
            containerResize = headerResize,
            containerRenderAfter = headerRenderAfter
          }

    headerGetSizeReq _wenv _node _children = (w, h)
      where
        w = fixedSize (sum (fromIntegral . currentWidth <$> model.columns))
        h = fixedSize dragHandleHeight

    headerResize _wenv node viewport _children = (resultNode node, assignedAreas)
      where
        Rect l t _w h = viewport
        widgetWidths = do
          w <- currentWidth <$> model.columns
          [w - dragHandleWidth, dragHandleWidth]
        (assignedAreas, _) = foldl' assignArea (mempty, l) widgetWidths
        assignArea (areas, colX) columnWidth =
          (areas S.:|> Rect colX t (fromIntegral columnWidth) h, colX + fromIntegral columnWidth)

    headerRenderAfter wenv node renderer =
      forM_ model.sortColumn (renderSortIndicator wenv node renderer)

    renderSortIndicator wenv node renderer sortCol = do
      drawSortIndicator renderer indRect (Just (accentColor wenv)) model.sortDirection
      where
        style = wenv ^. L.theme . L.basic . L.btnStyle
        Rect l t _w h = node ^. L.info . L.viewport
        size = style ^. L.text . non def . L.fontSize . non def
        colOffset = fromIntegral (sum (take (sortCol + 1) (currentWidth <$> model.columns)) - dragHandleWidth)
        indW = unFontSize size * 2 / 3
        pad = indW / 3
        indT = case model.sortDirection of
          SortAscending -> h - pad - indW
          SortDescending -> t + pad
        indL = l + colOffset - indW - pad
        indRect = Rect indL indT indW indW

headerButton :: WidgetEvent ep => Int -> Column ep a -> WidgetNode s (HagridEvent ep)
headerButton colIndex columnDef =
  button_ columnDef.name (OrderByColumn colIndex) [ellipsis]
    `styleBasic` [radius 0]

headerDragHandle :: WidgetEvent ep => Int -> Column ep a -> ModelColumn -> WidgetNode s (HagridEvent ep)
headerDragHandle colIndex columnDef column = tree
  where
    tree = defaultWidgetNode "Hagrid.HeaderDragHandle" (headerDragHandleWidget Nothing)

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
              result = resultNode newNode
              newNode = node & L.widget .~ headerDragHandleWidget newState
              newState = Just (HeaderDragHandleState _pX column.currentWidth)
          ButtonAction _point _btn BtnReleased _clicks -> Just result
            where
              result = resultReqs newNode [RaiseEvent (ResizeColumnFinished colIndex)]
              newNode = node & L.widget .~ headerDragHandleWidget Nothing
          Move (Point _pX _pY) -> Just result
            where
              result
                | Just nw <- newColumnW =
                    resizeRequest
                      & L.requests
                        <>~ S.fromList
                          [RaiseEvent (ResizeColumn colIndex nw)]
                | otherwise =
                    resultReqs node []
              newColumnW = do
                HeaderDragHandleState clickX columnW <- state
                pure (max columnDef.minWidth (columnW + fromFractional (_pX - clickX)))
          _ -> Nothing
          where
            resizeRequest = widgetResize (node ^. L.widget) wenv node vp (const True)
            vp = node ^. L.info . L.viewport

        render :: WidgetEnv s e -> WidgetNode s e -> Renderer -> IO ()
        render wenv node renderer = do
          drawRect renderer vp (Just (accentColor wenv)) Nothing
          where
            vp = node ^. L.info . L.viewport

contentPane :: (CompositeModel a, WidgetEvent ep) => [Column ep a] -> HagridModel a -> WidgetNode (HagridModel a) (HagridEvent ep)
contentPane columnDefs model = node
  where
    node =
      defaultWidgetNode "Hagrid.ContentPane" contentPaneContainer
        & L.children .~ S.fromList (mconcat childWidgetRows)

    childWidgetRows =
      [[cellWidget item widget | Column {widget} <- columnDefs] | item <- model.sortedItems]

    nRows = length model.sortedItems
    columnDefsSeq = S.fromList columnDefs

    contentPaneContainer =
      createContainer
        model
        def
          { containerGetSizeReq = contentGetSizeReq,
            containerResize = contentResize,
            containerRender = contentRender,
            containerHandleEvent = contentHandleEvent
          }

    contentGetSizeReq _wenv _node children = (w, h)
      where
        w = fixedSize (sum (fromIntegral . currentWidth <$> model.columns))
        h = fixedSize (sum (toRowHeights children columnDefsSeq))

    contentResize wenv node viewport children = (resultNode node, assignedAreas)
      where
        style = currentStyle wenv node
        Rect l t _w _h = fromMaybe def (removeOuterBounds style viewport)

        colXs = sizesToPositions (S.fromList (fromIntegral . currentWidth <$> model.columns))
        rowYs = sizesToPositions (toRowHeights children columnDefsSeq)

        assignedAreas = S.fromList $ do
          row <- [0 .. nRows - 1]
          (col, columnDef) <- indexed columnDefs
          pure (assignArea col columnDef row)

        assignArea col Column {paddingW, paddingH} row = Rect chX chY chW chH
          where
            chX = l + S.index colXs col + paddingW
            chY = t + S.index rowYs row + paddingH
            chW = S.index colXs (col + 1) - S.index colXs col - paddingW * 2
            chH = S.index rowYs (row + 1) - S.index rowYs row - paddingH * 2

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
        colXs = sizesToPositions (S.fromList (fromIntegral . currentWidth <$> model.columns))
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

initialModel :: [Column ep a] -> [a] -> HagridModel a
initialModel columnDefs items = model
  where
    model =
      sortItems columnDefs $
        HagridModel
          { sortedItems = items,
            columns = initialColumn <$> columnDefs,
            sortColumn = Nothing,
            sortDirection = SortAscending
          }
    initialColumn Column {name, initialWidth} =
      ModelColumn
        { name,
          currentWidth = initialWidth
        }

headerPaneKey :: Text
headerPaneKey = "Hagrid.headerPane"

contentPaneKey :: Text
contentPaneKey = "Hagrid.contentPane"

sortItems :: [Column ep a] -> HagridModel a -> HagridModel a
sortItems columnDefs model = case model.sortColumn of
  Just sc
    | Column {sortKey = SortWith f} <- columnDefs !! sc ->
        case model.sortDirection of
          SortAscending -> model {sortedItems = List.sortOn f model.sortedItems}
          SortDescending -> model {sortedItems = List.sortOn (Down . f) model.sortedItems}
  _ ->
    model

sizesToPositions :: Seq Double -> Seq Double
sizesToPositions = S.scanl (+) 0

toRowHeights :: Seq (WidgetNode s e1) -> Seq (Column e2 a) -> Seq Double
toRowHeights children columnDefs = mergeHeights <$> S.chunksOf (length columnDefs) children
  where
    mergeHeights rowWidgets =
      foldl' max 0 (S.zipWith widgetHeight columnDefs rowWidgets)

    widgetHeight Column {paddingH} widget =
      widget
        & _wnInfo
        & _wniSizeReqH
        & \r -> _szrFixed r + _szrFlex r + paddingH * 2

neighbours :: Seq a -> Seq (a, a, Bool)
neighbours = \case
  a :<| b :<| c :<| rest -> (a, b, False) :<| (b, c, True) :<| neighbours (c :<| rest)
  a :<| b :<| S.Empty -> S.singleton (a, b, False)
  _ -> S.empty

-- | Creates a column that displays a text value, and is sortable by the text.
textColumn ::
  -- | Name of the column, to display in the header.
  Text ->
  -- | Called with the item for each row to get the text to display for that row.
  (a -> Text) ->
  Column e a
textColumn name get = (defaultColumn name widget) {sortKey}
  where
    widget = LabelWidget get
    sortKey = SortWith get

-- | Creates a column that displays the result of calling @'show'@ on a value, and is sortable by the value.
showOrdColumn ::
  (Show b, Ord b) =>
  -- | Name of the column, to display in the header.
  Text ->
  -- | Called with the item for each row to get the value to display (via @'show'@) and sort by.
  (a -> b) ->
  Column e a
showOrdColumn name get = (defaultColumn name widget) {sortKey}
  where
    widget = LabelWidget (T.pack . show . get)
    sortKey = SortWith get

-- | Creates a column that displays the a custom widget in each cell.
widgetColumn ::
  -- | Name of the column, to display in the header.
  Text ->
  -- | Called with the item for each row to get the widget to display for that row.
  (forall s. a -> WidgetNode s e) ->
  Column e a
widgetColumn name get = defaultColumn name (CustomWidget get)

defaultColumn :: Text -> ColumnWidget e a -> Column e a
defaultColumn name widget =
  Column
    { name,
      widget,
      initialWidth = defaultColumnInitialWidth,
      sortKey = DontSort,
      minWidth = defaultColumnMinWidth,
      paddingW = defaultColumnPadding,
      paddingH = defaultColumnPadding,
      resizeHandler = Nothing,
      sortHandler = Nothing
    }

cellWidget :: (CompositeModel a, WidgetEvent e, WidgetModel s) => a -> ColumnWidget e a -> WidgetNode (HagridModel s) (HagridEvent e)
cellWidget item = \case
  LabelWidget get -> label_ (get item) [ellipsis]
  CustomWidget get -> widget
    where
      widget =
        compositeD_ "Hagrid.Cell" (WidgetValue item) buildUI handleEvent []
      buildUI _wenv model =
        get model
      handleEvent _wenv _node _model e =
        [Report (ParentEvent e)]

defaultColumnInitialWidth :: Int
defaultColumnInitialWidth = 100

defaultColumnMinWidth :: Int
defaultColumnMinWidth = 60

defaultColumnPadding :: Double
defaultColumnPadding = 10

flipSortDirection :: SortDirection -> SortDirection
flipSortDirection SortAscending = SortDescending
flipSortDirection SortDescending = SortAscending
