{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | A datagrid widget for the Monomer UI library.
module Monomer.Hagrid
  ( -- * Types
    HagridCfg,
    Column (..),
    ColumnAlign (..),
    ColumnWidget (..),
    ColumnFooterWidget (..),
    ColumnSortKey (..),
    SortDirection (..),
    ItemWithIndex,
    ScrollToRowCallback,

    -- * Configuration options
    initialSort,

    -- * Hagrid constructors
    hagrid,
    hagrid_,

    -- * Column Constructors
    textColumn,
    showOrdColumn,
    widgetColumn,

    -- * Messages
    scrollToRow,
  )
where

import Control.Applicative ((<|>))
import Control.Lens ((.~), (<>~), (^.))
import Control.Lens.Combinators (non)
import Control.Lens.Lens ((&))
import Control.Monad as X (forM_)
import Data.Data (Typeable)
import Data.Default.Class as X (Default, def)
import Data.Foldable (foldl')
import qualified Data.List as List
import Data.List.Index (indexed, izipWith, modifyAt)
import Data.Maybe (catMaybes, fromJust, isNothing, maybeToList)
import Data.Maybe as X (fromMaybe)
import Data.Ord (Down (Down))
import Data.Sequence (Seq ((:<|), (:|>)))
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (cast)
import Monomer
import qualified Monomer.Lens as L
import Monomer.Widgets.Container
import Monomer.Widgets.Single

-- | Configuration options for Hagrid widgets.
newtype HagridCfg s e = HagridCfg
  { cfgInitialSort :: Maybe (Int, SortDirection)
  }

instance Default (HagridCfg s e) where
  def = HagridCfg {cfgInitialSort = Nothing}

instance Semigroup (HagridCfg s e) where
  c1 <> c2 = HagridCfg {cfgInitialSort = c2.cfgInitialSort <|> c1.cfgInitialSort}

instance Monoid (HagridCfg s e) where
  mempty = def

-- | Configures the initial sort column and direction.
initialSort ::
  -- | The initial sort column (zero-indexed, out of bounds values will have no effect).
  Int ->
  -- | The initial sort direction.
  SortDirection ->
  HagridCfg s e
initialSort column direction =
  HagridCfg {cfgInitialSort = Just (column, direction)}

-- | A column definition.
data Column e a = Column
  { -- | The name of the column, displayed in the column header.
    name :: Text,
    -- | Creates the widget for each cell in the column.
    widget :: ColumnWidget e a,
    -- | Creates the widget for the column footer, if any.
    footerWidget :: ColumnFooterWidget e a,
    -- | How to align the widget within each cell in the column.
    align :: ColumnAlign,
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
data ColumnWidget e a
  = -- | Create a label widget.. The function receives the original item index (i.e.
    -- not the index in the sorted list) and the item itself.
    LabelWidget (Int -> a -> Text)
  | -- | Create a widget of arbitrary type. The function receives the original item
    -- index (i.e. not the index in the sorted list) and the item itself.
    CustomWidget (forall s. WidgetModel s => Int -> a -> WidgetNode s e)

-- | How to create the footer widget for a column.
data ColumnFooterWidget e a
  = -- | No footer widget for this column.
    NoFooterWidget
  | -- | Create a footer widget. The function receives the items in their current sort
    -- order, and also along with each item it's original (unsorted) index.
    CustomFooterWidget (forall s. WidgetModel s => [ItemWithIndex a] -> WidgetNode s e)

-- | How to align the widget within each cell of a column.
data ColumnAlign
  = ColumnAlignLeft
  | ColumnAlignRight
  deriving (Eq, Show)

-- | Whether a column can be sorted by the user clicking the column header, and if so, how.
data ColumnSortKey a
  = -- | Means that a column can't be sorted.
    DontSort
  | -- | Means that a column can be sorted, using the specified sort key function.
    forall b. Ord b => SortWith (a -> b)

-- | Whether a column is being sorted in ascending or descending order.
data SortDirection
  = SortAscending
  | SortDescending
  deriving (Eq, Show)

-- | A item in the grid, with its row index.
type ItemWithIndex a = (a, Int)

-- | Picks an item to scroll to, based on the sorted or original grid contents.
type ScrollToRowCallback a =
  -- | The items in the grid, in the originally provided order, along with each item's index
  -- in the current grid order.
  [ItemWithIndex a] ->
  -- | The row to scroll to, as an index into the sorted items (e.g. 0 is always the first row
  -- in the grid, regardless of the current order). 'Nothing' will cancel the scroll.
  Maybe Int

data HagridEvent ep
  = ContentScrollChange ScrollStatus
  | OrderByColumn Int
  | ResizeColumn Int Int
  | ResizeColumnFinished Int
  | forall a. Typeable a => ScrollToRow (ScrollToRowCallback a)
  | ParentEvent ep

data HagridModel a = HagridModel
  { sortedItems :: [ItemWithIndex a],
    columns :: [ModelColumn],
    sortColumn :: Maybe (Int, SortDirection)
  }
  deriving (Eq, Show)

data ModelColumn = ModelColumn
  { currentWidth :: Int,
    name :: Text
  }
  deriving (Eq, Show)

-- | The state of the header or footer, which have a scroll offset because they
-- scroll horizontally along with the content pane.
newtype OffsetXState = OffsetXState
  { offsetX :: Double
  }
  deriving (Eq, Show)

newtype OffsetXEvent = SetOffsetX Double

data HeaderDragHandleState = HeaderDragHandleState
  { dragStartMouseX :: Double,
    dragStartColumnW :: Int
  }
  deriving (Eq, Show)

newtype ContentPaneMessage a
  = ContentPaneScrollToRow (ScrollToRowCallback a)

-- | Creates a hagrid widget, using the default configuration.
hagrid ::
  forall a s e.
  (CompositeModel a, WidgetModel s, WidgetEvent e) =>
  -- | The definitions for each column in the grid.
  [Column e a] ->
  -- | The items for each row in the grid.
  [a] ->
  WidgetNode s e
hagrid = hagrid_ def

-- | Creates a hagrid widget, using the given configuration.
hagrid_ ::
  forall a s e.
  (CompositeModel a, WidgetModel s, WidgetEvent e) =>
  [HagridCfg s e] ->
  -- | The definitions for each column in the grid.
  [Column e a] ->
  -- | The items for each row in the grid.
  [a] ->
  WidgetNode s e
hagrid_ cfg columnDefs items = widget
  where
    -- todo: accept lens ?

    widget =
      compositeD_
        "Hagrid.Root"
        (WidgetValue (initialModel cfg columnDefs items))
        buildUI
        handleEvent
        [compositeMergeModel mergeModel]

    buildUI :: UIBuilder (HagridModel a) (HagridEvent e)
    buildUI _wenv model = tree
      where
        tree =
          vstack
            [ headerPane columnDefs model `nodeKey` headerPaneKey,
              contentScroll `nodeKey` contentScrollKey,
              footerPane columnDefs model `nodeKey` footerPaneKey
            ]
        contentScroll =
          scroll_ [onChange ContentScrollChange] $
            contentPane columnDefs model `nodeKey` contentPaneKey

    handleEvent :: EventHandler (HagridModel a) (HagridEvent e) sp e
    handleEvent wenv _node model = \case
      ScrollToRow row ->
        [Message (WidgetKey contentPaneKey) (ContentPaneScrollToRow row)]
      ContentScrollChange ScrollStatus {scrollDeltaX} ->
        [ Message (WidgetKey headerPaneKey) (SetOffsetX scrollDeltaX),
          Message (WidgetKey footerPaneKey) (SetOffsetX scrollDeltaX)
        ]
      OrderByColumn colIndex -> result
        where
          Column {sortHandler, sortKey} = columnDefs !! colIndex
          (sortColumn, sortedItems)
            | Just (c, dir) <- model.sortColumn,
              c == colIndex =
                let sortColumn = Just (colIndex, flipSortDirection dir)
                    sortedItems = reverse model.sortedItems
                 in (sortColumn, sortedItems)
            | otherwise =
                let sortColumn = Just (colIndex, SortAscending)
                    sortedItems = sortItems columnDefs sortColumn model.sortedItems
                 in (sortColumn, sortedItems)
          result = case sortKey of
            DontSort -> []
            SortWith _ -> Model model {sortColumn, sortedItems} : handler
          handler =
            Report <$> maybeToList (sortHandler <*> (snd <$> sortColumn))
      ResizeColumn colIndex newWidth ->
        [ Model (model {columns = modifyAt colIndex (\c -> c {currentWidth = newWidth}) model.columns}),
          Request (ResizeWidgets headerPaneId),
          Request (ResizeWidgets footerPaneId),
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
        footerPaneId = fromJust (widgetIdFromKey wenv (WidgetKey footerPaneKey))
        contentPaneId = fromJust (widgetIdFromKey wenv (WidgetKey contentPaneKey))

    -- If the column names have not changed then preseve the column widths and sort
    -- order too, otherwise unrelated model changes will reset the column widths/sort.
    mergeModel :: MergeModelHandler (HagridModel a) (HagridEvent e) s
    mergeModel _wenv _parentModel oldModel newModel = resultModel
      where
        resultModel
          | columnNames oldModel == columnNames newModel =
              newModel
                { columns = oldModel.columns,
                  sortColumn = oldModel.sortColumn,
                  sortedItems = sortItems columnDefs oldModel.sortColumn newModel.sortedItems
                }
          | otherwise =
              newModel
        columnNames m =
          (.name) <$> m.columns

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

headerPane :: forall s ep a. WidgetEvent ep => [Column ep a] -> HagridModel a -> WidgetNode s (HagridEvent ep)
headerPane columnDefs model = makeNode (OffsetXState 0)
  where
    makeNode :: OffsetXState -> WidgetNode s (HagridEvent ep)
    makeNode state = node
      where
        node =
          defaultWidgetNode "Hagrid.HeaderPane" (makeWidget state)
            & L.children .~ S.fromList childWidgets

        childWidgets =
          mconcat (izipWith childWidgetPair columnDefs model.columns)

        childWidgetPair i columnDef column = [btn, handle]
          where
            btn = headerButton i columnDef
            handle = headerDragHandle i columnDef column

    makeWidget :: OffsetXState -> Widget s (HagridEvent ep)
    makeWidget state = container
      where
        container =
          createContainer
            state
            def
              { containerChildrenOffset = Just (Point state.offsetX 0),
                containerUpdateCWenv = updateCWenv,
                containerMerge = merge,
                containerHandleMessage = handleMessage,
                containerGetSizeReq = getSizeReq,
                containerResize = resize,
                containerRenderAfter = renderAfter
              }

        -- needed to ensure child widgets don't disappear when scrolling
        updateCWenv wenv node _cnode _cidx = newWenv
          where
            style = currentStyle wenv node
            carea = getContentArea node style
            newWenv =
              wenv
                & L.viewport .~ moveRect (Point (-state.offsetX) 0) carea

        -- keep the scroll offset from the old node
        merge _wenv node _oldNode oldState = resultNode newNode
          where
            newNode = node & L.widget .~ makeWidget oldState

        handleMessage :: ContainerMessageHandler s (HagridEvent ep)
        handleMessage _wenv node _target msg = result
          where
            handleTypedMessage (SetOffsetX offsetX)
              | offsetX == state.offsetX = Nothing
              | otherwise =
                  Just . resultNode $
                    node & L.widget .~ makeWidget state {offsetX}
            result = cast msg >>= handleTypedMessage

        getSizeReq _wenv _node _children = (w, h)
          where
            w = fixedSize (sum (fromIntegral . currentWidth <$> model.columns) + hScrollFudgeFactor)
            h = fixedSize dragHandleHeight

        resize _wenv node viewport _children = (resultNode node, assignedAreas)
          where
            Rect l t _w h = viewport
            widgetWidths = do
              w <- currentWidth <$> model.columns
              [w - dragHandleWidth, dragHandleWidth]
            (assignedAreas, _) = foldl' assignArea (mempty, l) widgetWidths
            assignArea (areas, colX) columnWidth =
              (areas :|> Rect colX t (fromIntegral columnWidth) h, colX + fromIntegral columnWidth)

        renderAfter wenv node renderer =
          forM_ model.sortColumn (renderSortIndicator wenv node renderer)

        renderSortIndicator wenv node renderer (sortCol, sortDirection) = do
          drawSortIndicator renderer indRect (Just (accentColor wenv)) sortDirection
          where
            style = wenv ^. L.theme . L.basic . L.btnStyle
            Rect l t _w h = node ^. L.info . L.viewport
            size = style ^. L.text . non def . L.fontSize . non def
            colOffset = fromIntegral (sum (take (sortCol + 1) (currentWidth <$> model.columns)) - dragHandleWidth)
            indW = unFontSize size * 2 / 3
            pad = indW / 3
            indT = case sortDirection of
              SortAscending -> t + h - pad - indW
              SortDescending -> t + pad
            indL = l + state.offsetX + colOffset - indW - pad
            indRect = Rect indL indT indW indW

headerButton :: WidgetEvent ep => Int -> Column ep a -> WidgetNode s (HagridEvent ep)
headerButton colIndex columnDef =
  button_ columnDef.name (OrderByColumn colIndex) [ellipsis]
    `styleBasic` [radius 0]

footerPane ::
  forall s ep a.
  (CompositeModel a, CompositeModel s, Typeable ep) =>
  [Column ep a] ->
  HagridModel a ->
  WidgetNode (HagridModel s) (HagridEvent ep)
footerPane columnDefs model = makeNode (OffsetXState 0)
  where
    makeNode :: OffsetXState -> WidgetNode (HagridModel s) (HagridEvent ep)
    makeNode state = node
      where
        node =
          defaultWidgetNode "Hagrid.FooterPane" (makeWidget state)
            & L.children .~ S.fromList (catMaybes childWidgets)

    childWidgets :: [Maybe (WidgetNode (HagridModel s) (HagridEvent ep))]
    childWidgets = footerWidgetNode model.sortedItems . footerWidget <$> columnDefs

    makeWidget :: OffsetXState -> Widget (HagridModel s) (HagridEvent ep)
    makeWidget state = container
      where
        container =
          createContainer
            state
            def
              { containerChildrenOffset = Just (Point state.offsetX 0),
                containerUpdateCWenv = updateCWenv,
                containerMerge = merge,
                containerHandleMessage = handleMessage,
                containerGetSizeReq = getSizeReq,
                containerResize = resize
              }

        -- needed to ensure child widgets don't disappear when scrolling
        updateCWenv wenv node _cnode _cidx = newWenv
          where
            style = currentStyle wenv node
            carea = getContentArea node style
            newWenv =
              wenv
                & L.viewport .~ moveRect (Point (-state.offsetX) 0) carea

        -- keep the scroll offset from the old node
        merge _wenv node _oldNode oldState = resultNode newNode
          where
            newNode = node & L.widget .~ makeWidget oldState

        handleMessage :: ContainerMessageHandler (HagridModel s) (HagridEvent ep)
        handleMessage _wenv node _target msg = result
          where
            handleTypedMessage (SetOffsetX offsetX)
              | offsetX == state.offsetX = Nothing
              | otherwise =
                  Just . resultNode $
                    node & L.widget .~ makeWidget state {offsetX}
            result = cast msg >>= handleTypedMessage

        getSizeReq _wenv _node children = (w, h)
          where
            w = fixedSize (sum (fromIntegral . currentWidth <$> model.columns) + hScrollFudgeFactor)
            h = foldl' sizeReqMergeMax (fixedSize 0) (_wniSizeReqH . _wnInfo <$> children)

        resize _wenv node viewport _children = (resultNode node, assignedAreas)
          where
            Rect l t _w h = viewport
            (assignedAreas, _) = foldl' assignArea (mempty, l) (zip childWidgets model.columns)
            assignArea (areas, colX) (childWidget, ModelColumn {currentWidth}) = (newAreas, newColX)
              where
                newAreas
                  | isNothing childWidget = areas
                  | otherwise = areas :|> Rect colX t (fromIntegral currentWidth) h
                newColX = colX + fromIntegral currentWidth

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

-- | This needs to be at least as big as the width of a vertical scrollbar.
hScrollFudgeFactor :: Double
hScrollFudgeFactor = 100

contentPane ::
  forall a ep.
  (CompositeModel a, WidgetEvent ep) =>
  [Column ep a] ->
  HagridModel a ->
  WidgetNode (HagridModel a) (HagridEvent ep)
contentPane columnDefs model = node
  where
    node =
      defaultWidgetNode "Hagrid.ContentPane" contentPaneContainer
        & L.children .~ S.fromList (mconcat childWidgetRows)

    childWidgetRows =
      [ [cellWidget idx item widget | Column {widget} <- columnDefs]
        | (item, idx) <- model.sortedItems
      ]

    nCols = length columnDefs
    columnDefsSeq = S.fromList columnDefs

    contentPaneContainer =
      createContainer
        model
        def
          { containerGetSizeReq = getSizeReq,
            containerResize = resize,
            containerRender = render,
            containerHandleEvent = handleEvent,
            containerHandleMessage = handleMessage
          }

    getSizeReq _wenv _node children = (w, h)
      where
        w = fixedSize (sum (fromIntegral . currentWidth <$> model.columns))
        h = fixedSize (sum (toRowHeights children columnDefsSeq))

    resize wenv node viewport children = (resultNode node, assignedAreas)
      where
        style = currentStyle wenv node
        Rect l t _w _h = fromMaybe def (removeOuterBounds style viewport)

        colXs = sizesToPositions (S.fromList (fromIntegral . currentWidth <$> model.columns))
        rowYs = sizesToPositions (toRowHeights children columnDefsSeq)

        assignedAreas = do
          (rowN, row) <-
            S.mapWithIndex (,) (S.chunksOf nCols children)
          (colN, columnDef, widget) <-
            S.mapWithIndex (\i (cd, w) -> (i, cd, w)) (S.zip columnDefsSeq row)
          pure (assignArea colN columnDef rowN widget)

        assignArea col Column {paddingW, paddingH, align} row widget = Rect chX chY chW chH
          where
            (chX, chW)
              | widgetReqW >= cellW = (cellX, cellW)
              | align == ColumnAlignLeft = (cellX, widgetReqW)
              | otherwise = (cellX + cellW - widgetReqW, widgetReqW)
            (chY, chH) =
              (cellY, cellH)

            cellX = l + S.index colXs col + paddingW
            cellY = t + S.index rowYs row + paddingH
            cellW = S.index colXs (col + 1) - S.index colXs col - paddingW * 2
            cellH = S.index rowYs (row + 1) - S.index rowYs row - paddingH * 2

            widgetReqW =
              widget
                & _wnInfo
                & _wniSizeReqW
                & \r -> _szrFixed r + _szrFlex r

    render wenv node renderer = do
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

    handleEvent _wenv node _path = \case
      Move (Point _pX _pY) ->
        -- refresh which row shows as hovered
        Just (resultReqs node [RenderOnce])
      _ -> Nothing

    handleMessage :: ContainerMessageHandler (HagridModel a) (HagridEvent ep)
    handleMessage wenv node _path msg = result
      where
        result = cast msg >>= handleTypedMessage

        handleTypedMessage (ContentPaneScrollToRow callback) = result
          where
            result
              | Just row <- callback indexedItems,
                Just y1 <- S.lookup row rowYs,
                Just y2 <- S.lookup (row + 1) rowYs =
                  Just (resultReqs node [SendMessage scrollId (ScrollTo (Rect vp._rX (vp._rY + y1) 1 (y2 - y1)))])
              | otherwise =
                  Nothing

            indexedItems =
              model.sortedItems
                & indexed
                & List.sortOn (snd . snd)
                & map (\(sortedIndex, (item, _originalIndex)) -> (item, sortedIndex))

            vp = node ^. L.info . L.viewport
            rowYs = sizesToPositions (toRowHeights (node ^. L.children) columnDefsSeq)

            scrollId = fromJust (widgetIdFromKey wenv (WidgetKey contentScrollKey))

initialModel :: [HagridCfg s e] -> [Column ep a] -> [a] -> HagridModel a
initialModel cfg columnDefs items = model
  where
    model =
      HagridModel
        { sortedItems = sortItems columnDefs sortColumn (zip items [0 ..]),
          columns = initialColumn <$> columnDefs,
          sortColumn
        }

    sortColumn
      | Just (col, dir) <- (mconcat cfg).cfgInitialSort,
        col >= 0,
        col < length columnDefs =
          Just (col, dir)
      | otherwise = Nothing

    initialColumn Column {name, initialWidth} =
      ModelColumn
        { name,
          currentWidth = initialWidth
        }

dragHandleWidth :: Int
dragHandleWidth = 4

dragHandleHeight :: Double
dragHandleHeight = 40

headerPaneKey :: Text
headerPaneKey = "Hagrid.headerPane"

contentScrollKey :: Text
contentScrollKey = "Hagrid.contentScroll"

contentPaneKey :: Text
contentPaneKey = "Hagrid.contentPane"

footerPaneKey :: Text
footerPaneKey = "Hagrid.footerPane"

sortItems ::
  [Column ep a] ->
  Maybe (Int, SortDirection) ->
  [ItemWithIndex a] ->
  [ItemWithIndex a]
sortItems columnDefs sortColumn items =
  case modelSortKey columnDefs sortColumn of
    DontSort -> items
    SortWith f -> List.sortOn (f . fst) items

modelSortKey :: [Column ep a] -> Maybe (Int, SortDirection) -> ColumnSortKey a
modelSortKey columnDefs sortColumn = case sortColumn of
  Just (sc, dir)
    | Column {sortKey = SortWith f} <- columnDefs !! sc ->
        case dir of
          SortAscending -> SortWith f
          SortDescending -> SortWith (Down . f)
  _ ->
    DontSort

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
    widget = LabelWidget (const get)
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
    widget = LabelWidget (const (T.pack . show . get))
    sortKey = SortWith get

-- | Creates a column that displays the a custom widget in each cell.
widgetColumn ::
  -- | Name of the column, to display in the header.
  Text ->
  -- | Called with the (original, not sorted) index and the item for each row to get the widget to
  -- display for that row.
  (forall s. Int -> a -> WidgetNode s e) ->
  Column e a
widgetColumn name get = defaultColumn name (CustomWidget get)

defaultColumn :: Text -> ColumnWidget e a -> Column e a
defaultColumn name widget =
  Column
    { name,
      widget,
      footerWidget = NoFooterWidget,
      align = ColumnAlignLeft,
      initialWidth = defaultColumnInitialWidth,
      sortKey = DontSort,
      minWidth = defaultColumnMinWidth,
      paddingW = defaultColumnPadding,
      paddingH = defaultColumnPadding,
      resizeHandler = Nothing,
      sortHandler = Nothing
    }

cellWidget :: (CompositeModel a, WidgetEvent e, WidgetModel s) => Int -> a -> ColumnWidget e a -> WidgetNode (HagridModel s) (HagridEvent e)
cellWidget idx item = \case
  LabelWidget get -> label_ (get idx item) [ellipsis]
  CustomWidget get -> widget
    where
      widget =
        compositeD_ "Hagrid.Cell" (WidgetValue item) buildUI handleEvent []
      buildUI _wenv =
        get idx
      handleEvent _wenv _node _model e =
        [Report (ParentEvent e)]

footerWidgetNode ::
  (CompositeModel a, CompositeModel s, Typeable e) =>
  [ItemWithIndex a] ->
  ColumnFooterWidget e a ->
  Maybe (WidgetNode (HagridModel s) (HagridEvent e))
footerWidgetNode items = \case
  NoFooterWidget -> Nothing
  CustomFooterWidget get -> Just widget
    where
      widget =
        compositeD_ "Hagrid.FooterCell" (WidgetValue items) buildUI handleEvent []
      buildUI _wenv _model =
        get items
      handleEvent _wenv _node _model e =
        [Report (ParentEvent e)]

-- | Sends a message to the targeted 'hagrid' widget, that causes the
-- widget to scroll such that a specified row becomes visible.
scrollToRow ::
  forall s e sp ep a.
  (Typeable a, Typeable e) =>
  -- | The widget to target.
  WidgetKey ->
  -- | Determines which row to scroll to.
  ScrollToRowCallback a ->
  EventResponse s e sp ep
scrollToRow key row =
  Message key (ScrollToRow row :: HagridEvent e)

defaultColumnInitialWidth :: Int
defaultColumnInitialWidth = 100

defaultColumnMinWidth :: Int
defaultColumnMinWidth = 60

defaultColumnPadding :: Double
defaultColumnPadding = 10

flipSortDirection :: SortDirection -> SortDirection
flipSortDirection SortAscending = SortDescending
flipSortDirection SortDescending = SortAscending
