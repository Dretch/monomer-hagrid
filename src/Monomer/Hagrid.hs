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
    estimatedItemHeight,
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
import Control.Lens (ix, (.~), (^.), (^?!))
import Control.Lens.Combinators (non)
import Control.Lens.Lens ((&))
import Control.Lens.Operators ((%~))
import Control.Monad as X (forM_)
import Data.Data (Typeable)
import Data.Default.Class (Default, def)
import Data.Foldable (foldl')
import Data.Maybe (catMaybes, isNothing, maybeToList)
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
data HagridCfg s e = HagridCfg
  { cfgEstimatedItemHeight :: Maybe Double,
    cfgInitialSort :: Maybe (Int, SortDirection)
  }

instance Default (HagridCfg s e) where
  def =
    HagridCfg
      { cfgEstimatedItemHeight = Nothing,
        cfgInitialSort = Nothing
      }

instance Semigroup (HagridCfg s e) where
  c1 <> c2 =
    HagridCfg
      { cfgEstimatedItemHeight = c2.cfgEstimatedItemHeight <|> c1.cfgEstimatedItemHeight,
        cfgInitialSort = c2.cfgInitialSort <|> c1.cfgInitialSort
      }

instance Monoid (HagridCfg s e) where
  mempty = def

-- | Configures the estimated item height. This should be the average row height you expect
-- in your grid (including padding). This is used to show scrollbar size and position when
-- there are lots of rows and so not all the rows have been "inflated" into widgets. More
-- accurate values will improve performance and scrollbar position accuracy.
--
-- The default value is 40, which is roughly the height of a single line of text with the default
-- column padding (10).
estimatedItemHeight :: Double -> HagridCfg s e
estimatedItemHeight h =
  def {cfgEstimatedItemHeight = Just h}

-- | Configures the initial sort column and direction.
initialSort ::
  -- | The initial sort column (zero-indexed, out of bounds values will have no effect).
  Int ->
  -- | The initial sort direction.
  SortDirection ->
  HagridCfg s e
initialSort column direction =
  def {cfgInitialSort = Just (column, direction)}

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
    -- | The padding to the left and right of the widget in each cell of the column, in pixels (the default is 10).
    paddingW :: Double,
    -- | The padding above and below the widget in each cell in the column, in pixels (the default is 10).
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
    CustomWidget (forall s. (WidgetModel s) => Int -> a -> WidgetNode s e)

-- | How to create the footer widget for a column.
data ColumnFooterWidget e a
  = -- | No footer widget for this column.
    NoFooterWidget
  | -- | Create a footer widget. The function receives the items in their current sort
    -- order, and also along with each item it's original (unsorted) index.
    CustomFooterWidget (forall s. (WidgetModel s) => Seq (ItemWithIndex a) -> WidgetNode s e)

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
    forall b. (Ord b) => SortWith (a -> b)

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
  Seq (ItemWithIndex a) ->
  -- | The row to scroll to, as an index into the sorted items (e.g. 0 is always the first row
  -- in the grid, regardless of the current order). 'Nothing' will cancel the scroll.
  Maybe Int

data HagridEvent e
  = ContentScrollChange ScrollStatus
  | OrderByColumn Int
  | ResizeColumn Int Int
  | ResizeColumnFinished Int
  | forall a. (Typeable a) => ScrollToRow (ScrollToRowCallback a)
  | ScrollToRect Rect
  | ParentEvent e

data HagridModel a = HagridModel
  { sortedItems :: Seq (ItemWithIndex a),
    columns :: [ModelColumn],
    sortColumn :: Maybe (Int, SortDirection),
    mdlEstimatedItemHeight :: Double
  }
  deriving (Eq, Show)

data ModelColumn = ModelColumn
  { currentWidth :: Int,
    name :: Text
  }
  deriving (Eq, Show)

-- | The state of the header or footer.
data HeaderFooterState = HeaderFooterState
  { -- | The width of each column.
    columnWidths :: [Int],
    -- | The horizontal scroll position: always matches the content pane horizontal scroll position.
    offsetX :: Double
  }
  deriving (Eq, Show)

newtype HeaderFooterEvent = SetOffsetX Double

data HeaderDragHandleState = HeaderDragHandleState
  { dragStartMouseX :: Double,
    dragStartColumnW :: Int
  }
  deriving (Eq, Show)

data ContentPaneModel a = ContentPaneModel
  { -- | The width of each column.
    columnWidths :: [Int],
    -- | The visible area (relative to the content pane).
    visibleArea :: Rect,
    -- | The index in items of the special row that we try and keep in the same position
    -- (relative to the viewport) when items get resized.
    fixedRowIndex :: Int,
    -- | The Y position of the fixed row within the viewport. When items get resized, we try
    -- and keep this value fixed: it should only change when the user scrolls the viewport itself.
    fixedRowViewportOffset :: Double,
    -- | How many items there are before the inflated items.
    itemsBeforeInflated :: Int,
    -- | The items that have been inflated into actual widgets.
    inflatedItems :: Seq (ItemWithIndex a),
    -- | How many items there are after the inflated items.
    itemsAfterInflated :: Int,
    phase :: ContentPanePhase
  }
  deriving (Eq, Show)

data ContentPanePhase
  = ContentPaneIdle
  | ContentPaneReinflating
  deriving (Eq, Show)

data ContentPaneEvent e
  = SetVisibleArea {visibleArea :: Rect}
  | InnerResizeComplete
  | ContentPaneParentEvent e
  | forall a. (Typeable a) => ContentPaneScrollToRow (ScrollToRowCallback a)

-- | Creates a hagrid widget, using the default configuration.
hagrid ::
  forall a s e.
  (CompositeModel a, WidgetModel s, WidgetEvent e) =>
  -- | The definitions for each column in the grid.
  [Column e a] ->
  -- | The items for each row in the grid.
  Seq a ->
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
  Seq a ->
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
              contentScroll
                `styleBasic` [sizeReqW useExtra, sizeReqH useExtra]
                `nodeKey` contentScrollKey,
              footerPane columnDefs model `nodeKey` footerPaneKey
            ]
        contentScroll =
          scroll_ [onChange ContentScrollChange] $
            contentPaneOuter columnDefs model `nodeKey` contentPaneOuterKey
        useExtra =
          SizeReq
            { _szrFixed = 0,
              _szrFlex = 0,
              _szrExtra = 1,
              _szrFactor = 1
            }

    handleEvent :: EventHandler (HagridModel a) (HagridEvent e) sp e
    handleEvent _wenv _node model = \case
      ScrollToRow callback ->
        [Message (WidgetKey contentPaneOuterKey) (ContentPaneScrollToRow callback :: ContentPaneEvent e)]
      ScrollToRect rect ->
        [Message (WidgetKey contentScrollKey) (ScrollTo rect)]
      ContentScrollChange ScrollStatus {scrollDeltaX, scrollDeltaY, scrollVpSize} ->
        [ Message (WidgetKey headerPaneKey) (SetOffsetX scrollDeltaX),
          Message (WidgetKey contentPaneOuterKey) (SetVisibleArea {visibleArea} :: ContentPaneEvent e),
          Message (WidgetKey footerPaneKey) (SetOffsetX scrollDeltaX)
        ]
        where
          visibleArea = Rect (-scrollDeltaX) (-scrollDeltaY) scrollVpSize._sW scrollVpSize._sH
      OrderByColumn colIndex -> result
        where
          Column {sortHandler, sortKey} = columnDefs !! colIndex
          (sortColumn, sortedItems)
            | Just (c, dir) <- model.sortColumn,
              c == colIndex =
                let sortColumn = Just (colIndex, flipSortDirection dir)
                    sortedItems = S.reverse model.sortedItems
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
        [Model model {columns = model.columns & ix colIndex %~ (\c -> c {currentWidth = newWidth})}]
      ResizeColumnFinished colIndex -> result
        where
          ModelColumn {currentWidth} = model.columns !! colIndex
          Column {resizeHandler} = columnDefs !! colIndex
          result =
            Report <$> maybeToList (resizeHandler <*> Just currentWidth)
      ParentEvent e ->
        [Report e]

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

headerPane :: forall e a. (WidgetEvent e) => [Column e a] -> HagridModel a -> WidgetNode (HagridModel a) (HagridEvent e)
headerPane columnDefs model = makeNode (initialHeaderFooterState model)
  where
    makeNode :: HeaderFooterState -> WidgetNode (HagridModel a) (HagridEvent e)
    makeNode state = node
      where
        node =
          defaultWidgetNode "Hagrid.HeaderPane" (makeWidget state)
            & L.children .~ S.fromList childWidgets

        childWidgets =
          mconcat (zipWith3 childWidgetPair [0 ..] columnDefs model.columns)

        childWidgetPair i columnDef column = [btn, handle]
          where
            btn = headerButton i columnDef
            handle = headerDragHandle i columnDef column

    makeWidget :: HeaderFooterState -> Widget (HagridModel a) (HagridEvent e)
    makeWidget state = container
      where
        container =
          createHeaderFooter
            state
            makeWidget
            def {containerResize = resize, containerRenderAfter = renderAfter}

        resize _wenv node viewport _children = (resultNode node, assignedAreas)
          where
            Rect l t _w h = viewport
            widgetWidths = do
              -- center the drag handle inbetween the columns
              let wDeltas = dragHandleWidth / 2 : repeat dragHandleWidth
              (wd, w) <- zip wDeltas ((.currentWidth) <$> model.columns)
              [fromIntegral w - wd, dragHandleWidth]
            (assignedAreas, _) = foldl' assignArea (mempty, l) widgetWidths
            assignArea (areas, colX) columnWidth =
              (areas :|> Rect colX t columnWidth h, colX + columnWidth)

        renderAfter wenv node renderer =
          forM_ model.sortColumn (renderSortIndicator wenv node renderer)

        renderSortIndicator wenv node renderer (sortCol, sortDirection) = do
          drawSortIndicator renderer indRect (Just (accentColor wenv)) sortDirection
          where
            Rect l t w h = node ^?! L.children . ix (sortCol * 2) . L.info . L.viewport

            style = wenv ^. L.theme . L.basic . L.btnStyle
            size = style ^. L.text . non def . L.fontSize . non def

            -- put triangle corners at integer positions because it looks nicer
            indW = ceilingDouble (unFontSize size * 2 / 3)
            pad = ceilingDouble (unFontSize size * 2 / 9)

            indT = case sortDirection of
              SortAscending -> t + h - pad - indW
              SortDescending -> t + pad
            indL = l + w + state.offsetX - indW - pad
            indRect = Rect indL indT indW indW

headerButton :: (WidgetEvent e) => Int -> Column e a -> WidgetNode (HagridModel a) (HagridEvent e)
headerButton colIndex columnDef =
  button_ columnDef.name (OrderByColumn colIndex) [ellipsis]
    `styleBasic` [radius 0]

footerPane ::
  forall e a.
  (CompositeModel a, WidgetEvent e) =>
  [Column e a] ->
  HagridModel a ->
  WidgetNode (HagridModel a) (HagridEvent e)
footerPane columnDefs model = makeNode (initialHeaderFooterState model)
  where
    makeNode :: HeaderFooterState -> WidgetNode (HagridModel a) (HagridEvent e)
    makeNode state = node
      where
        node =
          defaultWidgetNode "Hagrid.FooterPane" (makeWidget state)
            & L.children .~ S.fromList (catMaybes childWidgets)

    childWidgets :: [Maybe (WidgetNode (HagridModel a) (HagridEvent e))]
    childWidgets = footerWidgetNode model.sortedItems . (.footerWidget) <$> columnDefs

    makeWidget :: HeaderFooterState -> Widget (HagridModel a) (HagridEvent e)
    makeWidget state = container
      where
        container =
          createHeaderFooter
            state
            makeWidget
            def {containerResize = resize}

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

initialHeaderFooterState :: HagridModel a -> HeaderFooterState
initialHeaderFooterState model =
  HeaderFooterState
    { columnWidths = (.currentWidth) <$> model.columns,
      offsetX = 0
    }

createHeaderFooter ::
  forall a e.
  HeaderFooterState ->
  (HeaderFooterState -> Widget (HagridModel a) (HagridEvent e)) ->
  Container (HagridModel a) (HagridEvent e) HeaderFooterState ->
  Widget (HagridModel a) (HagridEvent e)
createHeaderFooter state makeWidget container =
  createContainer
    state
    container
      { containerChildrenOffset = Just (Point state.offsetX 0),
        containerUpdateCWenv = updateCWenv,
        containerMerge = merge,
        containerHandleMessage = handleMessage,
        containerGetSizeReq = getSizeReq,
        containerUseScissor = True -- otherwise the buttons extend outside the header when the grid scrolls horizontally
      }
  where
    -- ensures child widgets don't disappear when scrolling
    updateCWenv wenv node _cnode _cidx = newWenv
      where
        style = currentStyle wenv node
        carea = getContentArea node style
        newWenv = wenv & L.viewport .~ moveRect (Point (-state.offsetX) 0) carea

    merge _wenv node _oldNode oldState = resultReqs newNode reqs
      where
        newNode = node & L.widget .~ makeWidget state {offsetX = oldState.offsetX}
        reqs = [ResizeWidgets (node ^. L.info . L.widgetId) | needResize]
        needResize = oldState.columnWidths /= state.columnWidths

    getSizeReq _wenv _node children = (w, h)
      where
        w = fixedSize (fromIntegral (sum state.columnWidths) + hScrollFudgeFactor)
        h = foldl' sizeReqMergeMax (fixedSize 0) (_wniSizeReqH . _wnInfo <$> children)

    handleMessage :: ContainerMessageHandler (HagridModel a) (HagridEvent e)
    handleMessage _wenv node _target msg = result
      where
        handleTypedMessage (SetOffsetX offsetX)
          | offsetX == state.offsetX = Nothing
          | otherwise =
              Just . resultNode $
                node & L.widget .~ makeWidget state {offsetX}
        result = cast msg >>= handleTypedMessage

headerDragHandle :: (WidgetEvent e) => Int -> Column e a -> ModelColumn -> WidgetNode s (HagridEvent e)
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
                singleGetSizeReq = getSizeReq,
                singleMerge = merge,
                singleHandleEvent = handleEvent,
                singleRender = render
              }

        getBaseStyle _wenv _node =
          Just def {_styleBasic = Just def {_sstCursorIcon = Just CursorSizeH}}

        getSizeReq _wenv _node =
          (fixedSize dragHandleWidth, fixedSize dragHandleHeight)

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
                      & L.requests %~ (:|> RaiseEvent (ResizeColumn colIndex nw))
                | otherwise =
                    resultReqs node []
              newColumnW = do
                HeaderDragHandleState clickX columnW <- state
                pure (max columnDef.minWidth (columnW + fromFractional (_pX - clickX)))
          _ -> Nothing
          where
            resizeRequest = widgetResize (node ^. L.widget) wenv node vp (const True)
            vp = node ^. L.info . L.viewport

        render wenv node renderer = do
          drawRect renderer vp (Just (accentColor wenv)) Nothing
          where
            vp = node ^. L.info . L.viewport

-- | This needs to be at least as big as the width of a vertical scrollbar.
hScrollFudgeFactor :: Double
hScrollFudgeFactor = 20

-- | Composite wrapper to allow creating/removing child widgets during resize.
contentPaneOuter ::
  forall a e.
  (CompositeModel a, WidgetEvent e) =>
  [Column e a] ->
  HagridModel a ->
  WidgetNode (HagridModel a) (HagridEvent e)
contentPaneOuter columnDefs model =
  compositeD_
    "Hagrid.ContentPaneOuter"
    (WidgetValue initialModel)
    buildUI
    handleEvent
    [compositeMergeModel mergeModel]
  where
    initialModel =
      ContentPaneModel
        { columnWidths = (.currentWidth) <$> model.columns,
          visibleArea = Rect 0 0 0 0,
          fixedRowIndex = 0,
          fixedRowViewportOffset = 0,
          itemsBeforeInflated = 0,
          inflatedItems = mempty,
          itemsAfterInflated = 0,
          phase = ContentPaneIdle
        }

    mergeModel :: MergeModelHandler (ContentPaneModel a) (ContentPaneEvent e) (HagridModel a)
    mergeModel _wenv parentModel oldModel newModel =
      oldModel {columnWidths, fixedRowIndex, itemsBeforeInflated, inflatedItems, itemsAfterInflated}
      where
        columnWidths = newModel.columnWidths
        fixedRowIndex = min (max 0 (length parentModel.sortedItems - 1)) oldModel.fixedRowIndex
        itemsBeforeInflated = min (length parentModel.sortedItems) oldModel.itemsBeforeInflated
        itemsAfterInflated = length parentModel.sortedItems - itemsBeforeInflated - length inflatedItems
        inflatedItems = takeAt itemsBeforeInflated (length oldModel.inflatedItems) parentModel.sortedItems

    buildUI _wenv =
      contentPaneInner (S.fromList columnDefs) model

    handleEvent _wenv node cpModel = \case
      SetVisibleArea visibleArea -> result
        where
          result = case cpModel.phase of
            ContentPaneIdle
              | visibleAreaMoved && (startItemsMissing || endItemsMissing) ->
                  let newModel =
                        cpModel
                          { visibleArea,
                            fixedRowIndex,
                            fixedRowViewportOffset,
                            itemsBeforeInflated = fixedRowIndex,
                            inflatedItems = mempty,
                            itemsAfterInflated = length model.sortedItems - fixedRowIndex,
                            phase = ContentPaneReinflating
                          }
                   in [Model newModel]
              | visibleAreaMoved ->
                  [Model cpModel {visibleArea, fixedRowIndex, fixedRowViewportOffset}]
              | otherwise -> []
            ContentPaneReinflating
              | visibleAreaMoved ->
                  [Model (cpModel :: ContentPaneModel a) {visibleArea}]
              | otherwise -> []

          (fixedRowIndex, fixedRowViewportOffset) = fixedRow minVisibleY rowHeights model cpModel
          (rowsStartY, rowHeights, rowsEndY) = rowPositions node

          visibleAreaMoved = not (roundedRectEq visibleArea cpModel.visibleArea)
          minVisibleY = visibleArea._rY
          maxVisibleY = visibleArea._rY + visibleArea._rH

          startItemsMissing = cpModel.itemsBeforeInflated > 0 && minVisibleY < rowsStartY
          endItemsMissing = cpModel.itemsAfterInflated > 0 && maxVisibleY > rowsEndY
      ContentPaneScrollToRow callback -> result
        where
          result
            | Just typedCb <- cast callback,
              Just row <- typedCb indexedItems =
                -- set the fixed index to the target row and let the viewport position be sorted out by the
                -- adjustment that follows the addition and resizing of the rows around the target row.
                let newModel =
                      cpModel
                        { fixedRowIndex = row,
                          fixedRowViewportOffset = 0,
                          itemsBeforeInflated = row,
                          inflatedItems = mempty,
                          phase = ContentPaneReinflating
                        }
                 in [Model newModel]
            | otherwise =
                []

          indexedItems =
            model.sortedItems
              & S.mapWithIndex (,)
              & S.sortOn (snd . snd)
              & fmap (\(sortedIndex, (item, _originalIndex)) -> (item, sortedIndex))
      InnerResizeComplete -> result
        where
          (rowsStartY, rowHeights, rowsEndY) = rowPositions node

          fixedRowY = rowsStartY + sum (S.take (cpModel.fixedRowIndex - cpModel.itemsBeforeInflated) rowHeights)

          result
            | itemsToPrepend > 0 || itemsToAppend > 0 =
                let inflatedItems =
                      takeAt
                        (cpModel.itemsBeforeInflated - itemsToPrepend)
                        (itemsToPrepend + length cpModel.inflatedItems + itemsToAppend)
                        model.sortedItems
                    itemsBeforeInflated = cpModel.itemsBeforeInflated - itemsToPrepend
                    itemsAfterInflated = length model.sortedItems - length inflatedItems - itemsBeforeInflated
                 in [Model cpModel {itemsBeforeInflated, inflatedItems, itemsAfterInflated}]
            | otherwise =
                -- Once we have finished adding items then, if the added items are not the same size as estimated,
                -- the row we want to scroll to might no longer be at the correct position in the viewport, so we
                -- need to adjust the scroll position to position it correctly.
                let adjustScrollEvt = [Report (ScrollToRect adjustScrollRect) | needAdjustScroll]
                    adjustScrollRect = Rect (vp._rX + cpModel.visibleArea._rX) (vp._rY + fixedRowY - cpModel.fixedRowViewportOffset) visibleWidth visibleHeight
                    needAdjustScroll = abs ((fixedRowY - cpModel.visibleArea._rY) - cpModel.fixedRowViewportOffset) >= 1
                 in [Model cpModel {phase = ContentPaneIdle}] <> adjustScrollEvt

          itemsToPrepend = itemsToAdd (fixedRowY - rowsStartY) 1 cpModel.itemsBeforeInflated
          itemsToAppend = itemsToAdd (rowsEndY - fixedRowY) 2 cpModel.itemsAfterInflated

          itemsToAdd existingItemsHeight f availableItems
            | not (null model.sortedItems) && existingItemsHeight < visibleHeight * f =
                let n = ceiling ((visibleHeight * f - existingItemsHeight) / model.mdlEstimatedItemHeight)
                 in min (max 8 (min 64 n)) availableItems
            | otherwise = 0

          vp = node ^. L.info . L.viewport
          visibleWidth = cpModel.visibleArea._rW
          visibleHeight = cpModel.visibleArea._rH
      ContentPaneParentEvent e ->
        [Report (ParentEvent e)]

contentPaneInner ::
  forall a e.
  (CompositeModel a, WidgetEvent e) =>
  Seq (Column e a) ->
  HagridModel a ->
  ContentPaneModel a ->
  WidgetNode (ContentPaneModel a) (ContentPaneEvent e)
contentPaneInner columnDefs model cpModel = node
  where
    node =
      defaultWidgetNode "Hagrid.ContentPaneInner" contentPaneContainer
        & L.children .~ rowWidgets

    contentPaneContainer =
      createContainer
        cpModel
        def
          { containerMerge = merge,
            containerGetSizeReq = getSizeReq,
            containerResize = resize
          }

    rowWidgets = S.mapWithIndex (contentPaneRow columnDefs cpModel) cpModel.inflatedItems

    merge _wenv newNode _oldNode oldState = resultReqs newNode reqs
      where
        reqs = [ResizeWidgets (newNode ^. L.info . L.widgetId) | needResize]
        needResize =
          oldState.columnWidths /= cpModel.columnWidths
            || oldState.itemsBeforeInflated /= cpModel.itemsBeforeInflated
            || length oldState.inflatedItems /= length cpModel.inflatedItems
            || oldState.itemsAfterInflated /= cpModel.itemsAfterInflated

    getSizeReq _wenv _node children = (w, h)
      where
        w = fixedSize (fromIntegral (sum cpModel.columnWidths))
        h = fixedSize (uninflatedHeights + inflatedHeights)

        uninflatedHeights = fromIntegral uninflatedItems * model.mdlEstimatedItemHeight
        uninflatedItems = cpModel.itemsBeforeInflated + cpModel.itemsAfterInflated

        inflatedHeights = sum (_szrFixed . _wniSizeReqH . _wnInfo <$> children)

    resize wenv node viewport children = (resultEvts node [InnerResizeComplete], rowAreas)
      where
        style = currentStyle wenv node
        innerVp = fromMaybe def (removeOuterBounds style viewport)

        startX = innerVp._rX
        startY = innerVp._rY + fromIntegral cpModel.itemsBeforeInflated * model.mdlEstimatedItemHeight

        sumColumnWidths = fromIntegral (sum cpModel.columnWidths)

        rowAreas = snd (foldl' foldRowAreas (startY, mempty) children)
        foldRowAreas (y, areas) child =
          (y + h, areas :|> Rect startX y sumColumnWidths h)
          where
            h = child ^. L.info . L.sizeReqH . L.fixed

contentPaneRow ::
  forall a e.
  (CompositeModel a, WidgetEvent e) =>
  Seq (Column e a) ->
  ContentPaneModel a ->
  Int ->
  ItemWithIndex a ->
  WidgetNode (ContentPaneModel a) (ContentPaneEvent e)
contentPaneRow columnDefs cpModel sortedIdx item = tree
  where
    tree =
      defaultWidgetNode "Hagrid.Row" widget
        & L.children .~ cellWidgets

    widget =
      createContainer
        (cpModel.columnWidths, item, sortedIdx)
        def
          { containerGetSizeReq = getSizeReq,
            containerResize = resize,
            containerRender = render
          }

    cellWidgets = do
      Column {widget} <- columnDefs
      pure (cellWidget item widget)

    getSizeReq _wenv _node children = (w, h)
      where
        w = fixedSize (fromIntegral (sum cpModel.columnWidths))
        h = fixedSize (toRowHeight columnDefs children)

    resize wenv node viewport children = (resultNode node, cellAreas)
      where
        style = currentStyle wenv node
        innerVp = fromMaybe def (removeOuterBounds style viewport)

        startX = innerVp._rX
        startY = innerVp._rY

        columnWidths = fromIntegral <$> S.fromList cpModel.columnWidths

        cellAreas = snd (foldl' foldCellAreas (startX, mempty) (S.zip3 columnWidths columnDefs children))
        foldCellAreas (x, areas) (colW, Column {paddingW, paddingH, align}, widget) =
          (x + colW, areas :|> Rect chX cellY chW cellH)
          where
            (chX, chW) = case align of
              ColumnAlignLeft -> (cellX, cellW)
              ColumnAlignRight -> (cellX + cellW - widgetW, widgetW)

            cellX = x + paddingW
            cellY = startY + paddingH
            cellW = colW - paddingW * 2
            cellH = viewport._rH - paddingH * 2

            widgetW =
              widget
                & _wnInfo
                & _wniSizeReqW
                & (\r -> _szrFixed r + _szrFlex r)
                & min cellW

    render wenv node renderer = do
      drawRect renderer vp bgColor Nothing
      drawLine renderer (Point vp._rX (vp._rY + vp._rH)) (Point (vp._rX + vp._rW) (vp._rY + vp._rH)) 1 (Just lineColor)
      forM_ (drop 1 colXs) $ \colX -> do
        drawLine renderer (Point (vp._rX + colX) vp._rY) (Point (vp._rX + colX) (vp._rY + vp._rH)) 1 (Just lineColor)
      where
        colXs = scanl (+) 0 (fromIntegral <$> cpModel.columnWidths)
        bgColor
          | mouseover = Just mouseOverColor
          | sortedIdx `mod` 2 == 1 = Just oddRowBgColor
          | otherwise = Nothing
        vp = node ^. L.info . L.viewport
        mouseover = pointInRect mouse vp
        mouse = wenv ^. L.inputStatus . L.mousePos
        mouseOverColor = (accentColor wenv) {_colorA = 0.3}
        oddRowBgColor = (accentColor wenv) {_colorA = 0.1}
        lineColor = accentColor wenv

initialModel :: [HagridCfg s e] -> [Column e a] -> Seq a -> HagridModel a
initialModel cfgs columnDefs items = model
  where
    model =
      HagridModel
        { sortedItems = sortItems columnDefs sortColumn (S.zip items (S.fromFunction (length items) id)),
          columns = initialColumn <$> columnDefs,
          sortColumn,
          mdlEstimatedItemHeight = max 1 (fromMaybe 40 cfg.cfgEstimatedItemHeight)
        }

    cfg = mconcat cfgs

    sortColumn
      | Just (col, dir) <- cfg.cfgInitialSort,
        col >= 0,
        col < length columnDefs =
          Just (col, dir)
      | otherwise = Nothing

    initialColumn Column {name, initialWidth, minWidth} =
      ModelColumn
        { name,
          currentWidth = max minWidth initialWidth
        }

-- | When the viewport position changes, this function computes the index and
-- position within the viewport of the new fixed row. This is the first row
-- whose y-position is at least minVisibleY
fixedRow :: Double -> Seq Double -> HagridModel a -> ContentPaneModel a -> (Int, Double)
fixedRow minVisibleY inflatedItemHeights model cpModel = (min maxRow row, offset)
  where
    (row, offset)
      | minVisibleY < inflatedStartY =
          let row = ceiling (minVisibleY / model.mdlEstimatedItemHeight)
              offset = (fromIntegral row * model.mdlEstimatedItemHeight) - minVisibleY
           in (row, offset)
      | otherwise =
          inflatedItem itemsBeforeInflated inflatedStartY inflatedItemHeights

    maxRow = length model.sortedItems - 1

    ContentPaneModel {itemsBeforeInflated} = cpModel

    inflatedStartY = fromIntegral itemsBeforeInflated * model.mdlEstimatedItemHeight

    inflatedItem i y = \case
      itemHeight :<| itemHeights
        | y + itemHeight >= minVisibleY ->
            (i + 1, (y + itemHeight) - minVisibleY)
        | otherwise ->
            inflatedItem (i + 1) (y + itemHeight) itemHeights
      S.Empty ->
        let indexInSection = ceiling ((minVisibleY - y) / model.mdlEstimatedItemHeight)
            row = i + indexInSection
            offset = (y + fromIntegral indexInSection * model.mdlEstimatedItemHeight) - minVisibleY
         in (row, offset)

dragHandleWidth :: Double
dragHandleWidth = 4

dragHandleHeight :: Double
dragHandleHeight = 40

headerPaneKey :: Text
headerPaneKey = "Hagrid.headerPane"

contentScrollKey :: Text
contentScrollKey = "Hagrid.contentScroll"

contentPaneOuterKey :: Text
contentPaneOuterKey = "Hagrid.contentPaneOuter"

footerPaneKey :: Text
footerPaneKey = "Hagrid.footerPane"

sortItems ::
  [Column e a] ->
  Maybe (Int, SortDirection) ->
  Seq (ItemWithIndex a) ->
  Seq (ItemWithIndex a)
sortItems columnDefs sortColumn items =
  case modelSortKey columnDefs sortColumn of
    DontSort -> items
    SortWith f -> S.sortOn (f . fst) items

modelSortKey :: [Column e a] -> Maybe (Int, SortDirection) -> ColumnSortKey a
modelSortKey columnDefs sortColumn = case sortColumn of
  Just (sc, dir)
    | Column {sortKey = SortWith f} <- columnDefs !! sc ->
        case dir of
          SortAscending -> SortWith f
          SortDescending -> SortWith (Down . f)
  _ ->
    DontSort

toRowHeight :: Seq (Column e a) -> Seq (WidgetNode s (ContentPaneEvent e)) -> Double
toRowHeight columnDefs = mergeHeights
  where
    mergeHeights rowWidgets =
      foldl' max 0 (S.zipWith widgetHeight columnDefs rowWidgets)

    widgetHeight Column {paddingH} widget =
      widget
        & _wnInfo
        & _wniSizeReqH
        & \r -> _szrFixed r + _szrFlex r + paddingH * 2

rowPositions :: forall s e. WidgetNode s e -> (Double, Seq Double, Double)
rowPositions node = (rowsStartY, rowHeights, rowsEndY)
  where
    vp = node._wnInfo._wniViewport
    childVps = _wniViewport . _wnInfo <$> node._wnChildren

    rowsStartY = case childVps of
      cvp :<| _ -> cvp._rY - vp._rY
      _ -> 0
    rowsEndY = case childVps of
      _ :|> cvp -> (cvp._rY - vp._rY) + cvp._rH
      _ -> 0
    rowHeights = _rH <$> childVps

takeAt :: Int -> Int -> Seq a -> Seq a
takeAt at len s =
  S.take len (S.drop at s)

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

cellWidget ::
  (CompositeModel a, WidgetEvent e) =>
  ItemWithIndex a ->
  ColumnWidget e a ->
  WidgetNode (ContentPaneModel a) (ContentPaneEvent e)
cellWidget (item, idx) = \case
  LabelWidget get -> label_ (get idx item) [ellipsis]
  CustomWidget get -> widget
    where
      widget =
        compositeD_ "Hagrid.Cell" (WidgetValue item) buildUI handleEvent []
      buildUI _wenv =
        get idx
      handleEvent _wenv _node _model e =
        [Report (ContentPaneParentEvent e)]

footerWidgetNode ::
  (CompositeModel a, WidgetEvent e) =>
  Seq (ItemWithIndex a) ->
  ColumnFooterWidget e a ->
  Maybe (WidgetNode (HagridModel a) (HagridEvent e))
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
--
-- Note that this is inherently dynamically typed. If the type of the callback
-- does not match the type of the targeted hagrid widget then the message
-- will be ignored.
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

ceilingDouble :: Double -> Double
ceilingDouble x = fromIntegral (ceiling x :: Int)

roundedRectEq :: Rect -> Rect -> Bool
roundedRectEq r1 r2 =
  roundedEq r1._rX r2._rX
    && roundedEq r1._rY r2._rY
    && roundedEq (r1._rX + r1._rW) (r2._rX + r2._rW)
    && roundedEq (r1._rY + r1._rH) (r2._rY + r2._rH)
  where
    roundedEq x y = (round x :: Int) == round y
