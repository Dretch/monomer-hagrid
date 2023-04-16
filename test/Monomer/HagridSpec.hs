module Monomer.HagridSpec (spec) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Lens ((&), (.~), (^.))
import qualified Data.Foldable as Foldable
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Text (Text)
import GHC.IO (unsafePerformIO)
import Monomer
import Monomer.Hagrid
import qualified Monomer.Lens as L
import Monomer.TestUtil
import Test.Hspec (Spec, describe, it, shouldBe)

data TestModel = TestModel
  deriving (Eq, Show)

data TestEvent

newtype TestItem = TestItem
  {sizeReq :: SizeReq}
  deriving (Eq, Show)

spec :: Spec
spec = do
  resize
  sorting
  merging
  messages

resize :: Spec
resize = describe "resize" $ do
  it "should assign cell widths according to column widths" $ do
    cellViewports
      [ (testColumn "Col 1" (.sizeReq)) {initialWidth = 33},
        (testColumn "Col 2" (.sizeReq)) {initialWidth = 77}
      ]
      [TestItem (fixedSize 10)]
      `shouldBe` [ Rect 0 40 33 10,
                   Rect 33 40 77 10
                 ]

  it "should assign single column cell heights according to cell heights" $ do
    cellViewports
      [ (testColumn "Col 1" (.sizeReq)) {initialWidth = 50}
      ]
      [TestItem (fixedSize 33), TestItem (fixedSize 77)]
      `shouldBe` [Rect 0 40 50 33, Rect 0 73 50 77]

  it "should assign multi column cell heights according to max cell height from row" $ do
    cellViewports
      [ (testColumn "Col 1" (const (fixedSize 33))) {initialWidth = 50},
        (testColumn "Col 2" (const (fixedSize 47))) {initialWidth = 50}
      ]
      [TestItem (fixedSize 0)]
      `shouldBe` [Rect 0 40 50 47, Rect 50 40 50 47]

  it "should use both fixed and flex height to calculate row height" $ do
    cellViewports
      [ (testColumn "Col 1" (.sizeReq)) {initialWidth = 50}
      ]
      [TestItem (fixedSize 10 & L.flex .~ 7)]
      `shouldBe` [Rect 0 40 50 17]

  it "should have zero-height footer when no footer widgets specified" $
    hagridViewports
      [(testColumn "Col 1" (const (fixedSize 33))) {initialWidth = 50}]
      [TestItem (fixedSize 0)]
      []
      "Hagrid.FooterPane"
      `shouldBe` [Rect 0 100 100 0]

  it "should expand into available space both vertically and horizontally" $ do
    viewports tree [] "Hagrid.Root" `shouldBe` [Rect 0 0 90 90]
  where
    tree =
      vstack
        [ hstack
            [ hagrid [] (mempty :: Seq TestItem),
              label10x10
            ],
          label10x10
        ]
    label10x10 = label "" `styleBasic` [width 10, height 10]

sorting :: Spec
sorting = describe "sorting" $ do
  it "should not sort rows by default" $ do
    cellViewports
      [(testColumn "Col 1" (.sizeReq)) {initialWidth = 50, sortKey = SortWith (_szrFixed . (.sizeReq))}]
      [ TestItem (fixedSize 20),
        TestItem (fixedSize 10),
        TestItem (fixedSize 30)
      ]
      `shouldBe` [ Rect 0 40 50 20,
                   Rect 0 60 50 10,
                   Rect 0 70 50 30
                 ]

  it "should sort in ascending order when column header clicked" $ do
    cellViewportsEvts
      [(testColumn "Col 1" (.sizeReq)) {initialWidth = 50, sortKey = SortWith (_szrFixed . (.sizeReq))}]
      [ TestItem (fixedSize 20),
        TestItem (fixedSize 10),
        TestItem (fixedSize 30)
      ]
      [Click (Point 10 10) BtnLeft 1]
      `shouldBe` [ Rect 0 40 50 10,
                   Rect 0 50 50 20,
                   Rect 0 70 50 30
                 ]

  it "should sort in descending order when column header clicked again" $ do
    cellViewportsEvts
      [(testColumn "Col 1" (.sizeReq)) {initialWidth = 50, sortKey = SortWith (_szrFixed . (.sizeReq))}]
      [ TestItem (fixedSize 20),
        TestItem (fixedSize 10),
        TestItem (fixedSize 30)
      ]
      [ Click (Point 10 10) BtnLeft 1,
        Click (Point 10 10) BtnLeft 1
      ]
      `shouldBe` [ Rect 0 40 50 30,
                   Rect 0 70 50 20,
                   Rect 0 90 50 10
                 ]

merging :: Spec
merging = describe "merging" $ do
  it "should preserve column widths when items change" $ do
    let col = textColumn "Col" (const "")
        startNode = nodeInit wenv (hagrid [col {initialWidth = 75}] (S.fromList [TestItem (fixedSize 1)]))
        mergedNode = nodeMerge wenv (hagrid [col {initialWidth = 64}] (S.fromList [TestItem (fixedSize 2)])) startNode
        resizedNode = nodeResize wenv mergedNode (mergedNode ^. L.info . L.viewport)
    columnWidths startNode `shouldBe` [75]
    columnWidths resizedNode `shouldBe` [75]

messages :: Spec
messages = describe "messages" $ do
  it "should give items and sorted indices to scrollToRow callback" $ do
    itemsMVar <- newEmptyMVar
    let cols =
          [(textColumn "Col" (const "")) {sortKey = SortWith (_szrFixed . (.sizeReq))}]
        items =
          S.fromList
            [ TestItem (fixedSize 1),
              TestItem (fixedSize 2),
              TestItem (fixedSize 3)
            ]
        {-# NOINLINE scrollToRowCallback #-}
        scrollToRowCallback cbItems =
          unsafePerformIO (Nothing <$ putMVar itemsMVar cbItems) -- hacky, but seems to work!
        buildUI _wenv _model =
          hagrid_ [initialSort 0 SortDescending] cols items `nodeKey` "testKey"
        handleEvent _wenv _node () () =
          [scrollToRow (WidgetKey "testKey") scrollToRowCallback]
        cmpNode =
          compositeV_ "test" () (error "should not be called") buildUI handleEvent [onInit ()]
        evts =
          nodeHandleEventEvts wenv [] cmpNode
    actualItems <- seq evts (takeMVar itemsMVar)
    actualItems
      `shouldBe` S.fromList
        [ (TestItem (fixedSize 1), 2),
          (TestItem (fixedSize 2), 1),
          (TestItem (fixedSize 3), 0)
        ]

testColumn :: Text -> (TestItem -> SizeReq) -> Column TestEvent TestItem
testColumn name getHeight =
  (widgetColumn name (testCellWidget getHeight)) {minWidth = 10, paddingW = 0, paddingH = 0}

-- | We test with custom widgets because these will create special "Hagrid.Cell" nodes in the widget
-- tree that we can later use to pick out the cell widgets.
testCellWidget :: (TestItem -> SizeReq) -> Int -> TestItem -> WidgetNode s TestEvent
testCellWidget getHeight _idx item = wgt
  where
    wgt = label "test" `styleBasic` [sizeReqW reqW, sizeReqH reqH]
    reqW = fixedSize 100
    reqH = getHeight item

cellViewports :: [Column TestEvent TestItem] -> [TestItem] -> [Rect]
cellViewports columnDefs items =
  cellViewportsEvts columnDefs items []

cellViewportsEvts :: [Column TestEvent TestItem] -> [TestItem] -> [SystemEvent] -> [Rect]
cellViewportsEvts columnDefs items evts =
  hagridViewports columnDefs items evts "Hagrid.Cell"

hagridViewports :: [Column TestEvent TestItem] -> [TestItem] -> [SystemEvent] -> WidgetType -> [Rect]
hagridViewports columnDefs items =
  viewports (hagrid columnDefs (S.fromList items))

viewports :: WidgetNode TestModel TestEvent -> [SystemEvent] -> WidgetType -> [Rect]
viewports wgt evts wType = Foldable.toList childVps
  where
    startNode = nodeInit wenv wgt
    ((wenv', eventedNode, _reqs), _) = nodeHandleEvents wenv WNoInit evts startNode
    resizedNode = nodeResize wenv' eventedNode (eventedNode ^. L.info . L.viewport)
    instanceTree = widgetGetInstanceTree (resizedNode ^. L.widget) wenv' resizedNode
    childVps = roundRectUnits . _wniViewport . _winInfo <$> widgetsOfType wType instanceTree

-- Extract the column widths by observing the locations of the special drag handle widgets
columnWidths :: WidgetNode TestModel TestEvent -> [Int]
columnWidths node = fromFractional <$> colWidths
  where
    (_, colWidths) =
      Foldable.foldl' (\(px, cws) (Rect x _y w _h) -> (x + w, (x + w / 2 - px) : cws)) (0, []) vps
    vps =
      _wniViewport . _winInfo <$> dragHandles
    dragHandles =
      widgetsOfType "Hagrid.HeaderDragHandle" instanceTree
    instanceTree =
      widgetGetInstanceTree (_wnWidget node) wenv node

widgetsOfType :: WidgetType -> WidgetInstanceNode -> [WidgetInstanceNode]
widgetsOfType typ node = result
  where
    result
      | (node ^. (L.info . L.widgetType)) == typ = node : childOnes
      | otherwise = childOnes
    childOnes =
      foldMap (widgetsOfType typ) (node ^. L.children)

windowSize :: Size
windowSize = Size 100 100

wenv :: WidgetEnv TestModel TestEvent
wenv = mockWenv TestModel & L.windowSize .~ windowSize
