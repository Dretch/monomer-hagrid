module Monomer.HagridSpec (spec) where

import Control.Lens ((&), (.~), (^.))
import qualified Data.Foldable as Foldable
import Data.Text (Text)
import Monomer
import Monomer.Hagrid
import qualified Monomer.Lens as L
import Monomer.TestUtil
import Test.Hspec (Spec, describe, it, pendingWith, shouldBe)

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

resize :: Spec
resize = describe "resize" $ do
  it "should assign cell widths according to column widths" $ do
    cellViewports
      [ (testColumn "Col 1" sizeReq) {initialWidth = 33},
        (testColumn "Col 2" sizeReq) {initialWidth = 77}
      ]
      [TestItem (fixedSize 10)]
      `shouldBe` [ Rect 0 40 33 10,
                   Rect 33 40 77 10
                 ]

  it "should assign single column cell heights according to cell heights" $ do
    cellViewports
      [ (testColumn "Col 1" sizeReq) {initialWidth = 50}
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
      [ (testColumn "Col 1" sizeReq) {initialWidth = 50}
      ]
      [TestItem (fixedSize 10 & L.flex .~ 7)]
      `shouldBe` [Rect 0 40 50 17]

sorting :: Spec
sorting = describe "sorting" $ do
  it "should not sort rows by default" $ do
    cellViewports
      [(testColumn "Col 1" sizeReq) {initialWidth = 50}]
      [TestItem (fixedSize 20), TestItem (fixedSize 10), TestItem (fixedSize 30)]
      `shouldBe` [Rect 0 40 50 20, Rect 0 60 50 10, Rect 0 70 50 30]

  it "should sort by ascending when column header clicked" $ do
    pendingWith "not sure how to make this test work yet"
    cellViewports_
      [(testColumn "Col 1" sizeReq) {initialWidth = 50}]
      [TestItem (fixedSize 20), TestItem (fixedSize 10), TestItem (fixedSize 30)]
      [Click (Point 10 10) BtnLeft 1]
      `shouldBe` [Rect 0 40 50 10, Rect 0 50 50 20, Rect 0 70 50 30]

  it "should sort by descending when column header clicked again" $ do
    pendingWith "not sure how to make this test work yet"

merging :: Spec
merging = describe "merging" $ do
  it "should preserve column widths when items change" $ do
    let col = textColumn "Col" (const "")
        startNode = nodeInit wenv (hagrid [col {initialWidth = 75}] [TestItem (fixedSize 1)])
        mergedNode = nodeMerge wenv (hagrid [col {initialWidth = 64}] [TestItem (fixedSize 2)]) startNode
        resizedNode = nodeResize wenv mergedNode (mergedNode ^. L.info . L.viewport)
    columnWidths startNode `shouldBe` [75]
    columnWidths resizedNode `shouldBe` [75]

testColumn :: Text -> (TestItem -> SizeReq) -> ColumnDef TestEvent TestItem
testColumn name getHeight =
  (widgetColumn name (testCellWidget getHeight)) {paddingW = 0, paddingH = 0}

-- | We test with custom widgets because these will create special "Hagrid.Cell" nodes in the widget
-- tree that we can later use to pick out the cell widgets.
testCellWidget :: (TestItem -> SizeReq) -> TestItem -> WidgetNode s TestEvent
testCellWidget getHeight item = wgt
  where
    wgt = label "test" `styleBasic` [sizeReqW reqW, sizeReqH reqH]
    reqW = fixedSize 10
    reqH = getHeight item

cellViewports :: [ColumnDef TestEvent TestItem] -> [TestItem] -> [Rect]
cellViewports columnDefs items =
  cellViewports_ columnDefs items []

cellViewports_ :: [ColumnDef TestEvent TestItem] -> [TestItem] -> [SystemEvent] -> [Rect]
cellViewports_ columnDefs items evts = Foldable.toList childVps
  where
    node = nodeInit wenv (hagrid columnDefs items)
    node' = nodeHandleEventRoot wenv evts node
    node'' = widgetGetInstanceTree (node ^. L.widget) wenv node'
    childVps = roundRectUnits . _wniViewport . _winInfo <$> widgetsOfType "Hagrid.Cell" node''

-- Extract the column widths by observing the locations of the special drag handle widgets
columnWidths :: WidgetNode TestModel TestEvent -> [Int]
columnWidths node = fromFractional <$> colWidths
  where
    (_, colWidths) =
      Foldable.foldl' (\(px, cws) (Rect x _y w _h) -> (x + w, (x + w - px) : cws)) (0, []) vps
    vps =
      _wniViewport . _winInfo <$> dragHandles
    dragHandles =
      widgetsOfType "Hagrid.HeaderDragHandle" instanceTree
    instanceTree =
      widgetGetInstanceTree (_wnWidget node) wenv node

widgetsOfType :: WidgetType -> WidgetInstanceNode -> [WidgetInstanceNode]
widgetsOfType typ node = thisOne <> childOnes
  where
    thisOne
      | (node ^. (L.info . L.widgetType)) == typ = [node]
      | otherwise = []
    childOnes =
      mconcat (widgetsOfType typ <$> Foldable.toList (node ^. L.children))

windowSize :: Size
windowSize = Size 100 100

wenv :: WidgetEnv TestModel TestEvent
wenv = mockWenv TestModel & L.windowSize .~ windowSize
