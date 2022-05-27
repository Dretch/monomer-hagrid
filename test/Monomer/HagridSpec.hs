module Monomer.HagridSpec (spec) where

import Control.Lens ((&), (.~), (^.))
import qualified Data.Foldable as Foldable
import Monomer
import Monomer.Hagrid
import qualified Monomer.Lens as L
import Monomer.TestUtil
import Test.Hspec (Spec, describe, it, shouldBe, pendingWith)

data TestModel = TestModel
  deriving (Eq, Show)

data TestEvent

newtype TestItem = TestItem
  {_tiHeight :: Double}
  deriving (Eq, Show)

spec :: Spec
spec = do
  resize
  sorting

resize :: Spec
resize = describe "resize" $ do
  it "should assign cell widths according to column widths" $ do
    cellViewports
      [ widgetColumn "Col 1" (testCellWidget _tiHeight) `columnPadding` 0 `columnInitialWidth` 33,
        widgetColumn "Col 2" (testCellWidget _tiHeight) `columnPadding` 0 `columnInitialWidth` 77
      ]
      [TestItem 10]
      `shouldBe` [ Rect 0 40 33 10,
                   Rect 33 40 77 10
                 ]

  it "should assign single column cell heights according to cell heights" $ do
    cellViewports
      [ widgetColumn "Col 1" (testCellWidget _tiHeight) `columnPadding` 0 `columnInitialWidth` 50
      ]
      [TestItem 33, TestItem 77]
      `shouldBe` [Rect 0 40 50 33, Rect 0 73 50 77]

  it "should assign multi column cell heights according to max cell height from row" $ do
    cellViewports
      [ widgetColumn "Col 1" (testCellWidget (const 33)) `columnPadding` 0 `columnInitialWidth` 50,
        widgetColumn "Col 2" (testCellWidget (const 47)) `columnPadding` 0 `columnInitialWidth` 50
      ]
      [TestItem 0]
      `shouldBe` [Rect 0 40 50 47, Rect 50 40 50 47]

sorting :: Spec
sorting = describe "sorting" $ do
  it "should not sort rows by default" $ do
    cellViewports
      [widgetColumn "Col 1" (testCellWidget _tiHeight) `columnPadding` 0 `columnInitialWidth` 50]
      [TestItem 20, TestItem 10, TestItem 30]
      `shouldBe` [Rect 0 40 50 20, Rect 0 60 50 10, Rect 0 70 50 30]
  
  it "should sort by ascending when column header clicked" $ do
    pendingWith "not sure how to make this test work yet"
    cellViewports_
      [widgetColumn "Col 1" (testCellWidget _tiHeight) `columnPadding` 0 `columnInitialWidth` 50]
      [TestItem 20, TestItem 10, TestItem 30]
      [Click (Point 10 10) BtnLeft 1]
      `shouldBe` [Rect 0 40 50 10, Rect 0 50 50 20, Rect 0 70 50 30]
  
  it "should sort by descending when column header clicked again" $ do
    pendingWith "not sure how to make this test work yet"

-- | We test with custom widgets because these will create special "Hagrid.Cell" nodes in the widget
-- tree that we can later use to pick out the cell widgets.
testCellWidget :: (TestItem -> Double) -> TestItem -> WidgetNode s TestEvent
testCellWidget getHeight item = wgt
  where
    wgt = label "test" `styleBasic` [sizeReqW reqW, sizeReqH reqH]
    reqW = fixedSize 10
    reqH = fixedSize (getHeight item)

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
