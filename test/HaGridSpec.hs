module HaGridSpec (spec) where

import Control.Lens ((&), (.~), (^.))
import qualified Data.Foldable as Foldable
import HaGrid
import Monomer
import qualified Monomer.Lens as L
import Monomer.TestUtil
import Test.Hspec (Spec, describe, it, shouldBe)

data TestModel = TestModel
  deriving (Eq, Show)

data TestEvent

newtype TestItem = TestItem
  {_tiHeight :: Double}
  deriving (Eq, Show)

spec :: Spec
spec = do
  resize

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

-- | We test with custom widgets because these will create special "HaGrid.Cell" nodes in the widget
-- tree that we can later use to pick out the cell widgets.
testCellWidget :: (TestItem -> Double) -> TestItem -> WidgetNode s TestEvent
testCellWidget getHeight item = wgt
  where
    wgt = label "test" `styleBasic` [sizeReqW reqW, sizeReqH reqH]
    reqW = fixedSize 10
    reqH = fixedSize (getHeight item)

cellViewports :: [ColumnDef TestEvent TestItem] -> [TestItem] -> [Rect]
cellViewports columnDefs items = Foldable.toList childVps
  where
    node = nodeInit wenv (haGrid columnDefs items)
    node' = widgetGetInstanceTree (node ^. L.widget) wenv node
    childVps = roundRectUnits . _wniViewport . _winInfo <$> widgetsOfType "HaGrid.Cell" node'

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
