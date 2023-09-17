module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM, forever)
import Data.Default.Class (def)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Text (Text, pack, words)
import Monomer
import Monomer.Hagrid (estimatedItemHeight, hagrid_, initialWidth, minWidth, showOrdColumn, textColumn, widgetColumn)
import Monomer.Widgets.Single
import System.Random.Stateful (globalStdGen, uniformRM)
import Prelude hiding (words)

newtype AppModel = AppModel
  { cellModels :: Seq CellModel
  }
  deriving (Eq, Show)

data CellModel = CellModel
  { info :: Text,
    dotColor :: Color,
    dotSize :: Double,
    dotSpeed :: Int,
    dotPhase :: Int
  }
  deriving (Eq, Show)

data AppEvent = AppInit | UpdatePhase

main :: IO ()
main = do
  cellModels <- forM (take 1_000 (zip infos colors)) $ \(info, dotColor) -> do
    dotSize <- uniformRM (30, 200) globalStdGen
    dotPhase <- uniformRM (0, 180) globalStdGen
    dotSpeed <- uniformRM (1, 4) globalStdGen
    pure CellModel {info, dotColor, dotSize, dotPhase, dotSpeed}
  startApp (model cellModels) handleEvent buildUI config
  where
    config =
      [ appWindowTitle "Hagrid Resizing Cells Example",
        appFontDef "Bold" "./assets/fonts/Cantarell/Cantarell-Bold.ttf",
        appFontDef "Regular" "./assets/fonts/Cantarell/Cantarell-Regular.ttf",
        appTheme darkTheme,
        appDisableAutoScale True,
        appWindowState (MainWindowNormal (1_200, 1_000)),
        appInitEvent AppInit
      ]
    model cellModels =
      AppModel {cellModels = S.fromList cellModels}
    colors =
      cycle [red, green, blue, yellow]
    infos =
      cycle . words $
        "This demo shows how hagrid behaves when cells are dynamically changing size."

buildUI :: UIBuilder AppModel AppEvent
buildUI _wenv model = tree
  where
    tree =
      hagrid_
        [estimatedItemHeight 60]
        [ widgetColumn "Line Index" (\i _ -> label (pack (show i))),
          (textColumn "Info" (.info)) {initialWidth = 200},
          (showOrdColumn "Phase" (.dotPhase)) {initialWidth = 300},
          (widgetColumn "Dot" dotWidget) {minWidth = 220}
        ]
        model.cellModels

handleEvent :: EventHandler AppModel AppEvent sp ep
handleEvent _wenv _node model = \case
  AppInit ->
    [ Producer $ \emit -> do
        forever $ do
          emit UpdatePhase
          threadDelay (1_000_000 `div` 30)
    ]
  UpdatePhase ->
    [Model model {cellModels = updatePhase <$> model.cellModels}]
    where
      updatePhase cellModel =
        cellModel {dotPhase = cellModel.dotPhase + cellModel.dotSpeed}

dotWidget :: Int -> CellModel -> WidgetNode s AppEvent
dotWidget _idx cellModel = tree
  where
    tree =
      defaultWidgetNode "DotWidget" $
        createSingle
          cellModel
          def
            { singleGetSizeReq = getSizeReq,
              singleMerge = merge,
              singleRender = render
            }

    size = cellModel.dotSize * abs (sin (fromIntegral cellModel.dotPhase / 100))

    merge _wenv node _oldNode oldState = resultReqs node [resizeReq | needResize]
      where
        resizeReq = ResizeWidgets node._wnInfo._wniWidgetId
        needResize = oldState /= cellModel

    getSizeReq _wenv _node =
      (fixedSize size, fixedSize size)

    render wenv node renderer =
      drawEllipse renderer carea {_rW = size, _rH = size} (Just cellModel.dotColor)
      where
        style = currentStyle wenv node
        carea = getContentArea node style
