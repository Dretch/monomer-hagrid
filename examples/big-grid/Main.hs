module Main (main) where

import Data.Function ((&))
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Text (Text, dropWhile, dropWhileEnd, pack, splitOn, strip)
import Data.Text.IO (readFile)
import Monomer
import Monomer.Hagrid (estimatedItemHeight, hagrid_, initialWidth, textColumn, widgetColumn)
import Prelude hiding (dropWhile, readFile)

newtype AppModel = AppModel
  { paragraphs :: Seq Text
  }
  deriving (Eq, Show)

data AppEvent

main :: IO ()
main = do
  paragraphs <- splitParagraphs <$> readFile "./assets/etc/war-and-peace.txt"
  startApp (model paragraphs) handleEvent buildUI config
  where
    config =
      [ appWindowTitle "Hagrid Big Grid Example",
        appFontDef "Bold" "./assets/fonts/Cantarell/Cantarell-Bold.ttf",
        appFontDef "Regular" "./assets/fonts/Cantarell/Cantarell-Regular.ttf",
        appTheme darkTheme,
        appDisableAutoScale True,
        appWindowState (MainWindowNormal (1200, 1000))
      ]
    model paragraphs =
      AppModel
        { paragraphs
        }

buildUI :: UIBuilder AppModel AppEvent
buildUI _wenv model = tree
  where
    tree =
      hagrid_
        [estimatedItemHeight 100]
        [ (textColumn "Author" (const "Leo Tolstoy")) {initialWidth = 180},
          (textColumn "Title" (const "War and Peace")) {initialWidth = 160},
          widgetColumn "Line Index" (\i _ -> label (pack (show i))),
          (widgetColumn "Line" (\_ para -> label_ para [multiline, ellipsis])) {initialWidth = 760}
        ]
        model.paragraphs

handleEvent :: EventHandler AppModel AppEvent sp ep
handleEvent _wenv _node _model = \case {}

splitParagraphs :: Text -> Seq Text
splitParagraphs s =
  splitOn "\n\n" s
    & S.fromList
    & fmap (dropWhile (== '\n') . dropWhileEnd (== '\n'))
    & S.filter ((/= mempty) . strip)
