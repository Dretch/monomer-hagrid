{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Data.Text (Text, lines, pack)
import Data.Text.IO (readFile)
import Monomer
import Monomer.Hagrid (hagrid, initialWidth, textColumn, widgetColumn)
import Prelude hiding (lines, readFile)

newtype AppModel = AppModel
  { bookLines :: [Text]
  }
  deriving (Eq, Show)

data AppEvent

main :: IO ()
main = do
  bookLines <- lines <$> readFile "./assets/etc/roughing-it.txt"
  startApp (model bookLines) handleEvent buildUI config
  where
    config =
      [ appWindowTitle "Hagrid Big Grid Example",
        appFontDef "Bold" "./assets/fonts/Cantarell/Cantarell-Bold.ttf",
        appFontDef "Regular" "./assets/fonts/Cantarell/Cantarell-Regular.ttf",
        appTheme darkTheme,
        appDisableAutoScale True,
        appWindowState (MainWindowNormal (1200, 1000))
      ]
    model bookLines =
      AppModel
        { bookLines
        }

buildUI :: UIBuilder AppModel AppEvent
buildUI _wenv model = tree
  where
    tree =
      hagrid
        [ (textColumn "Author" (const "Mark Twain")) {initialWidth = 140},
          (textColumn "Title" (const "Roughing It")) {initialWidth = 140},
          widgetColumn "Line Index" (\i _ -> label (pack (show i))),
          (textColumn "Line" id) {initialWidth = 800}
        ]
        model.bookLines

handleEvent :: EventHandler AppModel AppEvent sp ep
handleEvent _wenv _node _model = \case {}
