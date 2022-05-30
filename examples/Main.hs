{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Control.Lens (Ixed (ix), makeLensesFor, singular)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day, addDays, defaultTimeLocale, formatTime, fromGregorian)
import Monomer
import Monomer.Hagrid (Column (..), ColumnSortKey (SortWith), SortDirection, hagrid, showOrdColumn, textColumn, widgetColumn)
import Text.Printf (printf)

data AppModel = AppModel
  { theme :: Theme,
    spiders :: [Spider],
    columns :: [AppColumn]
  }
  deriving (Eq, Show)

newtype AppColumn = AppColumn
  {enabled :: Bool}
  deriving (Eq, Show)

data AppEvent
  = FeedSpider Text
  | NameColumnResized Int
  | NameColumnSorted SortDirection

data Spider = Spider
  { index :: Integer,
    species :: Text,
    name :: Text,
    dateOfBirth :: Day,
    weightKilos :: Double
  }
  deriving (Eq, Show)

makeLensesFor [("enabled", "_enabled")] ''AppColumn
makeLensesFor [("columns", "_columns"), ("theme", "_theme")] ''AppModel

main :: IO ()
main = startApp model handleEvent buildUI config
  where
    config =
      [ appWindowTitle "Hagrid Examples",
        appFontDef "Regular" "./assets/fonts/Cantarell/Cantarell-Regular.ttf",
        appDisableAutoScale True
      ]
    model =
      AppModel
        { theme = darkTheme,
          spiders = spiders,
          columns = AppColumn True <$ gridColumns
        }
    spiders = spider <$> [1 .. numSpiders]
    spider i =
      Spider
        { index = i,
          species = "Acromantula",
          name = T.pack (printf "Son of Aragog %d" i),
          dateOfBirth = addDays i (fromGregorian 1942 3 0),
          weightKilos = fromIntegral (numSpiders + 1 - i) * 2.3
        }
    numSpiders = 100

buildUI :: UIBuilder AppModel AppEvent
buildUI _wenv model = tree
  where
    tree =
      themeSwitch_ model.theme [themeClearBg] $
        vstack
          [ grid,
            vstack_
              [childSpacing]
              (themeConfigurer : columnConfigurers)
              `styleBasic` [padding 8]
          ]

    grid =
      hagrid
        (mconcat (zipWith column model.columns gridColumns))
        model.spiders

    column (AppColumn enabled) columnDef =
      [columnDef | enabled]

    themeConfigurer =
      hstack_
        [childSpacing]
        [ labeledRadio_ "Dark Theme" darkTheme _theme [textRight],
          labeledRadio_ "Light Theme" lightTheme _theme [textRight]
        ]

    columnConfigurers =
      zipWith columnConfigurer [0 .. length model.columns - 1] gridColumns

    columnConfigurer :: Int -> Column AppEvent Spider -> WidgetNode AppModel AppEvent
    columnConfigurer idx columnDef =
      labeledCheckbox_
        columnDef.name
        (_columns . singular (ix idx) . _enabled)
        [textRight]

handleEvent :: EventHandler AppModel AppEvent sp ep
handleEvent _wenv _node _model = \case
  FeedSpider name ->
    [Producer (const (putStrLn ("Feeding spider " <> T.unpack name)))]
  NameColumnResized colWidth ->
    [Producer (const (putStrLn ("Name column was resized: " <> show colWidth)))]
  NameColumnSorted direction ->
    [Producer (const (putStrLn ("Name column was sorted: " <> show direction)))]

gridColumns :: [Column AppEvent Spider]
gridColumns = cols
  where
    cols =
      [ showOrdColumn "Index" (.index),
        (textColumn "Name" (.name))
          { initialWidth = 300,
            resizeHandler = Just NameColumnResized,
            sortHandler = Just NameColumnSorted
          },
        (textColumn "Species" (.species))
          { initialWidth = 200
          },
        (textColumn "Date of Birth" (T.pack . formatTime defaultTimeLocale "%Y-%m-%d" . dateOfBirth))
          { initialWidth = 200
          },
        (textColumn "Weight (Kg)" (T.pack . printf "%.2f" . weightKilos))
          { sortKey = SortWith weightKilos,
            initialWidth = 200
          },
        (widgetColumn "Actions" actionsColumn)
          { initialWidth = 100,
            paddingW = 5,
            paddingH = 5
          }
      ]
    actionsColumn :: Spider -> WidgetNode s AppEvent
    actionsColumn spdr =
      button "Feed" (FeedSpider spdr.name)
