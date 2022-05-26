{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Control.Lens (abbreviatedFields, ix, makeLensesWith, singular, (^.))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day, addDays, defaultTimeLocale, formatTime, fromGregorian)
import HaGrid (ColumnDef (..), ColumnSortKey (SortWith), SortDirection, columnInitialWidth, columnPadding, columnResizeHandler, columnSortHandler, columnSortKey, haGrid, showOrdColumn, textColumn, widgetColumn)
import Monomer
import Text.Printf (printf)

data AppModel = AppModel
  { _appSpiders :: [Spider],
    _appColumns :: [AppColumn]
  }
  deriving (Eq, Show)

newtype AppColumn = AppColumn
  {_acEnabled :: Bool}
  deriving (Eq, Show)

data AppEvent
  = FeedSpider Text
  | NameColumnResized Int
  | NameColumnSorted HaGrid.SortDirection

data Spider = Spider
  { _sIndex :: Integer,
    _sSpecies :: Text,
    _sName :: Text,
    _sDateOfBirth :: Day,
    _sWeightKilos :: Double
  }
  deriving (Eq, Show)

makeLensesWith abbreviatedFields ''AppColumn
makeLensesWith abbreviatedFields ''AppModel

main :: IO ()
main = startApp model handleEvent buildUI config
  where
    config =
      [ appWindowTitle "HaGrid Examples",
        appFontDef "Regular" "./assets/fonts/Cantarell/Cantarell-Regular.ttf",
        appTheme darkTheme,
        appDisableAutoScale True
      ]
    model =
      AppModel {_appSpiders = spiders, _appColumns = AppColumn True <$ gridColumns}
    spiders = spider <$> [1 .. numSpiders]
    spider i =
      Spider
        { _sIndex = i,
          _sSpecies = "Acromantula",
          _sName = T.pack (printf "Son of Aragog %d" i),
          _sDateOfBirth = addDays i (fromGregorian 1942 3 0),
          _sWeightKilos = fromIntegral (numSpiders + 1 - i) * 2.3
        }
    numSpiders = 100

buildUI :: UIBuilder AppModel AppEvent
buildUI _wenv model = tree
  where
    tree =
      vstack
        [ grid,
          vstack_
            [childSpacing]
            (zipWith columnConfigurer [0 .. length (model ^. columns) - 1] gridColumns)
            `styleBasic` [padding 8]
        ]

    grid =
      HaGrid.haGrid
        (mconcat (zipWith column (model ^. columns) gridColumns))
        (_appSpiders model)

    column (AppColumn enabled) columnDef =
      [columnDef | enabled]

    columnConfigurer :: Int -> HaGrid.ColumnDef AppEvent Spider -> WidgetNode AppModel AppEvent
    columnConfigurer idx columnDef =
      labeledCheckbox_
        (_cdName columnDef) -- todo: somehow export getters but not setters?
        (columns . singular (ix idx) . enabled)
        [textRight]

handleEvent :: EventHandler AppModel AppEvent sp ep
handleEvent _wenv _node _model = \case
  FeedSpider name ->
    [Producer (const (putStrLn ("Feeding spider " <> T.unpack name)))]
  NameColumnResized colWidth ->
    [Producer (const (putStrLn ("Name column was resized: " <> show colWidth)))]
  NameColumnSorted direction ->
    [Producer (const (putStrLn ("Name column was sorted: " <> show direction)))]

gridColumns :: [HaGrid.ColumnDef AppEvent Spider]
gridColumns = cols
  where
    cols =
      [ HaGrid.showOrdColumn "Index" _sIndex,
        HaGrid.textColumn "Name" _sName
          `HaGrid.columnInitialWidth` 300
          `HaGrid.columnResizeHandler` NameColumnResized
          `HaGrid.columnSortHandler` NameColumnSorted,
        HaGrid.textColumn "Species" _sSpecies
          `HaGrid.columnInitialWidth` 200,
        HaGrid.textColumn "Date of Birth" (T.pack . formatTime defaultTimeLocale "%Y-%m-%d" . _sDateOfBirth)
          `HaGrid.columnInitialWidth` 200,
        HaGrid.textColumn "Weight (Kg)" (T.pack . printf "%.2f" . _sWeightKilos)
          `HaGrid.columnSortKey` HaGrid.SortWith _sWeightKilos
          `HaGrid.columnInitialWidth` 200,
        HaGrid.widgetColumn "Actions" actionsColumn
          `HaGrid.columnInitialWidth` 100
          `HaGrid.columnPadding` 5
      ]
    actionsColumn spdr =
      button "Feed" (FeedSpider (_sName spdr))
