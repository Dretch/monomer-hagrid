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
import Monomer
import Monomer.Hagrid (ColumnDef (..), ColumnSortKey (SortWith), SortDirection, columnInitialWidth, columnPadding, columnResizeHandler, columnSortHandler, columnSortKey, hagrid, showOrdColumn, textColumn, widgetColumn)
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
  | NameColumnSorted SortDirection

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
      [ appWindowTitle "Hagrid Examples",
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
      hagrid
        (mconcat (zipWith column (model ^. columns) gridColumns))
        (_appSpiders model)

    column (AppColumn enabled) columnDef =
      [columnDef | enabled]

    columnConfigurer :: Int -> ColumnDef AppEvent Spider -> WidgetNode AppModel AppEvent
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

gridColumns :: [ColumnDef AppEvent Spider]
gridColumns = cols
  where
    cols =
      [ showOrdColumn "Index" _sIndex,
        textColumn "Name" _sName
          `columnInitialWidth` 300
          `columnResizeHandler` NameColumnResized
          `columnSortHandler` NameColumnSorted,
        textColumn "Species" _sSpecies
          `columnInitialWidth` 200,
        textColumn "Date of Birth" (T.pack . formatTime defaultTimeLocale "%Y-%m-%d" . _sDateOfBirth)
          `columnInitialWidth` 200,
        textColumn "Weight (Kg)" (T.pack . printf "%.2f" . _sWeightKilos)
          `columnSortKey` SortWith _sWeightKilos
          `columnInitialWidth` 200,
        widgetColumn "Actions" actionsColumn
          `columnInitialWidth` 100
          `columnPadding` 5
      ]
    actionsColumn spdr =
      button "Feed" (FeedSpider (_sName spdr))
