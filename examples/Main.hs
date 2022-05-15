{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text as T
import HaGrid (ColumnSortKey (SortWith), columnInitialWidth, columnPadding, columnSortKey, haGrid, showOrdColumn, textColumn, widgetColumn)
import Monomer
import Text.Printf (printf)
import Data.Time (Day, fromGregorian, formatTime, defaultTimeLocale, addDays)

newtype AppModel = AppModel
  { _appSpiders :: [Spider]
  }
  deriving (Eq, Show)

newtype AppEvent
  = FeedSpider Text

data Spider = Spider
  { _sIndex :: Integer,
    _sSpecies :: Text,
    _sName :: Text,
    _sDateOfBirth :: Day,
    _sWeightKilos :: Double
  }
  deriving (Eq, Show)

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
      AppModel {_appSpiders = spiders}
    spiders = spider <$> [1 .. numSpiders]
    spider i =
      Spider
        { _sIndex = i,
          _sSpecies = "Acromantula",
          _sName = T.pack (printf "Son of Aragog %d" i),
          _sDateOfBirth = addDays i (fromGregorian 1942 3 0),
          _sWeightKilos = fromIntegral (numSpiders + 1 - i) * 2.3
        }
    numSpiders = 1000

buildUI :: UIBuilder AppModel AppEvent
buildUI _wenv model = grid
  where
    grid =
      haGrid
        [ showOrdColumn "Index" _sIndex,
          textColumn "Name" _sName
            `columnInitialWidth` 300,
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
        (_appSpiders model)
    actionsColumn spdr =
      button "Feed" (FeedSpider (_sName spdr))

handleEvent :: EventHandler AppModel AppEvent sp ep
handleEvent _wenv _node _model = \case
  FeedSpider name ->
    [Producer (const (putStrLn ("Feeding spider " <> T.unpack name)))]
