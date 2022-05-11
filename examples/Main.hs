{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text as T
import HaGrid (columnInitialWidth, customColumn, haGrid, showOrdColumn, textColumn, columnPadding)
import Monomer
import Text.Printf (printf)

newtype AppModel = AppModel
  { _appSpiders :: [Spider]
  }
  deriving (Eq, Show)

newtype AppEvent
  = FeedSpider Text

data Spider = Spider
  { _sSpecies :: Text,
    _sName :: Text,
    _sDateOfBirth :: Text,
    _sWeightKilos :: Integer
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
    spiders = spider <$> [1 .. 30]
    spider i =
      Spider
        { _sSpecies = "Acromantula",
          _sName = T.pack (printf "Son of Aragog %d" i),
          _sDateOfBirth = T.pack (printf "1942-04-%2d" i),
          _sWeightKilos = i * 2
        }

buildUI :: UIBuilder AppModel AppEvent
buildUI _wenv model = grid
  where
    grid =
      haGrid
        [ textColumn "Name" _sName
            `columnInitialWidth` 300,
          textColumn "Species" _sSpecies
            `columnInitialWidth` 200,
          textColumn "Date of Birth" _sDateOfBirth
            `columnInitialWidth` 200,
          showOrdColumn "Weight (Kg)" _sWeightKilos
            `columnInitialWidth` 200,
          customColumn "Actions" actionsColumn
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
