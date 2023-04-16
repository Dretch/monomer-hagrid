{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main (main) where

import Control.Lens (Ixed (ix), makeLensesFor, singular)
import Data.Sequence (Seq ((:|>)))
import qualified Data.Sequence as S
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Time (Day, addDays, defaultTimeLocale, formatTime, fromGregorian)
import Monomer
import Monomer.Hagrid (Column (..), ColumnAlign (..), ColumnFooterWidget (..), ColumnSortKey (..), SortDirection (..), hagrid_, initialSort, scrollToRow, showOrdColumn, textColumn, widgetColumn)
import Text.Printf (printf)

data AppModel = AppModel
  { theme :: Theme,
    spiders :: Seq Spider,
    columns :: [AppColumn],
    rowToScrollTo :: Int
  }
  deriving (Eq, Show)

newtype AppColumn = AppColumn
  {enabled :: Bool}
  deriving (Eq, Show)

data AppEvent
  = FeedSpider Int
  | AddSpider
  | NameColumnResized Int
  | NameColumnSorted SortDirection
  | ScrollToOriginalIndex

data Spider = Spider
  { index :: Int,
    species :: Text,
    name :: Text,
    dateOfBirth :: Day,
    weightKilos :: Double
  }
  deriving (Eq, Show)

makeLensesFor [("enabled", "_enabled")] ''AppColumn
makeLensesFor [("columns", "_columns"), ("theme", "_theme"), ("rowToScrollTo", "_rowToScrollTo")] ''AppModel

main :: IO ()
main = startApp model handleEvent buildUI config
  where
    config =
      [ appWindowTitle "Hagrid Basic Example",
        appFontDef "Bold" "./assets/fonts/Cantarell/Cantarell-Bold.ttf",
        appFontDef "Regular" "./assets/fonts/Cantarell/Cantarell-Regular.ttf",
        appDisableAutoScale True,
        appWindowState (MainWindowNormal (1200, 1000))
      ]
    model =
      AppModel
        { theme = darkTheme,
          spiders = spiders,
          columns = AppColumn True <$ gridColumns,
          rowToScrollTo = 0
        }
    spiders = S.fromFunction numSpiders spider
    spider i =
      Spider
        { index = i,
          species = "Acromantula",
          name = T.pack (printf "Son of Aragog %d" (i + 1)),
          dateOfBirth = addDays (fromIntegral i) (fromGregorian 1942 3 1),
          weightKilos = fromIntegral (numSpiders + 2 - i) * 2.3
        }
    numSpiders = 100

buildUI :: UIBuilder AppModel AppEvent
buildUI _wenv model = tree
  where
    tree =
      themeSwitch_ model.theme [themeClearBg] $
        vstack
          [ grid `nodeKey` hagridKey,
            vstack_
              [childSpacing]
              (themeConfigurer : columnConfigurers <> actionButtons)
              `styleBasic` [padding 8]
          ]

    grid =
      hagrid_
        [initialSort 1 SortDescending]
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

    actionButtons =
      [ hstack_
          [childSpacing]
          [ label "Scroll to index of unsorted list",
            numericField _rowToScrollTo,
            button "Go!" ScrollToOriginalIndex
          ],
        hstack_
          [childSpacing]
          [button "Add spider" AddSpider]
      ]

handleEvent :: EventHandler AppModel AppEvent sp ep
handleEvent _wenv _node model = \case
  FeedSpider idx
    | Just spdr <- S.lookup idx model.spiders ->
        [ Producer (const (putStrLn ("Feeding spider " <> T.unpack spdr.name))),
          Model model {spiders = S.update idx spdr {weightKilos = spdr.weightKilos + 1} model.spiders}
        ]
  FeedSpider _ ->
    []
  AddSpider ->
    [Model model {spiders = model.spiders :|> newSpider model}]
  NameColumnResized colWidth ->
    [Producer (const (putStrLn ("Name column was resized: " <> show colWidth)))]
  NameColumnSorted direction ->
    [Producer (const (putStrLn ("Name column was sorted: " <> show direction)))]
  ScrollToOriginalIndex ->
    [scrollToRow (WidgetKey hagridKey) (rowScrollIndex model)]

newSpider :: AppModel -> Spider
newSpider model =
  Spider
    { index = fromIntegral (length model.spiders),
      name = "Extra Spider " <> pack (show (length model.spiders)),
      species = "Spider plant",
      dateOfBirth = fromGregorian 2022 6 26,
      weightKilos = 0.01
    }

rowScrollIndex :: AppModel -> Seq (Spider, Int) -> Maybe Int
rowScrollIndex model items =
  snd <$> S.lookup model.rowToScrollTo items

gridColumns :: [Column AppEvent Spider]
gridColumns = cols
  where
    cols =
      [ (showOrdColumn "Index" (.index))
          { initialWidth = 120,
            align = ColumnAlignRight,
            footerWidget = CustomFooterWidget countFooter
          },
        (textColumn "Name" (.name))
          { initialWidth = 300,
            resizeHandler = Just NameColumnResized,
            sortHandler = Just NameColumnSorted
          },
        (textColumn "Species" (.species))
          { initialWidth = 200
          },
        (textColumn "Date of Birth" (T.pack . formatTime defaultTimeLocale "%Y-%m-%d" . (.dateOfBirth)))
          { initialWidth = 200
          },
        (textColumn "Weight (Kg)" (T.pack . printf "%.2f" . (.weightKilos)))
          { sortKey = SortWith (.weightKilos),
            initialWidth = 200,
            align = ColumnAlignRight,
            footerWidget = CustomFooterWidget sumWeightFooter
          },
        (widgetColumn "Actions" actionsColumn)
          { initialWidth = 100,
            paddingW = 5,
            paddingH = 5
          }
      ]

    countFooter spiders =
      labelledFooter "Count" (T.pack . show . length $ spiders)

    sumWeightFooter spiders = tree
      where
        tree = labelledFooter "Sum" (T.pack (printf "%.2f" totalWeightKilos))
        totalWeightKilos = sum ((.weightKilos) . fst <$> spiders)

    labelledFooter labelText text =
      hstack
        [ label labelText,
          filler,
          label text
            `styleBasic` [textFont "Bold"]
        ]
        `styleBasic` [padding 10]

    actionsColumn :: Int -> Spider -> WidgetNode s AppEvent
    actionsColumn idx _spdr =
      button "Feed" (FeedSpider idx)

hagridKey :: Text
hagridKey = "SpiderHagrid"
