{-# LANGUAGE OverloadedStrings #-}

-- | Data definition of hand patterns
--   and declaration for all accepted patterns
module Game.Mahjong.Pattern (
  -- * The `Pattern` type
  Pattern, name, chinese, score,

  -- * Utility function
  updateScore,


  -- * 1.0 Trivial Patterns
  chickenHand, allSequences, concealedHand, selfDrawn, allSimples, allTypes, illegalCall,


  -- * 2.0 Triplets and Quartets

  -- ** 2.1 Triplet
  allTriplets,

  -- ** 2.2 Concealed triplets
  twoConcealedTriplets, threeConcealedTriplets, fourConcealedTriplets,

  -- ** 2.3 Quartets
  oneQuartet, twoQuartets, threeQuartets, fourQuartets,


  -- * 3.0 Identical Sets
  twoIdenticalSequences, twoIdenticalSequencesTwice, threeIdenticalSequences, fourIdenticalSequences,


  -- * 4.0 Similar Sets

  -- ** 4.1 Similar sequences
  threeSimilarSequences,

  -- ** 4.2 Similar triplets
  littleThreeSimilarTriplets, threeSimilarTriplets,


  -- * 5.0 Consecutive Sets

  -- ** 5.1 Consecutive sequences
  threeConsecutiveSequences, nineTileStraight, threeConsecutiveSequencesTwice, fourConsecutiveSequences,

  -- ** 5.2 Consecutive triplets
  threeConsecutiveTriplets, fourConsecutiveTriplets, threeMothers,


  -- * 6.0 Suit Patterns

  -- ** 6.1 Mixed and pure
  mixedOneSuit, pureOneSuit,

  -- ** 6.2 Nine Gates
  nineGates,


  -- * 7.0 Terminal Tiles

  -- ** 7.1 Sequence and Triplet
  twoTailedTerminalSequences, twoTailedTerminalTriplets, twoTailedTerminals,

  -- ** 7.2 Mixed and Pure
  mixedLesserTerminals, pureLesserTerminals, mixedGreaterTerminals, pureGreaterTerminals,

  -- ** 7.3 Combination
  littleMountain, bigMountain,


  -- * 8.0 Honor Tiles

  -- ** 8.1 Winds
  windTriplet, littleThreeWinds, bigThreeWinds, littleFourWinds, bigFourWinds,

  -- ** 8.2 Dragons
  dragonTriplet, littleThreeDragons, bigThreeDragons,

  -- ** 8.3 Pure honors
  allHonors, allHonorPairs,


  -- * 9.0 Color Hands
  allGreen, allRed,


  -- * 10.0 Irregular Hands

  -- ** 10.1 Thirteen Orphans
  thirteenOrphans, thirteenOrphansWaits,

  -- ** 10.2 Seven Pairs

  -- ** 10.2.1 Basic seven pairs
  sevenPairs,

  -- ** 10.2.2 Specialized seven pairs
  sevenShiftedPairs, grandChariot, bambooForest, numerousNeighbors,


  -- * 11.0 Incidental bonuses

  -- ** 11.1 Final tile
  finalDraw, finalDiscard,

  -- ** 11.2 Winning on displacement tile
  winOnQuartet, winOnBonusTile,

  -- ** 11.3 Robbing a quartet
  robbingAQuartet,

  -- ** 11.4 Blessings / First tile
  blessingOfHeaven, blessingOfEarth,


  -- * 12.0 Bonus Tiles

  -- ** 12.1 Basic flower points
  bonusFlowerSeason,

  -- ** 12.2 Flower quartets
  fourFlowers, fourSeasons,

  -- ** 12.3 Both sets of bonus tile
  allBonusTiles
) where

import Game.Mahjong.Class

import qualified Data.Text as T

-------------------------------------------------------------------------------
-- Data Declaration
-------------------------------------------------------------------------------


data Pattern =
  Pattern { name    :: T.Text  -- ^ the english name
          , chinese :: T.Text  -- ^ the chinese name
          , score   :: Int     -- ^ the score
          } deriving (Eq, Show)

instance Pretty Pattern where
  pp (Pattern n _ s) = T.unpack n ++ " : " ++ show s


-------------------------------------------------------------------------------
-- Hands List
-------------------------------------------------------------------------------

-- 1.0 Trivial Patterns

chickenHand, allSequences, concealedHand, selfDrawn, allSimples, allTypes, illegalCall :: Pattern
chickenHand   = Pattern "Chicken Hand"   "雞和"  1
allSequences  = Pattern "All Sequences"  "平和"  5
concealedHand = Pattern "Concealed Hand" "門前清" 5
selfDrawn     = Pattern "Self Drawn"     "自摸"  5
allSimples    = Pattern "All Simple"     "斷么九" 5
allTypes      = Pattern "All Types"      "五門齊" 10
illegalCall   = Pattern "Illegal Call"   "詐和"  (-40)


-- 2.0 Triplets and Quartets

allTriplets :: Pattern
allTriplets            = Pattern "All Triplets" "對對和" 30

twoConcealedTriplets, threeConcealedTriplets, fourConcealedTriplets :: Pattern
twoConcealedTriplets   = Pattern "Two Concealed Triplets"   "兩暗刻" 5
threeConcealedTriplets = Pattern "Three Concealed Triplets" "三暗刻" 30
fourConcealedTriplets  = Pattern "Four Concealed Triplets"  "四暗刻" 125

oneQuartet, twoQuartets, threeQuartets, fourQuartets :: Pattern
oneQuartet             = Pattern "One Quartet"    "一槓" 5
twoQuartets            = Pattern "Two Quartets"   "兩槓" 20
threeQuartets          = Pattern "Three Quartets" "三槓" 120
fourQuartets           = Pattern "Four Quartets"  "四槓" 480


-- 3.0 Identical Sets

twoIdenticalSequences, twoIdenticalSequencesTwice, threeIdenticalSequences, fourIdenticalSequences :: Pattern
twoIdenticalSequences      = Pattern "Two Identical Sequences"       "一般高"   10
twoIdenticalSequencesTwice = Pattern "Two Identical Sequences Twice" "兩般高"   60
threeIdenticalSequences    = Pattern "Three Identical Sequences"     "一色三同順" 120
fourIdenticalSequences     = Pattern "Four Identical Sequences"      "一色四同順" 480


-- 4.0 Similar Sets

threeSimilarSequences :: Pattern
threeSimilarSequences       = Pattern "Three Similar Sequences"       "三色同順"   35

littleThreeSimilarTriplets, threeSimilarTriplets :: Pattern
littleThreeSimilarTriplets  = Pattern "Little Three Similar Triplets" "三色小同刻"  30
threeSimilarTriplets        = Pattern "Three Similar Triplets"        "三色同刻"   120


-- 5.0 Consecutive Sets

threeConsecutiveSequences, nineTileStraight, threeConsecutiveSequencesTwice, fourConsecutiveSequences :: Pattern
threeConsecutiveSequences      = Pattern "Three Consecutive Sequences"       "三連順"  30
nineTileStraight               = Pattern "Nine-Tile Straight"                "一氣通貫" 40
threeConsecutiveSequencesTwice = Pattern "Three Consecutive Sequences Twice" "雙三連順" 50
fourConsecutiveSequences       = Pattern "Four Consecutive Sequences"        "四連順"  100

threeConsecutiveTriplets, fourConsecutiveTriplets, threeMothers :: Pattern
threeConsecutiveTriplets       = Pattern "Three Consecutive Triplets"        "三連刻"  100
fourConsecutiveTriplets        = Pattern "Four Consecutive Triplets"         "四連刻"  200
threeMothers                   = Pattern "Three Mothers"                     "三娘教子" 400


-- 6.0 Suit Patterns

mixedOneSuit, pureOneSuit :: Pattern
mixedOneSuit       = Pattern "Mixed One-Suit" "混一色"  40
pureOneSuit        = Pattern "Pure One-Suit"  "清一色"  80

-- | NOTE: pure version only, impure version doens't count.
nineGates :: Pattern
nineGates          = Pattern "Nine Gates"     "九蓮寶燈" 480


-- 7.0 Terminal Tiles

twoTailedTerminalSequences, twoTailedTerminalTriplets, twoTailedTerminals :: Pattern
twoTailedTerminalSequences  = Pattern "Two-Tailed Terminal Sequences" "老少配"   5
twoTailedTerminalTriplets   = Pattern "Two-Tailed Terminal Triplets"  "老少副"   10
twoTailedTerminals          = Pattern "Two-Tailed Terminals"          "老少么九"  120

mixedLesserTerminals, pureLesserTerminals, mixedGreaterTerminals, pureGreaterTerminals :: Pattern
mixedLesserTerminals        = Pattern "Mixed Lesser Terminals"        "混全帶么九" 40
pureLesserTerminals         = Pattern "Pure Lesser Terminals"         "純全帶么九" 50
mixedGreaterTerminals       = Pattern "Mixed Greater Terminals"       "混么九"   100
pureGreaterTerminals        = Pattern "Pure Greater Terminals"        "清么九"   400

littleMountain, bigMountain :: Pattern
littleMountain              = Pattern "Little Mountain"               "小山滿"   320
bigMountain                 = Pattern "Big Mountain"                  "大山滿"   400


-- 8.0 Honor Tiles

windTriplet, littleThreeWinds, bigThreeWinds, littleFourWinds, bigFourWinds :: Pattern
windTriplet        = Pattern "Wind Triplet"         "風刻"  5
littleThreeWinds   = Pattern "Little Three Winds"   "小三風" 40
bigThreeWinds      = Pattern "Big Three Winds"      "大三風" 135
littleFourWinds    = Pattern "Little Four Winds"    "小四喜" 320
bigFourWinds       = Pattern "Big Four Winds"       "大四喜" 400

dragonTriplet, littleThreeDragons, bigThreeDragons :: Pattern
dragonTriplet      = Pattern "Dragon Triplet"       "箭刻"  10
littleThreeDragons = Pattern "Little Three Dragons" "小三元" 60
bigThreeDragons    = Pattern "Big Three Dragons"    "大三元" 160

allHonors, allHonorPairs :: Pattern
allHonors          = Pattern "All Honors"           "字一色" 320
allHonorPairs      = Pattern "All Honor Pairs"      "大七星" 480


-- 9.0 Color Hands

allGreen, allRed, allBlue :: Pattern
allGreen = Pattern "All Green" "緑一色" 400
allRed   = Pattern "All Red"   "紅孔雀" 480
allBlue  = Pattern "All Blue"  "藍一色" 320 -- not exported


-- 10.0 Irregular Hands

thirteenOrphans, thirteenOrphansWaits :: Pattern
thirteenOrphans      = Pattern "Thirteen Orphans"            "十三么九"      160
thirteenOrphansWaits = Pattern "Thirteen Orphans (13 Waits)" "十三么九 十三面"  320

sevenPairs, sevenShiftedPairs, grandChariot, bambooForest, numerousNeighbors :: Pattern
sevenPairs           = Pattern "Seven Pairs" "七對子" 40
sevenShiftedPairs    = Pattern "Seven Shifted Pairs"         "連七對"       320
grandChariot         = Pattern "Grand Chariot"               "大車輪"       400
bambooForest         = Pattern "Bamboo Forest"               "大竹林"       400
numerousNeighbors    = Pattern "Numerous Neighbors"          "大數隣"       400


-- 11.0 Incidental bonuses

finalDraw, finalDiscard :: Pattern
finalDraw        = Pattern "Final Draw"         "海底撈月" 10
finalDiscard     = Pattern "Final Discard"      "河底撈魚" 10

winOnQuartet, winOnBonusTile :: Pattern
winOnQuartet     = Pattern "Win on Quartet"     "嶺上開花" 10
winOnBonusTile   = Pattern "Win on Bonus Tile"  "花上自摸" 10

robbingAQuartet :: Pattern
robbingAQuartet  = Pattern "Robbing a Quartet"  "搶槓"   10

blessingOfHeaven, blessingOfEarth :: Pattern
blessingOfHeaven = Pattern "Blessing of Heaven" "天和"   155
blessingOfEarth  = Pattern "Blessing of Earth"  "地和"   155


-- 12.0 Bonus Tiles

bonusFlowerSeason, fourFlowers, fourSeasons, allBonusTiles :: Pattern
bonusFlowerSeason = Pattern "Bonus Tiles"     "花季牌"  2
fourFlowers       = Pattern "Four Flowers"    "齊四花"  20
fourSeasons       = Pattern "Four Seasons"    "齊四季"  20
allBonusTiles     = Pattern "All Bonus Tiles" "八仙過海" 40


-------------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------------

-- | This function assumes that the 2 patterns are identical
--   and simply add their points up; no checking is done.
--   Intended use is for adding points from dragon triplets,
--   similar sequences, bonus tiles, etc…
updateScore :: Pattern -> Int -> Pattern
updateScore (Pattern e c p) n = Pattern e c (p * n)
