-- |
-- Module      :  Game.Mahjong.Pattern
-- Copyright   :  Joseph Ching 2015
-- License     :  MIT
--
-- Maintainer  :  joseph.m.ching@gmail.com
-- Stability   :  experimental
-- Portability :  portable

-- | Data definition of hand patterns
--   and declaration for all accepted patterns
module Game.Mahjong.Pattern (
  -- * The `Pattern` type
  Pattern,

  -- * Utility function
  updateScore,


  -- * 1.0 Trivial Patterns
  chicken, allChows, concealed, selfDrawn, allSimples, allTypes, illegalCall,


  -- * 2.0 Pungs and Kongs

  -- ** 2.1 Pung
  allPungs,

  -- ** 2.2 Concealed pungs
  twoConcealedPungs, threeConcealedPungs, fourConcealedPungs,

  -- ** 2.3 Kongs
  oneKong, twoKongs, threeKongs, fourKongs,


  -- * 3.0 Identical Sets
  twoIdenticalChows, twoIdenticalChowsTwice, threeIdenticalChows, fourIdenticalChows,


  -- * 4.0 Similar Sets

  -- ** 4.1 Similar chows
  threeSimilarChows,

  -- ** 4.2 Similar pungs
  littleThreeSimilarPungs, threeSimilarPungs,


  -- * 5.0 Consecutive Sets

  -- ** 5.1 Consecutive chows
  threeConsecutiveChows, nineTileStraight, threeConsecutiveChowsTwice, fourConsecutiveChows,

  -- ** 5.2 Consecutive pungs
  threeConsecutivePungs, fourConsecutivePungs, threeMothers,


  -- * 6.0 Suit Patterns

  -- ** 6.1 Mixed and pure
  mixedOneSuit, pureOneSuit,

  -- ** 6.2 Nine Gates
  nineGates,


  -- * 7.0 Terminal Tiles

  -- ** 7.1 Chow and Pung
  twoTailedTerminalChows, twoTailedTerminalPungs, twoTailedTerminals, littleBoundlessMountain, bigBoundlessMountain,

  -- ** 7.2 Mixed and Pure
  mixedLesserTerminals, pureLesserTerminals, mixedGreaterTerminals, pureGreaterTerminals,


  -- * 8.0 Honor Tiles

  -- ** 8.1 Dragons
  dragonPung, littleThreeDragons, bigThreeDragons,

  -- ** 8.2 Winds
  windPung, littleThreeWinds, bigThreeWinds, littleFourWinds, bigFourWinds,

  -- ** 8.3 Pure honors
  allHonors, allHonorPairs,


  -- * 9.0 Seven Pairs

  -- ** 9.1 Basic seven pairs
  sevenPairs,

  -- ** 9.2 Specialized seven pairs
  sevenShiftedPairs, grandChariot, bambooForest, numberNeighborhood,


  -- * 10.0 Color Hands
  allRed, allGreen,


  -- * 11.0 Irregular Hands
  thirteenOrphansImpure, thirteenOrphansPure,


  -- * 12.0 Incidental bonuses

  -- ** 12.1 Final tile
  finalDraw, finalDiscard,

  -- ** 12.2 Winning on displacement tile
  winOnKong, winOnBonusTile,

  -- ** 12.3 Robbing a kong
  robbingAKong,

  -- ** 12.4 Blessings / First tile
  blessingOfHeaven, blessingOfEarth,


  -- * 13.0 Bonus Tiles

  -- ** 13.1 Basic flower points
  bonusFlowerSeason,

  -- ** 13.2 Flower kong
  fourFlowers, fourSeasons,

  -- ** 12.3 Both sets of bonus tile
  allBonusTiles
) where


-------------------------------------------------------------------------------

{- Data Declaration -}

data Pattern =
  Pattern { name    :: String  -- ^ the english name
          , chinese :: String  -- ^ the chinese name
          , score   :: Int     -- ^ the score
          } deriving (Eq, Show)


-------------------------------------------------------------------------------

{- Hand List -}

-- 1.0 Trivial Patterns

chicken, allChows, concealed, selfDrawn, allSimples, allTypes, illegalCall :: Pattern
chicken     = Pattern "Chicken Hand"   "雞和"   1
allChows    = Pattern "All Chow"       "平和"   5
concealed   = Pattern "Concealed Hand" "門前清" 5
selfDrawn   = Pattern "Self Drawn"     "自摸"   5
allSimples  = Pattern "All Simple"     "斷么九" 5
allTypes    = Pattern "All Types"      "五門齊" 10
illegalCall = Pattern "Illegal Call"   "詐和"   (-30)


-- 2.0 Pungs and Kongs

allPungs, twoConcealedPungs, threeConcealedPungs, fourConcealedPungs :: Pattern
allPungs            = Pattern "All Pungs"             "對對和" 30
twoConcealedPungs   = Pattern "Two Concealed Pungs"   "兩暗刻" 5
threeConcealedPungs = Pattern "Three Concealed Pungs" "三暗刻" 30
fourConcealedPungs  = Pattern "Four Concealed Pungs"  "四暗刻" 125

oneKong, twoKongs, threeKongs, fourKongs :: Pattern
oneKong             = Pattern "One Kong"    "一槓" 5
twoKongs            = Pattern "Two Kongs"   "兩槓" 20
threeKongs          = Pattern "Three Kongs" "三槓" 120
fourKongs           = Pattern "Four Kongs"  "四槓" 480


-- 3.0 Identical Chows

twoIdenticalChows, twoIdenticalChowsTwice, threeIdenticalChows, fourIdenticalChows :: Pattern
twoIdenticalChows      = Pattern "Identical Chow"            "一般高"     10
twoIdenticalChowsTwice = Pattern "Two Identical Chows Twice" "兩般高"     60
threeIdenticalChows    = Pattern "Three Identical Chows"     "一色三同順" 120
fourIdenticalChows     = Pattern "Four Identical Chows"      "一色四同順" 480


-- 4.0 Similar Sets

threeSimilarChows :: Pattern
threeSimilarChows       = Pattern "Three Similar Chows" "三色同順" 35

littleThreeSimilarPungs, threeSimilarPungs :: Pattern
littleThreeSimilarPungs = Pattern "Little Three Similar Pungs" "三色小同刻" 30
threeSimilarPungs       = Pattern "Three Similar Pungs"        "三色同刻"   120


-- 5.0 Consecutive Sets

threeConsecutiveChows, nineTileStraight, threeConsecutiveChowsTwice, fourConsecutiveChows :: Pattern
threeConsecutiveChows      = Pattern "Three Consecutive Chows"       "三連順"   30
nineTileStraight           = Pattern "Nine-Tile Straight"            "一氣通貫" 40
threeConsecutiveChowsTwice = Pattern "Three Consecutive Chows Twice" "雙三連順" 60
fourConsecutiveChows       = Pattern "Four Consecutive Chows"        "四連順"   100

threeConsecutivePungs, fourConsecutivePungs, threeMothers :: Pattern
threeConsecutivePungs      = Pattern "Three Consecutive Pungs" "三連刻"   100
fourConsecutivePungs       = Pattern "Four Consecutive Pungs"  "四連刻"   200
threeMothers               = Pattern "Three Mothers"           "三娘教子" 400


-- 6.0 Suit Patterns

mixedOneSuit, pureOneSuit :: Pattern
mixedOneSuit       = Pattern "Mixed One-Suit"       "混一色"     40
pureOneSuit        = Pattern "Pure One-Suit"        "清一色"     80

-- | NOTE: pure version only, impure version doens't count.
nineGates :: Pattern
nineGates          = Pattern "Nine Gates" "九蓮寶燈" 480


-- 7.0 Terminal Tiles

twoTailedTerminalChows, twoTailedTerminalPungs, twoTailedTerminals, littleBoundlessMountain, bigBoundlessMountain :: Pattern
twoTailedTerminalChows  = Pattern "Two-Tailed Terminal Chows" "老少配"   5
twoTailedTerminalPungs  = Pattern "Two-Tailed Terminal Pungs" "老少副"   15
twoTailedTerminals      = Pattern "Two-Tailed Terminals"      "老少么九" 240
littleBoundlessMountain = Pattern "Little Boundless Mountain" "小山滿"   320
bigBoundlessMountain    = Pattern "Big  Boundless Mountain"   "大山滿"   400

mixedLesserTerminals, pureLesserTerminals, mixedGreaterTerminals, pureGreaterTerminals :: Pattern
mixedLesserTerminals    = Pattern "Mixed Lesser Terminals"  "混全帶么九" 40
pureLesserTerminals     = Pattern "Pure Lesser Terminals"   "純全帶么九" 50
mixedGreaterTerminals   = Pattern "Mixed Greater Terminals" "混么九"     100
pureGreaterTerminals    = Pattern "Pure Greater Terminals"  "清么九"     400


-- 8.0 Honor Tiles

windPung, littleThreeWinds, bigThreeWinds, littleFourWinds, bigFourWinds :: Pattern
windPung           = Pattern "Wind Pung"          "風刻"   5
littleThreeWinds   = Pattern "Little Three Winds" "小三風" 30
bigThreeWinds      = Pattern "Big Three Winds"    "大三風" 120
littleFourWinds    = Pattern "Little Four Winds"  "小四喜" 320
bigFourWinds       = Pattern "Big Four Winds"     "大四喜" 400

dragonPung, littleThreeDragons, bigThreeDragons :: Pattern
dragonPung         = Pattern "Dragon Pung"          "箭刻"   10
littleThreeDragons = Pattern "Little Three Dragons" "小三元" 40
bigThreeDragons    = Pattern "Big Three Dragons"    "大三元" 130

allHonors, allHonorPairs :: Pattern
allHonors          = Pattern "All Honors"      "字一色" 320
allHonorPairs      = Pattern "All Honor Pairs" "大七星" 480


-- 9.0 Seven Pairs

sevenPairs :: Pattern
sevenPairs         = Pattern "Seven Pairs" "七對子" 30

sevenShiftedPairs, grandChariot, bambooForest, numberNeighborhood :: Pattern
sevenShiftedPairs  = Pattern "Seven Shifted Pairs" "連七對" 320
grandChariot       = Pattern "Grand Chariot"       "大車輪" 400
bambooForest       = Pattern "Bamboo Forest"       "大竹林" 400
numberNeighborhood = Pattern "Number Neighborhood" "大數隣" 400


-- 10.0 Color Hands

allGreen, allRed, allBlue :: Pattern
allGreen = Pattern "All Green" "緑一色" 400
allRed   = Pattern "All Red"   "紅孔雀" 480
allBlue  = Pattern "All Blue"  "藍一色" 400 -- not exported


-- 11.0 Irregular Hands

thirteenOrphansImpure, thirteenOrphansPure :: Pattern
thirteenOrphansImpure = Pattern "Thirteen Orphans (Impure)" "十三么九"        160
thirteenOrphansPure = Pattern "Thirteen Orphans (Pure)"     "十三么九 十三面" 320


-- 12.0 Incidental bonuses

finalDraw, finalDiscard :: Pattern
finalDraw        = Pattern "Final Draw"    "海底撈月" 10
finalDiscard     = Pattern "Final Discard" "河底撈魚" 10

winOnKong, winOnBonusTile :: Pattern
winOnKong        = Pattern "Win on Kong"       "嶺上開花" 10
winOnBonusTile   = Pattern "Win on Bonus Tile" "花上自摸" 10

robbingAKong :: Pattern
robbingAKong     = Pattern "Robbing a Kongs" "搶槓" 10

blessingOfHeaven, blessingOfEarth :: Pattern
blessingOfHeaven = Pattern "Blessing of Heaven" "天和" 155
blessingOfEarth  = Pattern "Blessing of Earth"  "地和" 155


-- 13.0 Bonus Tiles

bonusFlowerSeason :: Pattern
bonusFlowerSeason = Pattern "Non-seat Flower" "花季牌" 2

fourFlowers, fourSeasons :: Pattern
fourFlowers       = Pattern "Four Flowers" "齊四花" 10
fourSeasons       = Pattern "Four Seasons" "齊四季" 10

allBonusTiles :: Pattern
allBonusTiles     = Pattern "All Bonus Tiles" "八仙過海" 50



-------------------------------------------------------------------------------

{- Utility functions -}

-- | This function assumes that the 2 patterns are identical
--   and simply add their points up; no checking is done.
--   Intended use is for adding points from dragon pungs,
--   similar chows, bonus tiles, etc…
updateScore :: Pattern -> Int -> Pattern
updateScore (Pattern e c p) n = Pattern e c (p * n)

