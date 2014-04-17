module Game.Mahjong.Chart where

-- | Listing of all hand in tuple format


-- | 0.0 for when pattern is not satisfied

noHand = ()


-- | 1.0 Trivial Patterns

chickenHand = ("Chicken Hand", "雞和", 1)
allChowsHand = ("All Chow", "平和", 5)
concealedHand = ("Concealed Hand", "門前清", 5)
selfDrawnHand = ("Self Drawn", "自摸", 5)
allSimplesHand = ("All Simple", "斷么九", 5)
allTypeHand = ("All Types", "五門齊", 10)
illegalCallHand = ("Illegal Call", "詐和", -30)


-- | 2.0 Identical Chows

twoIdenticalChowsHand = ("Identical Chow", "一般高", 10)
twoIdenticalChowsTwiceHand = ("Two Identical Chows Twice", "兩般高", 60)
threeIdenticalChowsHand = ("Three Identical Chows", "一色三同順", 120)
fourIdenticalChowsHand = ("Four Identical Chows", "一色四同順", 480)


-- | 3.0 Pungs and Kongs

allPungsHand = ("All Pungs", "對對和", 30)
twoConcealedPungsHand = ("Two Concealed Pungs", "二暗刻", 5)
threeConcealedPungsHand = ("Three Concealed Pungs", "三暗刻", 30)
fourConcealedPungsHand = ("Four Concealed Pungs", "四暗刻", 125)

oneKongHand = ("One Kong", "一槓", 5)
twoKongsHand = ("Two Kongs", "兩槓", 20)
threeKongsHand = ("Three Kongs", "三槓", 120)
fourKongsHand = ("Four Kongs", "四槓", 480)


-- | 4.0 Similar Sets

threeSimilarChowsHand = ("Three Similar Chows", "三色同順", 35)

littleThreeSimilarPungsHand = ("Little Three Similar Pungs", "三色小同刻", 30)
threeSimilarPungsHand = ("Three Similar Pungs", "三色同刻", 120)


-- | 5.0 Consecutive Sets

threeConsecutiveChowsHand = ("Three Consecutive Chows", "三連順", 30)
nineTileStraightHand = ("Nine-Tile Straight", "一氣通貫", 40)
threeConsecutiveChowsTwiceHand = ("Three Consecutive Chows Twice", "雙三連順", 60)
fourConsecutiveChowsHand = ("Four Consecutive Chows", "四連順", 100)

threeConsecutivePungsHand = ("Three Consecutive Pungs", "三連刻", 100)
fourConsecutivePungsHand = ("Four Consecutive Pungs", "四連刻", 200)
threeMothersHand = ("Three Mothers", "三娘教子", 400)


-- | 6.0 Suit Patterns

mixedOneSuitHand = ("Mixed One-Suit", "混一色", 40)
pureOneSuitHand = ("Pure One-Suit", "清一色", 80)
littleTerminalClubHand = ("Little Terminal Club", "一色雙龍會", 100)
bigTerminalClubHand = ("Big Terminal Club", "清天龍會", 320)

nineGatesHand = ("Nine Gates", "九蓮寶燈", 480)


-- | 7.0 Terminal Tiles

twoTailedTerminalChowsHand = ("Two-Tailed Terminal Chows", "老少配", 5)
twoTailedTerminalPungsHand = ("Two-Tailed Terminal Pungs", "老少副", 15)
littleBoundlessMountainHand = ("Little Boundless Mountain", "小山滿", 320)
bigBoundlessMountainHand = ("Big  Boundless Mountain", "大山滿", 400)

mixedLesserTerminalsHand = ("Mixed Lesser Terminals", "混全帶么", 40)
pureLesserTerminalsHand = ("Pure Lesser Terminals", "純全帶么", 50)
mixedGreaterTerminalsHand = ("Mixed Greater Terminals", "混么九", 100)
pureGreaterTerminalsHand = ("Pure Greater Terminals", "清么九", 400)


-- | 8.0 Honor Tiles

dragonPungHand = ("Dragon Pung", "箭刻", 10)
seatWindHand = ("Seat Wind", "門風", 10)

littleThreeWindsHand = ("Little Three Winds", "小三風", 30)
bigThreeWindsHand = ("Big Three Winds", "大三風", 120)
littleFourWindsHand = ("Little Four Winds", "小四喜", 320)
bigFourWindsHand = ("Big Four Winds", "大四喜", 400)

littleThreeDragonsHand = ("Little Three Dragons", "小三元", 40)
bigThreeDragonsHand = ("Big Three Dragons", "大三元", 130)

allHonorsHand = ("All Honors", "字一色", 320)
allHonorPairsHand = ("All Honor Pairs", "大七星", 480)


-- | 9.0 Seven Pairs

sevenPairsHand = ("Seven Pairs", "七對子", 30)

sevenShiftedPairsHand = ("Seven Shifted Pairs", "連七對", 320)
grandChariotHand = ("Grand Chariot", "大車輪", 400)
bambooForestHand = ("Bamboo Forest", "大竹林", 400)
numberNeighborhoodHand = ("Number Neighborhood", "大數隣", 400)


-- | 10.0 Color Hands

allGreenHand = ("All Green", "緑一色", 400)
allRedHand = ("All Red", "紅孔雀", 480)
allBlueHand = ("All Blue", "藍一色", 400)


-- | 11.0 Irregular Hands

thirteenOrphansHand = ("Thirteen Orphans", "十三么九", 160)


-- | 12.0 Incidental bonuses

finalDrawHand = ("Final Draw", "海底撈月", 10)
finalDiscardHand = ("Final Discard", "河底撈魚", 10)

winOnKongHand = ("Win on Kong", "嶺上開花", 10)
winOnBonusTileHand = ("Win on Bonus Tile", "花上自摸", 10)

robbingAKongHand = ("Robbing a Kongs", "搶槓", 10)

blessingOfHeavenHand = ("Blessing of Heaven", "天和", 155)
blessingOfEarthHand = ("Blessing of Earth", "地和", 155)


-- | 13.0 Bonus Tiles

nonSeatFlowerHand = ("Non-seat Flower", "偏花", 2)
nonSeatSeasonHand = ("Non-seat Season", "偏季", 2)
seatFlowerHand = ("Seat Flower", "正花", 4)
seatSeasonHand = ("Seat Season", "正季", 4)

fourFlowersHand = ("Four Flowers", "齊四花", 10)
fourSeasonsHand = ("Four Seasons", "齊四季", 10)

allBonusTilesHand = ("All Bonus Tiles", "八仙過海", 50)
