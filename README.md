mj-score-eval
=============

A minor mahjong evaluation program to familiarize with Python

### Mahjong Scoring Guide

The scoring is based on the [Zung Jung scoring system](http://home.netvigator.com/~tarot/Mahjong/ZungJung/zj33_patterns_eng.html)



### 1.0 Trivial Patterns

#### 1.1 Chicken Hand (雞和) : 1
Basic winning hand that doesn't satisfy any patterns aside from having bonus tiles.


#### 1.2 All Sequences (平和) : 5
The hand contains 4 sequences; no triplets/kong.
(There are no other restrictions as to the eyes pair, single call, or concealed hand.)


#### 1.3 Concealed Hand (門前清) : 5
A regular hand which is concealed, without melding any exposed sets before winning. Winning on discard is okay. Concealed kong are okay.


#### 1.4 Self Drawn (自摸) : 5
self drawn the winning tile to complete the hand.


#### 1.5 No Terminals (斷么九) : 5
The hand consists entirely of middle number tiles (2 to 8); no terminals or honors.


#### 1.6 All Types (五門齊) : 10
The hand consists of 5 tile types coin, bamboo, character, wind and dragon.


#### 1.7 Illegal Call (詐和) : -30
Declares mahjong accidentally when the hand is not ready.



### 2.0 Identical Sets

Identical sets are sets in the same suit in the same numbers. Obviously, only sequences can be identical.

#### 2.1.1 Two Identical Sequences (一般高) : 10
Two sequences in the same suit in the same numbers.
Example: 3筒 4筒 5筒﹐ 3筒 4筒 5筒

#### 2.1.2 Two Identical Sequences Twice (兩般高) : 60
The hand contains two groups of "Two Identical Sequences".
Example: 3筒 4筒 5筒﹐ 3筒 4筒 5筒﹐ 7萬 8萬 9萬﹐ 7萬 8萬 9萬

#### 2.1.3 Three Identical Sequences (一色三同順) : 120
Three sequences in the same suit in the same numbers.
Example: 3筒 4筒 5筒﹐ 3筒 4筒 5筒﹐ 3筒 4筒 5筒

#### 2.1.4 Four Identical Sequences (一色四同順) : 480
Four sequences in the same suit in the same numbers.
Example: 3筒 4筒 5筒﹐ 3筒 4筒 5筒﹐ 3筒 4筒 5筒﹐ 3筒 4筒 5筒



### 3.0 Triplets and Kong

#### 3.1 All Triplets (對對和) : 30
The hand contains 4 set of triplets/kong; no sequences.


#### 3.2.1 Two Concealed Triplets (二暗刻) : 5
The hand contains two concealed triplets/concealed kong.

#### 3.2.2 Three Concealed Triplets (三暗刻) : 30
The hand contains three concealed triplets/concealed kong.

#### 3.2.3 Four Concealed Triplets (四暗刻) : 125
The hand contains four concealed triplets/concealed kong.


#### 3.3.1 One Kong (一槓) : 5
The hand contains one kong. (Irrespective of whether it is exposed or concealed; same below.)

#### 3.3.2 Two Kong (兩槓) : 20
The hand contains two kong.

#### 3.3.3 Three Kong (三槓) : 120
The hand contains three kong.

#### 3.3.4 Four Kong (四槓) : 480
The hand contains four kong.



### 4.0 Similar Sets

Similar Sets are sets in the same numbers across 3 different suits.
In Zung Jung, all 3 suits must be present; 2-suit patterns are not recognized.

#### 4.1 Three Similar Sequences (三色同順) : 35
Three sequences in the same numbers across three different suits.
Example: 3筒 4筒 5筒﹐ 3索 4索 5索﹐ 3萬 4萬 5萬


#### 4.2.1 Small Three Similar Triplets (三色小同刻) : 30
Two triplets/kong in the same number in two different suits, and the eye pair in the same number in the third suit.
Example: 4筒 4筒 4筒﹐ 4萬 4萬 4萬﹐ 4索 4索(eye)

#### 4.2.2 Three Similar Triplets (三色同刻) : 120
Three triplets/kong in the same number across three different suits.
Example: 4筒 4筒 4筒﹐ 4萬 4萬 4萬﹐ 4索 4索 4索



### 5.0 Consecutive Sets ####

Consecutive sets are sets in the same suit in consecutive numbers. The patterns here require three or more such sets.

#### 5.1 Nine-Tile Straight (一氣通貫) : 40
A "123" sequence, a "456" sequence, and a "789" sequence, all in the same suit.
(The hand must contain exactly the three sequences listed above.)
Example: 1萬 2萬 3萬﹐ 4萬 5萬 6萬﹐ 7萬 8萬 9萬


#### 5.2.1 Three Consecutive Triplets (三連刻) : 100
Three triplets/kong in consecutive numbers in the same suit.
Example: 4萬 4萬 4萬﹐ 5萬 5萬 5萬﹐ 6萬 6萬 6萬

#### 5.2.2 Four Consecutive Triplets (四連刻) : 200
Four triplets/kong in consecutive numbers in the same suit.
Example: 4萬 4萬 4萬﹐ 5萬 5萬 5萬﹐ 6萬 6萬 6萬﹐ 7萬 7萬 7萬

#### 5.2.3 Three Mothers (三娘教子) : 400
Hand consist of three consecutive triples and sequence of the same three tiles remaining.
Example: 4萬 4萬 4萬﹐ 5萬 5萬 5萬﹐ 6萬 6萬 6萬﹐ 7萬 7萬 7萬, 5萬 6萬 7萬



### 6.0 Suit Patterns

#### 6.1.1 Mixed One-Suit (混一色) : 40
The hand consists entirely of number tiles in one suit, plus honor tiles.

#### 6.1.2 Pure One-Suit (清一色) : 80
The hand consists entirely of number tiles in one suit.

#### 6.1.3 Small Terminal Club (一色雙龍會) : 100
The hand consists of the pattern 123-123-789-789-55 in a suit.

#### 6.1.4 Big Terminal Club (清天龍會) : 320
The hand consists of the pattern 111-123-789-999-55 in a suit.


#### 6.2 Nine Gates (九蓮寶燈) : 480
A 9-way call hand, with "1112345678999" in one suit in your hand, and winning on any one tile in the same suit.



### 7.0 Terminal Tiles

Terminals are the 1 and 9 number tiles.
The Greater patterns consist of terminal tiles only, while the Lesser patterns include also "123" and "789" sequences.

#### 7.1.1 Two-Tailed Terminal Sequences (老少配) : 5
A sequece of 123 and 789 in the same suit.

#### 7.1.2 Two-Tailed Terminal Triplets (老少副) : 15
A triplet/kong of 1s and 9s in the same suit.

#### 7.1.3 Small Boundless Mountain (小山滿) : 320
Pure one-suit hand satisfying pure lesser terminals, using up only six terminals of one suit.
Example: 1筒 2筒 3筒, 1筒 2筒 3筒, 7筒 8筒 9筒, 7筒 8筒 9筒, 1筒 1筒
Example: 1筒 2筒 3筒, 1筒 2筒 3筒, 1筒 2筒 3筒, 7筒 8筒 9筒, 9筒 9筒

#### 7.1.4 Big Boundless Mountain (大山滿) : 400
Pure one-suit hand satisfying pure lesser terminals, using up all eight terminals of one suit.
Example: 1筒 1筒 1筒, 1筒 2筒 3筒, 7筒 8筒 9筒, 7筒 8筒 9筒, 9筒 9筒
Example: 1筒 2筒 3筒, 1筒 2筒 3筒, 7筒 8筒 9筒, 9筒 9筒 9筒, 1筒 1筒


#### 7.2.1 Mixed Lesser Terminals (混全帶么) : 40
Every of the 4 sets in the hand, as well as the pair of eyes, includes a terminal tile or an honor tile.Example: 1索 1索 1索﹐ 1萬 2萬 3萬﹐ 7筒 8筒 9筒﹐ 中 中 中﹐ 9萬 9萬

#### 7.2.2 Pure Lesser Terminals (純全帶么) : 50
Every of the 4 sets in the hand, as well as the pair of eyes, includes a terminal number tile.
Example: 1索 2索 3索﹐ 1萬 1萬 1萬﹐ 7萬 8萬 9萬﹐ 9筒 9筒 9筒﹐ 1索 1索

#### 7.2.3 Mixed Greater Terminals (混么九) : 100
An "All Triplets" or "Seven Pairs" hand which consists entirely of terminal tiles and honor tiles. (Not applicable to a "Thirteen Terminals" hand.)
Example: 9索 9索 9索﹐ 1萬 1萬 1萬﹐ 西 西 西﹐ 發 發 發﹐ 1筒 1筒

#### 7.2.4 Pure Greater Terminals (清么九) : 400
The hand consists entirely of terminal number tiles.
Example: 1索 1索 1索﹐ 9萬 9萬 9萬﹐ 1筒 1筒 1筒﹐ 9筒 9筒 9筒﹐ 9索 9索



### 8.0 Honor Tiles

#### 8.1.1 Dragon Pung (箭刻) : 10 per set
A triplet/kong of a dragon tile.

#### 8.1.2 Seat Wind (門風) : 10
A triplet/kong of Seat Wind (your own Wind).
Note: in Zung Jung the Prevailing Wind is not recognized.


#### 8.2.1 Small Three Dragons (小三元) : 40 -> 60
Two triplet/kong of Dragons, plus a pair of Dragons as the eyes.
Example: 白 白 白﹐ 中 中 中﹐ 發 發(eyes)
(This hand always includes two Dragon triplets, so it scores at least 40+10+10=60 points.)

#### 8.2.2 Big Three Dragons (大三元) : 130 -> 160
Three triplet/kong of Dragons.
白 白 白﹐ 發 發 發﹐ 中 中 中
(This hand always includes three Dragon triplets, so it scores at least 130+10+10+10=160 points.)



#### 8.3.1 Small Three Winds (小三風) : 30
Two triplet/kong of Winds, plus a pair of Winds as the eyes.
Example: 西 西 西﹐ 北 北 北﹐ 東 東(eyes)

#### 8.3.2 Big Three Winds (大三風) : 120
Three triplet/kong of Winds.
Example: 東 東 東﹐ 南 南 南﹐ 北 北 北

#### 8.3.3 Small Four Winds (小四喜) : 320
Three triplet/kong of Winds, plus a pair of Winds as the eyes.
Example: 東 東 東﹐ 西 西 西﹐ 北 北 北﹐ 南 南(eyes)

#### 8.3.4 Big Four Winds (大四喜) : 400
Four triplet/kong of Winds.
東 東 東﹐ 南 南 南﹐ 西 西 西﹐ 北 北 北


#### 8.4.1 All Honors (字一色) : 320
The hand consists entirely of honor tiles.

#### 8.4.2 Seven Lucky Stars (大七星) : 480
The hand consists of pairs of each wind tiles and pairs of each dragon tiles, making seven honors pairs.



### 9.0 Seven Pairs

Seven pairs hand are hands that consists of seven pairs. A Seven Pairs hand cannot count those patterns which specifically require triplets, kong, or sequences. But it can count other patterns which do not have such requirements. Four identical tiles can count as two pairs as long as kong is not declared.

#### 9.1.1 Seven Pairs (七對子) : 30
The hand consists of seven pairs.
Example: 2索 2索﹐ 6萬 6萬﹐ 1筒 1筒﹐ 7筒 7筒﹐ 白 白﹐ 西 西﹐ 北 北

#### 9.1.2 Seven Shifted Pairs (連七對) : 320
The hand consists of seven shifted pairs from 1 to 7 or 2 to 8.

#### 9.1.3 Grant Chariot (大車輪) : 400
Seven shifted pairs from 2 to 8 of the coin suit.

#### 9.1.4 Bamboo Forest (大竹林) : 400
Seven shifted pairs from 2 to 8 of the bamboo suit.

#### 9.1.5 Numerous Neighbors (大數隣) : 400
Seven shifted pairs from 2 to 8 of the character suit.



### 10.0 Color Hands

#### 10.1 All Green (緑一色) : 400
The hand consists of tiles from the set 2, 3, 4, 6, 8 bamboo and the green dragon tile.


#### 10.2 All Red (紅孔雀) : 480
The hand consists of tiles from the set 1, 3, 5, 7 bamboo and the red dragon tile.


#### 10.3 All Blue (藍一色) : 400
The hand consists of tiles from the set 8 coin, the 4 winds, and the white dragon tile.



### 11.0 Irregular Hands

Irregular Hands are hands that do not consist of 4 sets and a pair. Irregular hands do not count for "Concealed Hand".

#### 11.1 Thirteen Terminals (十三么九) : 160
Among the 13 types of terminals and honors, the hand contains one pair of one type, and one tile each of the other 12 types.
If there are more than one winner in the game from the same discard, this hand has higher priority.



### 12.0 Incidental bonuses

Lucky bonuses for winning on rare opportunities.

#### 12.1.1 Final Draw (海底撈月) : 10
Self-draw win on the "seabed" tile (the last tile in the wall, excluding the king's tiles).

#### 12.1.2 Final Discard (河底撈魚) : 10
Winning on a discarded "riverbed" tile (the last discard by the player who has drawn the seabed tile).


#### 12.2 Win on Kong (嶺上開花) : 10
Self-draw win on a "supplement" tile after declaring a kong.
(If the supplement tile is also the seabed tile, both patterns can be counted.)

#### 12.2 Win on Bonus Tile (花上自摸) : 10
Self-draw win on a "supplement" tile after drawing a bonus tile.
(If the supplement tile is also the seabed tile, both patterns can be counted.)


#### 12.3 Robbing a Kong (搶槓) : 10
Winning by robbing a kong (when another player makes a "small exposed kong").


#### 12.4.1 Blessing of Heaven (天和) : 155
East winning with his initial 14-tile hand.
(Does not count if East has made a concealed kong.)

#### 12.4.2 Blessing of Earth (地和) : 155
A non-East player calling with his initial 13-tile hand, and winning on East's very first discard.
Does not count if East has made a concealed kong.



### 13.0 Bonus Tiles

#### 13.1.1 Non-seat Flower/Season (偏花) : 2 per tile
A flower/season tile which is not proper to one's seat.

#### 13.1.2 Seat Flower/Season (正花) : 4 per tile
A flower/season tile which is proper to one's seat.

Upon drawing the last flower/season to complete a flower/season kong, player only draw one extra tile.  Player does NOT draw two tiles from the tail wall (one from flower and one from declaring kong).

#### 13.2.1 Four Flowers (齊四花) : 10 -> 15
A complete set of all 4 Flower tiles.

#### 13.2.2 Four Seasons (齊四季) : 10 -> 15
A complete set of all 4 Season tiles.

#### 13.3 All Flowers (八仙過海) : 80 -> 100
Gathering all flowers and seasons to instantly win the game.  If a player has 7 of the bonus tile and another player draws the last remaining bonus tile, then the first said player can steal and win.
