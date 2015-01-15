mj-score-eval
=============

A minor mahjong evaluation program to familiarize with Python

### Mahjong Scoring Guide

The scoring is based on the [Zung Jung scoring system](http://home.netvigator.com/~tarot/Mahjong/ZungJung/zj33_patterns_eng.html), but includes hands that are not included in the Zung Jung scoring system as well.



### 1.0 Trivial Patterns

#### 1.1 Chicken Hand (雞和) : 1
Basic winning hand that doesn't satisfy any patterns aside from having bonus tiles.


#### 1.2 All Chows (平和) : 5
The hand contains 4 Chows; no Pungs/kongs.
(There are no other restrictions as to the eyes pair, single call, or concealed hand.)


#### 1.3 Concealed Hand (門前清) : 5
A regular hand which is concealed, without melding any exposed sets before winning. Winning on discard is okay. Concealed kongs are okay.


#### 1.4 Self Drawn (自摸) : 5
self drawn the winning tile to complete the hand.


#### 1.5 All Simples (斷么九) : 5
The hand consists entirely of middle number tiles (2 to 8); no terminals or honors.


#### 1.6 All Types (五門齊) : 10
The hand consists of 5 tile types coin, bamboo, character, wind and dragon.


#### 1.7 Illegal Call (詐和) : -30
Declares mahjong accidentally when the hand is not ready.



### 2.0 Pungs and Kongs

#### 3.1 All Pungs (對對和) : 30
The hand contains 4 set of Pungs/kongs; no Chows.


#### 3.2.1 Two Concealed Pungs (兩暗刻) : 5
The hand contains two concealed Pungs/concealed kongs.

#### 3.2.2 Three Concealed Pungs (三暗刻) : 30
The hand contains three concealed Pungs/concealed kongs.

#### 3.2.3 Four Concealed Pungs (四暗刻) : 125
The hand contains four concealed Pungs/concealed kongs.


#### 3.3.1 One Kong (一槓) : 5
The hand contains one kongs. (Irrespective of whether it is exposed or concealed; same below.)

#### 3.3.2 Two Kongs (兩槓) : 20
The hand contains two kongs.

#### 3.3.3 Three Kongs (三槓) : 120
The hand contains three kongs.

#### 3.3.4 Four Kongs (四槓) : 480
The hand contains four kongs.



### 3.0 Identical Sets

Identical sets are sets in the same suit in the same numbers. Obviously, only Chows can be identical.

#### 2.1.1 Two Identical Chows (一般高) : 10
Two Chows in the same suit in the same numbers.
Example: 3筒 4筒 5筒, 3筒 4筒 5筒

#### 2.1.2 Two Identical Chows Twice (兩般高) : 60
The hand contains two groups of "Two Identical Chows".
Example: 3筒 4筒 5筒, 3筒 4筒 5筒, 7萬 8萬 9萬, 7萬 8萬 9萬

#### 2.1.3 Three Identical Chows (一色三同順) : 120
Three Chows in the same suit in the same numbers.
Example: 3筒 4筒 5筒, 3筒 4筒 5筒, 3筒 4筒 5筒

#### 2.1.4 Four Identical Chows (一色四同順) : 480
Four Chows in the same suit in the same numbers.
Example: 3筒 4筒 5筒, 3筒 4筒 5筒, 3筒 4筒 5筒, 3筒 4筒 5筒



### 4.0 Similar Sets

Similar Sets are sets in the same numbers across 3 different suits.
In Zung Jung, all 3 suits must be present; 2-suit patterns are not recognized.

#### 4.1 Three Similar Chows (三色同順) : 35
Three Chows in the same numbers across three different suits.
Example: 3筒 4筒 5筒, 3索 4索 5索, 3萬 4萬 5萬


#### 4.2.1 Little Three Similar Pungs (三色小同刻) : 30
Two Pungs/kongs in the same number in two different suits, and the eye pair in the same number in the third suit.
Example: 4筒 4筒 4筒, 4萬 4萬 4萬, 4索 4索(eye)

#### 4.2.2 Three Similar Pungs (三色同刻) : 120
Three Pungs/kongs in the same number across three different suits.
Example: 4筒 4筒 4筒, 4萬 4萬 4萬, 4索 4索 4索



### 5.0 Consecutive Sets ####

Consecutive sets are sets in the same suit in consecutive numbers. The patterns here require three or more such sets.

#### 5.1.1 Three Consecutive Chows (三連順) : 30
Three chows in consecutive number in the same suit with step 1: a (n)(n+1)(n+2) sequence, a (n+1)(n+2)(n+3) sequence, and a (n+2)(n+3)(n+4) sequence; or step 2: a (n)(n+1)(n+2) sequence, a (n+2)(n+3)(n+4) sequence, and a (n+4)(n+5)(n+6) sequence.
Example: 1萬 2萬 3萬, 2萬 3萬 4萬, 3萬 4萬 5萬
Example: 1萬 2萬 3萬, 3萬 4萬 5萬, 5萬 6萬 7萬

#### 5.1.2 Nine-Tile Straight (一氣通貫) : 40
A "123" sequence, a "456" sequence, and a "789" sequence, all in the same suit.
(The hand must contain exactly the three Chows listed above.)
Example: 1萬 2萬 3萬, 4萬 5萬 6萬, 7萬 8萬 9萬

#### 5.1.3 Three Consecutive Chows Twice (雙三連順) : 50
Two of three consecutive chows in the same suit an (n)(n+1)(n+2) sequence, (n+1)(n+2)(n+3) sequence, (n+2)(n+3)(n+4) sequence, and a (n+4)(n+5)(n+6) sequence.
Example: 1萬 2萬 3萬, 2萬 3萬 4萬, 3萬 4萬 5萬, 5萬 6萬 7萬

#### 5.1.4 Four Consecutive Chows (四連順) : 100
Four chows in consecutive number in the same suit with step 1: a (n)(n+1)(n+2) sequence, a (n+1)(n+2)(n+3) sequence, a (n+2)(n+3)(n+4) sequence, and a (n+3)(n+4)(n+5); or step 2: a (n)(n+1)(n+2) sequence, a (n+2)(n+3)(n+4) sequence, a (n+4)(n+5)(n+6) sequence, and a (n+6)(n+7)(n+8) sequence.
Example: 1萬 2萬 3萬, 2萬 3萬 4萬, 3萬 4萬 5萬, 4萬 5萬 6萬
Example: 1萬 2萬 3萬, 3萬 4萬 5萬, 5萬 6萬 7萬, 7萬 8萬 9萬


#### 5.2.1 Three Consecutive Pungs (三連刻) : 100
Three Pungs/kongs in consecutive numbers in the same suit.
Example: 4萬 4萬 4萬, 5萬 5萬 5萬, 6萬 6萬 6萬

#### 5.2.2 Four Consecutive Pungs (四連刻) : 200
Four Pungs/kongs in consecutive numbers in the same suit.
Example: 4萬 4萬 4萬, 5萬 5萬 5萬, 6萬 6萬 6萬, 7萬 7萬 7萬

#### 5.2.3 Three Mothers (三娘教子) : 400
Hand consist of three consecutive triples and sequence of the same three tiles remaining.
Example: 4萬 4萬 4萬, 5萬 5萬 5萬, 6萬 6萬 6萬, 7萬 7萬 7萬, 5萬 6萬 7萬



### 6.0 Suit Patterns

#### 6.1.1 Mixed One-Suit (混一色) : 40
The hand consists entirely of number tiles in one suit, plus honor tiles.

#### 6.1.2 Pure One-Suit (清一色) : 80
The hand consists entirely of number tiles in one suit.


#### 6.2 Nine Gates (九蓮寶燈) : 480
A 9-way call hand, with "1112345678999" in one suit in your hand, and winning on any one tile in the same suit.



### 7.0 Terminal Tiles

Terminals are the 1 and 9 number tiles.
The Greater patterns consist of terminal tiles only, while the Lesser patterns include also "123" and "789" Chows.

#### 7.1.1 Two-Tailed Terminal Chows (老少順) : 5
A sequece of 123 and 789 in the same suit.

#### 7.1.2 Two-Tailed Terminal Pungs (老少刻) : 15
A triplet/kongs of 1s and 9s in the same suit.

#### 7.1.3 Two-Tailed Terminals (老少么) : 250
The hand consists of the pattern 111-123-789-999 in a suit.

#### 7.1.5 Little Mountain (小山滿) : 320
Pure one-suit hand satisfying pure lesser terminals, using up only six terminals of one suit.
Example: 1筒 2筒 3筒, 1筒 2筒 3筒, 7筒 8筒 9筒, 7筒 8筒 9筒, 1筒 1筒
Example: 1筒 2筒 3筒, 1筒 2筒 3筒, 1筒 2筒 3筒, 7筒 8筒 9筒, 9筒 9筒

#### 7.1.6 Big Mountain (大山滿) : 400
Pure one-suit hand satisfying pure lesser terminals, using up all eight terminals of one suit.
Example: 1筒 1筒 1筒, 1筒 2筒 3筒, 7筒 8筒 9筒, 7筒 8筒 9筒, 9筒 9筒
Example: 1筒 2筒 3筒, 1筒 2筒 3筒, 7筒 8筒 9筒, 9筒 9筒 9筒, 1筒 1筒


#### 7.2.1 Mixed Lesser Terminals (混全帶么) : 40
Every of the 4 sets in the hand, as well as the pair of eyes, includes a terminal tile or an honor tile.
Example: 1索 1索 1索, 1萬 2萬 3萬, 7筒 8筒 9筒, 中 中 中, 9萬 9萬

#### 7.2.2 Pure Lesser Terminals (純全帶么) : 50
Every of the 4 sets in the hand, as well as the pair of eyes, includes a terminal number tile.
Example: 1索 2索 3索, 1萬 1萬 1萬, 7萬 8萬 9萬, 9筒 9筒 9筒, 1索 1索

#### 7.2.3 Mixed Greater Terminals (混么九) : 100
An "All Pungs" or "Seven Pairs" hand which consists entirely of terminal tiles and honor tiles. (Not applicable to a "Thirteen Terminals" hand.)
Example: 9索 9索 9索, 1萬 1萬 1萬, 西 西 西, 發 發 發, 1筒 1筒

#### 7.2.4 Pure Greater Terminals (清么九) : 400
The hand consists entirely of terminal number tiles.
Example: 1索 1索 1索, 9萬 9萬 9萬, 1筒 1筒 1筒, 9筒 9筒 9筒, 9索 9索



### 8.0 Honor Tiles

#### 8.1.1 Seat Wind (門風) : 10
A triplet/kongs of Seat Wind (your own Wind).
Note: in Zung Jung the Prevailing Wind is not recognized.

#### 8.1.2 Dragon Pung (箭刻) : 10 per set
A triplet/kongs of a dragon tile.


#### 8.2.1 Little Three Winds (小三風) : 30
Two triplet/kongs of Winds, plus a pair of Winds as the eyes.
Example: 西 西 西, 北 北 北, 東 東(eyes)

#### 8.2.2 Big Three Winds (大三風) : 120
Three triplet/kongs of Winds.
Example: 東 東 東, 南 南 南, 北 北 北

#### 8.2.3 Little Four Winds (小四喜) : 320
Three triplet/kongs of Winds, plus a pair of Winds as the eyes.
Example: 東 東 東, 西 西 西, 北 北 北, 南 南(eyes)

#### 8.2.4 Big Four Winds (大四喜) : 400
Four triplet/kongs of Winds.
東 東 東, 南 南 南, 西 西 西, 北 北 北


#### 8.3.1 Little Three Dragons (小三元) : 40 -> 60
Two triplet/kongs of Dragons, plus a pair of Dragons as the eyes.
Example: 白 白 白, 中 中 中, 發 發(eyes)
(This hand always includes two Dragon Pungs, so it scores at least 40+10+10=60 points.)

#### 8.3.2 Big Three Dragons (大三元) : 130 -> 160
Three triplet/kongs of Dragons.
白 白 白, 發 發 發, 中 中 中
(This hand always includes three Dragon Pungs, so it scores at least 130+10+10+10=160 points.)


#### 8.4.1 All Honor Pungs (字一色) : 320
The hand consists entirely of honor tiles.

#### 8.4.2 All Honor Pairs (大七星) : 480
The hand consists of pairs of each wind tiles and pairs of each dragon tiles.



### 9.0 Seven Pairs

Seven pairs hand are hands that consists of seven pairs. A Seven Pairs hand cannot count those patterns which specifically require Pungs, kongs, or Chows. But it can count other patterns which do not have such requirements. Four identical tiles can count as two pairs as long as kongs is not declared.

#### 9.1.1 Seven Pairs (七對子) : 35
The hand consists of seven pairs.
Example: 2索 2索, 6萬 6萬, 1筒 1筒, 7筒 7筒, 白 白, 西 西, 北 北

#### 9.1.2 Seven Shifted Pairs (連七對) : 320
The hand consists of seven shifted pairs from 1 to 7 or 2 to 8.

#### 9.1.3 Grand Chariot (大車輪) : 400
Seven shifted pairs from 2 to 8 of the coin suit.

#### 9.1.4 Bamboo Forest (大竹林) : 400
Seven shifted pairs from 2 to 8 of the bamboo suit.

#### 9.1.5 Number Neighborhood (大數隣) : 400
Seven shifted pairs from 2 to 8 of the character suit.



### 10.0 Color Hands

#### 10.1 All Green (緑一色) : 400
The hand consists of tiles from the set 2, 3, 4, 6, 8 bamboo and the green dragon tile.


#### 10.2 All Red (紅孔雀) : 480
The hand consists of tiles from the set 1, 3, 5, 7 bamboo and the red dragon tile.



### 11.0 Irregular Hands

Irregular Hands are hands that do not consist of 4 sets and a pair. Irregular hands do not count for "Concealed Hand".

#### 11.1 Thirteen Orphans (十三么九) : 160
Among the 13 types of terminals and honors, the hand contains one pair of one type, and one tile each of the other 12 types.
If there are more than one winner in the game from the same discard, this hand has higher priority.



### 12.0 Incidental bonuses

Lucky bonuses for winning on rare opportunities.

#### 12.1.1 Final Draw (海底撈月) : 10
Self-draw win on the "seabed" tile (the last tile in the wall, excluding the king's tiles).

#### 12.1.2 Final Discard (河底撈魚) : 10
Winning on a discarded "riverbed" tile (the last discard by the player who has drawn the seabed tile).


#### 12.2 Win on Kongs (嶺上開花) : 10
Self-draw win on a "supplement" tile after declaring a kongs.
(If the supplement tile is also the seabed tile, both patterns can be counted.)

#### 12.2 Win on Bonus Tile (花上自摸) : 10
Self-draw win on a "supplement" tile after drawing a bonus tile.
(If the supplement tile is also the seabed tile, both patterns can be counted.)


#### 12.3 Robbing a Kongs (搶槓) : 10
Winning by robbing a kongs (when another player makes a "Little exposed kongs").


#### 12.4.1 Blessing of Heaven (天和) : 155
East winning with his initial 14-tile hand.
(Does not count if East has made a concealed kongs.)

#### 12.4.2 Blessing of Earth (地和) : 155
A non-East player calling with his initial 13-tile hand, and winning on East's very first discard.
Does not count if East has made a concealed kongs.



### 13.0 Bonus Tiles

#### 13.1.1 Non-seat Flower (偏花) : 2 per tile
A flower tile which is not proper to one's seat.

#### 13.1.2 Non-seat Season (偏季) : 2 per tile
A season tile which is not proper to one's seat.

#### 13.1.3 Seat Flower (正花) : 4 per tile
A flower tile which is proper to one's seat.

#### 13.1.4 Seat Season (正季) : 4 per tile
A season tile which is proper to one's seat.

Upon drawing the last flower/season to complete a flower/season kongs, player only draw one extra tile.  Player does NOT draw two tiles from the tail wall (one from flower and one from declaring kongs).

#### 13.2.1 All Flowers (齊四花) : 15
A complete set of all 4 Flower tiles.

#### 13.2.2 All Seasons (齊四季) : 15
A complete set of all 4 Season tiles.

#### 13.3 All Bonus Tiles (八仙過海) : 80
Gathering all flowers and seasons to instantly win the game.  If a player has 7 of the bonus tile and another player draws the last remaining bonus tile, then the first said player can steal and win.
