mj-score-eval
=============

A minor mahjong evaluation program for fun.

### Mahjong Scoring Guide

The scoring is based on the [Zung Jung scoring system](http://www.zj-mahjong.info/), but includes hands that are not included in the Zung Jung scoring system as well.



### 1.0 Trivial Patterns

#### 1.1 Chicken Hand (雞和) : 1
Basic minimal winning hand that doesn't satisfy any patterns other than having bonus tiles.


#### 1.2 All Chows (平和) : 5
The hand contains 4 Chows; no pungs / kongs.


#### 1.3 Concealed Hand (門前清) : 5
A regular hand which is concealed, without melding any exposed sets before winning. Winning on discard is okay. Concealed kongs are okay.


#### 1.4 Self Drawn (自摸) : 5
Self draw the winning tile to complete the hand.


#### 1.5 All Simples (斷么九) : 5
The hand consists entirely of middle number [simple] tiles (2 to 8); no terminals nor honors.


#### 1.6 All Types (五門齊) : 10
A hand consisting of all 5 tile types: the 4 melds and eyes are each of different tile types.


#### 1.7 Illegal Call (詐和) : -30
Declares mahjong accidentally when the hand is not ready.



### 2.0 Pungs and Kongs

#### 2.1 All Pungs (對對和) : 30
The hand contains 4 set of pungs / kongs; no chows.


#### 2.2.1 Two Concealed Pungs (兩暗刻) : 5
The hand contains two concealed pungs / concealed kongs.

#### 2.2.2 Three Concealed Pungs (三暗刻) : 30
The hand contains three concealed pungs / concealed kongs.

#### 2.2.3 Four Concealed Pungs (四暗刻) : 125
The hand contains four concealed pungs / concealed kongs.


#### 2.3.1 One Kong (一槓) : 5
The hand contains one kongs. (Irrespective of whether it is exposed or concealed; same below.)

#### 2.3.2 Two Kongs (兩槓) : 20
The hand contains two kongs.

#### 2.3.3 Three Kongs (三槓) : 120
The hand contains three kongs.

#### 2.3.4 Four Kongs (四槓) : 480
The hand contains four kongs.



### 3.0 Identical Sets

Identical sets are sets in the same suit in the same numbers. Obviously, only Chows can be identical.

#### 3.1.1 Two Identical Chows (一般高) : 10
Two Chows in the same suit in the same numbers.
Example: ![c3]![c4]![c5] ![c3]![c4]![c5]

#### 3.1.2 Two Identical Chows Twice (兩般高) : 60
The hand contains two groups of "Two Identical Chows".
Example: ![c3]![c4]![c5] ![c3]![c4]![c5] ![c7]![c8]![c9] ![c7]![c8]![c9]

#### 3.1.3 Three Identical Chows (一色三同順) : 120
Three Chows in the same suit in the same numbers.
Example: ![c3]![c4]![c5] ![c3]![c4]![c5] ![c3]![c4]![c5]

#### 3.1.4 Four Identical Chows (一色四同順) : 480
Four Chows in the same suit in the same numbers.
Example: ![c3]![c4]![c5] ![c3]![c4]![c5] ![c3]![c4]![c5] ![c3]![c4]![c5]



### 4.0 Similar Sets

Similar Sets are sets in the same numbers across 3 different suits.
In Zung Jung, all 3 suits must be present; 2-suit patterns are not recognized.

#### 4.1 Three Similar Chows (三色同順) : 35
Three Chows in the same numbers across three different suits.
Example: ![c3]![c4]![c5] ![b3]![b4]![b5] ![k3]![k4]![k5]


#### 4.2.1 Little Three Similar Pungs (三色小同刻) : 30
Two Pungs/kongs in the same number in two different suits, and the eye pair in the same number in the third suit.
Example: ![c4]![c4]![c4] ![k4]![k4]![k4] ![b4]![b4]

#### 4.2.2 Three Similar Pungs (三色同刻) : 120
Three Pungs/kongs in the same number across three different suits.
Example: ![c4]![c4]![c4] ![k4]![k4]![k4] ![b4]![b4]![b4]



### 5.0 Consecutive Sets ####

Consecutive sets are sets in the same suit in consecutive numbers. The patterns here require three or more such sets.

#### 5.1.1 Three Consecutive Chows (三連順) : 30
Three chows in consecutive number in the same suit with step 1: a (n)(n+1)(n+2) sequence, a (n+1)(n+2)(n+3) sequence, and a (n+2)(n+3)(n+4) sequence; or step 2: a (n)(n+1)(n+2) sequence, a (n+2)(n+3)(n+4) sequence, and a (n+4)(n+5)(n+6) sequence.
Example: ![k1]![k1]![k3] ![k1]![k3]![k4] ![k3]![k4]![k5]
Example: ![k1]![k1]![k3] ![k3]![k4]![k5] ![k5]![k6]![k7]

#### 5.1.2 Nine-Tile Straight (一氣通貫) : 40
A "123" sequence, a "456" sequence, and a "789" sequence, all in the same suit.
(The hand must contain exactly the three Chows listed above.)
Example: ![k1]![k1]![k3] ![k4]![k5]![k6] ![k7]![k8]![k9]

#### 5.1.3 Three Consecutive Chows Twice (雙三連順) : 50
Two of three consecutive chows in the same suit.  At the start: (n)(n+1)(n+2) sequence, (n+1)(n+2)(n+3) sequence, (n+2)(n+3)(n+4) sequence, and a (n+4)(n+5)(n+6) sequence; or at the end: (n)(n+1)(n+2) sequence, (n+2)(n+3)(n+4) sequence, (n+3)(n+4)(n+5) sequence, and a (n+4)(n+5)(n+6) sequence.
Example: ![k1]![k1]![k3] ![k1]![k3]![k4] ![k3]![k4]![k5] ![k5]![k6]![k7] (Chow #1,2,3 and #1,3,4 makes two consecutive chows)
Example: ![k1]![k1]![k3] ![k3]![k4]![k5] ![k4]![k5]![k6] ![k5]![k6]![k7] (Chow #1,2,4 and #2,3,4 makes two consecutive chows)

#### 5.1.4 Four Consecutive Chows (四連順) : 100
Four chows in consecutive number in the same suit with step 1: a (n)(n+1)(n+2) sequence, a (n+1)(n+2)(n+3) sequence, a (n+2)(n+3)(n+4) sequence, and a (n+3)(n+4)(n+5); or step 2: a (n)(n+1)(n+2) sequence, a (n+2)(n+3)(n+4) sequence, a (n+4)(n+5)(n+6) sequence, and a (n+6)(n+7)(n+8) sequence.
Example: ![k1]![k1]![k3] ![k1]![k3]![k4] ![k3]![k4]![k5] ![k4]![k5]![k6]
Example: ![k1]![k1]![k3] ![k3]![k4]![k5] ![k5]![k6]![k7] ![k7]![k8]![k9]


#### 5.2.1 Three Consecutive Pungs (三連刻) : 100
Three Pungs/kongs in consecutive numbers in the same suit.
Example: ![k4]![k4]![k4] ![k5]![k5]![k5] ![k6]![k6]![k6]

#### 5.2.2 Four Consecutive Pungs (四連刻) : 200
Four Pungs/kongs in consecutive numbers in the same suit.
Example: ![k4]![k4]![k4] ![k5]![k5]![k5] ![k6]![k6]![k6] ![k7]![k7]![k7]

#### 5.2.3 Three Mothers (三娘教子) : 400
Hand consist of three consecutive triples and sequence of the same three tiles remaining.
Example: ![k4]![k4]![k4] ![k5]![k5]![k5] ![k6]![k6]![k6] ![k7]![k7]![k7] ![k5]![k6]![k7]



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

#### 7.1.3 Two-Tailed Terminals (老少么九) : 200
The hand consists of the pattern 111-123-789-999 in a suit.

#### 7.1.5 Little Mountain (小山滿) : 320
Pure one-suit hand satisfying pure lesser terminals, using up only six terminals of one suit.
Example: ![c1]![c2]![c3] ![c1]![c2]![c3] ![c7]![c8]![c9] ![c7]![c8]![c9] ![c1]![c1]
Example: ![c1]![c2]![c3] ![c1]![c2]![c3] ![c1]![c2]![c3] ![c7]![c8]![c9] ![c9]![c9]

#### 7.1.6 Big Mountain (大山滿) : 400
Pure one-suit hand satisfying pure lesser terminals, using up all eight terminals of one suit.
Example: ![c1]![c1]![c1] ![c1]![c2]![c3] ![c7]![c8]![c9] ![c7]![c8]![c9] ![c9]![c9]
Example: ![c1]![c2]![c3] ![c1]![c2]![c3] ![c7]![c8]![c9] ![c9]![c9]![c9] ![c1]![c1]


#### 7.2.1 Mixed Lesser Terminals (混全帶么九) : 40
Every of the 4 sets in the hand, as well as the pair of eyes, includes a terminal tile or an honor tile.
Example: ![b1]![b1]![b1] ![k1]![k1]![k3] ![c7]![c8]![c9] ![dr]![dr]![dr] ![k9]![k9]

#### 7.2.2 Pure Lesser Terminals (純全帶么九) : 50
Every of the 4 sets in the hand, as well as the pair of eyes, includes a terminal number tile.
Example: ![b1]![s2]![b3] ![k1]![k1]![k1] ![k7]![k8]![k9] ![c9]![c9]![c9] ![b1]![b1]

#### 7.2.3 Mixed Greater Terminals (混么九) : 100
An "All Pungs" or "Seven Pairs" hand which consists entirely of terminal tiles and honor tiles. (Not applicable to a "Thirteen Terminals" hand.)
Example: ![s9]![s9]![s9] ![k1]![k1]![k1] ![ww]![ww]![ww] ![dg]![dg]![dg] ![c1]![c1]

#### 7.2.4 Pure Greater Terminals (清么九) : 400
The hand consists entirely of terminal number tiles.
Example: ![b1]![b1]![b1] ![k9]![k9]![k9] ![c1]![c1]![c1] ![c9]![c9]![c9] ![s9]![s9]



### 8.0 Honor Tiles

#### 8.1.1 Wind Pung (風刻) : 5
A triplet/kongs of a wind tile.
Note: Seat wind and Prevailing Wind are not recognized.

#### 8.1.2 Little Three Winds (小三風) : 30
Two triplet/kongs of Winds, plus a pair of Winds as the eyes.
Example: ![ww]![ww]![ww] ![wn]![wn]![wn] ![we]![we]

#### 8.1.3 Big Three Winds (大三風) : 120
Three triplet/kongs of Winds.
Example: ![we]![we]![we] ![ws]![ws]![ws] ![wn]![wn]![wn]

#### 8.1.4 Little Four Winds (小四喜) : 320
Three triplet/kongs of Winds, plus a pair of Winds as the eyes.
Example: ![we]![we]![we] ![ww]![ww]![ww] ![wn]![wn]![wn] ![ws]![ws]

#### 8.1.5 Big Four Winds (大四喜) : 400
Four triplet/kongs of Winds.
![we]![we]![we] ![ws]![ws]![ws] ![ww]![ww]![ww] ![wn]![wn]![wn]


#### 8.2.1 Dragon Pung (箭刻) : 10 per set
A triplet/kongs of a dragon tile.

#### 8.2.2 Little Three Dragons (小三元) : 40 -> 60
Two triplet/kongs of Dragons, plus a pair of Dragons as the eyes.
Example: ![dw]![dw]![dw] ![dr]![dr]![dr] ![dg]![dg]
(This hand always includes two Dragon Pungs, so it scores at least 40+10+10=60 points.)

#### 8.2.3 Big Three Dragons (大三元) : 130 -> 160
Three triplet/kongs of Dragons.
![dw]![dw]![dw] ![dg]![dg]![dg] ![dr]![dr]![dr]
(This hand always includes three Dragon Pungs, so it scores at least 130+10+10+10=160 points.)


#### 8.3.1 All Honor Pungs (字一色) : 320
The hand consists entirely of honor tiles.

#### 8.3.2 All Honor Pairs (大七星) : 480
The hand consists of pairs of each wind tiles and pairs of each dragon tiles.



### 9.0 Seven Pairs

Seven pairs hand are hands that consists of seven pairs. A Seven Pairs hand cannot count those patterns which specifically require Pungs, kongs, or Chows. But it can count other patterns which do not have such requirements. Four identical tiles can count as two pairs as long as kongs is not declared.

#### 9.1.1 Seven Pairs (七對子) : 35
The hand consists of seven pairs.
Example: ![s2]![s2] ![k6]![k6] ![c1]![c1] ![c7]7筒, ![dw]![dw] ![ww]![ww] ![wn]![wn]

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

#### 13.1 Bonus Flower / Season (花季牌) : 2 per tile
Each flower or season bonus tile is worth 2 points unless completing the set.


Note: Upon drawing the last flower/season to complete a flower/season kongs, player only draw one extra tile.  Player does NOT draw two tiles from the tail wall (one from flower and one from declaring kongs).

#### 13.2.1 All Flowers (齊四花) : 15
A complete set of all 4 Flower tiles.

#### 13.2.2 All Seasons (齊四季) : 15
A complete set of all 4 Season tiles.


#### 13.3 All Bonus Tiles (八仙過海) : 80
Gathering all flowers and seasons to instantly win the game.  If a player has 7 of the bonus tile and another player draws the last remaining bonus tile, then the first said player can steal and win.


[c1]: web/img/gif/c1.gif "C1"
[c2]: web/img/gif/c2.gif "C2"
[c3]: web/img/gif/c3.gif "C3"
[c4]: web/img/gif/c4.gif "C4"
[c5]: web/img/gif/c5.gif "C5"
[c6]: web/img/gif/c6.gif "C6"
[c7]: web/img/gif/c7.gif "C7"
[c8]: web/img/gif/c8.gif "C8"
[c9]: web/img/gif/c9.gif "C9"

[b1]: web/img/gif/b1.gif "B1"
[b2]: web/img/gif/b2.gif "B2"
[b3]: web/img/gif/b3.gif "B3"
[b4]: web/img/gif/b4.gif "B4"
[b5]: web/img/gif/b5.gif "B5"
[b6]: web/img/gif/b6.gif "B6"
[b7]: web/img/gif/b7.gif "B7"
[b8]: web/img/gif/b8.gif "B8"
[b9]: web/img/gif/b9.gif "B9"

[k1]: web/img/gif/k1.gif "K1"
[k2]: web/img/gif/k2.gif "K2"
[k3]: web/img/gif/k3.gif "K3"
[k4]: web/img/gif/k4.gif "K4"
[k5]: web/img/gif/k5.gif "K5"
[k6]: web/img/gif/k6.gif "K6"
[k7]: web/img/gif/k7.gif "K7"
[k8]: web/img/gif/k8.gif "K8"
[k9]: web/img/gif/k9.gif "K9"

[we]: web/img/gif/we.gif "WE"
[ws]: web/img/gif/ws.gif "WS"
[ww]: web/img/gif/ww.gif "WW"
[wn]: web/img/gif/wn.gif "WN"

[dr]: web/img/gif/dr.gif "DR"
[dg]: web/img/gif/dg.gif "DG"
[dw]: web/img/gif/dw.gif "DW"

[f1]: web/img/gif/f1.gif "F1"
[f2]: web/img/gif/f2.gif "F2"
[f3]: web/img/gif/f3.gif "F3"
[f4]: web/img/gif/f4.gif "F4"

[s1]: web/img/gif/s1.gif "S1"
[s2]: web/img/gif/s2.gif "S2"
[s3]: web/img/gif/s3.gif "S3"
[s4]: web/img/gif/s4.gif "S4"

[bk]: web/img/gif/back.gif "Hidden"

