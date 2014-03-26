#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-

### chart.py just has a bunch of tuples consisting of the hand names in English
### , Chinese, and the points the hand score.  score.py can compose a list of
### these that a hand satisfies, as well as computing the score of the given
### hand.



#####################
### Scoring table ###
#####################

### Trivial Patterns

chicken      = ("Chicken Hand", "雞和", 1)
all_chow     = ("All Chow", "平和", 5)
concealed    = ("Concealed Hand", "門前清", 5)
self_drawn   = ("Self Drawn", "自摸", 5)
all_simple   = ("All Simple", "斷么九", 5)
all_type     = ("All Types", "五門齊", 10)
illegal_hand = ("Illegal Hand", "詐和", -30)



### 2.0 Identical Chows

two_identical_chow       = ("Identical Chow", "一般高", 10)
two_identical_chow_twice = ("Two Identical Chows Twice", "兩般高", 60)
three_identical_chows    = ("Three Identical Chows", "一色三同順", 120)
four_identical_chows     = ("Four Identical Chows", "一色四同順", 480)



### 3.0 Pungs and Kongs

all_pung              = ("All Pungs", "對對和", 30)
two_concealed_pungs   = ("Two Concealed Pungs", "二暗刻", 5)
three_concealed_pungs = ("Three Concealed Pungs", "三暗刻", 30)
four_concealed_pungs  = ("Four Concealed Pungs", "四暗刻", 125)

one_kong   = ("One Kong", "一槓", 5)
two_kong   = ("Two Kongs", "兩槓", 20)
three_kong = ("Three Kongs", "三槓", 120)
four_kong  = ("Four Kongs", "四槓", 480)



### 4.0 Similar Sets

three_similar_chows = ("Three Similar Chows", "三色同順", 35)

small_three_similar_pungs = ("Small Three Similar Pungs", "三色小同刻", 30)
three_similar_pungs       = ("Three Similar Pungs", "三色同刻", 120)



### 5.0 Consecutive Sets ####

nine_tile_straight = ("Nine-Tile Straight", "一氣通貫", 40)

three_consecutive_pungs = ("Three Consecutive Pungs", "三連刻", 100)
four_consecutive_pings  = ("Four Consecutive Pungs", "四連刻", 200)
three_mothers           = ("Three Mothers", "三娘教子", 400)



### 6.0 Suit Patterns

mixed_one_suit      = ("Mixed One-Suit", "混一色", 40)
pure_one_suit       = ("Pure One-Suit", "清一色", 80)
small_terminal_club = ("Small Terminal Club", "一色雙龍會", 100)
big_terminal_club   = ("Big Terminal Club", "清天龍會", 320)

nine_gates = ("Nine Gates", "九蓮寶燈", 480)



### 7.0 Terminal Tiles

two_tailed_terminal_chow = ("Two-Tailed Terminal Chows", "老少配", 5)
two_tailed_terminal_pung = ("Two-Tailed Terminal Pungs", "老少副", 15)
small_mountain           = ("Small Mountain", "小山滿", 320)
big_mountain             = ("Big Mountain", "大山滿", 400)

mixed_lesser_terminal  = ("Mixed Lesser Terminals", "混全帶么", 40)
pure_lesser_terminal   = ("Pure Lesser Terminals", "純全帶么", 50)
mixed_greater_terminal = ("Mixed Greater Terminals", "混么九", 100)
pure_greater_terminal  = ("Pure Greater Terminals", "清么九", 400)



### 8.0 Honor Tiles

dragon_pung = ("Dragon Pung", "箭刻", 10)
seat_wind   = ("Seat Wind", "門風", 10)

small_three_winds = ("Small Three Winds", "小三風", 30)
big_three_winds   = ("Big Three Winds", "大三風", 120)
small_four_winds  = ("Small Four Winds", "小四喜", 320)
big_four_winds    = ("Big Four Winds", "大四喜", 400)

small_three_dragons = ("Small Three Dragons", "小三元", 40)
big_three_dragons   = ("Big Three Dragons", "大三元", 130)

all_honor_pungs = ("All Honor Pungs", "字一色", 320)
all_honor_pairs = ("All Honor Pairs", "大七星", 480)



### 9.0 Seven Pairs

seven_pairs         = ("Seven Pairs", "七對子", 30)
seven_shifted_pairs = ("Seven Shifted Pairs", "連七對", 320)
grand_chariot       = ("Grand Chariot", "大車輪", 400)
bamboo_forest       = ("Bamboo Forest", "大竹林", 400)
number_neighborhood = ("Number Neighborhood", "大數隣", 400)



### 10.0 Color Hands

all_green = ("All Green", "緑一色", 400)
all_red   = ("All Red", "紅孔雀", 480)
all_blue  = ("All Blue", "藍一色", 400)



### 11.0 Irregular Hands

thirteen_orphans = ("Thirteen Orphans", "十三么九", 160)



### 12.0 Incidental bonuses

final_draw    = ("Final Draw", "海底撈月", 10)
final_discard = ("Final Discard", "河底撈魚", 10)

win_on_kong       = ("Win on Kong", "嶺上開花", 10)
win_on_bonus_tile = ("Win on Bonus Tile", "花上自摸", 10)

robbing_a_kong = ("Robbing a Kongs", "搶槓", 10)

blessing_of_heaven = ("Blessing of Heaven", "天和", 155)
blessing_of_earth  = ("Blessing of Earth", "地和", 155)



### 13.0 Bonus Tiles

non_seat_flower = ("Non-seat Flower", "偏花", 2)
non_seat_season = ("Non-seat Season", "偏季", 2)
seat_flower     = ("Seat Flower", "正花", 4)
seat_season     = ("Seat Season", "正季", 4)
four_flowers    = ("Four Flowers", "齊四花", 10)
four_seasons    = ("Four Seasons", "齊四季", 10)
all_bonus_tiles = ("All Bonus Tiles", "八仙過海", 50)
