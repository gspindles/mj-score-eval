#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-

"""chart.py has a bunch of tuples consisting of the hand names in English,
Chinese, and the points the hand score.  score.py can compose a list of these
that a hand satisfies, as well as computing the score of the given hand.

"""


# Scoring table
#
#     List out tuple represetation for each hand.


# 0.0 for when pattern is not satisfied

nothing = ()


# 1.0 Trivial Patterns

chicken = ('Chicken Hand', u'雞和', 1)
all_chows = ('All Chow', u'平和', 5)
concealed = ('Concealed Hand', u'門前清', 5)
self_drawn = ('Self Drawn', u'自摸', 5)
all_simples = ('All Simple', u'斷么九', 5)
all_type = ('All Types', u'五門齊', 10)
illegal_call = ('Illegal Call', u'詐和', -30)


# 2.0 Identical Chows

two_identical_chows = ('Identical Chow', u'一般高', 10)
two_identical_chows_twice = ('Two Identical Chows Twice', u'兩般高', 60)
three_identical_chows = ('Three Identical Chows', u'一色三同順', 120)
four_identical_chows = ('Four Identical Chows', u'一色四同順', 480)


# 3.0 Pungs and Kongs

all_pungs = ('All Pungs', u'對對和', 30)
two_concealed_pungs = ('Two Concealed Pungs', u'兩暗刻', 5)
three_concealed_pungs = ('Three Concealed Pungs', u'三暗刻', 30)
four_concealed_pungs = ('Four Concealed Pungs', u'四暗刻', 125)

one_kong = ('One Kong', u'一槓', 5)
two_kongs = ('Two Kongs', u'兩槓', 20)
three_kongs = ('Three Kongs', u'三槓', 120)
four_kongs = ('Four Kongs', u'四槓', 480)


# 4.0 Similar Sets

three_similar_chows = ('Three Similar Chows', u'三色同順', 35)

little_three_similar_pungs = ('Little Three Similar Pungs', u'三色小同刻', 30)
three_similar_pungs = ('Three Similar Pungs', u'三色同刻', 120)


# 5.0 Consecutive Sets

three_consecutive_chows = ('Three Consecutive Chows', u'三連順', 30)
nine_tile_straight = ('Nine-Tile Straight', u'一氣通貫', 40)
three_consecutive_chows_twice = ('Three Consecutive Chows Twice', u'雙三連順', 60)
four_consecutive_chows = ('Four Consecutive Chows', u'四連順', 100)

three_consecutive_pungs = ('Three Consecutive Pungs', u'三連刻', 100)
four_consecutive_pungs = ('Four Consecutive Pungs', u'四連刻', 200)
three_mothers = ('Three Mothers', u'三娘教子', 400)


# 6.0 Suit Patterns

mixed_one_suit = ('Mixed One-Suit', u'混一色', 40)
pure_one_suit = ('Pure One-Suit', u'清一色', 80)
little_terminal_club = ('Little Terminal Club', u'一色雙龍會', 100)
big_terminal_club = ('Big Terminal Club', u'清天龍會', 320)

nine_gates = ('Nine Gates', u'九蓮寶燈', 480)


# 7.0 Terminal Tiles

two_tailed_terminal_chows = ('Two-Tailed Terminal Chows', u'老少配', 5)
two_tailed_terminal_pungs = ('Two-Tailed Terminal Pungs', u'老少副', 15)
little_boundless_mountain = ('Little Boundless Mountain', u'小山滿', 320)
big_boundless_mountain = ('Big  Boundless Mountain', u'大山滿', 400)

mixed_lesser_terminals = ('Mixed Lesser Terminals', u'混全帶么', 40)
pure_lesser_terminals = ('Pure Lesser Terminals', u'純全帶么', 50)
mixed_greater_terminals = ('Mixed Greater Terminals', u'混么九', 100)
pure_greater_terminals = ('Pure Greater Terminals', u'清么九', 400)


# 8.0 Honor Tiles

dragon_pung = ('Dragon Pung', u'箭刻', 10)
seat_wind = ('Seat Wind', u'門風', 10)

little_three_winds = ('Little Three Winds', u'小三風', 30)
big_three_winds = ('Big Three Winds', u'大三風', 120)
little_four_winds = ('Little Four Winds', u'小四喜', 320)
big_four_winds = ('Big Four Winds', u'大四喜', 400)

little_three_dragons = ('Little Three Dragons', u'小三元', 40)
big_three_dragons = ('Big Three Dragons', u'大三元', 130)

all_honors = ('All Honors', u'字一色', 320)
all_honor_pairs = ('All Honor Pairs', u'大七星', 480)


# 9.0 Seven Pairs

seven_pairs = ('Seven Pairs', u'七對子', 30)

seven_shifted_pairs = ('Seven Shifted Pairs', u'連七對', 320)
grand_chariot = ('Grand Chariot', u'大車輪', 400)
bamboo_forest = ('Bamboo Forest', u'大竹林', 400)
number_neighborhood = ('Number Neighborhood', u'大數隣', 400)


# 10.0 Color Hands

all_green = ('All Green', u'緑一色', 400)
all_red = ('All Red', u'紅孔雀', 480)
all_blue = ('All Blue', u'藍一色', 400)


# 11.0 Irregular Hands

thirteen_orphans = ('Thirteen Orphans', u'十三么九', 160)


# 12.0 Incidental bonuses

final_draw = ('Final Draw', u'海底撈月', 10)
final_discard = ('Final Discard', u'河底撈魚', 10)

win_on_kong = ('Win on Kong', u'嶺上開花', 10)
win_on_bonus_tile = ('Win on Bonus Tile', u'花上自摸', 10)

robbing_a_kong = ('Robbing a Kongs', u'搶槓', 10)

blessing_of_heaven = ('Blessing of Heaven', u'天和', 155)
blessing_of_earth = ('Blessing of Earth', u'地和', 155)


# 13.0 Bonus Tiles

non_seat_flower = ('Non-seat Flower', u'偏花', 2)
non_seat_season = ('Non-seat Season', u'偏季', 2)
seat_flower = ('Seat Flower', u'正花', 4)
seat_season = ('Seat Season', u'正季', 4)

four_flowers = ('Four Flowers', u'齊四花', 10)
four_seasons = ('Four Seasons', u'齊四季', 10)

all_bonus_tiles = ('All Bonus Tiles', u'八仙過海', 50)
