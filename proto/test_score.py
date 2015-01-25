#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-

"""test_score.py runs each evalution function in score.py on each respective
hand exmaple in examples_score.py.

"""

import score as s
import examples_score as es


# Tests
#
#     Print out all the results

def print_(r):
    """Print out the result of each evalution."""

    if r:
        # print r[0] + ' : ' + r[1] + ' : ' + str(r[2])
        print r[0] + ' : ' + str(r[2])
    else:
        print 'FAILED'


# 1.0 Trivial Patterns

print_(s._is_chicken())
print_(s._is_all_chows(es.h_all_chows))
print_(s._is_concealed_hand(es.h_concealed))
print_(s._is_self_drawn())
print_(s._is_all_simples(es.h_all_simples_1))
print_(s._is_all_simples(es.h_all_simples_2))
print_(s._is_all_types(es.h_all_types))
print_(s._is_illegal_call())


# 2.0 Identical Chows

print_(s._is_two_identical_chows(es.h_2_id_chows))
print_(s._is_two_identical_chows_twice(es.h_2_id_chows_2x))
print_(s._is_three_identical_chows(es.h_3_id_chows))
print_(s._is_four_identical_chows(es.h_4_id_chows))


# 3.0 Pungs and Kongs

print_(s._is_all_pungs(es.h_all_pungs))

print_(s._is_two_concealed_pungs(es.h_2_concealed_pungs))
print_(s._is_three_concealed_pungs(es.h_3_concealed_pungs))
print_(s._is_four_concealed_pungs(es.h_4_concealed_pungs))

print_(s._is_one_kong(es.h_1_kong))
print_(s._is_two_kongs(es.h_2_kongs))
print_(s._is_three_kongs(es.h_3_kongs))
print_(s._is_four_kongs(es.h_4_kongs))


# 4.0 Similar Sets

print_(s._is_three_similar_chows(es.h_3_similar_chows))

print_(s._is_little_three_similar_pungs(es.h_little_3_similar_pungs))
print_(s._is_three_similar_pungs(es.h_3_similar_pungs))


# 5.0 Consecutive Sets

print_(s._is_nine_tile_straight(es.h_9_tile_straight))

print_(s._is_three_consecutive_pungs(es.h_3_consecutive_pungs_1))
print_(s._is_three_consecutive_pungs(es.h_3_consecutive_pungs_2))
print_(s._is_four_consecutive_pungs(es.h_4_consecutive_pungs))
print_(s._is_three_mothers(es.h_3_mothers))


# 6.0 Suit Patterns

print_(s._is_mixed_one_suit(es.h_mixed_1_suit_1))
print_(s._is_mixed_one_suit(es.h_mixed_1_suit_2))
print_(s._is_pure_one_suit(es.h_pure_1_suit_1))
print_(s._is_pure_one_suit(es.h_pure_1_suit_2))
print_(s._is_little_terminal_club(es.h_little_terminal_club))
print_(s._is_big_terminal_club(es.h_big_terminal_club))

# print_(s._is_nine_gates(es.h_nine_gates))


# 7.0 Terinal Tiles

print_(s._is_two_tailed_terminal_chows(es.h_two_tailed_terminal_chows_1))
print_(s._is_two_tailed_terminal_chows(es.h_two_tailed_terminal_chows_2))
print_(s._is_two_tailed_terminal_chows(es.h_two_tailed_terminal_chows_3))
print_(s._is_two_tailed_terminal_pungs(es.h_two_tailed_terminal_pungs_1))
print_(s._is_two_tailed_terminal_pungs(es.h_two_tailed_terminal_pungs_2))
print_(s._is_little_boundless_mountain(es.h_little_boundless_mountain_1))
print_(s._is_little_boundless_mountain(es.h_little_boundless_mountain_2))
print_(s._is_big_boundless_mountain(es.h_big_boundless_mountain_1))
print_(s._is_big_boundless_mountain(es.h_big_boundless_mountain_2))

print_(s._is_mixed_lesser_terminals(es.h_mixed_lesser_terminals))
print_(s._is_pure_lesser_terminals(es.h_pure_lesser_terminals))
print_(s._is_mixed_greater_terminals(es.h_mixed_greater_terminals_1))
print_(s._is_mixed_greater_terminals(es.h_mixed_greater_terminals_2))
print_(s._is_pure_greater_terminals(es.h_pure_greater_terminals_1))
print_(s._is_pure_greater_terminals(es.h_pure_greater_terminals_2))


# 8.0 Honor Tiles

print_(s._is_dragon_pung_hand(es.h_dragon_pung))
print_(s._is_seat_wind(es.h_seat_wind, 0))

print_(s._is_little_three_winds(es.h_little_3_winds))
print_(s._is_big_three_winds(es.h_big_3_winds))
print_(s._is_little_four_winds(es.h_little_4_winds))
print_(s._is_big_four_winds(es.h_big_4_winds))

print_(s._is_little_three_dragons(es.h_little_3_dragons))
print_(s._is_big_three_dragons(es.h_big_3_dragons))

print_(s._is_all_honors(es.h_all_honors_1))
print_(s._is_all_honors(es.h_all_honors_2))
print_(s._is_all_honor_pairs(es.h_all_honor_pairs))


# 9.0 Seven Pairs

print_(s._is_seven_pairs(es.h_7_pairs_1))
print_(s._is_seven_pairs(es.h_7_pairs_2))
print_(s._is_seven_pairs(es.h_7_pairs_3))
print_(s._is_seven_pairs(es.h_7_pairs_4))

print_(s._is_seven_shifted_pairs(es.h_7_shifted_pairs_1))
print_(s._is_seven_shifted_pairs(es.h_7_shifted_pairs_2))
print_(s._is_grand_chariot(es.h_grand_chariot))
print_(s._is_bamboo_forest(es.h_bamboo_forest))
print_(s._is_number_neighborhood(es.h_number_neighborhood))


# 10.0 Color Hands

print_(s._is_all_green(es.h_all_green))
print_(s._is_all_red(es.h_all_red))
print_(s._is_all_blue(es.h_all_blue))


# 11.0 Irregular Hands

# print_(s._is_thirdteen_orphans(es.h_13_orphans))


# 12.0 Inidental Bonuses

print_(s._is_final_draw())
print_(s._is_final_discard())
print_(s._is_win_on_kong())
print_(s._is_win_on_bonus_tile())
print_(s._is_robbing_a_kong())
print_(s._is_blessing_of_heaven())
print_(s._is_blessing_of_earth())


# 13.0 Bous Tiles

print_(s._is_non_seat_flower(es.h_non_seat_flower, 0))
print_(s._is_non_seat_season(es.h_non_seat_season, 0))
print_(s._is_seat_flower(es.h_seat_flower, 0))
print_(s._is_seat_season(es.h_seat_season, 0))
print_(s._is_four_flowers(es.h_4_flowers))
print_(s._is_four_seasons(es.h_4_seasons))
print_(s._is_all_bonus_tiles(es.h_all_bonus_tiles))
