#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-

### score.py takes in a list of melds that is calcalated from hand.py.  From
### this list, score.py will produce the list of patterns the hand satisfies. In a
### sense, hend.py do all the grunt work while scope.py just match the patterns
### and assign values.

import hand  as h
import tile  as t
import fp    as f
import chart as c



##############################
### Meld Related Functions ###
##############################

# m is for melded
# n is for concealed
# c is for chow
# p is for pung
# k is for kong
# e is for eye
# s is for simple
# t is for terminal
# w is for wind
# d is for dragon
# b is for bonus (flowers, seasons, north[3ma], animals)
# h is for special hands (mainly nine gates and 13 orphans)
# l is for leftover (mainly for 8 bonus tiles)

# basic building blocks

def _is_melded(meld):
    return 'm' in t.fst(meld)

def _is_concealed(meld):
    return 'n' in t.fst(meld)

def _is_chow(meld):
    return 'c' in t.fst(meld)

def _is_pung(meld):
    return 'p' in t.fst(meld) or 'k' in t.fst(meld)

def _is_kong(meld):
    return 'k' in t.fst(meld)

def _is_eye(meld):
    return 'e' in t.fst(meld)

def _is_simple(meld):
    return 's' in t.fst(meld)

def _is_terminal(meld):
    return 't' in t.fst(meld)

def _is_wind(meld):
    return 'w' in t.fst(meld)

def _is_dragon(meld):
    return 'd' in t.fst(meld)

def _is_bonus(meld):
    return 'b' in t.fst(meld)



# compound funcs

def _is_meld(meld):
    return _is_chow(meld) or _is_pung(meld) or _is_kong(meld)

def _is_simple_chow(meld):
    return _is_chow(meld) and _is_simple(meld)

def _is_terminal_chow(meld):
    return _is_chow(meld) and _is_terminal(meld)

def _is_simple_pung(meld):
    return _is_pung(meld) and _is_simple(meld)

def _is_terminal_pung(meld):
    return _is_pung(meld) and _is_terminal(meld)

def _is_wind_pung(meld):
    return _is_pung(meld) and _is_wind(meld)

def _is_dragon_pung(meld):
    return _is_pung(meld) and _is_dragon(meld)

def _is_honor_pung(meld):
    return _is_pung(meld) and (_is_wind(meld) or _is_dragon(meld))

def _is_concealed_pung(meld):
    return _is_pung(meld) and _is_concealed(meld)

def _is_simple_meld(meld):
    return _is_chow(meld) or _is_simple_pung(meld)

def _is_terminal_meld(meld):
    return _is_terminal_chow(meld) or _is_terminal_pung(meld)

def _is_outside_pung(meld):
    return _is_pung(meld) and ( _is_terminal(meld) or _is_wind(meld) or _is_dragon(meld))

def _is_outside_meld(meld):
    return _is_terminal_meld(meld) or _is_honor_pung(meld)

def _is_simple_eye(meld):
    return _is_eye(meld) and _is_simple(meld)

def _is_terminal_eye(meld):
    return _is_eye(meld) and _is_terminal(meld)

def _is_wind_eye(meld):
    return _is_eye(meld) and _is_wind(meld)

def _is_dragon_eye(meld):
    return _is_eye(meld) and _is_dragon(meld)

def _is_honor_eye(meld):
    return _is_eye(meld) and (_is_wind(meld) or _is_dragon(meld))

def _is_outside_eye(meld):
    return _is_eye(meld) and (_is_terminal(meld) or _is_wind(meld) or _is_dragon(meld))



# extraction

def _get_melds(hand):
    return f.filter_func(_is_meld, hand)

def _get_eyes(hand):
    return f.filter_func(_is_eye, hand)

def _get_bonus(hand):
    return f.filter_func(_is_bonus, hand)

def _get_tiles(hand):
    l = f.map_func(t.snd, hand)
    return [t for m in l for t in m]


#########################
### Utility Functions ###
#########################

def _get_str_rep(meld):
    return f.map_func(t.show_tile, t.snd(meld))

def _join_str_rep(meld):
    return f.fold_func(f.add_, "", get_str_rep(meld))



########################
### Hand Evaluations ###
########################

### 1.0 Trivial Patterns

# chicken is only when your hand satisfies no other patterns aside form bonus tiles
# no need to implement as it is the only option when all patterns fails
def _is_chicken():
    return c.chicken

def _is_all_chows(hand):
    if f.and_func(f.map_func(_is_chow, _get_melds(hand))):
        return c.all_chow
    return c.nothing

# note: this applies when no meld has been made prior to calling mahjong
# when mahjong is called, unless self drawn, the tile claimed completes a MELDED set
def _is_concealed_hand():
    return c.concealed

def _is_self_drawn(hand):
    return c.self_drawn

# works for both 4 meld and eye, or seven pairs
def _is_all_simples(hand):
    if f.and_func(_is_simple, _get_melds(hand) + _get_eyes(hand)):
        return c._is_all_simples
    return c.nothing

def _is_all_types(hand):
    ts = _get_tiles(hand)
    has_coin      = f.or_func(f.map_func(t.is_coin, ts))
    has_bamboo    = f.or_func(f.map_func(t.is_bamboo, ts))
    has_character = f.or_func(f.map_func(t.is_character, ts))
    has_wind      = f.or_func(f.map_func(t.is_wind, ts))
    has_dragon    = f.or_func(f.map_func(t.is_dragon, ts))
    if has_coin and has_bamboo and has_character and has_wind and has_dragon:
        return c.all_type
    return c.nothing

def _is_illegal_call(hand):
    return c._is_illegal_call



### 2.0 Identical Sets

def _is_two_identical_chows(hand):
    lfunc = lambda x: _join_str_rep(t.snd(x))
    d = to_dict(f.map_func(lf, _get_melds(hand)))
    if len(d) == 3:
        if sorted(d.values()) == [1,1,2]:
            return c.two_identical_chows
    return c.nothing

def _is_two_identical_chows_twice(hand):
    lfunc = lambda x: _join_str_rep(t.snd(x))
    d = to_dict(f.map_func(lfunc, _get_melds(hand)))
    if len(d) == 2:
        if d.values() == [2,2]:
            return c.two_identical_chows_twice
    return c.nothing

def _is_three_identical_chows(hand):
    lfunc = lambda x: _join_str_rep(t.snd(x))
    d = to_dict(f.map_func(lfunc, _get_melds(hand)))
    if len(d) == 2:
        if sorted(d.values()) == [1,3]:
            return c.three_identical_chows
    return c.nothing

def _is_four_identical_chows(hand):
    lfunc = lambda x: _join_str_rep(t.snd(x))
    d = to_dict(f.map_func(lfunc, _get_melds(hand)))
    if len(d) == 1:
        if d.values() == [4]:
            return c.four_identical_chows
    return c.nothing



### 3.0 Pungs and Kongs

def _is_all_pungs(hand):
    if f.and_func(f.map_func(is_pung, _get_melds(hand))):
        return c.all_pungs
    return c.nothing


def _is_two_concealed_pungs(hand):
    if f.count_with(_is_concealed_pung, _get_melds(hand)) == 2:
        return c.two_concealed_pungs
    return c.nothing

def _is_three_concealed_pungs(hand):
    if f.count_with(_is_concealed_pung, _get_melds(hand) ) == 3:
        return c.three_concealed_pungs
    return c.nothing

def _is_four_concealed_pungs(hand):
    if f.count_with(_is_concealed_pung, _get_melds(hand)) == 4:
        return c.four_concealed_pungs
    return c.nothing



def _is_one_kong(hand):
    if f.count_with(_is_kong, _get_melds(hand)) == 1:
        return c.one_kongs
    return c.nothing

def _is_two_kongs(hand):
    if f.count_with(_is_kong, _get_melds(hand)) == 2:
        return c.two_kongs
    return c.nothing

def _is_three_kongs(hand):
    if f.count_with(_is_kong, _get_melds(hand)) == 3:
        return c.three_kongs
    return c.nothing

def _is_four_kongs(hand):
    if f.count_with(_is_kong, _get_melds(hand)) == 4:
        return c.four_kongs
    return c.nothing



### 4.0 Similar Sets

def _is_three_similar_chows(hand):
    pass


def _is_small_three_similar_pungs(hand):
    pass

def _is_three_similar_pungs(hand):
    pass



### 5.0 Consecutive Sets

def _is_nine_tile_straight(hand):
    pass

def _is_three_consecutive_pungs(hand):
    pass

def _is_four_consecutive_pungs(hand):
    pass

def _is_three_mothers(hand):
    pass



### 6.0 Suit Patterns

def _is_mixed_one_suit(hand):
    pass

def _is_pure_one_suit(hand):
    pass


def _is_small_dragon_club(hand):
    pass

def _is_big_dragon_club(hand):
    pass


def _is_nine_gates(hand):
    pass



### 7.0 Terminal Tiles

def _is_two_tailed_terminal_chows(hand):
    pass

def _is_two_tailed_terminal_pungs(hand):
    pass


def _is_small_boundless_mountain(hand):
    pass

def _is_big_boundless_mountain(hand):
    pass


def _is_mixed_lesser_terminals(hand):
    pass


def _is_pure_lesser_terminals(hand):
    pass

def _is_mixed_greater_germinals(hand):
    pass

def _is_pure_greater_terminals(hand):
    pass



### 8.0 Honor Tiles

def _is_dragon_pung(hand):
    pass

def _is_seat_wind(hand):
    pass


def _is_small_three_dragons(hand):
    pass

def _is_big_three_dragons(hand):
    pass


def _is_small_three_winds(hand):
    pass

def _is_big_three_winds(hand):
    pass

def _is_small_four_winds(hand):
    pass

def _is_big_four_winds(hand):
    if f.and_func(f.map_func(is_wind_pung, _get_melds(hand))):
        return c.big_four_winds
    return c.nothing


def _is_all_honor_pungs(hand):
    if f.and_func(f.map_func(is_honor_pung, _get_melds(hand))):
        if f.and_func(_is_honor_eye, _get_eyes(hand)):
            return c.all_honor_pungs
    return c.nothing

def _is_all_honor_pairs(hand):
    es = f.map_func(t.snd, _get_eyes(hands))
    if len(h.to_dict(es)) == 7:
        ts = h.sort_tiles([tile for tile in Set(es)])
        honors = [tile for tile in t.honor_tiles]
        if ts == honors:
            return c.all_honor_pairs
    return c.nothing



### 9.0 Seven Pairs

def _is_seven_pairs(hand):
    d = to_dict(f.map_func(t.snd, _get_eyes(hands)))
    if len(d) == 7:
        if f.and_func(f.map_func(lambda x: x == 2, d.values())):
            return 30
    if len(d) == 6:
        if sorted(d.values()) == [2, 2, 2, 2, 2, 4]:
            return 30
    if len(d) == 5:
        if sorted(d.values()) == [2, 2, 2, 4, 4]:
            return 30
    if len(d) == 4:
        if sorted(d.values()) == [2, 4, 4, 4]:
            return 30
    return 0

def _is_seven_shifted_pairs(hand):
    es = f.map_func(t.snd, _get_eyes(hands))
    if len(h.to_dict(es)) == 7:
        ts = f.map_func(t.snd, h.sort_tiles([tile for tile in Set(es)]))
        if ts == range(1, 8) or ts == range(3, 10):
                return c.seven_shifted_pairs
    return c.nothing

def _is_grand_chariot(hand):
    es = f.map_func(t.snd, _get_eyes(hands))
    if len(h.to_dict(es)) == 7:
        ts = h.sort_tiles([tile for tile in Set(es)])
        lfunc = lambda x: t.fst(x) == t.tile_types[0]
        if f.and_func(f.map_func(lfunc, ts)):
            if f.map_func(t.snd, ts) == range(2, 9):
                return c.grand_chariot
    return c.nothing

def _is_bamboo_forest(hand):
    es = f.map_func(t.snd, _get_eyes(hands))
    if len(h.to_dict(es)) == 7:
        ts = h.sort_tiles([tile for tile in Set(es)])
        lfunc = lambda x: t.fst(x) == t.tile_types[1]
        if f.and_func(f.map_func(lfunc, ts)):
            if f.map_func(t.snd, ts) == range(2, 9):
                return c.bamboo_forest
    return c.nothing

def _is_number_neighborhood(hand):
    es = f.map_func(t.snd, _get_eyes(hands))
    if len(h.to_dict(es)) == 7:
        ts = h.sort_tiles([tile for tile in Set(es)])
        lfunc = lambda x: t.fst(x) == t.tile_types[2]
        if f.and_func(f.map_func(lfunc, ts)):
            if f.map_func(t.snd, ts) == range(2, 9):
                return c.number_neighborhood
    return c.nothing



### 10.0 Color Hands

def _is_all_green(hand):
    ms = _get_melds(hand)
    lfunc = lambda m: f.and_func(f.map_func(t.is_green, m))
    if f.and_func(f.map_func(lfunc, ms)):
        return c.all_green
    return c.nothing


def _is_all_red(hand):
    ms = _get_melds(hand)
    lfunc = lambda m: f.and_func(f.map_func(t.is_red, m))
    if f.and_func(f.map_func(lfunc, ms)):
        return c.all_red
    return c.nothing


def _is_all_blue(hand):
    ms = _get_melds(hand)
    lfunc = lambda m: f.and_func(f.map_func(t.is_blue, m))
    if f.and_func(f.map_func(lfunc, ms)):
        return c.all_blue
    return c.nothing



### 11.0 Irregular Hands

def _is_thirteen_orphans(hand):
    # need to check all thirteen terminal tiles are in the hand
    # and that no other tiles exists in the hand
    # pigeonhole principle: 14 tiles fitting into 13 slots, one must be repeated
    h = Set( hand['held'] + [hand['last']] )
    s = Set(t.edge_tiles)
    if h.issubset(s) and s.issubset(h):
        return c.thirteen_orphans
    return c.nothing



### 12.0 Incidental bonuses

def _is_final_draw():
    return c.final_draw


def _is_final_discard():
    return c.final_discard


def _is_win_on_kong():
    return c.win_on_kong

def _is_win_on_bonus():
    return c.win_on_bonus_tile

def _is_robbing_a_kong():
    return c.robbing_a_kong


def _is_blessing_of_heaven():
    return c.blessing_of_heaven

def _is_blessing_of_earth():
    return c.blessing_of_earth



### 13.0 Bonus Tiles

def _is_non_seat_flower(hand, seat):
    fs = f.filter_func(t.is_flower, _get_bonus(hand))
    nsfs = [f for f in fs if f != t.flower_tiles[seat]]
    if len(nsfs) > 0:
        return (c.non_seat_flower[0], c.non_seat_flower[1], c.non_seat_flower * len(nsfs))
    return c.nothing

def _is_non_seat_season(hand, seat):
    ss = f.filter_func(t.is_season, _get_bonus(hand))
    nsss = [s for s in ss if s != t.season_tiles[seat]]
    if len(nsss) > 0:
        return (c.non_seat_season[0], c.non_seat_season[1], c.non_seat_season * len(nsss))
    return c.nothing

def _is_seat_flower(hand, seat):
    fs = f.filter_func(t.is_flower, _get_bonus(hand))
    if t.flower_tiles[seat] in fs
        return c.seat_flower
    return c.nothing

def _is_seat_season(hand, seat):
    ss = f.filter_func(t.is_season, _get_bonus(hand))
    if t.season_tiles[seat] in ss
        return c.seat_season
    return c.nothing



def _is_four_flowers(hand):
    fs = f.filter_func(t.is_flower, _get_bonus(hand))
    if len(fs) == 4:
        return c.all_flowers
    return c.nothing

def _is_four_seasons(hand):
    ss = f.filter_func(t.is_season, _get_bonus(hand))
    if len(ss) == 4:
        return c.all_seasons
    return c.nothing



def _is_all_flowers(hand):
    bs = _get_bonus(hand)
    if len(bs) == 8:
        return c.all_bonus_tiles
    return c.nothing
