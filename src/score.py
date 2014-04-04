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
from sets import Set


##############################
### Meld Related Functions ###
##############################

# m is for melded
# n is for concealed
# c is for chow
# p is for pung
# k is for kong
# e is for eye
# h is for special hands (mainly nine gates and 13 orphans)
# l is for leftover (mainly for 8 bonus tiles)

# C is for coin
# B is for bamboo
# K is for character
# W is for wind
# D is for dragon
# S is for simple
# T is for terminal
# b is for bonus (flowers, seasons, north[3ma], animals)

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


def _is_coin(meld):
    return 'C' in t.fst(meld)

def _is_bamboo(meld):
    return 'B' in t.fst(meld)

def _is_character(meld):
    return 'K' in t.fst(meld)

def _is_wind(meld):
    return 'W' in t.fst(meld)

def _is_dragon(meld):
    return 'D' in t.fst(meld)

def _is_simple(meld):
    return 'S' in t.fst(meld)

def _is_terminal(meld):
    return 'T' in t.fst(meld)

def _is_bonus(meld):
    return 'b' in t.fst(meld)



# compound funcs

def _is_meld(meld):
    return _is_chow(meld) or _is_pung(meld) or _is_kong(meld)

def _is_suit_meld(meld):
    return _is_coin(meld) or _is_bamboo(meld) or _is_character(meld)

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
    ms = f.map_func(t.snd, hand)
    return [tile for m in ms for tile in m]


#########################
### Utility Functions ###
#########################

def _get_str_rep(meld):
    return f.map_func(t.show_tile, t.snd(meld))

def _join_str_rep(meld):
    return f.fold_func(f.add_, "", _get_str_rep(meld))

def to_dict_melds(melds):
    d = {}
    for m in melds:
        k = _join_str_rep(m)
        if d.has_key(k):
            d[k] += 1
        else:
            d[k] = 1
    return d

def to_dict_tiles(tiles):
    d = {}
    for m in tiles:
        k = t.fst(m)
        if d.has_key(k):
            d[k] += 1
        else:
            d[k] = 1
    return d


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
        return c.all_chows
    return c.nothing

# note: this applies when no meld has been made prior to calling mahjong
# when mahjong is called, unless self drawn, the tile claimed completes a MELDED set
def _is_concealed_hand(hand):
    cms = f.filter_func(_is_concealed, _get_melds(hand) + _get_eyes(hand))
    if len(cms) >= 4:
        return c.concealed
    return c.nothing

def _is_self_drawn():
    return c.self_drawn

# works for both 4 meld and eye, or seven pairs
def _is_all_simples(hand):
    if f.and_func(f.map_func(_is_simple, _get_melds(hand) + _get_eyes(hand))):
        return c.all_simples
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

def _is_illegal_call():
    return c.illegal_call



### 2.0 Identical Sets

def _is_two_identical_chows(hand):
    d = to_dict_melds(_get_melds(hand))
    if len(d) == 3:
        if sorted(d.values()) == [1,1,2]:
            return c.two_identical_chows
    return c.nothing

def _is_two_identical_chows_twice(hand):
    d = to_dict_melds(_get_melds(hand))
    if len(d) == 2:
        if d.values() == [2,2]:
            return c.two_identical_chows_twice
    return c.nothing

def _is_three_identical_chows(hand):
    d = to_dict_melds(_get_melds(hand))
    if len(d) == 2:
        if sorted(d.values()) == [1,3]:
            return c.three_identical_chows
    return c.nothing

def _is_four_identical_chows(hand):
    d = to_dict_melds(_get_melds(hand))
    if len(d) == 1:
        if d.values() == [4]:
            return c.four_identical_chows
    return c.nothing



### 3.0 Pungs and Kongs

def _is_all_pungs(hand):
    if f.and_func(f.map_func(_is_pung, _get_melds(hand))):
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
        return c.one_kong
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


def _is_little_three_similar_pungs(hand):
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
    ts = f.flatten(f.map_func(t.snd, _get_melds(hand) + _get_eyes(hand)))
    s = Set(f.map_func(t.fst, ts))
    if len(s) == 1:
        return c.pure_one_suit
    return c.nothing

def _is_little_terminal_club(hand):
    ts = f.flatten(f.map_func(t.snd, _get_melds(hand) + _get_eyes(hand)))
    s = sorted(f.map_func(t.snd, ts))
    if s == [1, 1, 2, 2, 3, 3, 5, 5, 7, 7, 8, 8, 9, 9]:
        return c.little_terminal_club
    return c.nothing

def _is_big_terminal_club(hand):
    ts = f.flatten(f.map_func(t.snd, _get_melds(hand) + _get_eyes(hand)))
    s = sorted(f.map_func(t.snd, ts))
    if s == [1, 1, 1, 1, 2, 3, 5, 5, 7, 8, 9, 9, 9, 9]:
        return c.big_terminal_club
    return c.nothing


def _is_nine_gates(hand):
    pass



### 7.0 Terminal Tiles

def _is_two_tailed_terminal_chows(hand):
    pass


def _is_two_tailed_terminal_pungs(hand):
    pass



def _is_little_boundless_mountain(hand):
    if _is_pure_one_suit(hand) and _is_pure_lesser_terminals(hand):
        ts = f.flatten(f.map_func(t.snd, _get_melds(hand) + _get_eyes(hand)))
        tts = f.filter_func(t.is_terminal, ts)
        if len(tts) == 6:
            return c.little_boundless_mountain
    return c.nothing

def _is_big_boundless_mountain(hand):
    if _is_pure_one_suit(hand) and _is_pure_lesser_terminals(hand):
        ts = f.flatten(f.map_func(t.snd, _get_melds(hand) + _get_eyes(hand)))
        tts = f.filter_func(t.is_terminal, ts)
        if len(tts) == 8:
            return c.big_boundless_mountain
    return c.nothing


def _is_mixed_lesser_terminals(hand):
    if f.and_func(f.map_func(_is_outside_meld, _get_melds(hand))):
        if f.and_func(f.map_func(_is_outside_eye, _get_eyes(hand))):
            return c.mixed_lesser_terminals
    return c.nothing


def _is_pure_lesser_terminals(hand):
    if f.and_func(f.map_func(_is_terminal_meld, _get_melds(hand))):
        if f.and_func(f.map_func(_is_outside_eye, _get_eyes(hand))):
            return c.pure_lesser_terminals
    return c.nothing

def _is_mixed_greater_terminals(hand):
    if f.and_func(f.map_func(_is_outside_pung, _get_melds(hand))):
        if f.and_func(f.map_func(_is_outside_eye, _get_eyes(hand))):
            return c.mixed_greater_terminals
    return c.nothing

def _is_pure_greater_terminals(hand):
    if f.and_func(f.map_func(_is_terminal_pung, _get_melds(hand))):
        if f.and_func(f.map_func(_is_outside_eye, _get_eyes(hand))):
            return c.pure_greater_terminals
    return c.nothing



### 8.0 Honor Tiles

def _is_dragon_pung_hand(hand):
    dps = f.filter_func(_is_dragon_pung, _get_melds(hand))
    if len(dps) > 0:
        return (c.dragon_pung[0], c.dragon_pung[1], c.dragon_pung[2] * len(dps))
    return c.nothing

def _is_seat_wind(hand, seat):
    wps = f.filter_func(_is_wind_pung, _get_melds(hand))
    wts = f.flatten(f.map_func(t.snd, wps))
    if t.wind_tiles[seat] in wts:
        return c.seat_wind
    return c.nothing


def _is_little_three_winds(hand):
    wps = f.filter_func(_is_wind_pung, _get_melds(hand))
    if len(wps) == 2:
        if _is_wind_eye(_get_eyes(hand)[0]):
            return c.little_three_winds
    return c.nothing

def _is_big_three_winds(hand):
    wps = f.filter_func(_is_wind_pung, _get_melds(hand))
    if len(wps) == 3:
        if not _is_wind_eye(_get_eyes(hand)[0]):
            return c.big_three_winds
    return c.nothing


def _is_little_four_winds(hand):
    wps = f.filter_func(_is_wind_pung, _get_melds(hand))
    if len(wps) == 3:
        if _is_wind_eye(_get_eyes(hand)[0]):
            return c.little_four_winds
    return c.nothing

def _is_big_four_winds(hand):
    if f.and_func(f.map_func(_is_wind_pung, _get_melds(hand))):
        return c.big_four_winds
    return c.nothing


def _is_little_three_dragons(hand):
    dps = f.filter_func(_is_dragon_pung, _get_melds(hand))
    if len(dps) == 2:
        if _is_dragon_eye(_get_eyes(hand)[0]):
            return c.little_three_dragons
    return c.nothing

def _is_big_three_dragons(hand):
    dps = f.filter_func(_is_dragon_pung, _get_melds(hand))
    if len(dps) == 3:
        return c.big_three_dragons
    return c.nothing


def _is_all_honors(hand):
    if f.and_func(f.map_func(_is_honor_pung, _get_melds(hand))):
        if f.and_func(f.map_func(_is_honor_eye, _get_eyes(hand))):
            return c.all_honors
    return c.nothing

def _is_all_honor_pairs(hand):
    ts = Set(f.flatten(f.map_func(t.snd, _get_eyes(hand))))
    if len(ts) == 7:
        ts = h.sort_tiles([tile for tile in ts])
        honors = [tile for tile in t.honor_tiles]
        if ts == honors:
            return c.all_honor_pairs
    return c.nothing



### 9.0 Seven Pairs

def _is_seven_pairs(hand):
    d = to_dict_tiles(f.flatten(f.map_func(t.snd, _get_eyes(hand))))
    if len(d) == 7:
        if d.values() == f.repeat(2, 7):
            return c.seven_pairs
    if len(d) == 6:
        if sorted(d.values()) == [2, 2, 2, 2, 2, 4]:
            return c.seven_pairs
    if len(d) == 5:
        if sorted(d.values()) == [2, 2, 2, 4, 4]:
            return c.seven_pairs
    if len(d) == 4:
        if sorted(d.values()) == [2, 4, 4, 4]:
            return c.seven_pairs
    return c.nothing

def _is_seven_shifted_pairs(hand):
    if _is_pure_one_suit(hand):
        ts = Set(f.flatten(f.map_func(t.snd, _get_eyes(hand))))
        if len(ts) == 7:
            vs = sorted(f.map_func(t.snd, [tile for tile in ts]))
            if vs == range(1, 8) or vs == range(3, 10):
                return c.seven_shifted_pairs
    return c.nothing

def _is_grand_chariot(hand):
    ts = Set(f.flatten(f.map_func(t.snd, _get_eyes(hand))))
    if len(ts) == 7:
        tts = f.map_func(t.fst, [tile for tile in ts])
        vs = sorted(f.map_func(t.snd, [tile for tile in ts]))
        lfunc = lambda x: x == t.tile_types[0]
        if f.and_func(f.map_func(lfunc, tts)):
            if vs == range(2, 9):
                return c.grand_chariot
    return c.nothing

def _is_bamboo_forest(hand):
    ts = Set(f.flatten(f.map_func(t.snd, _get_eyes(hand))))
    if len(ts) == 7:
        tts = f.map_func(t.fst, [tile for tile in ts])
        vs = sorted(f.map_func(t.snd, [tile for tile in ts]))
        lfunc = lambda x: x == t.tile_types[1]
        if f.and_func(f.map_func(lfunc, tts)):
            if vs == range(2, 9):
                return c.bamboo_forest
    return c.nothing

def _is_number_neighborhood(hand):
    ts = Set(f.flatten(f.map_func(t.snd, _get_eyes(hand))))
    if len(ts) == 7:
        tts = f.map_func(t.fst, [tile for tile in ts])
        vs = sorted(f.map_func(t.snd, [tile for tile in ts]))
        lfunc = lambda x: x == t.tile_types[2]
        if f.and_func(f.map_func(lfunc, tts)):
            if vs == range(2, 9):
                return c.number_neighborhood
    return c.nothing



### 10.0 Color Hands

def _is_all_green(hand):
    ts = f.flatten(f.map_func(t.snd, _get_melds(hand) + _get_eyes(hand)))
    if f.and_func(f.map_func(t.is_green, ts)):
        return c.all_green
    return c.nothing


def _is_all_red(hand):
    ts = f.flatten(f.map_func(t.snd, _get_melds(hand) + _get_eyes(hand)))
    if f.and_func(f.map_func(t.is_red, ts)):
        return c.all_green
    return c.nothing


def _is_all_blue(hand):
    ts = f.flatten(f.map_func(t.snd, _get_melds(hand) + _get_eyes(hand)))
    if f.and_func(f.map_func(t.is_blue, ts)):
        return c.all_green
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
    # save this for now, change it later


### 12.0 Incidental bonuses

def _is_final_draw():
    return c.final_draw


def _is_final_discard():
    return c.final_discard


def _is_win_on_kong():
    return c.win_on_kong

def _is_win_on_bonus_tile():
    return c.win_on_bonus_tile

def _is_robbing_a_kong():
    return c.robbing_a_kong


def _is_blessing_of_heaven():
    return c.blessing_of_heaven

def _is_blessing_of_earth():
    return c.blessing_of_earth



### 13.0 Bonus Tiles

def _is_non_seat_flower(hand, seat):
    if f.or_func(f.map_func(_is_bonus, hand)):
        fls = f.filter_func(t.is_flower, t.snd(_get_bonus(hand)[0]))
        nsfs = [fl for fl in fls if fl != t.flower_tiles[seat + 1]]
        if len(nsfs) > 0:
            return (c.non_seat_flower[0], c.non_seat_flower[1], c.non_seat_flower[2] * len(nsfs))
    return c.nothing

def _is_non_seat_season(hand, seat):
    if f.or_func(f.map_func(_is_bonus, hand)):
        sns = f.filter_func(t.is_season, t.snd(_get_bonus(hand)[0]))
        nsss = [sn for sn in sns if sn != t.season_tiles[seat + 1]]
        if len(nsss) > 0:
            return (c.non_seat_season[0], c.non_seat_season[1], c.non_seat_season[2] * len(nsss))
    return c.nothing

def _is_seat_flower(hand, seat):
    if f.or_func(f.map_func(_is_bonus, hand)):
        fls = f.filter_func(t.is_flower, t.snd(_get_bonus(hand)[0]))
        if t.flower_tiles[seat] in fls:
            return c.seat_flower
    return c.nothing

def _is_seat_season(hand, seat):
    if f.or_func(f.map_func(_is_bonus, hand)):
        sns = f.filter_func(t.is_season, t.snd(_get_bonus(hand)[0]))
        if t.season_tiles[seat] in sns:
            return c.seat_season
    return c.nothing



def _is_four_flowers(hand):
    if f.or_func(f.map_func(_is_bonus, hand)):
        fls = f.filter_func(t.is_flower, t.snd(_get_bonus(hand)[0]))
        if len(fls) == 4:
            return c.four_flowers
    return c.nothing

def _is_four_seasons(hand):
    if f.or_func(f.map_func(_is_bonus, hand)):
        sns = f.filter_func(t.is_season, t.snd(_get_bonus(hand)[0]))
        if len(sns) == 4:
            return c.four_seasons
    return c.nothing



def _is_all_bonus_tiles(hand):
    if f.or_func(f.map_func(_is_bonus, hand)):
        bs = t.snd(_get_bonus(hand)[0])
        if len(bs) == 8:
            return c.all_bonus_tiles
    return c.nothing
