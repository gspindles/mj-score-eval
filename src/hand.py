#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-

import tile  as t
import fp    as f
import score as s
from   sets  import Set



########################
### Class Definition ###
########################

# Definition:
# A meld is a list of 3 or 4 (kong) tiles that make up a chow, pung, or kong
# A hand is a dictionary consisting of 5 key value pairs
#     'held'      : is a list of tiles currently holding on hand
#     'concealed' : is concealed melds
#     'melded'    : is a list of melds
#     'eye'       : is a pair of tiles
#     'last'      : is the last winning tile
#     'bonus'     : is a list of flowers and seasons

# leave this for now, should just delete it later since we are not doing OO
class Hand:

    _held      = []
    _concealed = []
    _melded    = []
    _eye       = []
    _last      = None
    _bonus     = []

    def __init__(self, held, concealed, melded, eye, last, bonus):
        if tiles is not None:
            self._held      = held
            self._concealed = concealed
            self._melded    = melded
            self._eye       = eye
            self._last      = last
            self._bonus     = bonus

    @property
    def held(self):
        return self._held

    @property
    def concealed(self):
        return self._concealed

    @property
    def melded(self):
        return self._melded

    @property
    def eye(self):
        return self._eye

    @property
    def last(self):
        return self._last

    @property
    def bonus(self):
        return self._bonus

    def as_dict(self):
        return { 'held'      : self._held
               , 'concealed' : self._concealed
               , 'melded'    : self._melded
               , 'eye'       : self._eye
               , 'last'      : self._last
               , 'bonus'     : self._bonus
               }

# initially, everything is in hand['held']
# as soon as melding takes place, the melded tiles are appended to hand['melded']
# flowers and season goes to hand['bonus']
# as for winning tile, the associated meld is treated as 'melded'
# so during gameplay, we first have to convert the hand dictionary to list
# to check for seven pairs or thirteen orphans before proceeding to evaluated as a normal hand
def figure_out_hand(hand):
    l = []
    if is_thirteen_orphans(hand):
        l.append(s.thirteen_orphans)
    if is_seven_pairs(hand):
        l.append(s.seven_pairs)
    return l

def get_score(hands):
    if len(hands) > 0:
        values = f.map_func(t.snd, hands)
        return f.fold_func(f.add_, 0, values)
    return 0


######################################
### Hand Conversions and Utilities ###
######################################

def sort_tiles(tiles):
    return f.sort_by(t.compare, tiles)

def get_str_rep(tiles):
    return f.map_func(t.show_tile, tiles)

def join_str_rep(tiles):
    return f.fold_func(f.add_, "", get_str_rep(tiles))

def to_list(hand):
    l = []
    if hand['concealed'] != None or hand['concealed'] != []:
        for m in hand['concealed']:
            for t in m:
                l.append(t)
    if hand['melded'] != None or hand['melded'] != []:
        for m in hand['melded']:
            for t in m:
                l.append(t)
    if hand['held'] != None or hand['held'] != []:
        for t in hand['held']:
            l.append(t)
    if hand['eye'] != None or hand['eye'] != []:
        for t in hand['eye']:
            l.append(t)
    else:
        if hand['last'] != None:
            l.append(hand['last'])
    return l

def to_dict(tiles):
    d = {}
    for tile in tiles:
        s = t.show_tile(tile)
        if d.has_key(s):
            d[s] += 1
        else:
            d[s] = 1
    return d

def get_melds(hand):
    return [c for c in hand['concealed']] + [m for m in hand['melded']]



#############
### Melds ###
#############

def is_chow(tiles):
    if len(tiles) == 3:
        meld = sort_tiles(tiles)
        suits = f.map_func(t.fst, meld)
        if suits[0] == suits[1] == suits[2]:
            values = f.map_func(t.snd, meld)
            if values[0] + 2 == values[1] + 1 == values[2]:
                return True
    return False

def is_pung(tiles):
    if len(tiles) == 3:
        if tiles[0] == tiles[1] == tiles[2]:
            return True
    return False

def is_kong(tiles):
    if len(tiles) == 4:
        if tiles[0] == tiles[1] == tiles[2] == tiles[3]:
            return True
    return False

def is_eye(tiles):
    if len(tiles) == 2:
        if tile.fst(tiles) == tile.snd(tiles):
            return True
    return False

def is_terminal_meld(tiles):
    if is_chow(tiles) or is_pung(tiles) or is_kong(tiles):
        meld = sort_tiles(tiles)
        if t.is_terminal(meld[0]) or t.is_terminal(meld[2]):
            return True
    return False

def is_honor_meld(tiles):
    if is_pung(tiles) or is_kong(tiles):
        if t.is_honor(tiles[0]):
            return True
    return False

def has_terminal(tiles):
    return f.or_func(f.map_func(t.is_term, tiles))

def has_honor(tiles):
    return f.or_func(f.map_func(t.is_honor, tiles))

def is_outside(tiles):
    return has_terminal(tiles) or has_honor(tiles)



########################
### Hand Evaluations ###
########################



### 1.0 Trivial Patterns

# chicken is only when your hand satisfies no other patterns aside form bonus tiles
def is_chicken(hand):
    pass


def is_all_chows(hand):
    melds = f.map_func(sort_tiles, f.filter(is_meld, get_melds(hand)) )
    if f.and_func( f.map_func(is_chow, melds) ):
        return True
    return False


def is_concealed_hand(hand):
    if hand['melded'] == []:
        return True
    return False


def is_self_drawn(hand):
    pass


def is_all_simples(hand):
    pass


def is_all_types(hand):
    pass


def is_illegal_call(hand):
    pass


### 2.0 Identical Sets

def is_two_identical_chows(hand):
    d = to_dict( f.map_func(join_str_rep, get_melds(hand)) )
    if len(d) == 3:
        if sorted(d.values()) == [1,1,2]:
            return True
    return False

def is_two_identical_chows_twice(hand):
    d = to_dict( f.map_func(join_str_rep, get_melds(hand)) )
    if len(d) == 2:
        if d.values() == [2,2]:
            return True
    return False

def is_three_identical_chows(hand):
    d = to_dict( f.map_func(join_str_rep, get_melds(hand)) )
    if len(d) == 2:
        if sorted(d.values()) == [1,3]:
            return True
    return False

def is_four_identical_chows(hand):
    d = to_dict( f.map_func(join_str_rep, get_melds(hand)) )
    if len(d) == 1:
        if d.values() == [4]:
            return True
    return False



### 3.0 Pungs and Kongs

def is_all_pungs(hand):
    if f.and_func( f.map_func(lambda x: is_pung(x) or is_kong(x), get_melds(hand)) ):
        return True
    return False


def is_two_concealed_pungs(hand):
    return _is_x_concealed_pungs(hand, 2)

def is_three_concealed_pungs(hand):
    return _is_x_concealed_pungs(hand, 3)

def is_four_concealed_pungs(hand):
    return _is_x_concealed_pungs(hand, 4)

def _is_x_concealed_pungs(hand, x):
    ps = f.filter_func(lambda x: is_pung(x) or is_kong(x), hand['concealed'])
    if len(ps) == x:
        return True
    return False


def is_one_kong(hand):
    return _is_x_kongs(hand, 1)

def is_two_kongs(hand):
    return _is_x_kongs(hand, 2)

def is_three_kongs(hand):
    return _is_x_kongs(hand, 3)

def is_four_kongs(hand):
    return _is_x_kongs(hand, 4)

def _is_x_kongs(hand, x):
    ks = f.filter_func(is_kong, get_melds(hand))
    if len(ks) == x:
        return True
    return False



### 4.0 Similar Sets

def is_three_similar_chows(hand):
    pass


def is_small_three_similar_pungs(hand):
    pass

def is_three_similar_pungs(hand):
    pass



### 5.0 Consecutive Sets

def is_nine_tile_straight(hand):
    pass

def is_three_consecutive_pungs(hand):
    pass

def is_four_consecutive_pungs(hand):
    pass

def is_three_mothers(hand):
    pass



### 6.0 Suit Patterns

def is_mixed_one_suit(hand):
    pass

def is_pure_one_suit(hand):
    pass


def is_small_dragon_club(hand):
    pass

def is_big_dragon_club(hand):
    pass


def is_nine_gates(hand):
    pass



### 7.0 Terminal Tiles

def is_two_tailed_terminal_chows(hand):
    pass

def is_two_tailed_terminal_pungs(hand):
    pass


def is_small_boundless_mountain(hand):
    pass

def is_big_boundless_mountain(hand):
    pass


def is_mixed_lesser_terminals(hand):
    pass


def is_pure_lesser_terminals(hand):
    pass

def is_mixed_greater_germinals(hand):
    pass

def is_pure_greater_terminals(hand):
    pass



### 8.0 Honor Tiles

def is_dragon_pung(hand):
    pass

def is_seat_wind(hand):
    pass


def is_small_three_dragons(hand):
    pass

def is_big_three_dragons(hand):
    pass


def is_small_three_winds(hand):
    pass

def is_big_three_winds(hand):
    pass

def is_small_four_winds(hand):
    pass

def is_big_four_winds(hand):
    melds = get_melds(hand)
    if f.and_func(f.map_func(is_honor_meld, melds)):
        if f.and_func(f.map_func(t.is_wind, f.flatten(melds))):
            return True
    return False


def is_all_honor_pungs(hand):
    if f.and_func(f.map_func(is_honor_meld, get_melds(hand))):
        return True
    return False 


def is_all_honor_pairs(hand):
    if _is_seven_unique_pairs(hand) > 0:
        ts = sort_tiles( [tile for tile in Set( hand['held'] + [hand['last']] )] )
        honors = [tile for tile in t.honor_tiles]
        if ts == honors:
            return 480
    return 0



### 9.0 Seven Pairs

def is_seven_pairs(hand):
    d = to_dict( hand['held'] + [hand['last']] )
    if len(d) == 7:
        if f.and_func( f.map_func(lambda x: x == 2, d.values()) ):
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

def _is_seven_unique_pairs(hand):
    d = to_dict( hand['held'] + [hand['last']] )
    if len(d) == 7:
        if f.and_func( f.map_func(lambda x: x == 2, d.values()) ):
            return 30
    return 0

def is_seven_shifted_pairs(hand):
    if _is_seven_unique_pairs(hand) > 0:
        ts = sort_tiles( [tile for tile in Set( hand['held'] + [hand['last']] )] )
        suit = t.fst(ts[0])
        if f.and_func( f.map_func(lambda x: t.fst(x) == suit, ts) ):
            values = f.map_func(t.snd, ts)
            if values == range(1,8) or values == range(3,10):
                return 320
    return 0

def is_grand_chariot(hand):
    return _is_seven_shifted_simple_pairs(hand, t.tile_types[0])

def is_bamboo_forest(hand):
    return _is_seven_shifted_simple_pairs(hand, t.tile_types[1])

def is_number_neighborhood(hand):
    return _is_seven_shifted_simple_pairs(hand, t.tile_types[2])

def _is_seven_shifted_simple_pairs(hand, suit):
    if _is_seven_unique_pairs(hand) > 0:
        ts = sort_tiles( [tile for tile in Set( hand['held'] + [hand['last']] )] )
        if f.and_func( f.map_func(lambda x: t.fst(x) == suit, ts) ):
            values = f.map_func(t.snd, ts)
            if values == range(2,9):
                return 400
    return 0



### 10.0 Color Hands

def is_all_green(hand):
    pass


def is_all_red(hand):
    pass


def is_all_blue(hand):
    pass



### 11.0 Irregular Hands

def is_thirteen_orphans(hand):
    # need to check all thirteen terminal tiles are in the hand
    # and that no other tiles exists in the hand
    # pigeonhole principle: 14 tiles fitting into 13 slots, one must be repeated
    h = Set( hand['held'] + [hand['last']] )
    s = Set(t.edge_tiles)
    if h.issubset(s) and s.issubset(h):
        return 160
    else:
        return 0



### 12.0 Incidental bonuses

def is_final_draw():
    return 10


def is_final_discard():
    return 10


def is_win_on_kong():
    return 10

def is_win_on_bonus():
    return 10

def is_robbing_kong():
    return 10


def is_blessing_of_heaven():
    return 155

def is_blessing_of_earth():
    return 155



### 13.0 Bonus Tiles

def is_non_seat_flower():
    return 2

def is_seat_flower():
    return 4



def is_four_flowers(hand):
    pass

def is_four_seasons(hand):
    pass


def is_all_flowers(hand):
    pass
