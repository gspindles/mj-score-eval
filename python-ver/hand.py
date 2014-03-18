#!/usr/bin/env python
# -*- coding: utf-8 -*-

import tile as t
import fp   as f

########################
### Class Definition ###
########################

h1 = [ ('C', 7), ('C', 8), ('C', 9)
     , ('W', 1), ('W', 1), ('W', 1)
     , ('K', 1), ('K', 2), ('K', 3)
     , ('B', 4), ('B', 5), ('B', 6)
     , ('D', 1), ('D', 1)
     ]


h2 = { 'concealed': [ [ ('B', 4), ('B', 5), ('B', 6) ]
                    , [ ('D', 1), ('D', 1) ]
                    ]
     , 'melded': [ [ ('C', 7), ('C', 8), ('C', 9) ]
                 , [ ('W', 1), ('W', 1), ('W', 1) ]
                 , [ ('K', 1), ('K', 2), ('K', 3) ]
                 ]
     , 'bonus': [ ('F', 1), ('S', 2) ]
     }

h_A_seq = { 'concealed': [ [ ('B', 4), ('B', 5), ('B', 6) ]
                         , [ ('D', 1), ('D', 1) ]
                         ]
          , 'melded': [ [ ('C', 7), ('C', 8), ('C', 9) ]
                      , [ ('B', 2), ('B', 3), ('B', 4) ]
                      , [ ('K', 1), ('K', 2), ('K', 3) ]
                      ]
          , 'bonus': [ ('F', 1), ('S', 2) ]
          }


# leave this for now, should just delete it later since we are not doing OO
class Hand:

    _concealed = None
    _melded    = None
    _bonus     = None

    def __init__(self, concealed, melded, bonus):
        if tiles is not None:
            self._concealed = concealed
            self._melded    = melded
            self._bonus     = bonus

    @property
    def concealed(self):
        return self._concealed

    @property
    def melded(self):
        return self._melded

    @property
    def bonus(self):
        return self._bonus

    def as_dict(self):
        return { 'concealed' : self._concealed
               , 'melded'    : self._melded
               , 'bonus'     : self._bonus
               }

# def sort_hand(hand):
#     return Hand( sorted(hand.tiles, cmp=tile.compare) )

def sort_tiles(tiles):
    return f.sort_by(t.compare, tiles)


#############
### Melds ###
#############


# only checks if its a 3 or 4 set to distinguish it from eyes
def is_meld(tiles):
    return 3 <= len(tiles) <= 4

def is_terminal_meld(tiles):
    pass

def is_honor_meld(tiles):
    pass

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

def has_terminal(tiles):
    return f.or_func( f.map_func(t.is_term, tiles) )

def has_honor(tiles):
    return f.or_func( f.map_func(t.is_honor, tiles) )

def is_outside(tiles):
    return has_terminal(tiles) or has_honor(tiles)


########################
### Hand evaluations ###
########################

# for now, assumes all the hand is a dictionary with concealed and melded
# with both keys containing list of tiles

def get_melds(hand):
    return [c for c in hand['concealed']] + [m for m in hand['melded']]


### 1.0 Trivial Patterns

# chicken is only when your hand satisfies no other patterns aside form bonus tiles
def is_chicken(hand):
    return 1


def is_all_sequences(hand):
    melds = f.map_func(sort_tiles, f.filter(is_meld, get_melds(hand)) )
    if f.and_func( f.map_func(is_chow, melds) ):
        return 5
    return 0


def is_concealed_hand(hand):
    pass


def is_self_drawn(hand):
    pass


def is_no_terminals(hand):
    pass


def is_all_types(hand):
    pass


def is_illegal_call(hand):
    return -30


### 2.0 Identical Sets

def is_two_identical_sequences(hand):
    pass

def is_two_identical_sequences_twice(hand):
    pass

def is_three_identical_sequences(hand):
    pass

def is_four_identical_sequences(hand):
    melds = f.map_func(sort_tiles, f.filter(is_meld, get_melds(hands)) )
    if melds[0] == melds[1] == melds[2] == melds[3]:
        return 480
    return 0


### 3.0 Triplets and Kong ###

def is_all_triplets(hand):
    pass

def is_two_concealed_triplets(hand):
    pass

def is_three_concealed_triplets(hand):
    pass

def is_four_concealed_triplets(hand):
    pass


def is_one_kong(hand):
    pass

def is_two_kong(hand):
    pass

def is_three_kong(hand):
    pass

def is_four_kong(hand):
    pass


### 4.0 Similar Sets

def is_three_similar_sequences(hand):
    pass

def is_small_three_similar_triplets(hand):
    pass

def is_three_similar_triplets(hand):
    pass


### 5.0 Consecutive Sets

def is_nine_tile_straight(hand):
    pass

def is_three_consecutive_triplets(hand):
    pass

def is_four_consecutive_triplets(hand):
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

def is_two_tailed_terminal_sequences(hand):
	pass

def is_two_tailed_terminal_triplets(hand):
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


def is_small_three_dragons(hand):
	pass

def is_big_three_dragons(hand):
	pass


def is_seat_wind(hand):
	pass

def is_small_three_winds(hand):
	pass

def is_big_three_winds(hand):
	pass

def is_small_four_winds(hand):
	pass

def is_big_four_winds(hand):
	pass


def is_all_honors(hand):
	pass

def is_seven_lucky_stars(hand):
	pass


### 9.0 Seven Pairs

def is_seven_pairs(hand):
	pass


def is_seven_shifted_pairs(hand):
	pass

def is_grant_chariot(hand):
	pass

def is_bamboo_forest(hand):
	pass

def is_numerous_neighbors(hand):
	pass


### 10.0 Color Hands

def is_all_green(hand):
	pass


def is_all_red(hand):
	pass


def is_all_blue(hand):
	pass


### 11.0 Irregular Hands

def is_thirteen_terminals(hand):
    pass


### 12.0 Incidental bonuses

def is_final_draw(hand):
	pass


def is_final_discard(hand):
	pass


def is_win_on_kong(hand):
	pass

def is_win_on_bonus(hand):
	pass

def is_robbing_kong(hand):
	pass


def is_blessing_of_heaven(hand):
	return 150

def is_blessing_of_earth(hand):
	return 150


### 13.0 Bonus Tiles

def is_non_seat_flower(hand):
	return 2

def is_seat_flower(hand):
	return 4



def is_four_flowers(hand):
	pass

def is_four_seasons(hand):
	pass


def is_all_flowers(hand):
	pass
