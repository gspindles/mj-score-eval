#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-

### hand.py is responsible for conversion from a hand dictionary to a valid
### hand list.  The player's hand is represented as a dictionary with held
### tiles, concealed melds, visible melds, bonus tiles, last tile etc.  From
### this, we will produce a list melds that will be handed to score.py for
### scoring the points the hand is worth.

import fp    as f
import tile  as t
import chart as c
from   sets  import Set



############
### Hand ###
############

# initially, everything is in hand['held']
# as soon as melding takes place, the melded tiles are appended to hand['melded']
# flowers and season goes to hand['bonus']
# as for winning tile, the associated meld is treated as 'melded'
# so during gameplay, we first have to convert the hand dictionary to list
# to check for seven pairs or thirteen orphans before proceeding to evaluated as a normal hand


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
# D is for dragon
# W is for wind
# S is for simple
# T is for terminal
# b is for bonus (flowers, seasons, north[3ma], animals)


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
    return f.sort_with(t.compare, tiles)

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

def get_melds(hand):
    return [c for c in hand['concealed']] + [m for m in hand['melded']]



#############
### Melds ###
#############

def _is_chow(tiles):
    if len(tiles) == 3:
        meld = sort_tiles(tiles)
        suits = f.map_func(t.fst, meld)
        if suits[0] == suits[1] == suits[2]:
            values = f.map_func(t.snd, meld)
            if values[0] + 2 == values[1] + 1 == values[2]:
                return True
    return False

def _is_pung(tiles):
    if len(tiles) == 3:
        if tiles[0] == tiles[1] == tiles[2]:
            return True
    return False

def _is_kong(tiles):
    if len(tiles) == 4:
        if tiles[0] == tiles[1] == tiles[2] == tiles[3]:
            return True
    return False

def _is_eye(tiles):
    if len(tiles) == 2:
        if tile.fst(tiles) == tile.snd(tiles):
            return True
    return False

def _is_terminal_meld(tiles):
    if _is_chow(tiles) or _is_pung(tiles) or _is_kong(tiles):
        meld = sort_tiles(tiles)
        if t.is_terminal(meld[0]) or t.is_terminal(meld[2]):
            return True
    return False

def _is_honor_meld(tiles):
    if _is_pung(tiles) or _is_kong(tiles):
        if t.is_honor(tiles[0]):
            return True
    return False

def _has_terminal(tiles):
    return f.or_func(f.map_func(t.is_term, tiles))

def _has_honor(tiles):
    return f.or_func(f.map_func(t.is_honor, tiles))

def _is_outside(tiles):
    return _has_terminal(tiles) or _has_honor(tiles)
