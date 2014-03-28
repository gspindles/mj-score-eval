#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-

### hand.py is responsible for conversion from a hand dictionary to a valid
### hand list.  The player's hand is represented as a dictionary with held
### tiles, concealed melds, visible melds, bonus tiles, last tile etc.  From
### this, we will produce a list melds that will be handed to score.py for
### scoring the points the hand is worth.

import tile as t
import fp   as f
from   sets import Set



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
# s is for simple
# t is for terminal
# d is for dragon
# w is for wind

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
