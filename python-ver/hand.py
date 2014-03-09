#!/usr/bin/env python
# -*- coding: utf-8 -*-

import tile
import fp
import maybe

########################
### Class Definition ###
########################

h = [ ('C', 7), ('C', 8), ('C', 9)
    , ('W', 1), ('W', 1), ('W', 1)
    , ('K', 1), ('K', 2), ('K', 3)
    , ('B', 4), ('B', 5), ('B', 6)
    , ('D', 1), ('D', 1)
    ]

class Hand:

    _tiles = None

    def __init__(self, tiles):
        if tiles is not None:
            self._tiles = tiles

    @property
    def tiles(self):
        return self._tiles

def sort_hand(hand):
    return Hand( sorted(hand.tiles, cmp=tile.compare) )

def is_chow(tiles):
    if len(tiles) == 3:
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

def is_meld(tiles):
    return is_chow(tiles) or is_pung(tiles) or is_kong(tiles)

def score(hand):
    pass

category = {
      'basic'
    , 'chow'
    }