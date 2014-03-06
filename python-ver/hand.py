#!/usr/bin/env python
# -*- coding: utf-8 -*-

import tile
import util
import maybe

########################
### Class Definition ###
########################

class hand:
    
    _tiles = None

    def __init__(self, tiles):
        if tiles is not None:
            self._tiles = tiles

    @property
    def tiles(self):
        return self._tiles

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
