#!/usr/bin/env python
# -*- coding: utf-8 -*-

from random import shuffle
from fp     import repeat #, flatten, map_func, fold_func, _cons

########################
### Data Definitions ###
########################

tile_types = ['C', 'B', 'K', 'W', 'D', 'F', 'S', 'A']

coin_tiles = ( ('C', 1), ('C', 2), ('C', 3), ('C', 4), ('C', 5)
             , ('C', 6), ('C', 7), ('C', 8), ('C', 9)
             )

bamboo_tiles = ( ('B', 1), ('B', 2), ('B', 3), ('B', 4), ('B', 5)
               , ('B', 6), ('B', 7), ('B', 8), ('B', 9)
               )

character_tiles = ( ('K', 1), ('K', 2), ('K', 3), ('K', 4), ('K', 5)
                  , ('K', 6), ('K', 7), ('K', 8), ('K', 9)
                  )

wind_tiles = ( ('W', 1), ('W', 2), ('W', 3), ('W', 4) )

dragon_tiles = ( ('D', 1), ('D', 2), ('D', 3) )

flower_tiles = ( ('F', 1), ('F', 2), ('F', 3), ('F', 4) )

season_tiles = ( ('S', 1), ('S', 2), ('S', 3), ('S', 4) )

animal_tiles = ( ('A', 1), ('A', 2), ('A', 3), ('A', 4) )

regular_tiles = coin_tiles + bamboo_tiles + character_tiles + wind_tiles + dragon_tiles

bonus_tiles = flower_tiles + season_tiles


terminal_tiles = ( ('C', 1), ('C', 9), ('B', 1), ('B', 9), ('K', 1), ('K', 9) )

honor_tiles = wind_tiles + dragon_tiles

edge_tiles = terminal_tiles + honor_tiles

def get_wall():
    w = []
    for t in regular_tiles:
        w = w + repeat(t, 4)
    for t in bonus_tiles:
        w.append(t)
    # doing it with fp would look like:
    # w = flatten( map_func(lambda x: repeat(x,4), regular_tiles) )
    # w = fold_func(_cons, w, bonus_tiles)
    # but that would just be "unpythonic"
    shuffle(w)
    return w


###################
### Conversions ###
###################

def show_tile(tile):
    return fst(tile) + str( snd(tile) )

def read_tile(tile):
    if len(tile) == 2:
        return (tile[0], int( tile[1] ) )

rank = { 'C': 10
       , 'B': 20
       , 'K': 30
       , 'W': 40
       , 'D': 50
       , 'F': 60
       , 'S': 70
       , 'A': 80
       }

def get_rank(tile):
    return rank[ fst(tile) ] + snd(tile)

def compare(tile1, tile2):
    if get_rank(tile1) < get_rank(tile2):
        return -1
    elif get_rank(tile1) > get_rank(tile2):
        return 1
    return 0



###########################
### Queries about Tiles ###
###########################

def is_coin(tile):
    if fst(tile) == 'C':
        return True
    return False

def is_bamboo(tile):
    if fst(tile) == 'B':
        return True
    return False

def is_character(tile):
    if fst(tile) == 'K':
        return True
    return False

def is_suit(tile):
    return is_coin(tile) or is_bamboo(tile) or is_character(tile)

def is_simple(tile):
    if is_suit(tile):
        s = snd(tile)
        if s > 1 and s < 9:
            return True
    return False

def is_terminal(tile):
    if is_suit(tile):
        s = snd(tile)
        if s == 1 or s == 9:
            return True
    return False

def is_wind(tile):
    if fst(tile) == 'W':
        return True
    return False

def is_dragon(tile):
    if fst(tile) == 'D':
        return True
    return False

def is_honor(tile):
    return is_wind(tile) or is_dragon(tile)

def is_edge(tile):
    return is_terminal(tile) or is_honor(tile)

def is_flower(tile):
    if fst(tile) == 'F':
        return True
    return False

def is_season(tile):
    if fst(tile) == 'S':
        return True
    return False

def is_animal(tile):
    if fst(tile) == 'A':
        return True
    return False

# could probably check if tile is in bonus_tile too
# but that seems way more comparisons than just looking at the suit
def is_bonus(tile):
    return is_flower(tile) or is_season(tile) or is_animal(tile)

def is_green(tile):
    green = [('B', 2), ('B', 3), ('B', 4), ('B', 6), ('B', 8), ('D', 2)]
    return tile in green

def is_red(tile):
    red = [('B', 1), ('B', 5), ('B', 7), ('B', 9), ('D', 1)]
    return tile in red

def is_blue(tile):
    blue = [('C', 8), ('W', 1), ('W', 2), ('W', 3), ('W', 4), ('D', 3)]
    return tile in blue



###########################
### Utilities functions ###
###########################

def fst(tile):
    return tile[0]

def snd(tile):
    return tile[1]

def succ(tile):
    if is_suit(tile):
        if snd(tile) < 9:
            return (fst(tile), snd(tile) + 1)
    return None

def pred(tile):
    if is_suit(tile):
        if snd(tile) > 1:
            return (fst(tile), snd(tile) - 1)
    return None
