import random

########################
### Data Definitions ###
########################

regular_tiles = (
      ('C', 1), ('C', 2), ('C', 3), ('C', 4), ('C', 5)
    , ('C', 6), ('C', 7), ('C', 8), ('C', 9)
    , ('B', 1), ('B', 2), ('B', 3), ('B', 4), ('B', 5)
    , ('B', 6), ('B', 7), ('B', 8), ('B', 9)
    , ('K', 1), ('K', 2), ('K', 3), ('K', 4), ('K', 5)
    , ('K', 6), ('K', 7), ('K', 8), ('K', 9)
    , ('W', 1), ('W', 2), ('W', 3), ('W', 4)
    , ('D', 1), ('D', 2), ('D', 3)
    )

bonus_tiles = (
      ('F', 1), ('F', 2), ('F', 3), ('F', 4)
    , ('S', 1), ('S', 2), ('S', 3), ('S', 4)
    )

def get_wall():
    w = []
    for t in regular_tiles:
        w.append(t)
        w.append(t)
        w.append(t)
        w.append(t)
    for t in bonus_tiles:
        w.append(t)
    random.shuffle(w)
    return w



###################
### Conversions ###
###################

def to_string(tile):
    return fst(tile) + str( snd(tile) )

def read_tile(tile):
    if len(tile) == 2:
        return (tile[0], int( tile[1] ) )



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
    return isCoin(tile) or isBamboo(tile) or isCharacter(tile)

def is_simple(tile):
    if isSuit(tile):
        s = snd(tile)
        if s > 1 and s < 9:
            return True
    return False

def is_terminal(tile):
    if isSuit(tile):
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
    return isWind(tile) or isDragon(tile)

def is_Edge(tile):
    return isTerminal(tile) or isHonor(tile)

def is_flower(tile):
    if fst(tile) == 'F':
        return True
    return False

def is_season(tile):
    if fst(tile) == 'S':
        return True
    return False

# could probably check if tile is in bonus_tile too
# but that seems way more comparisons than just looking at the suit
def is_bonus(tile):
    return isFlower(tile) or isSeason(tile)

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

def map_func(func, list):
    l = []
    for i in list:
        l.append( func(i) )
    return l

def filter_func(func, list):
    l = []
    for i in list:
        if func(i):
            l.append(i)
    return l
