#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-

"""score.py takes in a list of melds that is calcalated from hand.py.  From
this list, score.py will produce the list of patterns the hand satisfies. In a
sense, hend.py do all the grunt work while scope.py just match the patterns and
assign values.

"""

import hand as h
import tile as t
import fp as f
import chart as c


# Meld Related Functions
#
#     r is for revealed
#     n is for concealed
#     c is for chow
#     p is for pung
#     k is for kong
#     e is for eye
#     h is for special hands (mainly nine gates and 13 orphans)
#     l is for leftover (mainly for 8 bonus tiles)
#
#     C is for coin
#     B is for bamboo
#     K is for character
#     W is for wind
#     D is for dragon
#     S is for simple
#     T is for terminal
#     b is for bonus (flowers, seasons, north[3ma], animals)


# Basic Building Blocks
#
#     A set of predicates for querying about a meld or a pair of eyes.

def _is_coin(meld):
    """Predicate returning whether the set is coin."""

    return 'C' in t.fst(meld)


def _is_bamboo(meld):
    """Predicate returning whether the set is bamboo."""

    return 'B' in t.fst(meld)


def _is_character(meld):
    """Predicate returning whether the set is character."""

    return 'K' in t.fst(meld)


def _is_wind(meld):
    """Predicate returning whether the set is wind."""

    return 'W' in t.fst(meld)


def _is_dragon(meld):
    """Predicate returning whether the set is dragon."""

    return 'D' in t.fst(meld)


def _is_bonus(meld):
    """Predicate returning whether the set is bonus."""

    return 'b' in t.fst(meld)


def _is_simple(meld):
    """Predicate returning whether the set is simple."""

    return 'S' in t.fst(meld)


def _is_terminal(meld):
    """Predicate returning whether the set is terminal."""

    return 'T' in t.fst(meld)


def _is_revealed(meld):
    """Predicate returning whether the set is revealed."""

    return 'r' in t.fst(meld)


def _is_concealed(meld):
    """Predicate returning whether the set is concealed."""

    return 'n' in t.fst(meld)


def _is_chow(meld):
    """Predicate returning whether the set is a chow."""

    return 'c' in t.fst(meld)


def _is_pung(meld):
    """Predicate returning whether the set is a pung or a kong."""

    return 'p' in t.fst(meld) or 'k' in t.fst(meld)


def _is_kong(meld):
    """Predicate returning whether the set is a kong."""

    return 'k' in t.fst(meld)


def _is_eye(meld):
    """Predicate returning whether the set is a pair of eyes."""

    return 'e' in t.fst(meld)


# Compound Functions
#
#     Combine the basic functions above to create more complex predicates used
#     in the next section.

def _is_meld(meld):
    """Predicate returning whether the set is a meld."""

    return _is_chow(meld) or _is_pung(meld) or _is_kong(meld)


def _is_coin_chow(meld):
    """Predicate returning whether the set is a coin chow."""

    return _is_chow(meld) and _is_coin(meld)


def _is_bamboo_chow(meld):
    """Predicate returning whether the set is a bamboo chow."""

    return _is_chow(meld) and _is_bamboo(meld)


def _is_character_chow(meld):
    """Predicate returning whether the set is a character chow."""

    return _is_chow(meld) and _is_character(meld)


def _is_simple_chow(meld):
    """Predicate returning whether the set is a simple chow."""

    return _is_chow(meld) and _is_simple(meld)


def _is_terminal_chow(meld):
    """Predicate returning whether the set is a terminal chow."""

    return _is_chow(meld) and _is_terminal(meld)


def _is_suit_chow(meld):
    """Predicate returning whether the set is a suit chow."""

    return _is_chow(meld) and _is_suit_meld(meld)


def _is_coin_pung(meld):
    """Predicate returning whether the set is a coin pung."""

    return _is_pung(meld) and _is_coin(meld)


def _is_bamboo_pung(meld):
    """Predicate returning whether the set is a bamboo pung."""

    return _is_pung(meld) and _is_bamboo(meld)


def _is_character_pung(meld):
    """Predicate returning whether the set is a character pung."""

    return _is_pung(meld) and _is_character(meld)


def _is_wind_pung(meld):
    """Predicate returning whether the set is a wind pung."""

    return _is_pung(meld) and _is_wind(meld)


def _is_dragon_pung(meld):
    """Predicate returning whether the set is a dragon pung."""

    return _is_pung(meld) and _is_dragon(meld)


def _is_simple_pung(meld):
    """Predicate returning whether the set is a simple pung."""

    return _is_pung(meld) and _is_simple(meld)


def _is_terminal_pung(meld):
    """Predicate returning whether the set is a terminal pung."""

    return _is_pung(meld) and _is_terminal(meld)


def _is_suit_pung(meld):
    """Predicate returning whether the set is a suit pung."""

    return _is_pung(meld) and _is_suit_meld(meld)


def _is_honor_pung(meld):
    """Predicate returning whether the set is a honor pung."""

    return _is_pung(meld) and (_is_wind(meld) or _is_dragon(meld))


def _is_outileside_pung(meld):
    """Predicate returning whether the set is a outileside pung."""

    outileside = _is_terminal(meld) or _is_wind(meld) or _is_dragon(meld)
    return _is_pung(meld) and outileside


def _is_concealed_pung(meld):
    """Predicate returning whether the set is a concealed pung."""

    return _is_pung(meld) and _is_concealed(meld)


def _is_simple_meld(meld):
    """Predicate returning whether the set is a simple meld."""

    return _is_chow(meld) or _is_simple_pung(meld)


def _is_terminal_meld(meld):
    """Predicate returning whether the set is a terminal meld."""

    return _is_terminal_chow(meld) or _is_terminal_pung(meld)


def _is_suit_meld(meld):
    """Predicate returning whether the set is a suit meld."""

    return _is_coin(meld) or _is_bamboo(meld) or _is_character(meld)


def _is_outileside_meld(meld):
    """Predicate returning whether the set is a outileside meld."""

    return _is_terminal_meld(meld) or _is_honor_pung(meld)


def _is_simple_eye(meld):
    """Predicate returning whether the set is a pair of simple eyes."""

    return _is_eye(meld) and _is_simple(meld)


def _is_terminal_eye(meld):
    """Predicate returning whether the set is a pair of terminal eyes."""

    return _is_eye(meld) and _is_terminal(meld)


def _is_wind_eye(meld):
    """Predicate returning whether the set is a pair of wind eyes."""

    return _is_eye(meld) and _is_wind(meld)


def _is_dragon_eye(meld):
    """Predicate returning whether the set is a pair of dragon eyes."""

    return _is_eye(meld) and _is_dragon(meld)


def _is_honor_eye(meld):
    """Predicate returning whether the set is a pair of honor eyes."""

    return _is_eye(meld) and (_is_wind(meld) or _is_dragon(meld))


def _is_outileside_eye(meld):
    """Predicate returning whether the set is a pair of outileside eyes."""

    outileside = _is_terminal(meld) or _is_wind(meld) or _is_dragon(meld)
    return _is_eye(meld) and outileside


# Extraction Methods
#
#     Extract out melds and eyes and tiles out of a hand.

def _get_melds(hand):
    """Filter out the melds in a hand."""

    return f.filter_(_is_meld, hand)


def _get_eyes(hand):
    """Filter out the eyes in a hand."""

    return f.filter_(_is_eye, hand)


def _get_bonus(hand):
    """Filter out the bonus tiles in a hand."""

    return f.filter_(_is_bonus, hand)


def _get_tiles(hand):
    """Flatten the hand down to a list of tile."""

    melds = f.map_(t.snd, hand)
    return [tile for meld in melds for tile in meld]


# Utility Functions
#
#     A small set of conversions functions for a variety of uses.

def _meld_str_rep(meld):
    """Make a string representation based on the tiles in the meld."""

    str_reps = f.map_(t.show_tile, t.snd(meld))
    return f.fold_(f.add_, "", str_reps)


def _tile_num_rep(meld):
    """Make a string representation based on the values of the tiles in the
    meld.

    """

    meld_values = f.map_(t.snd, t.snd(meld))
    if 3 <= len(meld_values) <= 4:  # treat kong as pung
        return str(meld_values[0]) + str(meld_values[1]) + str(meld_values[2])
    if len(meld_values) == 2:  # for eye? Necessary?
        return str(meld_values[0]) + str(meld_values[1])


def _tile_num_inc(num_rep):
    """Given a string representation based of the values of the tiles in the
    pung, return the string representation of the pung of the next value.

    """

    return str(int(num_rep) + 111)


# Hand Evaluations
#
#     Fuction for evaluate each type of hand.  The combined one might not use
#     all of these.  There are patterns that can't be determined just by
#     looking at the hand as well that requires game play information, such as
#     anything in the incidental section.  These are likely to be implemented
#     in hand.py

# 1.0 Trivial Patterns

def _is_chicken():
    """Returns the respective tuple when the hand is chicken. Chicken is only
    when your hand satisfies no other patterns aside form bonus tiles.  No need
    to implement as it is the only option when all patterns fails.

    """

    return c.chicken


def _is_all_chows(hand):
    """Returns the respective tuple when the hand consistiles of all chows."""

    if f.all_map_(_is_chow, _get_melds(hand)):
        return c.all_chows
    return c.nothing


def _is_concealed_hand(hand):
    """Returns the respective tuple when the it is a concealed hand. This is
    implemented in hand.py.  Note that upon calling mahjong, unless the tile
    is self drawn, the last tile captured completes a revealed meld.

    """

    return c.concealed


def _is_self_drawn():
    """Return the respective tuple when the player self draws the winning
    tile.

    """

    return c.self_drawn


# works for both 4 meld and eye, or seven pairs
def _is_all_simples(hand):
    """Returns the respective tuple when all the tile in the hand are simple
    tile. Both traditional hand or seven pairs version are accepted.

    """

    if f.all_map_(_is_simple, _get_melds(hand) + _get_eyes(hand)):
        return c.all_simples
    return c.nothing


def _is_all_types(hand):
    """Returns the respective tuple when the hand contains all 5 types of
    tiles. Both traditional hand or seven pairs version are accepted.

    """

    tiles = _get_tiles(hand)
    has_coin = f.any_map_(t.is_coin, tiles)
    has_bamboo = f.any_map_(t.is_bamboo, tiles)
    has_character = f.any_map_(t.is_character, tiles)
    has_wind = f.any_map_(t.is_wind, tiles)
    has_dragon = f.any_map_(t.is_dragon, tiles)
    if has_coin and has_bamboo and has_character and has_wind and has_dragon:
        return c.all_type
    return c.nothing


def _is_illegal_call():
    """Returns the respective tuple when the mahjong call was illegal."""

    return c.illegal_call


# 2.0 Identical sets

def _is_two_identical_chows(hand):
    """Returns the respective tuple when the hand has two identical chows."""
    d = f.to_dict_with_(_meld_str_rep, _get_melds(hand))
    if len(d) == 3:
        if sorted(d.values()) == [1, 1, 2]:
            return c.two_identical_chows
    return c.nothing


def _is_two_identical_chows_twice(hand):
    """Returns the respective tuple when the hand has two identical chows
    twice.

    """

    d = f.to_dict_with_(_meld_str_rep, _get_melds(hand))
    if len(d) == 2:
        if d.values() == [2, 2]:
            return c.two_identical_chows_twice
    return c.nothing


def _is_three_identical_chows(hand):
    """Returns the respective tuple when the hand has three identical chows."""

    d = f.to_dict_with_(_meld_str_rep, _get_melds(hand))
    if len(d) == 2:
        if sorted(d.values()) == [1, 3]:
            return c.three_identical_chows
    return c.nothing


def _is_four_identical_chows(hand):
    """Returns the respective tuple when the hand has four identical chows."""

    d = f.to_dict_with_(_meld_str_rep, _get_melds(hand))
    if len(d) == 1:
        if d.values() == [4]:
            return c.four_identical_chows
    return c.nothing


# 3.0 Pungs and Kongs

def _is_all_pungs(hand):
    """Returns the respective tuple when the hand consists only of pungs."""

    if f.all_map_(_is_pung, _get_melds(hand)):
        return c.all_pungs
    return c.nothing


def _is_two_concealed_pungs(hand):
    """Returns the respective tuple when the hand has two concealed pungs."""
    if f.count_with_(_is_concealed_pung, _get_melds(hand)) == 2:
        return c.two_concealed_pungs
    return c.nothing


def _is_three_concealed_pungs(hand):
    """Returns the respective tuple when the hand has three concealed pungs."""

    if f.count_with_(_is_concealed_pung, _get_melds(hand)) == 3:
        return c.three_concealed_pungs
    return c.nothing


def _is_four_concealed_pungs(hand):
    """Returns the respective tuple when the hand has four concealed pungs."""

    if f.count_with_(_is_concealed_pung, _get_melds(hand)) == 4:
        return c.four_concealed_pungs
    return c.nothing


def _is_one_kong(hand):
    """Returns the respective tuple when the hand has a kong."""

    if f.count_with_(_is_kong, _get_melds(hand)) == 1:
        return c.one_kong
    return c.nothing


def _is_two_kongs(hand):
    """Returns the respective tuple when the hand has two kongs."""

    if f.count_with_(_is_kong, _get_melds(hand)) == 2:
        return c.two_kongs
    return c.nothing


def _is_three_kongs(hand):
    """Returns the respective tuple when the hand has three kongs."""

    if f.count_with_(_is_kong, _get_melds(hand)) == 3:
        return c.three_kongs
    return c.nothing


def _is_four_kongs(hand):
    """Returns the respective tuple when the hand has four kongs."""

    if f.count_with_(_is_kong, _get_melds(hand)) == 4:
        return c.four_kongs
    return c.nothing


# 4.0 Similar set

def _is_three_similar_chows(hand):
    """Returns the respective tuple when the hand has three similar chows of
    the same number across three suit.

    """

    ms = _get_melds(hand)
    has_coin = f.any_map_(_is_coin_chow, ms)
    has_bamboo = f.any_map_(_is_bamboo_chow, ms)
    has_character = f.any_map_(_is_character_chow, ms)
    if has_coin and has_bamboo and has_character:
        # chows of all three types are present, then one is repeated.  So pick
        # one that has one meld only and that should determine the sequence
        d = f.to_dict_with_(_tile_num_rep, ms)
        if len(d) == 1:
            return c.three_similar_chows
        if len(d) == 2:
            if sorted(d.values()) == [1, 3]:
                return c.three_similar_chows
    return c.nothing


def _is_little_three_similar_pungs(hand):
    """Returns the respective tuple when the hand satisfies little three
    similar pungs.

    """

    if len(_get_eyes(hand)) == 1:
        # no need to check for suit because we only have 4 of each tiles
        val = t.snd(t.fst(t.snd(_get_eyes(hand)[0])))
        val_pung_str = f.fold_(f.add_, '', f.repeat_(str(val), 3))
        d = f.to_dict_with_(_tile_num_rep, _get_melds(hand))
        if val_pung_str in d:
            if d[val_pung_str] == 2:
                return c.little_three_similar_pungs
    return c.nothing


def _is_three_similar_pungs(hand):
    """Returns the respective tuple when the hand has pungs of the same number
    across three suit.

    """

    ms = _get_melds(hand)
    has_coin = f.any_map_(_is_coin_pung, ms)
    has_bamboo = f.any_map_(_is_bamboo_pung, ms)
    has_character = f.any_map_(_is_character_pung, ms)
    if has_coin and has_bamboo and has_character:
        # chows of all three types are present, then one is repeated so pick
        # one that has one meld only and that should determine the sequence
        d = f.to_dict_with_(_tile_num_rep, ms)
        if len(d) == 1:
            return c.three_similar_pungs
        if len(d) == 2:
            if sorted(d.values()) == [1, 3]:
                return c.three_similar_pungs
    return c.nothing


# 5.0 Consecutive setiles

def _is_nine_tile_straight(hand):
    """Returns the respective tuple when the hands has nine tile straight."""

    ms = _get_melds(hand)
    coin_chows = f.filter_(_is_coin_chow, ms)
    bamboo_chows = f.filter_(_is_bamboo_chow, ms)
    character_chows = f.filter_(_is_character_chow, ms)

    def _has_straight(melds):
        d = f.to_dict_with_(_tile_num_rep, melds)
        lfunc = lambda x: f.in_(d, x)
        if f.all_map_(lfunc, ['123', '456', '789']):
            return c.nine_tile_straight

    if len(coin_chows) >= 3:
        return _has_straight(coin_chows)
    elif len(bamboo_chows) >= 3:
        return _has_straight(bamboo_chows)
    elif len(character_chows) >= 3:
        return _has_straight(character_chows)
    else:
        return c.nothing


def _is_three_consecutive_pungs(hand):
    """Returns the respective tuple when the hand three increasing pungs in the
    same suit.

    """

    ms = _get_melds(hand)
    coin_pungs = f.filter_(_is_coin_pung, ms)
    bamboo_pungs = f.filter_(_is_bamboo_pung, ms)
    character_pungs = f.filter_(_is_character_pung, ms)

    def _is_consecutive(melds):
        ps = sorted(f.map_(_tile_num_rep, melds))
        if len(melds) == 3:
            if ps == f.iterate_(_tile_num_inc, ps[0], 2):
                return c.three_consecutive_pungs
        if len(melds) == 4:
            lower = ps[0:3] == f.iterate_(_tile_num_inc, ps[0], 2)
            upper = ps[1:4] == f.iterate_(_tile_num_inc, ps[1], 2)
            if lower or upper:
                return c.three_consecutive_pungs
        return c.nothing

    if len(coin_pungs) >= 3:
        return _is_consecutive(coin_pungs)
    elif len(bamboo_pungs) >= 3:
        return _is_consecutive(bamboo_pungs)
    elif len(character_pungs) >= 3:
        return _is_consecutive(character_pungs)
    else:
        return c.nothing


def _is_four_consecutive_pungs(hand):
    """Returns the respective tuple when the hand four increasing pungs in the
    same suit.

    """

    ms = _get_melds(hand)
    tiles = f.concat_map_(t.snd, ms)
    s = set(f.map_(t.fst, tiles))
    if len(s) == 1:
        ps = sorted(f.map_(_tile_num_rep, ms))
        if ps[0:4] == f.iterate_(_tile_num_inc, ps[0], 3):
            return c.four_consecutive_pungs
    return c.nothing


def _is_three_mothers(hand):
    """Returns the respective tuple when the hand has three increasing pungs
    along with a chow of the 3 tiles in the same suit.

    """

    ms = _get_melds(hand)
    tiles = f.concat_map_(t.snd, ms)
    s = set(f.map_(t.fst, tiles))
    if len(s) == 1:
        ps = sorted(f.map_(_tile_num_rep, ms))
        test = f.iterate_(_tile_num_inc, ps[0], 2)
        test.append(''.join(f.map_(t.fst, test)))
        lfunc = lambda x: x in ps
        if f.all_map_(lfunc, test):
            return c.three_mothers
    return c.nothing


# 6.0 Suit Patterns

def _is_mixed_one_suit(hand):
    """Returns the respective tuple when the hand consists of tiles from a
    single suit along with honor tiles. Both traditional hand or seven pairs
    version are accepted.

    """

    melds = _get_melds(hand) + _get_eyes(hand)
    has_honor = f.count_with_(_is_honor_pung, melds)
    has_coin = f.count_with_(_is_coin, melds)
    has_bamboo = f.count_with_(_is_bamboo, melds)
    has_char = f.count_with_(_is_character, melds)
    in_coin = has_honor and has_coin and not has_bamboo and not has_char
    in_bamboo = has_honor and not has_coin and has_bamboo and not has_char
    in_char = has_honor and not has_coin and not has_bamboo and has_char
    if in_coin or in_bamboo or in_char:
        return c.mixed_one_suit
    return c.nothing


def _is_pure_one_suit(hand):
    """Returns the respective tuple when the tile of hand are all from the same
    suit. Both traditional hand or seven pairs version are accepted.

    """

    tiles = f.concat_map_(t.snd, _get_melds(hand) + _get_eyes(hand))
    s = set(f.map_(t.fst, tiles))
    if len(s) == 1:
        return c.pure_one_suit
    return c.nothing


def _is_little_terminal_club(hand):
    """Returns the respective tuple when the hand satisfies the pattern
    112233-55-778899 in the same suit.

    """

    tiles = f.concat_map_(t.snd, _get_melds(hand) + _get_eyes(hand))
    s = sorted(f.map_(t.snd, tiles))
    if s == [1, 1, 2, 2, 3, 3, 5, 5, 7, 7, 8, 8, 9, 9]:
        return c.little_terminal_club
    return c.nothing


def _is_big_terminal_club(hand):
    """Returns the respective tuple when the hand satisfies the pattern
    111123-55-789999 in the same suit.

    """

    tiles = f.concat_map_(t.snd, _get_melds(hand) + _get_eyes(hand))
    s = sorted(f.map_(t.snd, tiles))
    if s == [1, 1, 1, 1, 2, 3, 5, 5, 7, 8, 9, 9, 9, 9]:
        return c.big_terminal_club
    return c.nothing


def _is_nine_gates(hand):
    """Returns the respective tuple when the hand satisfies the pattern
    1112345678999 along with any tile of the same suit.

    """

    pass


# 7.0 Terminal Tiles

def _is_two_tailed_terminal_chows(hand):
    """Returns the respective tuple when the hand has chows of 123 and 789 in
    the same suit.

    """

    pass


def _is_two_tailed_terminal_pungs(hand):
    """Returns the respective tuple when the hand has chows of 111 and 999 of
    the same suit.

    """

    pass


def _is_little_boundless_mountain(hand):
    """Returns the respective tuple when the hand consists of only terminal
    melds and terminal eye in the same suit, using up six of the eight terminal
    tiles in the suit.

    """

    if _is_pure_one_suit(hand) and _is_pure_lesser_terminals(hand):
        tiles = f.concat_map_(t.snd, _get_melds(hand) + _get_eyes(hand))
        ttiles = f.filter_(t.is_terminal, tiles)
        if len(ttiles) == 6:
            return c.little_boundless_mountain
    return c.nothing


def _is_big_boundless_mountain(hand):
    """Returns the respective tuple when the hand consists of only terminal
    melds and terminal eye in the same suit, using all eight terminal tiles in
    the suit.

    """

    if _is_pure_one_suit(hand) and _is_pure_lesser_terminals(hand):
        tiles = f.concat_map_(t.snd, _get_melds(hand) + _get_eyes(hand))
        ttiles = f.filter_(t.is_terminal, tiles)
        if len(ttiles) == 8:
            return c.big_boundless_mountain
    return c.nothing


def _is_mixed_lesser_terminals(hand):
    """Returns the respective tuple when the hand consists of combinations of
    terminal chows and pungs of any suit, honor pungs along with terminal or
    honor eyes.

    """

    if f.all_map_(_is_outileside_meld, _get_melds(hand)):
        if f.all_map_(_is_outileside_eye, _get_eyes(hand)):
            return c.mixed_lesser_terminals
    return c.nothing


def _is_pure_lesser_terminals(hand):
    """Returns the respective tuple when the hand consists of combinations of
    terminal chows and pungs of any suit, with terminal or honor eyes.

    """

    if f.all_map_(_is_terminal_meld, _get_melds(hand)):
        if f.all_map_(_is_outileside_eye, _get_eyes(hand)):
            return c.pure_lesser_terminals
    return c.nothing


def _is_mixed_greater_terminals(hand):
    """Returns the respective tuple when the hand consists of terminal and
    honor tiles only.  Both traditional hand or seven pairs version are
    accepted.

    """

    if f.all_map_(_is_outileside_pung, _get_melds(hand)):
        if f.all_map_(_is_outileside_eye, _get_eyes(hand)):
            return c.mixed_greater_terminals
    return c.nothing


def _is_pure_greater_terminals(hand):
    """Returns the respective tuple when the hand consists of terminal tiles
    only.

    """

    if f.all_map_(_is_terminal_pung, _get_melds(hand)):
        if f.all_map_(_is_outileside_eye, _get_eyes(hand)):
            return c.pure_greater_terminals
    return c.nothing


# 8.0 Honor Tiles

def _is_dragon_pung_hand(hand):
    """Returns the respective tuple for each dragon pung."""

    dragon_pungs = f.filter_(_is_dragon_pung, _get_melds(hand))
    if len(dragon_pungs) > 0:
        value = c.dragon_pung[2] * len(dragon_pungs)
        return (c.dragon_pung[0], c.dragon_pung[1], value)
    return c.nothing


def _is_seat_wind(hand, seat):
    """Returns the respective tuple when the hand contains a pung of the
    individual seat wind.

    """

    wind_pungs = f.filter_(_is_wind_pung, _get_melds(hand))
    wtiles = f.concat_map_(t.snd, wind_pungs)
    if t.wind_tiles[seat] in wtiles:
        return c.seat_wind
    return c.nothing


def _is_little_three_winds(hand):
    """Returns the respective tuple when the hand contains two wind pungs
    and a third pair of wind as eyes.

    """

    wind_pungs = f.filter_(_is_wind_pung, _get_melds(hand))
    if len(wind_pungs) == 2:
        if _is_wind_eye(_get_eyes(hand)[0]):
            return c.little_three_winds
    return c.nothing


def _is_big_three_winds(hand):
    """Returns the respective tuple when the hand contains three wind pungs."""

    wind_pungs = f.filter_(_is_wind_pung, _get_melds(hand))
    if len(wind_pungs) == 3:
        if not _is_wind_eye(_get_eyes(hand)[0]):
            return c.big_three_winds
    return c.nothing


def _is_little_four_winds(hand):
    """Returns the respective tuple when the hand contains three wind pungs and
    a pair of the remaining wind as eyes.

    """

    wind_pungs = f.filter_(_is_wind_pung, _get_melds(hand))
    if len(wind_pungs) == 3:
        if _is_wind_eye(_get_eyes(hand)[0]):
            return c.little_four_winds
    return c.nothing


def _is_big_four_winds(hand):
    """Returns the respective tuple when the hand contain four wind pungs."""

    if f.all_map_(_is_wind_pung, _get_melds(hand)):
        return c.big_four_winds
    return c.nothing


def _is_little_three_dragons(hand):
    """Returns the respective tuple when the hand contains two dragon pungs and
    a pair of the remaining dragon as eyes.

    """

    dragon_pungs = f.filter_(_is_dragon_pung, _get_melds(hand))
    if len(dragon_pungs) == 2:
        if _is_dragon_eye(_get_eyes(hand)[0]):
            return c.little_three_dragons
    return c.nothing


def _is_big_three_dragons(hand):
    """Returns the respective tuple when the hand contains three dragon
    pungs.

    """

    dragon_pungs = f.filter_(_is_dragon_pung, _get_melds(hand))
    if len(dragon_pungs) == 3:
        return c.big_three_dragons
    return c.nothing


def _is_all_honors(hand):
    """Returns the respective tuple when the hand consists of teminal tiles
    only.  Both traditional hand or seven pairs version are accepted except for
    a specialized version below.

    """

    if f.all_map_(_is_honor_pung, _get_melds(hand)):
        if f.all_map_(_is_honor_eye, _get_eyes(hand)):
            return c.all_honors
    return c.nothing


def _is_all_honor_pairs(hand):
    """Returns the respective tuple when the hand consists of pairs of all four
    wind and all three dragons.

    """

    tiles = set(f.concat_map_(t.snd, _get_eyes(hand)))
    if len(tiles) == 7:
        tiles = h.sort_tiles([tile for tile in tiles])
        honors = [tile for tile in t.honor_tiles]
        if tiles == honors:
            return c.all_honor_pairs
    return c.nothing


# 9.0 Seven Pairs

def _is_seven_pairs(hand):
    """Returns the respective tuple when the hand consists of seven pairs of
    eyes.  Seven pairs can be combined with all simples, all types, mixed one
    suit, pure one suit, and mixed greater terminals.

    """

    d = f.to_dict_with_(t.show_tile, f.concat_map_(t.snd, _get_eyes(hand)))
    if len(d) == 7:
        if d.values() == f.repeat_(2, 7):
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
    """Returns the respective tuple when the hand consists of seven shifted
    pairs of the same suit with range of one to seven or three to nine.

    """

    if _is_pure_one_suit(hand):
        tiles = set(f.concat_map_(t.snd, _get_eyes(hand)))
        if len(tiles) == 7:
            vs = sorted(f.map_(t.snd, [tile for tile in tiles]))
            if vs == range(1, 8) or vs == range(3, 10):
                return c.seven_shifted_pairs
    return c.nothing


def _is_grand_chariot(hand):
    """Returns the respective tuple when the hand consists of seven shifted
    pairs of the coin suit with range of two to eight.

    """

    tiles = set(f.concat_map_(t.snd, _get_eyes(hand)))
    if len(tiles) == 7:
        ttiles = f.map_(t.fst, [tile for tile in tiles])
        vs = sorted(f.map_(t.snd, [tile for tile in tiles]))
        lfunc = lambda x: x == t.tile_types[0]
        if f.all_map_(lfunc, ttiles):
            if vs == range(2, 9):
                return c.grand_chariot
    return c.nothing


def _is_bamboo_forest(hand):
    """Returns the respective tuple when the hand consists of seven shifted
    pairs of the bamboo suit with range of two to eight.

    """

    tiles = set(f.concat_map_(t.snd, _get_eyes(hand)))
    if len(tiles) == 7:
        ttiles = f.map_(t.fst, [tile for tile in tiles])
        vs = sorted(f.map_(t.snd, [tile for tile in tiles]))
        lfunc = lambda x: x == t.tile_types[1]
        if f.all_map_(lfunc, ttiles):
            if vs == range(2, 9):
                return c.bamboo_forest
    return c.nothing


def _is_number_neighborhood(hand):
    """Returns the respective tuple when the hand consists of seven shifted
    pairs of the character suit with range of two to eight.

    """
    tiles = set(f.concat_map_(t.snd, _get_eyes(hand)))
    if len(tiles) == 7:
        ttiles = f.map_(t.fst, [tile for tile in tiles])
        vs = sorted(f.map_(t.snd, [tile for tile in tiles]))
        lfunc = lambda x: x == t.tile_types[2]
        if f.all_map_(lfunc, ttiles):
            if vs == range(2, 9):
                return c.number_neighborhood
    return c.nothing


# 10.0 Color Hands


def _is_all_green(hand):
    """Returns the respective tuple when the hand consists of all green tiles.

    """

    tiles = f.concat_map_(t.snd, _get_melds(hand) + _get_eyes(hand))
    if f.all_map_(t.is_green, tiles):
        return c.all_green
    return c.nothing


def _is_all_red(hand):
    """Returns the respective tuple when the hand consists of all red tiles."""

    tiles = f.concat_map_(t.snd, _get_melds(hand) + _get_eyes(hand))
    if f.all_map_(t.is_red, tiles):
        return c.all_red
    return c.nothing


def _is_all_blue(hand):
    """Returns the respective tuple when the hand consists of all blue tiles.

    """

    tiles = f.concat_map_(t.snd, _get_melds(hand) + _get_eyes(hand))
    if f.all_map_(t.is_blue, tiles):
        return c.all_blue
    return c.nothing


# 11.0 Irregular Hands

def _is_thirteen_orphans(hand):
    """Returns the respective tuple when the hand consists of a teminals of
    each suit, one of each wind, and one of each dragon, with an addition of
    any of the edge tiles as eyes.

    """

    # need to check all thirteen terminal tiles are in the hand and that no
    # other tiles existiles in the hand. Apply pigeonhole principle to see that
    # since we have fourteen tiles fitting into thirdteen slot, then one must
    # be repeated
    h = set(_get_tiles(hand))
    s = set(t.edge_tiles)
    if h.issubset(s) and s.issubset(h):
        return c.thirteen_orphans
    return c.nothing
    # save this for now, change it later


# 12.0 Incidental bonuses

def _is_final_draw():
    """Returns the respective tuple when the player draws the seabed tile and
    go out.

    """

    return c.final_draw


def _is_final_discard():
    """Returns the respective tuple when the player claims the discarded seabed
    tile and go out.

    """

    return c.final_discard


def _is_win_on_kong():
    """Returns the respective tuple when the player go out upon drawing the
    replacement tile after melding a kong.

    """

    return c.win_on_kong


def _is_win_on_bonus_tile():
    """Returns the respective tuple when the player go out upon drawing the
    replacement tile after drawing a bonus tile.

    """

    return c.win_on_bonus_tile


def _is_robbing_a_kong():
    """Returns the respective tuple when the player robs another player's kong
    to go out.  This only occurs when the other player promotes an already
    melded pung to a kong, and this is precisely the tile the player is waiting
    for to go out. So the player then robs the last tile from the other player
    to call mahjong.

    """

    return c.robbing_a_kong


def _is_blessing_of_heaven():
    """Returns the respective tuple when the dealer go out with intial dealt
    hand.  The hand only counts as blessing of heaven if no kong has been made.

    """

    return c.blessing_of_heaven


def _is_blessing_of_earth():
    """Returns the respective tuple when nondealer player go out with the very
    first discard from the dealer prior to any meld, revealed or concealed,
    has been made.

    """

    return c.blessing_of_earth


# 13.0 Bonus Tiles

def _is_non_seat_flower(hand, seat):
    """Returns the respective tuple for each non-seat flower."""

    if f.any_map_(_is_bonus, hand):
        flowers = f.filter_(t.is_flower, t.snd(_get_bonus(hand)[0]))
        nonseat_flowers = [flower for flower in flowers
                           if flower != t.flower_tiles[seat]]
        if len(nonseat_flowers) > 0:
            value = c.non_seat_flower[2] * len(nonseat_flowers)
            return (c.non_seat_flower[0], c.non_seat_flower[1], )
    return c.nothing


def _is_non_seat_season(hand, seat):
    """Returns the respective tuple for each non-seat season."""

    if f.any_map_(_is_bonus, hand):
        seasons = f.filter_(t.is_season, t.snd(_get_bonus(hand)[0]))
        nonseat_seasons = [season for season in seasons
                           if season != t.season_tiles[seat]]
        if len(nonseat_seasons) > 0:
            value = c.non_seat_season[2] * len(nonseat_seasons)
            return (c.non_seat_season[0], c.non_seat_season[1], value)
    return c.nothing


def _is_seat_flower(hand, seat):
    """Returns the respective tuple for seat flower."""

    if f.any_map_(_is_bonus, hand):
        flowers = f.filter_(t.is_flower, t.snd(_get_bonus(hand)[0]))
        if t.flower_tiles[seat] in flowers:
            return c.seat_flower
    return c.nothing


def _is_seat_season(hand, seat):
    """Returns the respective tuple for seat season."""

    if f.any_map_(_is_bonus, hand):
        seasons = f.filter_(t.is_season, t.snd(_get_bonus(hand)[0]))
        if t.season_tiles[seat] in seasons:
            return c.seat_season
    return c.nothing


def _is_four_flowers(hand):
    """Returns the respective tuple for obtaining all four flowers."""

    if f.any_map_(_is_bonus, hand):
        flowers = f.filter_(t.is_flower, t.snd(_get_bonus(hand)[0]))
        if len(flowers) == 4:
            return c.four_flowers
    return c.nothing


def _is_four_seasons(hand):
    """Returns the respective tuple for obtaining all four seasons."""

    if f.any_map_(_is_bonus, hand):
        seasons = f.filter_(t.is_season, t.snd(_get_bonus(hand)[0]))
        if len(seasons) == 4:
            return c.four_seasons
    return c.nothing


def _is_all_bonus_tiles(hand):
    """Returns the respective tuple for obtaining all eight bonus tiles."""
    if f.any_map_(_is_bonus, hand):
        bonus_tiles = t.snd(_get_bonus(hand)[0])
        if len(bonus_tiles) == 8:
            return c.all_bonus_tiles
    return c.nothing
