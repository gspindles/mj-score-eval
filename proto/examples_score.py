#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-

"""tests.py for now contains a list of exmple hands to be converted and
evaluated.

"""


# 1.0 Trivial Patterns

h_chicken = \
    [(['r', 'c', 'C', 'T'], [('C', 7), ('C', 8), ('C', 9)]),
     (['r', 'p', 'W'], [('W', 1), ('W', 1), ('W', 1)]),
     (['r', 'c', 'K', 'S'], [('K', 1), ('K', 2), ('K', 3)]),
     (['r', 'c', 'B', 'S'], [('B', 4), ('B', 5), ('B', 6)]),
     (['c', 'e', 'W'], [('W', 3), ('W', 3)]),
     (['b'], [('F', 1), ('S', 2)])
     ]

h_all_chows = \
    [(['n', 'c', 'B', 'S'], [('B', 4), ('B', 5), ('B', 6)]),
     (['r', 'c', 'C', 'T'], [('C', 7), ('C', 8), ('C', 9)]),
     (['r', 'c', 'B', 'S'], [('B', 2), ('B', 3), ('B', 4)]),
     (['r', 'c', 'K', 'T'], [('K', 1), ('K', 2), ('K', 3)]),
     (['n', 'e', 'D'], [('D', 1), ('D', 1)]),
     (['b'], [('F', 1), ('S', 2)])
     ]

h_concealed = \
    [(['n', 'c', 'C', 'T'], [('C', 7), ('C', 8), ('C', 9)]),
     (['n', 'p', 'W'], [('W', 1), ('W', 1), ('W', 1)]),
     (['n', 'c', 'K', 'S'], [('K', 1), ('K', 2), ('K', 3)]),
     (['n', 'c', 'B', 'S'], [('B', 4), ('B', 5), ('B', 6)]),
     (['c', 'e', 'W'], [('W', 3), ('W', 3)]),
     (['b'], [('F', 1), ('S', 2)])
     ]

h_self_drawn = []

h_all_simples_1 = \
    [(['n', 'c', 'B', 'S'], [('B', 4), ('B', 5), ('B', 6)]),
     (['r', 'c', 'C', 'S'], [('C', 6), ('C', 7), ('C', 8)]),
     (['r', 'p', 'B', 'S'], [('B', 3), ('B', 3), ('B', 3)]),
     (['r', 'p', 'K', 'S'], [('K', 2), ('K', 2), ('K', 2)]),
     (['r', 'e', 'B', 'S'], [('B', 2), ('B', 2)]),
     (['b'], [('F', 1), ('S', 2)])
     ]

h_all_simples_2 = \
    [(['n', 'e', 'C', 'S'], [('C', 2), ('C', 2)]),
     (['n', 'e', 'C', 'S'], [('C', 5), ('C', 5)]),
     (['n', 'e', 'C', 'S'], [('C', 8), ('C', 8)]),
     (['n', 'e', 'B', 'S'], [('B', 3), ('B', 3)]),
     (['n', 'e', 'B', 'S'], [('B', 6), ('B', 6)]),
     (['n', 'e', 'B', 'S'], [('B', 8), ('B', 8)]),
     (['r', 'e', 'K', 'S'], [('K', 8), ('K', 8)])
     ]

h_all_types = \
    [(['r', 'c', 'C', 'T'], [('C', 7), ('C', 8), ('C', 9)]),
     (['r', 'p', 'W'], [('W', 1), ('W', 1), ('W', 1)]),
     (['r', 'c', 'K', 'T'], [('K', 1), ('K', 2), ('K', 3)]),
     (['r', 'c', 'B', 'S'], [('B', 4), ('B', 5), ('B', 6)]),
     (['r', 'e', 'D'], [('D', 3), ('D', 3)]),
     (['b'], [('F', 1)])
     ]

h_illegal_call = []


# 2.0 Identical Chows

h_2_id_chows = \
    [(['n', 'c', 'B', 'S'], [('B', 4), ('B', 5), ('B', 6)]),
     (['r', 'c', 'C', 'S'], [('C', 6), ('C', 7), ('C', 8)]),
     (['r', 'p', 'W'], [('W', 1), ('W', 1), ('W', 1)]),
     (['r', 'c', 'C', 'S'], [('C', 6), ('C', 7), ('C', 8)]),
     (['r', 'e', 'D'], [('D', 3), ('D', 3)])
     ]

h_2_id_chows_2x = \
    [(['n', 'c', 'B', 'S'], [('B', 4), ('B', 5), ('B', 6)]),
     (['r', 'c', 'C', 'S'], [('C', 6), ('C', 7), ('C', 8)]),
     (['r', 'p', 'W'], [('B', 4), ('B', 5), ('B', 6)]),
     (['r', 'c', 'C', 'S'], [('C', 6), ('C', 7), ('C', 8)]),
     (['r', 'e', 'D'], [('D', 3), ('D', 3)])
     ]

h_3_id_chows = \
    [(['n', 'c', 'C', 'S'], [('C', 6), ('C', 7), ('C', 8)]),
     (['r', 'c', 'C', 'S'], [('C', 6), ('C', 7), ('C', 8)]),
     (['r', 'p', 'B', 'W'], [('B', 4), ('B', 5), ('B', 6)]),
     (['r', 'c', 'C', 'S'], [('C', 6), ('C', 7), ('C', 8)]),
     (['r', 'e', 'D'], [('D', 3), ('D', 3)])
     ]

h_4_id_chows = \
    [(['n', 'c', 'C', 'S'], [('C', 6), ('C', 7), ('C', 8)]),
     (['r', 'c', 'C', 'S'], [('C', 6), ('C', 7), ('C', 8)]),
     (['r', 'p', 'C', 'W'], [('C', 6), ('C', 7), ('C', 8)]),
     (['r', 'c', 'C', 'S'], [('C', 6), ('C', 7), ('C', 8)]),
     (['r', 'e', 'D'], [('D', 3), ('D', 3)])
     ]


# 3.0 Pungs and Kongs

h_all_pungs = \
    [(['n', 'k', 'B', 'S'], [('B', 4), ('B', 4), ('B', 4), ('B', 4)]),
     (['r', 'p', 'C', 'S'], [('C', 7), ('C', 7), ('C', 7)]),
     (['r', 'p', 'B', 'S'], [('B', 2), ('B', 2), ('B', 2)]),
     (['r', 'p', '­K', 'S'], [('K', 3), ('K', 3), ('K', 3)]),
     (['n', 'e', 'D'], [('D', 1), ('D', 1)])
     ]


h_2_concealed_pungs = \
    [(['n', 'k', 'B', 'S'], [('B', 4), ('B', 4), ('B', 4), ('B', 4)]),
     (['n', 'p', 'B', 'S'], [('B', 2), ('B', 2), ('B', 2)]),
     (['r', 'p', 'C', 'S'], [('C', 7), ('C', 7), ('C', 7)]),
     (['r', 'k', 'K', 'S'], [('K', 3), ('K', 3), ('K', 3), ('K', 3)]),
     (['n', 'e', 'D'], [('D', 1), ('D', 1)])
     ]

h_3_concealed_pungs = \
    [(['n', 'k', 'B', 'S'], [('B', 4), ('B', 4), ('B', 4), ('B', 4)]),
     (['n', 'p', 'B', 'S'], [('B', 2), ('B', 2), ('B', 2)]),
     (['n', 'k', 'K', 'S'], [('K', 3), ('K', 3), ('K', 3), ('K', 3)]),
     (['r', 'p', 'C', 'S'], [('C', 7), ('C', 7), ('C', 7)]),
     (['r', 'e', 'D'], [('D', 1), ('D', 1)])
     ]

h_4_concealed_pungs = \
    [(['n', 'k', 'B', 'S'], [('B', 4), ('B', 4), ('B', 4), ('B', 4)]),
     (['n', 'p', 'B', 'S'], [('B', 2), ('B', 2), ('B', 2)]),
     (['n', 'k', 'K', 'S'], [('K', 3), ('K', 3), ('K', 3), ('K', 3)]),
     (['n', 'p', 'C', 'S'], [('C', 7), ('C', 7), ('C', 7)]),
     (['r', 'e', 'D'], [('D', 1), ('D', 1)]),
     (['b'], [('F', 1), ('S', 2)])
     ]


h_1_kong = \
    [(['n', 'k', 'B', 'S'], [('B', 4), ('B', 4), ('B', 4), ('B', 4)]),
     (['r', 'c', 'C', 'T'], [('C', 7), ('C', 8), ('C', 9)]),
     (['r', 'c', 'B', 'S'], [('B', 2), ('B', 3), ('B', 4)]),
     (['r', 'c', 'K', 'T'], [('K', 1), ('K', 2), ('K', 3)]),
     (['r', 'e', 'D'], [('D', 1), ('D', 1)]),
     (['b'], [('F', 1), ('S', 2)])
     ]

h_2_kongs = \
    [(['n', 'k', 'B', 'S'], [('B', 4), ('B', 4), ('B', 4), ('B', 4)]),
     (['r', 'k', 'C', 'T'], [('C', 7), ('C', 7), ('C', 7), ('C', 7)]),
     (['r', 'c', 'B', 'S'], [('B', 2), ('B', 3), ('B', 4)]),
     (['r', 'c', 'K', 'S'], [('K', 3), ('K', 4), ('K', 5)]),
     (['r', 'e', 'D'], [('D', 1), ('D', 1)]),
     (['b'], [('F', 1), ('S', 2)])
     ]

h_3_kongs = \
    [(['n', 'k', 'C', 'S'], [('B', 4), ('B', 4), ('B', 4), ('B', 4)]),
     (['r', 'k', 'C', 'S'], [('C', 7), ('C', 7), ('C', 7), ('C', 7)]),
     (['r', 'k', 'B', 'S'], [('B', 2), ('B', 2), ('B', 2), ('B', 2)]),
     (['r', 'c', 'K', 'S'], [('K', 3), ('K', 4), ('K', 5)]),
     (['r', 'e', 'D'], [('D', 1), ('D', 1)]),
     (['b'], [('F', 1), ('S', 2)])
     ]

h_4_kongs = \
    [(['n', 'k', 'B', 'S'], [('B', 4), ('B', 4), ('B', 4), ('B', 4)]),
     (['r', 'k', 'C', 'S'], [('C', 7), ('C', 7), ('C', 7), ('C', 7)]),
     (['r', 'k', 'B', 'S'], [('B', 2), ('B', 2), ('B', 2), ('B', 2)]),
     (['r', 'k', 'K', 'S'], [('K', 3), ('K', 3), ('K', 3), ('K', 3)]),
     (['r', 'e', 'D'], [('D', 1), ('D', 1)]),
     (['b'], [('F', 1), ('S', 2)])
     ]


# 4.0 Similar Sets

h_3_similar_chows = \
    [(['c', 'c', 'B', 'S'], [('B', 4), ('B', 5), ('B', 6)]),
     (['r', 'c', 'C', 'S'], [('C', 4), ('C', 5), ('C', 6)]),
     (['r', 'p', 'B', 'S'], [('B', 2), ('B', 2), ('B', 2)]),
     (['r', 'c', 'K', 'S'], [('K', 4), ('K', 5), ('K', 6)]),
     (['r', 'e', 'D'], [('D', 1), ('D', 1)])
     ]


h_little_3_similar_pungs = \
    [(['n', 'p', 'B', 'S'], [('B', 4), ('B', 4), ('B', 4)]),
     (['r', 'p', 'C', 'S'], [('C', 4), ('C', 4), ('C', 4)]),
     (['r', 'c', 'B', 'T'], [('B', 7), ('B', 8), ('B', 9)]),
     (['n', 'k', 'B', 'S'], [('B', 2), ('B', 2), ('B', 2), ('B', 2)]),
     (['r', 'e', 'K', 'S'], [('K', 4), ('K', 4)])
     ]

h_3_similar_pungs = \
    [(['n', 'p', 'B', 'S'], [('B', 4), ('B', 4), ('B', 4)]),
     (['r', 'p', 'C', 'S'], [('C', 4), ('C', 4), ('C', 4)]),
     (['r', 'c', 'B', 'T'], [('B', 7), ('B', 8), ('B', 9)]),
     (['r', 'p', 'K', 'T'], [('K', 4), ('K', 4), ('K', 4)]),
     (['n', 'e', 'D'], [('D', 1), ('D', 1)])
     ]


# 5.0 Consecutive Sets

h_9_tile_straight = \
    [(['n', 'p', 'B', 'S'], [('B', 4), ('B', 4), ('B', 4)]),
     (['n', 'c', 'C', 'T'], [('C', 1), ('C', 2), ('C', 3)]),
     (['r', 'c', 'C', 'S'], [('C', 4), ('C', 5), ('C', 6)]),
     (['r', 'c', 'C', 'T'], [('C', 7), ('C', 8), ('C', 9)]),
     (['n', 'e', 'W'], [('W', 4), ('W', 4)])
     ]


h_3_consecutive_pungs_1 = \
    [(['n', 'p', 'B', 'S'], [('B', 4), ('B', 4), ('B', 4)]),
     (['r', 'p', 'B', 'S'], [('B', 5), ('B', 5), ('B', 5)]),
     (['r', 'p', 'B', 'S'], [('B', 6), ('B', 6), ('B', 6)]),
     (['r', 'c', 'C', 'T'], [('C', 1), ('C', 2), ('C', 2)]),
     (['n', 'e', 'W'], [('W', 4), ('W', 4)])
     ]

h_3_consecutive_pungs_2 = \
    [(['n', 'c', 'B', 'T'], [('B', 1), ('B', 2), ('B', 2)]),
     (['n', 'p', 'B', 'S'], [('B', 4), ('B', 4), ('B', 4)]),
     (['r', 'p', 'B', 'S'], [('B', 5), ('B', 5), ('B', 5)]),
     (['r', 'p', 'B', 'S'], [('B', 6), ('B', 6), ('B', 6)]),
     (['n', 'e', 'W'], [('W', 4), ('W', 4)])
     ]

h_4_consecutive_pungs = \
    [(['n', 'p', 'B', 'S'], [('B', 4), ('B', 4), ('B', 4)]),
     (['r', 'p', 'B', 'S'], [('B', 5), ('B', 5), ('B', 5)]),
     (['r', 'p', 'B', 'S'], [('B', 6), ('B', 6), ('B', 6)]),
     (['r', 'p', 'B', 'S'], [('B', 7), ('B', 7), ('B', 7)]),
     (['n', 'e', 'C', 'S'], [('C', 4), ('C', 4)])
     ]

h_3_mothers = \
    [(['n', 'p', 'B', 'S'], [('B', 4), ('B', 4), ('B', 4)]),
     (['r', 'p', 'B', 'S'], [('B', 5), ('B', 5), ('B', 5)]),
     (['r', 'p', 'B', 'S'], [('B', 6), ('B', 6), ('B', 6)]),
     (['r', 'c', 'B', 'S'], [('B', 4), ('B', 5), ('B', 6)]),
     (['n', 'e', 'C', 'S'], [('C', 4), ('C', 4)])
     ]


# 6.0 Suit Patterns

h_mixed_1_suit_1 = \
    [(['n', 'p', 'B', 'S'], [('B', 3), ('B', 3), ('B', 3)]),
     (['r', 'p', 'W'], [('W', 4), ('W', 4), ('W', 4)]),
     (['r', 'p', 'D'], [('D', 1), ('D', 1), ('D', 1)]),
     (['r', 'c', 'B', 'S'], [('B', 4), ('B', 5), ('B', 6)]),
     (['n', 'e', 'B', 'S'], [('B', 8), ('B', 8)])
     ]

h_mixed_1_suit_2 = \
    [(['n', 'e', 'B', 'T'], [('B', 1), ('B', 1)]),
     (['n', 'e', 'B', 'S'], [('B', 3), ('B', 3)]),
     (['n', 'e', 'B', 'S'], [('B', 5), ('B', 5)]),
     (['n', 'e', 'B', 'S'], [('B', 6), ('B', 6)]),
     (['n', 'e', 'W'], [('W', 2), ('W', 2)]),
     (['n', 'e', 'W'], [('W', 4), ('W', 4)]),
     (['r', 'e', 'D'], [('D', 1), ('D', 1)])
     ]

h_pure_1_suit_1 = \
    [(['n', 'c', 'B', 'T'], [('B', 1), ('B', 2), ('B', 3)]),
     (['r', 'p', 'B', 'S'], [('B', 4), ('B', 4), ('B', 4)]),
     (['r', 'c', 'B', 'S'], [('B', 6), ('B', 7), ('B', 8)]),
     (['r', 'c', 'B', 'S'], [('B', 5), ('B', 6), ('B', 7)]),
     (['n', 'e', 'B', 'T'], [('B', 9), ('B', 9)])
     ]

h_pure_1_suit_2 = \
    [(['n', 'e', 'B', 'T'], [('B', 1), ('B', 1)]),
     (['n', 'e', 'B', 'S'], [('B', 3), ('B', 3)]),
     (['n', 'e', 'B', 'S'], [('B', 4), ('B', 4)]),
     (['n', 'e', 'B', 'S'], [('B', 5), ('B', 5)]),
     (['n', 'e', 'B', 'S'], [('B', 6), ('B', 6)]),
     (['n', 'e', 'B', 'S'], [('B', 8), ('B', 8)]),
     (['r', 'e', 'B', 'T'], [('B', 9), ('B', 9)])
     ]

h_little_terminal_club = \
    [(['n', 'c', 'B', 'T'], [('B', 1), ('B', 2), ('B', 3)]),
     (['r', 'c', 'B', 'T'], [('B', 1), ('B', 2), ('B', 3)]),
     (['r', 'c', 'B', 'T'], [('B', 7), ('B', 8), ('B', 9)]),
     (['r', 'c', 'B', 'T'], [('B', 7), ('B', 8), ('B', 9)]),
     (['n', 'e', 'S'], [('B', 5), ('B', 5)])
     ]

h_big_terminal_club = \
    [(['n', 'c', 'B', 'T'], [('B', 1), ('B', 2), ('B', 3)]),
     (['r', 'p', 'B', 'T'], [('B', 1), ('B', 1), ('B', 1)]),
     (['r', 'c', 'B', 'T'], [('B', 7), ('B', 8), ('B', 9)]),
     (['r', 'p', 'B', 'T'], [('B', 9), ('B', 9), ('B', 9)]),
     (['n', 'e', 'S'], [('B', 5), ('B', 5)])
     ]


h_nine_gates = \
    [(['r', 'h', 'B', 'T', 'S'],
     [('B', 1), ('B', 1),
      ('B', 1), ('B', 2), ('B', 3),
      ('B', 4), ('B', 5), ('B', 6),
      ('B', 6), ('B', 7), ('B', 8),
      ('B', 9), ('B', 9), ('B', 9)
      ]
      ),
     (['b'], [('F', 3)])
     ]


# 7.0 Terminal Tiles

h_two_tailed_terminal_chows_1 = \
    [(['n', 'p', 'C', 'S'], [('C', 3), ('C', 3), ('C', 3)]),
     (['n', 'c', 'B', 'T'], [('B', 1), ('B', 2), ('B', 3)]),
     (['r', 'c', 'B', 'T'], [('B', 7), ('B', 8), ('B', 9)]),
     (['r', 'c', 'K', 'S'], [('K', 5), ('K', 6), ('K', 7)]),
     (['n', 'e', 'W'], [('W', 4), ('W', 4)])
     ]

h_two_tailed_terminal_chows_2 = \
    [(['n', 'c', 'B', 'T'], [('B', 1), ('B', 2), ('B', 3)]),
     (['n', 'c', 'B', 'T'], [('B', 1), ('B', 2), ('B', 3)]),
     (['r', 'c', 'B', 'T'], [('B', 7), ('B', 8), ('B', 9)]),
     (['r', 'c', 'K', 'S'], [('K', 5), ('K', 6), ('K', 7)]),
     (['n', 'e', 'W'], [('W', 4), ('W', 4)])
     ]

h_two_tailed_terminal_chows_3 = \
    [(['n', 'c', 'C', 'T'], [('C', 1), ('C', 2), ('C', 3)]),
     (['n', 'c', 'B', 'T'], [('B', 1), ('B', 2), ('B', 3)]),
     (['r', 'c', 'B', 'T'], [('B', 7), ('B', 8), ('B', 9)]),
     (['r', 'c', 'C', 'T'], [('C', 7), ('C', 8), ('C', 9)]),
     (['n', 'e', 'K'], [('K', 4), ('K', 4)])
     ]

h_two_tailed_terminal_pungs_1 = \
    [(['n', 'p', 'C', 'S'], [('C', 3), ('C', 3), ('C', 3)]),
     (['n', 'p', 'B', 'T'], [('B', 1), ('B', 1), ('B', 1)]),
     (['r', 'p', 'B', 'T'], [('B', 9), ('B', 9), ('B', 9)]),
     (['r', 'c', 'K', 'S'], [('K', 5), ('K', 6), ('K', 7)]),
     (['n', 'e', 'W'], [('W', 4), ('W', 4)])
     ]

h_two_tailed_terminal_pungs_2 = \
    [(['n', 'p', 'C', 'T'], [('C', 1), ('C', 1), ('C', 1)]),
     (['n', 'p', 'B', 'T'], [('B', 1), ('B', 1), ('B', 1)]),
     (['r', 'p', 'B', 'T'], [('B', 9), ('B', 9), ('B', 9)]),
     (['r', 'p', 'C', 'T'], [('C', 9), ('C', 9), ('C', 9)]),
     (['n', 'e', 'K'], [('K', 4), ('K', 4)])
     ]

h_little_boundless_mountain_1 = \
    [(['n', 'c', 'B', 'T'], [('B', 1), ('B', 2), ('B', 3)]),
     (['r', 'c', 'B', 'T'], [('B', 1), ('B', 2), ('B', 3)]),
     (['r', 'c', 'B', 'T'], [('B', 7), ('B', 8), ('B', 9)]),
     (['r', 'c', 'B', 'T'], [('B', 7), ('B', 8), ('B', 9)]),
     (['n', 'e', 'B', 'T'], [('B', 9), ('B', 9)])
     ]

h_little_boundless_mountain_2 = \
    [(['n', 'c', 'B', 'T'], [('B', 1), ('B', 2), ('B', 3)]),
     (['r', 'c', 'B', 'T'], [('B', 1), ('B', 2), ('B', 3)]),
     (['r', 'c', 'B', 'T'], [('B', 1), ('B', 2), ('B', 3)]),
     (['r', 'c', 'B', 'T'], [('B', 7), ('B', 8), ('B', 9)]),
     (['n', 'e', 'B', 'T'], [('B', 9), ('B', 9)])
     ]

h_big_boundless_mountain_1 = \
    [(['n', 'c', 'B', 'T'], [('B', 1), ('B', 2), ('B', 3)]),
     (['r', 'p', 'B', 'T'], [('B', 1), ('B', 1), ('B', 1)]),
     (['r', 'c', 'B', 'T'], [('B', 7), ('B', 8), ('B', 9)]),
     (['r', 'c', 'B', 'T'], [('B', 7), ('B', 8), ('B', 9)]),
     (['n', 'e', 'B', 'T'], [('B', 9), ('B', 9)])
     ]

h_big_boundless_mountain_2 = \
    [(['n', 'c', 'B', 'T'], [('B', 1), ('B', 2), ('B', 3)]),
     (['r', 'c', 'B', 'T'], [('B', 1), ('B', 2), ('B', 3)]),
     (['r', 'c', 'B', 'T'], [('B', 7), ('B', 8), ('B', 9)]),
     (['r', 'p', 'B', 'T'], [('B', 9), ('B', 9), ('B', 9)]),
     (['n', 'e', 'B', 'T'], [('B', 1), ('B', 1)])
     ]


h_mixed_lesser_terminals = \
    [(['n', 'c', 'B', 'T'], [('B', 1), ('B', 2), ('B', 3)]),
     (['r', 'p', 'C', 'T'], [('C', 1), ('C', 1), ('C', 1)]),
     (['r', 'c', 'K', 'T'], [('K', 7), ('K', 8), ('K', 9)]),
     (['r', 'p', 'D'], [('D', 2), ('D', 2), ('D', 2)]),
     (['n', 'e', 'W'], [('W', 1), ('W', 1)])
     ]

h_pure_lesser_terminals = \
    [(['n', 'c', 'B', 'T'], [('B', 1), ('B', 2), ('B', 3)]),
     (['r', 'p', 'C', 'T'], [('C', 1), ('C', 1), ('C', 1)]),
     (['r', 'c', 'K', 'T'], [('K', 7), ('K', 8), ('K', 9)]),
     (['r', 'p', 'B', 'T'], [('B', 9), ('B', 9), ('B', 9)]),
     (['n', 'e', 'K', 'T'], [('K', 1), ('K', 1)])
     ]

h_mixed_greater_terminals_1 = \
    [(['n', 'p', 'K', 'T'], [('K', 1), ('K', 1), ('K', 1)]),
     (['r', 'p', 'C', 'T'], [('C', 9), ('C', 9), ('C', 9)]),
     (['r', 'p', 'B', 'T'], [('B', 9), ('B', 9), ('B', 9)]),
     (['r', 'p', 'D'], [('D', 2), ('D', 2), ('D', 2)]),
     (['n', 'e', 'W'], [('W', 1), ('W', 1)])
     ]

h_mixed_greater_terminals_2 = \
    [(['n', 'e', 'C', 'T'], [('C', 9), ('C', 9)]),
     (['n', 'e', 'B', 'T'], [('B', 1), ('B', 1)]),
     (['n', 'e', 'B', 'T'], [('B', 9), ('B', 9)]),
     (['n', 'e', 'K', 'T'], [('K', 1), ('K', 1)]),
     (['n', 'e', 'W'], [('W', 2), ('W', 2)]),
     (['n', 'e', 'W'], [('W', 4), ('W', 4)]),
     (['r', 'e', 'D'], [('D', 1), ('D', 1)])
     ]


h_pure_greater_terminals_1 = \
    [(['n', 'p', 'C', 'T'], [('C', 1), ('C', 1), ('C', 1)]),
     (['r', 'p', 'C', 'T'], [('C', 9), ('C', 9), ('C', 9)]),
     (['r', 'p', 'B', 'T'], [('B', 1), ('B', 1), ('B', 1)]),
     (['r', 'p', 'B', 'T'], [('B', 9), ('B', 9), ('B', 9)]),
     (['n', 'e', 'K', 'T'], [('K', 9), ('K', 9)])
     ]

h_pure_greater_terminals_2 = \
    [(['n', 'e', 'C', 'T'], [('C', 1), ('C', 1)]),
     (['n', 'e', 'C', 'T'], [('C', 1), ('C', 1)]),
     (['n', 'e', 'C', 'T'], [('C', 9), ('C', 9)]),
     (['n', 'e', 'B', 'T'], [('B', 1), ('B', 1)]),
     (['n', 'e', 'B', 'T'], [('B', 9), ('B', 9)]),
     (['n', 'e', 'K', 'T'], [('K', 1), ('K', 1)]),
     (['r', 'e', 'K', 'T'], [('K', 9), ('K', 9)])
     ]


# 8.0 Honor Tiles

h_dragon_pung = \
    [(['n', 'p', 'B', 'S'], [('B', 2), ('B', 2), ('B', 2)]),
     (['n', 'c', 'K', 'S'], [('K', 3), ('K', 4), ('K', 5)]),
     (['r', 'c', 'C', 'T'], [('C', 1), ('C', 2), ('C', 3)]),
     (['r', 'p', 'D'], [('D', 1), ('D', 1), ('D', 1)]),
     (['r', 'e', 'D'], [('D', 3), ('D', 3)]),
     (['b'], [('F', 4)])
     ]

h_seat_wind = \
    [(['n', 'p', 'B', 'S'], [('B', 2), ('B', 2), ('B', 2)]),
     (['n', 'c', 'K', 'S'], [('K', 3), ('K', 4), ('K', 5)]),
     (['r', 'c', 'C', 'T'], [('C', 1), ('C', 2), ('C', 3)]),
     (['r', 'p', 'W'], [('W', 1), ('W', 1), ('W', 1)]),
     (['r', 'e', 'D'], [('D', 3), ('D', 3)]),
     (['b'], [('F', 4)])
     ]


h_little_3_winds = \
    [(['n', 'p', 'C', 'S'], [('C', 2), ('C', 2), ('C', 2)]),
     (['n', 'p', 'W'], [('W', 3), ('W', 3), ('W', 3)]),
     (['r', 'c', 'K', 'T'], [('K', 1), ('K', 2), ('K', 3)]),
     (['r', 'p', 'W'], [('W', 4), ('W', 4), ('W', 4)]),
     (['r', 'e', 'W'], [('W', 1), ('W', 1)]),
     (['b'], [('F', 4)])
     ]

h_big_3_winds = \
    [(['n', 'p', 'W'], [('W', 2), ('W', 2), ('W', 2)]),
     (['n', 'p', 'W'], [('W', 3), ('W', 3), ('W', 3)]),
     (['r', 'c', 'K', 'T'], [('K', 1), ('K', 2), ('K', 3)]),
     (['r', 'p', 'W'], [('W', 4), ('W', 4), ('W', 4)]),
     (['r', 'e', 'C', 'S'], [('C', 4), ('C', 4)]),
     (['b'], [('F', 4)])
     ]

h_little_4_winds = \
    [(['n', 'p', 'W'], [('W', 2), ('W', 2), ('W', 2)]),
     (['n', 'c', 'K', 'T'], [('K', 1), ('K', 2), ('K', 3)]),
     (['r', 'p', 'W'], [('W', 3), ('W', 3), ('W', 3)]),
     (['r', 'p', 'W'], [('W', 4), ('W', 4), ('W', 4)]),
     (['r', 'e', 'W'], [('W', 1), ('W', 1)]),
     (['b'], [('F', 4)])
     ]

h_big_4_winds = \
    [(['n', 'p', 'W'], [('W', 2), ('W', 2), ('W', 2)]),
     (['r', 'p', 'W'], [('W', 3), ('W', 3), ('W', 3)]),
     (['r', 'p', 'W'], [('W', 4), ('W', 4), ('W', 4)]),
     (['r', 'p', 'W'], [('W', 1), ('W', 1), ('W', 1)]),
     (['r', 'e', 'C', 'S'], [('C', 4), ('C', 4)]),
     (['b'], [('F', 4)])
     ]


h_little_3_dragons = \
    [(['n', 'p', 'D'], [('D', 2), ('D', 2), ('D', 2)]),
     (['n', 'c', 'K', 'S'], [('K', 3), ('K', 4), ('K', 5)]),
     (['r', 'p', 'C', 'T'], [('C', 1), ('C', 1), ('C', 1)]),
     (['r', 'p', 'D'], [('D', 1), ('D', 1), ('D', 1)]),
     (['r', 'e', 'D'], [('D', 3), ('D', 3)])
     ]

h_big_3_dragons = \
    [(['n', 'p', 'D'], [('D', 2), ('D', 2), ('D', 2)]),
     (['n', 'c', 'B', 'T'], [('B', 1), ('B', 2), ('B', 3)]),
     (['r', 'p', 'D'], [('D', 1), ('D', 1), ('D', 1)]),
     (['r', 'p', 'D'], [('D', 3), ('D', 3), ('D', 3)]),
     (['r', 'e', 'C', 'S'], [('C', 3), ('C', 3)])
     ]


h_all_honors_1 = \
    [(['n', 'p', 'D'], [('D', 2), ('D', 2), ('D', 2)]),
     (['n', 'p', 'W'], [('W', 3), ('W', 3), ('W', 3)]),
     (['r', 'p', 'W'], [('W', 1), ('W', 1), ('W', 1)]),
     (['r', 'p', 'D'], [('D', 1), ('D', 1), ('D', 1)]),
     (['r', 'e', 'W'], [('W', 4), ('W', 4)])
     ]

h_all_honors_2 = \
    [(['n', 'e', 'W'], [('W', 1), ('W', 1)]),
     (['n', 'e', 'W'], [('W', 2), ('W', 2)]),
     (['n', 'e', 'W'], [('W', 2), ('W', 2)]),
     (['n', 'e', 'D'], [('D', 1), ('D', 1)]),
     (['n', 'e', 'D'], [('D', 2), ('D', 2)]),
     (['n', 'e', 'D'], [('D', 3), ('D', 3)]),
     (['r', 'e', 'W'], [('W', 4), ('W', 4)])
     ]

h_all_honor_pairs = \
    [(['n', 'e', 'W'], [('W', 1), ('W', 1)]),
     (['n', 'e', 'W'], [('W', 2), ('W', 2)]),
     (['n', 'e', 'W'], [('W', 3), ('W', 3)]),
     (['n', 'e', 'D'], [('D', 1), ('D', 1)]),
     (['n', 'e', 'D'], [('D', 2), ('D', 2)]),
     (['n', 'e', 'D'], [('D', 3), ('D', 3)]),
     (['r', 'e', 'W'], [('W', 4), ('W', 4)])
     ]


# 9.0 Seven Pairs

h_7_pairs_1 = \
    [(['n', 'e', 'C', 'T'], [('C', 1), ('C', 1)]),
     (['n', 'e', 'C', 'S'], [('C', 7), ('C', 7)]),
     (['n', 'e', 'B', 'S'], [('B', 3), ('B', 3)]),
     (['n', 'e', 'K', 'S'], [('K', 6), ('K', 6)]),
     (['n', 'e', 'W'], [('W', 4), ('W', 4)]),
     (['n', 'e', 'D'], [('D', 2), ('D', 2)]),
     (['r', 'e', 'D'], [('D', 3), ('D', 3)])
     ]

h_7_pairs_2 = \
    [(['n', 'e', 'C', 'T'], [('C', 1), ('C', 1)]),
     (['n', 'e', 'C', 'T'], [('C', 1), ('C', 1)]),
     (['n', 'e', 'B', 'S'], [('B', 3), ('B', 3)]),
     (['n', 'e', 'K', 'S'], [('K', 6), ('K', 6)]),
     (['n', 'e', 'W'], [('W', 4), ('W', 4)]),
     (['n', 'e', 'D'], [('D', 2), ('D', 2)]),
     (['r', 'e', 'D'], [('D', 3), ('D', 3)])
     ]

h_7_pairs_3 = \
    [(['n', 'e', 'C', 'T'], [('C', 1), ('C', 1)]),
     (['n', 'e', 'C', 'T'], [('C', 1), ('C', 1)]),
     (['n', 'e', 'B', 'S'], [('B', 3), ('B', 3)]),
     (['n', 'e', 'B', 'S'], [('B', 3), ('B', 3)]),
     (['n', 'e', 'W'], [('W', 4), ('W', 4)]),
     (['n', 'e', 'D'], [('D', 2), ('D', 2)]),
     (['r', 'e', 'D'], [('D', 3), ('D', 3)])
     ]

h_7_pairs_4 = \
    [(['n', 'e', 'C', 'T'], [('C', 1), ('C', 1)]),
     (['n', 'e', 'C', 'T'], [('C', 1), ('C', 1)]),
     (['n', 'e', 'B', 'S'], [('B', 3), ('B', 3)]),
     (['n', 'e', 'B', 'S'], [('B', 3), ('B', 3)]),
     (['n', 'e', 'W'], [('W', 4), ('W', 4)]),
     (['n', 'e', 'W'], [('W', 4), ('W', 4)]),
     (['r', 'e', 'D'], [('D', 3), ('D', 3)])
     ]


h_7_shifted_pairs_1 = \
    [(['n', 'e', 'C', 'T'], [('C', 1), ('C', 1)]),
     (['n', 'e', 'C', 'S'], [('C', 2), ('C', 2)]),
     (['n', 'e', 'C', 'S'], [('C', 3), ('C', 3)]),
     (['n', 'e', 'C', 'S'], [('C', 4), ('C', 4)]),
     (['n', 'e', 'C', 'S'], [('C', 5), ('C', 5)]),
     (['n', 'e', 'C', 'S'], [('C', 6), ('C', 6)]),
     (['r', 'e', 'C', 'S'], [('C', 7), ('C', 7)])
     ]

h_7_shifted_pairs_2 = \
    [(['n', 'e', 'K', 'S'], [('K', 3), ('K', 3)]),
     (['n', 'e', 'K', 'S'], [('K', 4), ('K', 4)]),
     (['n', 'e', 'K', 'S'], [('K', 5), ('K', 5)]),
     (['n', 'e', 'K', 'S'], [('K', 6), ('K', 6)]),
     (['n', 'e', 'K', 'S'], [('K', 7), ('K', 7)]),
     (['n', 'e', 'K', 'S'], [('K', 8), ('K', 8)]),
     (['r', 'e', 'K', 'T'], [('K', 9), ('K', 9)])
     ]

h_grand_chariot = \
    [(['n', 'e', 'C', 'S'], [('C', 2), ('C', 2)]),
     (['n', 'e', 'C', 'S'], [('C', 3), ('C', 3)]),
     (['n', 'e', 'C', 'S'], [('C', 4), ('C', 4)]),
     (['n', 'e', 'C', 'S'], [('C', 5), ('C', 5)]),
     (['n', 'e', 'C', 'S'], [('C', 6), ('C', 6)]),
     (['n', 'e', 'C', 'S'], [('C', 7), ('C', 7)]),
     (['r', 'e', 'C', 'S'], [('C', 8), ('C', 8)])
     ]

h_bamboo_forest = \
    [(['n', 'e', 'B', 'S'], [('B', 2), ('B', 2)]),
     (['n', 'e', 'B', 'S'], [('B', 3), ('B', 3)]),
     (['n', 'e', 'B', 'S'], [('B', 4), ('B', 4)]),
     (['n', 'e', 'B', 'S'], [('B', 5), ('B', 5)]),
     (['n', 'e', 'B', 'S'], [('B', 6), ('B', 6)]),
     (['n', 'e', 'B', 'S'], [('B', 7), ('B', 7)]),
     (['r', 'e', 'B', 'S'], [('B', 8), ('B', 8)])
     ]

h_number_neighborhood = \
    [(['n', 'e', 'K', 'S'], [('K', 2), ('K', 2)]),
     (['n', 'e', 'K', 'S'], [('K', 3), ('K', 3)]),
     (['n', 'e', 'K', 'S'], [('K', 4), ('K', 4)]),
     (['n', 'e', 'K', 'S'], [('K', 5), ('K', 5)]),
     (['n', 'e', 'K', 'S'], [('K', 6), ('K', 6)]),
     (['n', 'e', 'K', 'S'], [('K', 7), ('K', 7)]),
     (['r', 'e', 'K', 'S'], [('K', 8), ('K', 8)])
     ]


# 10.0 Color Hands

h_all_green = \
    [(['n', 'p', 'B', 'S'], [('B', 2), ('B', 2), ('B', 2)]),
     (['r', 'c', 'B', 'S'], [('B', 2), ('B', 3), ('B', 4)]),
     (['r', 'p', 'B', 'S'], [('B', 6), ('B', 6), ('B', 6)]),
     (['r', 'p', 'D'], [('D', 2), ('D', 2), ('D', 2)]),
     (['r', 'e', 'B', 'S'], [('B', 8), ('B', 8)])
     ]

h_all_red = \
    [(['n', 'p', 'B', 'S'], [('B', 1), ('B', 1), ('B', 1)]),
     (['r', 'p', 'B', 'S'], [('B', 5), ('B', 5), ('B', 5)]),
     (['r', 'p', 'B', 'S'], [('B', 9), ('B', 9), ('B', 9)]),
     (['r', 'p', 'D'], [('D', 1), ('D', 1), ('D', 1)]),
     (['r', 'e', 'B', 'S'], [('B', 7), ('B', 7)])
     ]

h_all_blue = \
    [(['n', 'p', 'W'], [('W', 2), ('W', 2), ('W', 2)]),
     (['r', 'p', 'W'], [('W', 3), ('W', 3), ('W', 3)]),
     (['r', 'p', 'W'], [('C', 8), ('C', 8), ('C', 8)]),
     (['r', 'p', 'W'], [('D', 3), ('D', 3), ('D', 3)]),
     (['r', 'e', 'W'], [('W', 4), ('W', 4)]),
     (['b'], [('F', 4)])
     ]


# 11.0 Irregular Hands

h_13_orphans = \
    [(['r', 'h', 'B', 'T', 'W', 'D'],
      [('C', 1), ('C', 9), ('B', 1), ('B', 9), ('K', 1), ('K', 9),
       ('W', 1), ('W', 2), ('W', 3), ('W', 4),
       ('D', 1), ('D', 2), ('D', 2), ('D', 3)
       ]
      ),
     (['b'], [('F', 1)])
     ]


# 12.0 Incidental Bonuses

h_final_draw = []

h_final_discard = []

h_win_on_kong = []

h_win_on_bonus_tile = []

h_robbing_a_kong = []

h_blessing_of_heaven = []

h_blessing_of_earth = []


# 13.0 Bonus Tiles

h_non_seat_flower = \
    [(['r', 'c', 'C', 'T'], [('C', 7), ('C', 8), ('C', 9)]),
     (['r', 'p', 'W'], [('W', 1), ('W', 1), ('W', 1)]),
     (['r', 'c', 'K', 'S'], [('K', 1), ('K', 2), ('K', 3)]),
     (['r', 'c', 'B', 'S'], [('B', 4), ('B', 5), ('B', 6)]),
     (['c', 'e', 'W'], [('W', 3), ('W', 3)]),
     (['b'], [('F', 3), ('S', 2)])
     ]

h_non_seat_season = \
    [(['r', 'c', 'C', 'T'], [('C', 7), ('C', 8), ('C', 9)]),
     (['r', 'p', 'W'], [('W', 1), ('W', 1), ('W', 1)]),
     (['r', 'c', 'K', 'S'], [('K', 1), ('K', 2), ('K', 3)]),
     (['r', 'c', 'B', 'S'], [('B', 4), ('B', 5), ('B', 6)]),
     (['c', 'e', 'W'], [('W', 3), ('W', 3)]),
     (['b'], [('F', 2), ('S', 4)])
     ]

h_seat_flower = \
    [(['r', 'c', 'C', 'T'], [('C', 7), ('C', 8), ('C', 9)]),
     (['r', 'p', 'W'], [('W', 1), ('W', 1), ('W', 1)]),
     (['r', 'c', 'K', 'S'], [('K', 1), ('K', 2), ('K', 3)]),
     (['r', 'c', 'B', 'S'], [('B', 4), ('B', 5), ('B', 6)]),
     (['c', 'e', 'W'], [('W', 3), ('W', 3)]),
     (['b'], [('F', 1), ('S', 2)])
     ]

h_seat_season = \
    [(['r', 'c', 'C', 'T'], [('C', 7), ('C', 8), ('C', 9)]),
     (['r', 'p', 'W'], [('W', 1), ('W', 1), ('W', 1)]),
     (['r', 'c', 'K', 'S'], [('K', 1), ('K', 2), ('K', 3)]),
     (['r', 'c', 'B', 'S'], [('B', 4), ('B', 5), ('B', 6)]),
     (['c', 'e', 'W'], [('W', 3), ('W', 3)]),
     (['b'], [('F', 2), ('S', 1)])
     ]

h_4_flowers = \
    [(['r', 'c', 'C', 'T'], [('C', 7), ('C', 8), ('C', 9)]),
     (['r', 'p', 'W'], [('W', 1), ('W', 1), ('W', 1)]),
     (['r', 'c', 'K', 'S'], [('K', 1), ('K', 2), ('K', 3)]),
     (['r', 'c', 'B', 'S'], [('B', 4), ('B', 5), ('B', 6)]),
     (['c', 'e', 'W'], [('W', 3), ('W', 3)]),
     (['b'], [('F', 1), ('F', 2), ('F', 3), ('F', 4)])
     ]

h_4_seasons = \
    [(['r', 'c', 'C', 'T'], [('C', 7), ('C', 8), ('C', 9)]),
     (['r', 'p', 'W'], [('W', 1), ('W', 1), ('W', 1)]),
     (['r', 'c', 'K', 'S'], [('K', 1), ('K', 2), ('K', 3)]),
     (['r', 'c', 'B', 'S'], [('B', 4), ('B', 5), ('B', 6)]),
     (['c', 'e', 'W'], [('W', 3), ('W', 3)]),
     (['b'], [('S', 1), ('S', 2), ('S', 3), ('S', 4)])
     ]

h_all_bonus_tiles = \
    [(['r', 'c', 'C', 'T'], [('C', 7), ('C', 8), ('C', 9)]),
     (['r', 'p', 'W'], [('W', 1), ('W', 1), ('W', 1)]),
     (['r', 'c', 'K', 'S'], [('K', 1), ('K', 2), ('K', 3)]),
     (['l'], [('B', 4), ('B', 5), ('B', 6), ('B', 7)]),
     (['b'], [('F', 1), ('F', 2), ('F', 3), ('F', 4),
              ('S', 1), ('S', 2), ('S', 3), ('S', 4)])
     ]
