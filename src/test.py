#!/usr/bin/env python
# -*- coding: utf-8 -*-

import tile as t
import fp   as f
import hand as h

# make a wall
w = []
w = t.get_wall()

# sort the wall
s = f.sort_by(t.compare, w)

# check if sorting is correct
l = f.map_func(t.get_rank, s)

wr = f.flatten( f.map_func(lambda x: f.repeat(x,4), t.regular_tiles) )
wr = f.fold_func(f._cons, wr, t.bonus_tiles)


################
### Examples ###
################

### 1.0 Trivial Patterns

h_chicken = { 'held' : [ ('W', 3) ]
            , 'concealed' : []
            , 'melded' : [ [ ('C', 7), ('C', 8), ('C', 9) ]
                         , [ ('W', 1), ('W', 1), ('W', 1) ]
                         , [ ('K', 1), ('K', 2), ('K', 3) ]
                         , [ ('B', 4), ('B', 5), ('B', 6) ]
                         ]
            , 'last' : ('W', 3)
            , 'bonus' : [ ('F', 1) ]
            }

h_all_seq = { 'held': [ ('D', 1) ]
            , 'concealed': [ [ ('B', 4), ('B', 5), ('B', 6) ] ]
            , 'melded': [ [ ('C', 7), ('C', 8), ('C', 9) ]
                      , [ ('B', 2), ('B', 3), ('B', 4) ]
                      , [ ('K', 1), ('K', 2), ('K', 3) ]
                      ]
            , 'last' : ('D', 1)
            , 'bonus': [ ('F', 1), ('S', 2) ]
            }

h_concealed = {}

h_self_drawn = {}

h_all_simples = { 'held': [ ('B', 2) ]
                , 'concealed': [ [ ('B', 4), ('B', 5), ('B', 6) ] ]
                , 'melded': [ [ ('C', 6), ('C', 7), ('C', 8) ]
                            , [ ('B', 3), ('B', 3), ('B', 3) ]
                            , [ ('K', 2), ('K', 2), ('K', 2) ]
                            ]
                , 'last' : ('B', 2)
                , 'bonus': [ ('F', 1), ('S', 2) ]
                }

h_all_types = { 'held' : [ ('D', 3) ]
              , 'concealed' : []
              , 'melded' : [ [ ('C', 7), ('C', 8), ('C', 9) ]
                           , [ ('W', 1), ('W', 1), ('W', 1) ]
                           , [ ('K', 1), ('K', 2), ('K', 3) ]
                           , [ ('B', 4), ('B', 5), ('B', 6) ]
                           ]
              , 'last' : ('D', 3)
              , 'bonus' : [ ('F', 1) ]
              }

h_illegal_call = {}



### 2.0 Identical Sets




# 3.0 Pungs and Kongs

h_1_kong = { 'held' : [ ('D', 1) ]
           , 'concealed': [ [ ('B', 4), ('B', 4), ('B', 4), ('B', 4) ] ]
           , 'melded': [ [ ('C', 7), ('C', 8), ('C', 9) ]
                       , [ ('B', 2), ('B', 3), ('B', 4) ]
                       , [ ('K', 1), ('K', 2), ('K', 3) ]
                       ]
           , 'last' : ('D', 1)
           , 'bonus': [ ('F', 1), ('S', 2) ]
           }



### 4.0 Similar Sets




### 5.0 Consecutive Sets




### 6.0 Suit Patterns




### 7.0 Terminal Tiles




### 8.0 Honor Tiles



h_all_honor_pairs = {'held': [ ('W', 1), ('W', 1), ('W', 2), ('W', 2), ('W', 3), ('W', 3)
                             , ('W', 4), ('W', 4), ('D', 1), ('D', 1), ('D', 2), ('D', 2)
                             , ('D', 3)
                             ]
                    , 'concealed': []
                    , 'melded': []
                    , 'last': ('D', 3)
                    , 'bonus': [ ('F', 4) ]
                    }


### 9.0 Seven Pairs

h_7_pairs_1 = {'held': [ ('C', 1), ('C', 1), ('C', 7), ('C', 7), ('B', 3), ('B', 3)
                       , ('K', 6), ('K', 6), ('W', 4), ('W', 4), ('D', 2), ('D', 2)
                       , ('D', 3)
                       ]
              , 'concealed': []
              , 'melded': []
              , 'last': ('D', 3)
              , 'bonus': [ ('F', 1) ]
              }

h_7_pairs_2 = {'held': [ ('C', 1), ('C', 1), ('C', 1), ('C', 1), ('B', 3), ('B', 3)
                       , ('K', 6), ('K', 6), ('W', 4), ('W', 4), ('D', 2), ('D', 2)
                       , ('D', 3)
                       ]
              , 'concealed': []
              , 'melded': []
              , 'last': ('D', 3)
              , 'bonus': [ ('F', 1) ]
              }

h_7_pairs_3 = {'held': [ ('C', 1), ('C', 1), ('C', 1), ('C', 1), ('B', 3), ('B', 3)
                       , ('B', 3), ('B', 3), ('W', 4), ('W', 4), ('D', 2), ('D', 2)
                       , ('D', 3)
                       ]
              , 'concealed': []
              , 'melded': []
              , 'last': ('D', 3)
              , 'bonus': [ ('F', 1) ]
              }

h_7_pairs_4 = {'held': [ ('C', 1), ('C', 1), ('C', 1), ('C', 1), ('B', 3), ('B', 3)
                       , ('B', 3), ('B', 3), ('W', 4), ('W', 4), ('D', 2), ('D', 2)
                       , ('D', 2)
                       ]
              , 'concealed': []
              , 'melded': []
              , 'last': ('D', 2)
              , 'bonus': [ ('F', 1) ]
              }


h_7_shifted_pairs = {'held': [ ('C', 1), ('C', 1), ('C', 2), ('C', 2), ('C', 3), ('C', 3)
                             , ('C', 4), ('C', 4), ('C', 5), ('C', 5), ('C', 6), ('C', 6)
                             , ('C', 7)
                             ]
                    , 'concealed': []
                    , 'melded': []
                    , 'last': ('C', 7)
                    , 'bonus': [ ('F', 1) ]
                    }

h_grand_chariot = {'held': [ ('C', 2), ('C', 2), ('C', 3), ('C', 3), ('C', 4), ('C', 4)
                           , ('C', 5), ('C', 5), ('C', 6), ('C', 6), ('C', 7), ('C', 7)
                           , ('C', 8)
                           ]
                  , 'concealed': []
                  , 'melded': []
                  , 'last': ('C', 8)
                  , 'bonus': [ ('F', 3) ]
                  }

h_bamboo_forest = {'held': [ ('B', 2), ('B', 2), ('B', 3), ('B', 3), ('B', 4), ('B', 4)
                           , ('B', 5), ('B', 5), ('B', 6), ('B', 6), ('B', 7), ('B', 8)
                           , ('B', 8)
                           ]
                  , 'concealed': []
                  , 'melded': []
                  , 'last': ('B', 7)
                  , 'bonus': [ ('F', 2) ]
                  }

h_number_neighborhood = {'held': [ ('K', 2), ('K', 2), ('K', 3), ('K', 3), ('K', 4), ('K', 4)
                                 , ('K', 5), ('K', 5), ('K', 6), ('K', 7), ('K', 7), ('K', 8)
                                 , ('K', 8)
                                 ]
                        , 'concealed': []
                        , 'melded': []
                        , 'last': ('K', 6)
                        , 'bonus': [ ('F', 4) ]
                        }



### 10.0 Color Hands




### 11.0 Irregular Hands

h_13o = { 'held': [ ('C', 1), ('C', 9), ('B', 1), ('B', 9), ('K', 1), ('K', 9)
                  , ('W', 1), ('W', 2), ('W', 3), ('W', 4), ('D', 1), ('D', 2)
                  , ('D', 2)
                  ]
        , 'concealed': []
        , 'melded': []
        , 'last': ('D', 3)
        , 'bonus': [ ('F', 1) ]
        }



### 12.0 Incidental Bonuses




### 13.0 Bonus Tiles



