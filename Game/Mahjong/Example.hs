-- |
-- Module      :  Game.Mahjong.Example
-- Copyright   :  Joseph Ching 2014
-- License     :  MIT
--
-- Maintainer  :  joseph.m.ching@gmail.com
-- Stability   :  experimental
-- Portability :  portable

-- | Data definition of tiles
--   along with tile related functions
module Game.Mahjong.Example

where

import Game.Mahjong.Hand
import Game.Mahjong.Meld

-- | Make a list of hands for testing


-- | 1.0 Trivial Patterns

chickenHand :: Hand
chickenHand = [ (['r', 'c', 'C', 'T'], [(C, 7), (C, 8), (C, 9)])
              , (['r', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
              , (['r', 'c', 'K', 'S'], [(K, 1), (K, 2), (K, 3)])
              , (['r', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
              , (['c', 'e', 'W'], [(W, 3), (W, 3)])
              , (['b'], [(F, 1), (S, 2)])
              ]

allChowsHand :: Hand 
allChowsHand = [ (['n', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
               , (['r', 'c', 'C', 'T'], [(C, 7), (C, 8), (C, 9)])
               , (['r', 'c', 'B', 'S'], [(B, 2), (B, 3), (B, 4)])
               , (['r', 'c', 'K', 'T'], [(K, 1), (K, 2), (K, 3)])
               , (['n', 'e', 'D'], [(D, 1), (D, 1)])
               , (['b'], [(F, 1), (S, 2)])
               ]

concealedHand :: Hand 
concealedHand = [ (['n', 'c', 'C', 'T'], [(C, 7), (C, 8), (C, 9)])
                , (['n', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
                , (['n', 'c', 'K', 'S'], [(K, 1), (K, 2), (K, 3)])
                , (['n', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
                , (['c', 'e', 'W'], [(W, 3), (W, 3)])
                , (['b'], [(F, 1), (S, 2)])
                ]

selfDrawnHand :: Hand
selfDrawnHand = []

allSimpleHand2 :: Hand
allSimpleHand2 = [ (['n', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
                 , (['r', 'c', 'C', 'S'], [(C, 6), (C, 7), (C, 8)])
                 , (['r', 'p', 'B', 'S'], [(B, 3), (B, 3), (B, 3)])
                 , (['r', 'p', 'K', 'S'], [(K, 2), (K, 2), (K, 2)])
                 , (['r', 'e', 'B', 'S'], [(B, 2), (B, 2)])
                 , (['b'], [(F, 1), (S, 2)])
                 ]

allSimpleHand2 = Hand
allSimpleHand2 = [ (['n', 'e', 'C', 'S'], [(C, 2), (C, 2)])
                 , (['n', 'e', 'C', 'S'], [(C, 5), (C, 5)])
                 , (['n', 'e', 'C', 'S'], [(C, 8), (C, 8)])
                 , (['n', 'e', 'B', 'S'], [(B, 3), (B, 3)])
                 , (['n', 'e', 'B', 'S'], [(B, 6), (B, 6)])
                 , (['n', 'e', 'B', 'S'], [(B, 8), (B, 8)])
                 , (['r', 'e', 'K', 'S'], [(K, 8), (K, 8)])
                 ]

AllTypesHand :: Hand
AllTypesHand = [ (['r', 'c', 'C', 'T'], [(C, 7), (C, 8), (C, 9)])
               , (['r', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
               , (['r', 'c', 'K', 'T'], [(K, 1), (K, 2), (K, 3)])
               , (['r', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
               , (['r', 'e', 'D'], [(D, 3), (D, 3)])
               , (['b'], [(F, 1)])
               ]


-- | 2.0 Identical Chows

twoIdenticalChowsHand :: Hand
twoIdenticalChowsHand = [ (['n', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
                        , (['r', 'c', 'C', 'S'], [(C, 6), (C, 7), (C, 8)])
                        , (['r', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
                        , (['r', 'c', 'C', 'S'], [(C, 6), (C, 7), (C, 8)])
                        , (['r', 'e', 'D'], [(D, 3), (D, 3)])
                        ]

twoIdenticalChowsTwiceHand :: Hand
twoIdenticalChowsTwiceHand = [ (['n', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
                             , (['r', 'c', 'C', 'S'], [(C, 6), (C, 7), (C, 8)])
                             , (['r', 'p', 'W'], [(B, 4), (B, 5), (B, 6)])
                             , (['r', 'c', 'C', 'S'], [(C, 6), (C, 7), (C, 8)])
                             , (['r', 'e', 'D'], [(D, 3), (D, 3)])
                             ]

threeIdenticalChowsHand :: Hand
threeIdenticalChowsHand = [ (['n', 'c', 'C', 'S'], [(C, 6), (C, 7), (C, 8)])
                          , (['r', 'c', 'C', 'S'], [(C, 6), (C, 7), (C, 8)])
                          , (['r', 'p', 'B', 'W'], [(B, 4), (B, 5), (B, 6)])
                          , (['r', 'c', 'C', 'S'], [(C, 6), (C, 7), (C, 8)])
                          , (['r', 'e', 'D'], [(D, 3), (D, 3)])
                          ]

fourIdenticalChowsHand :: Hand
fourIdenticalChowsHand = [ (['n', 'c', 'C', 'S'], [(C, 6), (C, 7), (C, 8)])
                         , (['r', 'c', 'C', 'S'], [(C, 6), (C, 7), (C, 8)])
                         , (['r', 'p', 'C', 'W'], [(C, 6), (C, 7), (C, 8)])
                         , (['r', 'c', 'C', 'S'], [(C, 6), (C, 7), (C, 8)])
                         , (['r', 'e', 'D'], [(D, 3), (D, 3)])
                         ]


-- | 3.0 Pungs and Kongs

allPungsHand :: Hand
allPungsHand = [ (['n', 'k', 'B', 'S'], [(B, 4), (B, 4), (B, 4), (B, 4)])
               , (['r', 'p', 'C', 'S'], [(C, 7), (C, 7), (C, 7)])
               , (['r', 'p', 'B', 'S'], [(B, 2), (B, 2), (B, 2)])
               , (['r', 'p', '­K', 'S'], [(K, 3), (K, 3), (K, 3)])
               , (['n', 'e', 'D'], [(D, 1), (D, 1)])
               ]

twoConcealedPungsHand :: Hand
twoConcealedPungsHand = [ (['n', 'k', 'B', 'S'], [(B, 4), (B, 4), (B, 4), (B, 4)])
                        , (['n', 'p', 'B', 'S'], [(B, 2), (B, 2), (B, 2)])
                        , (['r', 'p', 'C', 'S'], [(C, 7), (C, 7), (C, 7)])
                        , (['r', 'k', 'K', 'S'], [(K, 3), (K, 3), (K, 3), (K, 3)])
                        , (['n', 'e', 'D'], [(D, 1), (D, 1)])
                        ]

threeConcealedPungsHand :: Hand
threeConcealedPungsHand = [ (['n', 'k', 'B', 'S'], [(B, 4), (B, 4), (B, 4), (B, 4)])
                          , (['n', 'p', 'B', 'S'], [(B, 1), (B, 2), (B, 2)])
                          , (['n', 'k', 'K', 'S'], [(K, 3), (K, 3), (K, 3), (K, 3)])
                          , (['r', 'p', 'C', 'S'], [(C, 7), (C, 7), (C, 7)])
                          , (['r', 'e', 'D'], [(D, 1), (D, 1)])
                          ]

fourConcealedPunsgHand :: Hand
fourConcealedPunsgHand = [ (['n', 'k', 'B', 'S'], [(B, 4), (B, 4), (B, 4), (B, 4)])
                         , (['n', 'p', 'B', 'S'], [(B, 2), (B, 2), (B, 2)])
                         , (['n', 'k', 'K', 'S'], [(K, 3), (K, 3), (K, 3), (K, 3)])
                         , (['n', 'p', 'C', 'S'], [(C, 7), (C, 7), (C, 7)])
                         , (['r', 'e', 'D'], [(D, 1), (D, 1)])
                         , (['b'], [(F, 1), (S, 2)])
                         ]

oneKongHand :: Hand
oneKongHand = [ (['n', 'k', 'B', 'S'], [(B, 4), (B, 4), (B, 4), (B, 4)])
              , (['r', 'c', 'C', 'T'], [(C, 7), (C, 8), (C, 9)])
              , (['r', 'c', 'B', 'S'], [(B, 2), (B, 3), (B, 4)])
              , (['r', 'c', 'K', 'T'], [(K, 1), (K, 2), (K, 3)])
              , (['r', 'e', 'D'], [(D, 1), (D, 1)])
              , (['b'], [(F, 1), (S, 2)])
              ]

twoKongsHand :: Hand
twoKongsHand = [ (['n', 'k', 'B', 'S'], [(B, 4), (B, 4), (B, 4), (B, 4)])
               , (['r', 'k', 'C', 'T'], [(C, 7), (C, 7), (C, 7), (C, 7)])
               , (['r', 'c', 'B', 'S'], [(B, 2), (B, 3), (B, 4)])
               , (['r', 'c', 'K', 'S'], [(K, 3), (K, 4), (K, 5)])
               , (['r', 'e', 'D'], [(D, 1), (D, 1)])
               , (['b'], [(F, 1), (S, 2)])
               ]

threeKongsHand :: Hand
threeKongsHand = [ (['n', 'k', 'C', 'S'], [(B, 4), (B, 4), (B, 4), (B, 4)])
                 , (['r', 'k', 'C', 'S'], [(C, 7), (C, 7), (C, 7), (C, 7)])
                 , (['r', 'k', 'B', 'S'], [(B, 2), (B, 2), (B, 2), (B, 2)])
                 , (['r', 'c', 'K', 'S'], [(K, 3), (K, 4), (K, 5)])
                 , (['r', 'e', 'D'], [(D, 1), (D, 1)])
                 , (['b'], [(F, 1), (S, 2)])
                 ]

fourKongsHand :: Hand
fourKongsHand = [ (['n', 'k', 'B', 'S'], [(B, 4), (B, 4), (B, 4), (B, 4)])
                , (['r', 'k', 'C', 'S'], [(C, 7), (C, 7), (C, 7), (C, 7)])
                , (['r', 'k', 'B', 'S'], [(B, 2), (B, 2), (B, 2), (B, 2)])
                , (['r', 'k', 'K', 'S'], [(K, 3), (K, 3), (K, 3), (K, 3)])
                , (['r', 'e', 'D'], [(D, 1), (D, 1)])
                , (['b'], [(F, 1), (S, 2)])
                ]


-- | 4.0 Similar Sets

threeSimilarChowsHand :: Hand
threeSimilarChowsHand = [ (['c', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
                        , (['r', 'c', 'C', 'S'], [(C, 4), (C, 5), (C, 6)])
                        , (['r', 'p', 'B', 'S'], [(B, 2), (B, 2), (B, 2)])
                        , (['r', 'c', 'K', 'S'], [(K, 4), (K, 5), (K, 6)])
                        , (['r', 'e', 'D'], [(D, 1), (D, 1)])
                        ]

littleThreeSimilarPungsHand :: Hand
littleThreeSimilarPungsHand = [ (['n', 'p', 'B', 'S'], [(B, 4), (B, 4), (B, 4)])
                              , (['r', 'p', 'C', 'S'], [(C, 4), (C, 4), (C, 4)])
                              , (['r', 'c', 'B', 'T'], [(B, 7), (B, 8), (B, 9)])
                              , (['n', 'k', 'B', 'S'], [(B, 2), (B, 2), (B, 2), (B, 2)])
                              , (['r', 'e', 'K', 'S'], [(K, 4), (K, 4)])
                              ]

threeSimilarPungsHand :: Hand
threeSimilarPungsHand = [ (['n', 'p', 'B', 'S'], [(B, 4), (B, 4), (B, 4)])
                        , (['r', 'p', 'C', 'S'], [(C, 4), (C, 4), (C, 4)])
                        , (['r', 'c', 'B', 'T'], [(B, 7), (B, 8), (B, 9)])
                        , (['r', 'p', 'K', 'T'], [(K, 4), (K, 4), (K, 4)])
                        , (['n', 'e', 'D'], [(D, 1), (D, 1)])
                        ]


-- | 5.0 Consecutive Sets

nineTileStraightHand :: Hand
nineTileStraightHand = [ (['n', 'p', 'B', 'S'], [(B, 4), (B, 4), (B, 4)])
                       , (['n', 'c', 'C', 'T'], [(C, 1), (C, 2), (C, 3)])
                       , (['r', 'c', 'C', 'S'], [(C, 4), (C, 5), (C, 6)])
                       , (['r', 'c', 'C', 'T'], [(C, 7), (C, 8), (C, 9)])
                       , (['n', 'e', 'W'], [(W, 4), (W, 4)])
                       ]

threeConsecutivePungsHand1 :: Hand
threeConsecutivePungsHand1 = [ (['n', 'p', 'B', 'S'], [(B, 4), (B, 4), (B, 4)])
                             , (['r', 'p', 'B', 'S'], [(B, 5), (B, 5), (B, 5)])
                             , (['r', 'p', 'B', 'S'], [(B, 6), (B, 6), (B, 6)])
                             , (['r', 'c', 'C', 'T'], [(C, 1), (C, 2), (C, 2)])
                             , (['n', 'e', 'W'], [(W, 4), (W, 4)])
                             ]

threeConsecutivePungsHand2 :: Hand
threeConsecutivePungsHand2 = [ (['n', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 2)])
                             , (['n', 'p', 'B', 'S'], [(B, 4), (B, 4), (B, 4)])
                             , (['r', 'p', 'B', 'S'], [(B, 5), (B, 5), (B, 5)])
                             , (['r', 'p', 'B', 'S'], [(B, 6), (B, 6), (B, 6)])
                             , (['n', 'e', 'W'], [(W, 4), (W, 4)])
                             ]

fourConsecutivePungsHand :: Hand
fourConsecutivePungsHand = [ (['n', 'p', 'B', 'S'], [(B, 4), (B, 4), (B, 4)])
                           , (['r', 'p', 'B', 'S'], [(B, 5), (B, 5), (B, 5)])
                           , (['r', 'p', 'B', 'S'], [(B, 6), (B, 6), (B, 6)])
                           , (['r', 'p', 'B', 'S'], [(B, 7), (B, 7), (B, 7)])
                           , (['n', 'e', 'C', 'S'], [(C, 4), (C, 4)])
                           ]

threeMothersHand :: Hand
threeMothersHand = [ (['n', 'p', 'B', 'S'], [(B, 4), (B, 4), (B, 4)])
                   , (['r', 'p', 'B', 'S'], [(B, 5), (B, 5), (B, 5)])
                   , (['r', 'p', 'B', 'S'], [(B, 6), (B, 6), (B, 6)])
                   , (['r', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
                   , (['n', 'e', 'C', 'S'], [(C, 4), (C, 4)])
                   ]


-- | 6.0 Suit Patterns

mixedOneSuitHand1 :: Hand
mixedOneSuitHand1 = [ (['n', 'p', 'B', 'S'], [(B, 3), (B, 3), (B, 3)])
                    , (['r', 'p', 'W'], [(W, 4), (W, 4), (W, 4)])
                    , (['r', 'p', 'D'], [(D, 1), (D, 1), (D, 1)])
                    , (['r', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
                    , (['n', 'e', 'B', 'S'], [(B, 8), (B, 8)])
                    ]

mixedOneSuitHand2 :: Hand
mixedOneSuitHand2 = [ (['n', 'e', 'B', 'T'], [(B, 1), (B, 1)])
                    , (['n', 'e', 'B', 'S'], [(B, 3), (B, 3)])
                    , (['n', 'e', 'B', 'S'], [(B, 5), (B, 5)])
                    , (['n', 'e', 'B', 'S'], [(B, 6), (B, 6)])
                    , (['n', 'e', 'W'], [(W, 2), (W, 2)])
                    , (['n', 'e', 'W'], [(W, 4), (W, 4)])
                    , (['r', 'e', 'D'], [(D, 1), (D, 1)])
                    ]

pureOneSuitHand1 :: Hand
pureOneSuitHand1 = [ (['n', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                   , (['r', 'p', 'B', 'S'], [(B, 4), (B, 4), (B, 4)])
                   , (['r', 'c', 'B', 'S'], [(B, 6), (B, 7), (B, 8)])
                   , (['r', 'c', 'B', 'S'], [(B, 5), (B, 6), (B, 7)])
                   , (['n', 'e', 'B', 'T'], [(B, 9), (B, 9)])
                   ]

pureOneSuitHand2 :: Hand
pureOneSuitHand2 = [ (['n', 'e', 'B', 'T'], [(B, 1), (B, 1)])
                   , (['n', 'e', 'B', 'S'], [(B, 3), (B, 3)])
                   , (['n', 'e', 'B', 'S'], [(B, 4), (B, 4)])
                   , (['n', 'e', 'B', 'S'], [(B, 5), (B, 5)])
                   , (['n', 'e', 'B', 'S'], [(B, 6), (B, 6)])
                   , (['n', 'e', 'B', 'S'], [(B, 8), (B, 8)])
                   , (['r', 'e', 'B', 'T'], [(B, 9), (B, 9)])
                   ]

littleTerminalClubHand :: Hand
littleTerminalClubHand = [ (['n', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                         , (['r', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                         , (['r', 'c', 'B', 'T'], [(B, 7), (B, 8), (B, 9)])
                         , (['r', 'c', 'B', 'T'], [(B, 7), (B, 8), (B, 9)])
                         , (['n', 'e', 'S'], [(B, 5), (B, 5)])
                         ]

bigTerminalClubHand :: Hand
bigTerminalClubHand = [ (['n', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                      , (['r', 'p', 'B', 'T'], [(B, 1), (B, 1), (B, 1)])
                      , (['r', 'c', 'B', 'T'], [(B, 7), (B, 8), (B, 9)])
                      , (['r', 'p', 'B', 'T'], [(B, 9), (B, 9), (B, 9)])
                      , (['n', 'e', 'S'], [(B, 5), (B, 5)])
                      ]

nineGatesHand :: Hand
nineGatesHand = [ ( ['r', 'h', 'B', 'T', 'S']
                  , [(B, 1), (B, 1)
                     (B, 1), (B, 2), (B, 3)
                     (B, 4), (B, 5), (B, 6)
                     (B, 6), (B, 7), (B, 8)
                     (B, 9), (B, 9), (B, 9)
                    ]
                  )
                , (['b'], [(F, 3)])
                ]


-- | 7.0 Terminal Tiles

twoTailedTerminalChowsHand1 :: Hand
twoTailedTerminalChowsHand1 = [ (['n', 'p', 'C', 'S'], [(C, 3), (C, 3), (C, 3)])
                              , (['n', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                              , (['r', 'c', 'B', 'T'], [(B, 7), (B, 8), (B, 9)])
                              , (['r', 'c', 'K', 'S'], [(K, 5), (K, 6), (K, 7)])
                              , (['n', 'e', 'W'], [(W, 4), (W, 4)])
                              ]

twoTailedTerminalChowsHand2 :: Hand
twoTailedTerminalChowsHand2 = [ (['n', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                              , (['n', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                              , (['r', 'c', 'B', 'T'], [(B, 7), (B, 8), (B, 9)])
                              , (['r', 'c', 'K', 'S'], [(K, 5), (K, 6), (K, 7)])
                              , (['n', 'e', 'W'], [(W, 4), (W, 4)])
                              ]

twoTailedTerminalChowsHand3 :: Hand
twoTailedTerminalChowsHand3 = [ (['n', 'c', 'C', 'T'], [(C, 1), (C, 2), (C, 3)])
                              , (['n', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                              , (['r', 'c', 'B', 'T'], [(B, 7), (B, 8), (B, 9)])
                              , (['r', 'c', 'C', 'T'], [(C, 7), (C, 8), (C, 9)])
                              , (['n', 'e', 'K'], [(K, 4), (K, 4)])
                              ]

twoTailedTerminalPungsHand1 :: Hand
twoTailedTerminalPungsHand1 = [ (['n', 'p', 'C', 'S'], [(C, 3), (C, 3), (C, 3)])
                              , (['n', 'p', 'B', 'T'], [(B, 1), (B, 1), (B, 1)])
                              , (['r', 'p', 'B', 'T'], [(B, 9), (B, 9), (B, 9)])
                              , (['r', 'c', 'K', 'S'], [(K, 5), (K, 6), (K, 7)])
                              , (['n', 'e', 'W'], [(W, 4), (W, 4)])
                              ]

twoTailedTerminalPungsHand2 :: Hand
twoTailedTerminalPungsHand2 = [ (['n', 'p', 'C', 'T'], [(C, 1), (C, 1), (C, 1)])
                              , (['n', 'p', 'B', 'T'], [(B, 1), (B, 1), (B, 1)])
                              , (['r', 'p', 'B', 'T'], [(B, 9), (B, 9), (B, 9)])
                              , (['r', 'p', 'C', 'T'], [(C, 9), (C, 9), (C, 9)])
                              , (['n', 'e', 'K'], [(K, 4), (K, 4)])
                              ]

littleBoundlessMountainHand1 :: Hand
littleBoundlessMountainHand1 = [ (['n', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                               , (['r', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                               , (['r', 'c', 'B', 'T'], [(B, 7), (B, 8), (B, 9)])
                               , (['r', 'c', 'B', 'T'], [(B, 7), (B, 8), (B, 9)])
                               , (['n', 'e', 'B', 'T'], [(B, 9), (B, 9)])
                               ]

littleBoundlessMountainHand2 :: Hand
littleBoundlessMountainHand2 = [ (['n', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                               , (['r', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                               , (['r', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                               , (['r', 'c', 'B', 'T'], [(B, 7), (B, 8), (B, 9)])
                               , (['n', 'e', 'B', 'T'], [(B, 9), (B, 9)])
                               ]

bigBoundlessMountainHand1 :: Hand
bigBoundlessMountainHand1 = [ (['n', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                            , (['r', 'p', 'B', 'T'], [(B, 1), (B, 1), (B, 1)])
                            , (['r', 'c', 'B', 'T'], [(B, 7), (B, 8), (B, 9)])
                            , (['r', 'c', 'B', 'T'], [(B, 7), (B, 8), (B, 9)])
                            , (['n', 'e', 'B', 'T'], [(B, 9), (B, 9)])
                            ]

bigBoundlessMountainHand2 :: Hand
bigBoundlessMountainHand2 = [ (['n', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                            , (['r', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                            , (['r', 'c', 'B', 'T'], [(B, 7), (B, 8), (B, 9)])
                            , (['r', 'p', 'B', 'T'], [(B, 9), (B, 9), (B, 9)])
                            , (['n', 'e', 'B', 'T'], [(B, 1), (B, 1)])
                            ]

mixedLesserTerminalHand :: Hand
mixedLesserTerminalHand = [ (['n', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                          , (['r', 'p', 'C', 'T'], [(C, 1), (C, 1), (C, 1)])
                          , (['r', 'c', 'K', 'T'], [(K, 7), (K, 8), (K, 9)])
                          , (['r', 'p', 'D'], [(D, 2), (D, 2), (D, 2)])
                          , (['n', 'e', 'W'], [(W, 1), (W, 1)])
                          ]

pureLesserTerminalHand :: Hand
pureLesserTerminalHand = [ (['n', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                         , (['r', 'p', 'C', 'T'], [(C, 1), (C, 1), (C, 1)])
                         , (['r', 'c', 'K', 'T'], [(K, 7), (K, 8), (K, 9)])
                         , (['r', 'p', 'B', 'T'], [(B, 9), (B, 9), (B, 9)])
                         , (['n', 'e', 'K', 'T'], [(K, 1), (K, 1)])
                         ]

mixedGreaterTerminalHand1 :: Hand
mixedGreaterTerminalHand1 = [ (['n', 'p', 'K', 'T'], [(K, 1), (K, 1), (K, 1)])
                            , (['r', 'p', 'C', 'T'], [(C, 9), (C, 9), (C, 9)])
                            , (['r', 'p', 'B', 'T'], [(B, 9), (B, 9), (B, 9)])
                            , (['r', 'p', 'D'], [(D, 2), (D, 2), (D, 2)])
                            , (['n', 'e', 'W'], [(W, 1), (W, 1)])
                            ]

mixedGreaterTerminalHand2 :: Hand
mixedGreaterTerminalHand2 = [ (['n', 'e', 'C', 'T'], [(C, 9), (C, 9)])
                            , (['n', 'e', 'B', 'T'], [(B, 1), (B, 1)])
                            , (['n', 'e', 'B', 'T'], [(B, 9), (B, 9)])
                            , (['n', 'e', 'K', 'T'], [(K, 1), (K, 1)])
                            , (['n', 'e', 'W'], [(W, 2), (W, 2)])
                            , (['n', 'e', 'W'], [(W, 4), (W, 4)])
                            , (['r', 'e', 'D'], [(D, 1), (D, 1)])
                            ]

pureGreaterTerminalHand1 :: Hand
pureGreaterTerminalHand1 = [ (['n', 'p', 'C', 'T'], [(C, 1), (C, 1), (C, 1)])
                           , (['r', 'p', 'C', 'T'], [(C, 9), (C, 9), (C, 9)])
                           , (['r', 'p', 'B', 'T'], [(B, 1), (B, 1), (B, 1)])
                           , (['r', 'p', 'B', 'T'], [(B, 9), (B, 9), (B, 9)])
                           , (['n', 'e', 'K', 'T'], [(K, 9), (K, 9)])
                           ]

pureGreaterTermimalHand2 :: Hand
pureGreaterTermimalHand2 = [ (['n', 'e', 'C', 'T'], [(C, 1), (C, 1)])
                           , (['n', 'e', 'C', 'T'], [(C, 1), (C, 1)])
                           , (['n', 'e', 'C', 'T'], [(C, 9), (C, 9)])
                           , (['n', 'e', 'B', 'T'], [(B, 1), (B, 1)])
                           , (['n', 'e', 'B', 'T'], [(B, 9), (B, 9)])
                           , (['n', 'e', 'K', 'T'], [(K, 1), (K, 1)])
                           , (['r', 'e', 'K', 'T'], [(K, 9), (K, 9)])
                           ]


-- | 8.0 Honor Tiles

dragonPungHand :: Hand
dragonPungHand = [ (['n', 'p', 'B', 'S'], [(B, 2), (B, 2), (B, 2)])
                 , (['n', 'c', 'K', 'S'], [(K, 3), (K, 4), (K, 5)])
                 , (['r', 'c', 'C', 'T'], [(C, 1), (C, 2), (C, 3)])
                 , (['r', 'p', 'D'], [(D, 1), (D, 1), (D, 1)])
                 , (['r', 'e', 'D'], [(D, 3), (D, 3)])
                 , (['b'], [(F, 4)])
                 ]

seatWindHand :: Hand
seatWindHand = [ (['n', 'p', 'B', 'S'], [(B, 2), (B, 2), (B, 2)])
               , (['n', 'c', 'K', 'S'], [(K, 3), (K, 4), (K, 5)])
               , (['r', 'c', 'C', 'T'], [(C, 1), (C, 2), (C, 3)])
               , (['r', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
               , (['r', 'e', 'D'], [(D, 3), (D, 3)])
               , (['b'], [(F, 4)])
               , ]

littleThreeWindsHand :: Hand
littleThreeWindsHand = [ (['n', 'p', 'C', 'S'], [(C, 2), (C, 2), (C, 2)])
                       , (['n', 'p', 'W'], [(W, 3), (W, 3), (W, 3)])
                       , (['r', 'c', 'K', 'T'], [(K, 1), (K, 2), (K, 3)])
                       , (['r', 'p', 'W'], [(W, 4), (W, 4), (W, 4)])
                       , (['r', 'e', 'W'], [(W, 1), (W, 1)])
                       , (['b'], [(F, 4)])
                       ]

bigThreeWindsHand :: Hand
bigThreeWindsHand = [ (['n', 'p', 'W'], [(W, 2), (W, 2), (W, 2)])
                    , (['n', 'p', 'W'], [(W, 3), (W, 3), (W, 3)])
                    , (['r', 'c', 'K', 'T'], [(K, 1), (K, 2), (K, 3)])
                    , (['r', 'p', 'W'], [(W, 4), (W, 4), (W, 4)])
                    , (['r', 'e', 'C', 'S'], [(C, 4), (C, 4)])
                    , (['b'], [(F, 4)])
                    ]

littleFourWindsHand :: Hand
littleFourWindsHand = [ (['n', 'p', 'W'], [(W, 2), (W, 2), (W, 2)])
                      , (['n', 'c', 'K', 'T'], [(K, 1), (K, 2), (K, 3)])
                      , (['r', 'p', 'W'], [(W, 3), (W, 3), (W, 3)])
                      , (['r', 'p', 'W'], [(W, 4), (W, 4), (W, 4)])
                      , (['r', 'e', 'W'], [(W, 1), (W, 1)])
                      , (['b'], [(F, 4)])
                      ]

bigFourWindsHand :: Hand
bigFourWindsHand = [ (['n', 'p', 'W'], [(W, 2), (W, 2), (W, 2)])
                   , (['r', 'p', 'W'], [(W, 3), (W, 3), (W, 3)])
                   , (['r', 'p', 'W'], [(W, 4), (W, 4), (W, 4)])
                   , (['r', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
                   , (['r', 'e', 'C', 'S'], [(C, 4), (C, 4)])
                   , (['b'], [(F, 4)])
                   ]

littleThreeDragonsHand :: Hand
littleThreeDragonsHand = [ (['n', 'p', 'D'], [(D, 2), (D, 2), (D, 2)])
                         , (['n', 'c', 'K', 'S'], [(K, 3), (K, 4), (K, 5)])
                         , (['r', 'p', 'C', 'T'], [(C, 1), (C, 1), (C, 1)])
                         , (['r', 'p', 'D'], [(D, 1), (D, 1), (D, 1)])
                         , (['r', 'e', 'D'], [(D, 3), (D, 3)])
                         ]

bigThreeDragonsHand :: Hand
bigThreeDragonsHand = [ (['n', 'p', 'D'], [(D, 2), (D, 2), (D, 2)])
                      , (['n', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                      , (['r', 'p', 'D'], [(D, 1), (D, 1), (D, 1)])
                      , (['r', 'p', 'D'], [(D, 3), (D, 3), (D, 3)])
                      , (['r', 'e', 'C', 'S'], [(C, 3), (C, 3)])
                      ]

allHonorsHand1 :: Hand
allHonorsHand1 = [ (['n', 'p', 'D'], [(D, 2), (D, 2), (D, 2)])
                 , (['n', 'p', 'W'], [(W, 3), (W, 3), (W, 3)])
                 , (['r', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
                 , (['r', 'p', 'D'], [(D, 1), (D, 1), (D, 1)])
                 , (['r', 'e', 'W'], [(W, 4), (W, 4)])
                 ]

allHonorsHand2 :: Hand
allHonorsHand2 = [ (['n', 'e', 'W'], [(W, 1), (W, 1)])
                 , (['n', 'e', 'W'], [(W, 2), (W, 2)])
                 , (['n', 'e', 'W'], [(W, 2), (W, 2)])
                 , (['n', 'e', 'D'], [(D, 1), (D, 1)])
                 , (['n', 'e', 'D'], [(D, 2), (D, 2)])
                 , (['n', 'e', 'D'], [(D, 3), (D, 3)])
                 , (['r', 'e', 'W'], [(W, 4), (W, 4)])
                 ]

allHonorPairsHand :: Hand
allHonorPairsHand = [ (['n', 'e', 'W'], [(W, 1), (W, 1)])
                    , (['n', 'e', 'W'], [(W, 2), (W, 2)])
                    , (['n', 'e', 'W'], [(W, 3), (W, 3)])
                    , (['n', 'e', 'D'], [(D, 1), (D, 1)])
                    , (['n', 'e', 'D'], [(D, 2), (D, 2)])
                    , (['n', 'e', 'D'], [(D, 3), (D, 3)])
                    , (['r', 'e', 'W'], [(W, 4), (W, 4)])
                    ]


-- | 9.0 Seven Pairs

sevenPairsHand1 :: Hand
sevenPairsHand1 = [ (['n', 'e', 'C', 'T'], [(C, 1), (C, 1)])
                  , (['n', 'e', 'C', 'S'], [(C, 7), (C, 7)])
                  , (['n', 'e', 'B', 'S'], [(B, 3), (B, 3)])
                  , (['n', 'e', 'K', 'S'], [(K, 6), (K, 6)])
                  , (['n', 'e', 'W'], [(W, 4), (W, 4)])
                  , (['n', 'e', 'D'], [(D, 2), (D, 2)])
                  , (['r', 'e', 'D'], [(D, 3), (D, 3)])
                  ]

sevenPairsHand2 :: Hand
sevenPairsHand2 = [ (['n', 'e', 'C', 'T'], [(C, 1), (C, 1)])
                  , (['n', 'e', 'C', 'T'], [(C, 1), (C, 1)])
                  , (['n', 'e', 'B', 'S'], [(B, 3), (B, 3)])
                  , (['n', 'e', 'K', 'S'], [(K, 6), (K, 6)])
                  , (['n', 'e', 'W'], [(W, 4), (W, 4)])
                  , (['n', 'e', 'D'], [(D, 2), (D, 2)])
                  , (['r', 'e', 'D'], [(D, 3), (D, 3)])
                  ]

sevenPairsHand3 :: Hand
sevenPairsHand3 = [ (['n', 'e', 'C', 'T'], [(C, 1), (C, 1)])
                  , (['n', 'e', 'C', 'T'], [(C, 1), (C, 1)])
                  , (['n', 'e', 'B', 'S'], [(B, 3), (B, 3)])
                  , (['n', 'e', 'B', 'S'], [(B, 3), (B, 3)])
                  , (['n', 'e', 'W'], [(W, 4), (W, 4)])
                  , (['n', 'e', 'D'], [(D, 2), (D, 2)])
                  , (['r', 'e', 'D'], [(D, 3), (D, 3)])
                  ]

sevenPairsHand4 :: Hand
sevenPairsHand4 = [ (['n', 'e', 'C', 'T'], [(C, 1), (C, 1)])
                  , (['n', 'e', 'C', 'T'], [(C, 1), (C, 1)])
                  , (['n', 'e', 'B', 'S'], [(B, 3), (B, 3)])
                  , (['n', 'e', 'B', 'S'], [(B, 3), (B, 3)])
                  , (['n', 'e', 'W'], [(W, 4), (W, 4)])
                  , (['n', 'e', 'W'], [(W, 4), (W, 4)])
                  , (['r', 'e', 'D'], [(D, 3), (D, 3)])
                  ]

sevenShiftedPairsHand1 :: Hand
sevenShiftedPairsHand1 = [ (['n', 'e', 'C', 'T'], [(C, 1), (C, 1)])
                         , (['n', 'e', 'C', 'S'], [(C, 2), (C, 2)])
                         , (['n', 'e', 'C', 'S'], [(C, 3), (C, 3)])
                         , (['n', 'e', 'C', 'S'], [(C, 4), (C, 4)])
                         , (['n', 'e', 'C', 'S'], [(C, 5), (C, 5)])
                         , (['n', 'e', 'C', 'S'], [(C, 6), (C, 6)])
                         , (['r', 'e', 'C', 'S'], [(C, 7), (C, 7)])
                         ]

sevenShiftedPairsHand2 :: Hand
sevenShiftedPairsHand2 = [ (['n', 'e', 'K', 'S'], [(K, 3), (K, 3)])
                         , (['n', 'e', 'K', 'S'], [(K, 4), (K, 4)])
                         , (['n', 'e', 'K', 'S'], [(K, 5), (K, 5)])
                         , (['n', 'e', 'K', 'S'], [(K, 6), (K, 6)])
                         , (['n', 'e', 'K', 'S'], [(K, 7), (K, 7)])
                         , (['n', 'e', 'K', 'S'], [(K, 8), (K, 8)])
                         , (['r', 'e', 'K', 'T'], [(K, 9), (K, 9)])
                         ]

grandChariotHand :: Hand
grandChariotHand = [ (['n', 'e', 'C', 'S'], [(C, 2), (C, 2)])
                   , (['n', 'e', 'C', 'S'], [(C, 3), (C, 3)])
                   , (['n', 'e', 'C', 'S'], [(C, 4), (C, 4)])
                   , (['n', 'e', 'C', 'S'], [(C, 5), (C, 5)])
                   , (['n', 'e', 'C', 'S'], [(C, 6), (C, 6)])
                   , (['n', 'e', 'C', 'S'], [(C, 7), (C, 7)])
                   , (['r', 'e', 'C', 'S'], [(C, 8), (C, 8)])
                   ]

bambooForestHand :: Hand
bambooForestHand = [ (['n', 'e', 'B', 'S'], [(B, 2), (B, 2)])
                   , (['n', 'e', 'B', 'S'], [(B, 3), (B, 3)])
                   , (['n', 'e', 'B', 'S'], [(B, 4), (B, 4)])
                   , (['n', 'e', 'B', 'S'], [(B, 5), (B, 5)])
                   , (['n', 'e', 'B', 'S'], [(B, 6), (B, 6)])
                   , (['n', 'e', 'B', 'S'], [(B, 7), (B, 7)])
                   , (['r', 'e', 'B', 'S'], [(B, 8), (B, 8)])
                   ]

numberNeighborhoodHand :: Hand
numberNeighborhoodHand = [ (['n', 'e', 'K', 'S'], [(K, 2), (K, 2)])
                         , (['n', 'e', 'K', 'S'], [(K, 3), (K, 3)])
                         , (['n', 'e', 'K', 'S'], [(K, 4), (K, 4)])
                         , (['n', 'e', 'K', 'S'], [(K, 5), (K, 5)])
                         , (['n', 'e', 'K', 'S'], [(K, 6), (K, 6)])
                         , (['n', 'e', 'K', 'S'], [(K, 7), (K, 7)])
                         , (['r', 'e', 'K', 'S'], [(K, 8), (K, 8)])
                         ]


-- | 10.0 Color Hands

allGreenHand :: Hand
allGreenHand = [ (['n', 'p', 'B', 'S'], [(B, 2), (B, 2), (B, 2)])
               , (['r', 'c', 'B', 'S'], [(B, 2), (B, 3), (B, 4)])
               , (['r', 'p', 'B', 'S'], [(B, 6), (B, 6), (B, 6)])
               , (['r', 'p', 'D'], [(D, 2), (D, 2), (D, 2)])
               , (['r', 'e', 'B', 'S'], [(B, 8), (B, 8)])
               ]

allRedHand :: Hand
allRedHand = [ (['n', 'p', 'B', 'S'], [(B, 1), (B, 1), (B, 1)])
             , (['r', 'p', 'B', 'S'], [(B, 5), (B, 5), (B, 5)])
             , (['r', 'p', 'B', 'S'], [(B, 9), (B, 9), (B, 9)])
             , (['r', 'p', 'D'], [(D, 1), (D, 1), (D, 1)])
             , (['r', 'e', 'B', 'S'], [(B, 7), (B, 7)])
             ]

allBlueHand :: Hand
allBlueHand = [ (['n', 'p', 'W'], [(W, 2), (W, 2), (W, 2)])
              , (['r', 'p', 'W'], [(W, 3), (W, 3), (W, 3)])
              , (['r', 'p', 'W'], [(C, 8), (C, 8), (C, 8)])
              , (['r', 'p', 'W'], [(D, 3), (D, 3), (D, 3)])
              , (['r', 'e', 'W'], [(W, 4), (W, 4)])
              , (['b'], [(F, 4)])
              ]


-- | 11.0 Irregular Hands

thirteenOrphanHand :: Hand
thirteenOrphanHand = [ ( ['r', 'h', 'B', 'T', 'W', 'D']
                       , [ (C, 1), (C, 9), (B, 1), (B, 9), (K, 1), (K, 9)
                           (W, 1), (W, [(C, 1), (C, 2), (W, 3), (W, 4)
                           (D, 1), (D, [(C, 1), (C, 2), (D, 2), (D, 3)
                         ]
                       )
                     , (['b'], [(F, 1, [(C, 1), ('C')])
                     ]


-- | 12.0 Incidental Bonuses

finalDrawHand :: Hand []
finalDrawHand = [] 

finalDiscardHand :: Hand []
finalDiscardHand = []

winOnKongHand :: Hand []
winOnKongHand = []

winOnBonusTileHand :: Hand []
winOnBonusTileHand = []

robbingKongHand :: Hand []
robbingKongHand = []

blessingOfHeavenHand :: Hand []
blessingOfHeavenHand = []

blessingOfEarthHand :: Hand []
blessingOfEarthHand = []


-- | 13.0 Bonus Tiles

nonSeatFlowerHand :: Hand
nonSeatFlowerHand = [ (['r', 'c', 'C', 'T'], [(C, 7), (C, 8), (C, 9)])
                    , (['r', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
                    , (['r', 'c', 'K', 'S'], [(K, 1), (K, 2), (K, 3)])
                    , (['r', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
                    , (['c', 'e', 'W'], [(W, 3), (W, 3)])
                    , (['b'], [(F, 3), (S, 2)])
                    ]

nonSeatSeasonHand :: Hand
nonSeatSeasonHand = [ (['r', 'c', 'C', 'T'], [(C, 7), (C, 8), (C, 9)])
                    , (['r', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
                    , (['r', 'c', 'K', 'S'], [(K, 1), (K, 2), (K, 3)])
                    , (['r', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
                    , (['c', 'e', 'W'], [(W, 3), (W, 3)])
                    , (['b'], [(F, 2), (S, 4)])
                    ]

seatFlowerHand :: Hand
seatFlowerHand = [ (['r', 'c', 'C', 'T'], [(C, 7), (C, 8), (C, 9)])
                 , (['r', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
                 , (['r', 'c', 'K', 'S'], [(K, 1), (K, 2), (K, 3)])
                 , (['r', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
                 , (['c', 'e', 'W'], [(W, 3), (W, 3)])
                 , (['b'], [(F, 1), (S, 2)])
                 ]

seatSeasonHand :: Hand
seatSeasonHand = [ (['r', 'c', 'C', 'T'], [(C, 7), (C, 8), (C, 9)])
                 , (['r', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
                 , (['r', 'c', 'K', 'S'], [(K, 1), (K, 2), (K, 3)])
                 , (['r', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
                 , (['c', 'e', 'W'], [(W, 3), (W, 3)])
                 , (['b'], [(F, 2), (S, 1)])
                 ]

fourFlowersHand :: Hand
fourFlowersHand = [ (['r', 'c', 'C', 'T'], [(C, 7), (C, 8), (C, 9)])
                  , (['r', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
                  , (['r', 'c', 'K', 'S'], [(K, 1), (K, 2), (K, 3)])
                  , (['r', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
                  , (['c', 'e', 'W'], [(W, 3), (W, 3)])
                  , (['b'], [(F, 1), (F, 2), (F, 3), (F, 4)])
                  ]

fourSeasonsHand :: Hand
fourSeasonsHand = [ (['r', 'c', 'C', 'T'], [(C, 7), (C, 8), (C, 9)])
                  , (['r', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
                  , (['r', 'c', 'K', 'S'], [(K, 1), (K, 2), (K, 3)])
                  , (['r', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
                  , (['c', 'e', 'W'], [(W, 3), (W, 3)])
                  , (['b'], [(S, 1), (S, 2), (S, 3), (S, 4)])
                  ]

allBonusTileHand :: Hand
allBonusTileHand = [ (['r', 'c', 'C', 'T'], [(C, 7), (C, 8), (C, 9)])
                   , (['r', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
                   , (['r', 'c', 'K', 'S'], [(K, 1), (K, 2), (K, 3)])
                   , (['l'], [(B, 4), (B, 5), (B, 6), (B, 7)])
                   , (['b'], [ (F, 1), (F, 2), (F, 3), (F, 4)
                             , (S, 1), (S, 2), (S, 3), (S, 4)
                             ]
                     )
                   ]
