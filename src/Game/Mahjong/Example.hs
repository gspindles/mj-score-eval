{- SLOW PROGRESS IN CONVERTING THESE OVER TO NEW DATA FORMAT -}
{- MAINLY BECAUSE THIS DEPENDS ON IMPLEMENTATION OF ALL OTHER TO FINALIZE FIRST }

-- |
-- Module      :  Game.Mahjong.Example
-- Copyright   :  Joseph Ching 2015
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

chickenEx :: Hand
chickenEx = Hand
  { getMelds = [ makeChow  Revealed  $ Coin Seven 
               , makePung  Revealed  $ Wind East 
               , makeChow  Revealed  $ Character One 
               , makeChow  Concealed $ Bamboo Four
               ]
  , lastMeld =   makeEye   Revealed  $ Wind West
  , bonusH   =   makeBonus           $ [Flower One, Season Two]
  }

allChowsEx :: Hand 
allChowsEx = Hand
  { getMelds = [ makeChow  Revealed  $ Bamboo Four
               , makeChow  Revealed  $ Coin Seven 
               , makeChow  Revealed  $ Bamboo Two
               , makeChow  Concealed $ Character One
               ]
  , lastMeld =   makeEye   Concealed $ Dragon Red
  , bonusH   =   makeBonus           $ [Flower One, Season Two]
  }

concealedEx :: Hand 
concealedEx = Hand
  { getMelds = [ makeChow  Concealed $ Coin Seven
               , makePung  Concealed $ Wind East 
               , makeChow  Concealed $ Character One 
               , makeChow  Concealed $ Bamboo Four 
               ]
  , lastMeld =   makeEye   Revealed  $ Wind West
  , bonusH   =   makeBonus           $ [Flower One, Season Two]
  }

selfDrawnEx :: Hand
selfDrawnEx = []

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

AllTypesEx :: Hand
AllTypesEx = [ (['r', 'c', 'C', 'T'], [(C, 7), (C, 8), (C, 9)])
               , (['r', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
               , (['r', 'c', 'K', 'T'], [(K, 1), (K, 2), (K, 3)])
               , (['r', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
               , (['r', 'e', 'D'], [(D, 3), (D, 3)])
               , (['b'], [(F, 1)])
               ]


-- | 2.0 Identical Chows

twoIdenticalChowsEx :: Hand
twoIdenticalChowsEx = [ (['n', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
                        , (['r', 'c', 'C', 'S'], [(C, 6), (C, 7), (C, 8)])
                        , (['r', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
                        , (['r', 'c', 'C', 'S'], [(C, 6), (C, 7), (C, 8)])
                        , (['r', 'e', 'D'], [(D, 3), (D, 3)])
                        ]

twoIdenticalChowsTwiceEx :: Hand
twoIdenticalChowsTwiceEx = [ (['n', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
                             , (['r', 'c', 'C', 'S'], [(C, 6), (C, 7), (C, 8)])
                             , (['r', 'p', 'W'], [(B, 4), (B, 5), (B, 6)])
                             , (['r', 'c', 'C', 'S'], [(C, 6), (C, 7), (C, 8)])
                             , (['r', 'e', 'D'], [(D, 3), (D, 3)])
                             ]

threeIdenticalChowsEx :: Hand
threeIdenticalChowsEx = [ (['n', 'c', 'C', 'S'], [(C, 6), (C, 7), (C, 8)])
                          , (['r', 'c', 'C', 'S'], [(C, 6), (C, 7), (C, 8)])
                          , (['r', 'p', 'B', 'W'], [(B, 4), (B, 5), (B, 6)])
                          , (['r', 'c', 'C', 'S'], [(C, 6), (C, 7), (C, 8)])
                          , (['r', 'e', 'D'], [(D, 3), (D, 3)])
                          ]

fourIdenticalChowsEx :: Hand
fourIdenticalChowsEx = [ (['n', 'c', 'C', 'S'], [(C, 6), (C, 7), (C, 8)])
                         , (['r', 'c', 'C', 'S'], [(C, 6), (C, 7), (C, 8)])
                         , (['r', 'p', 'C', 'W'], [(C, 6), (C, 7), (C, 8)])
                         , (['r', 'c', 'C', 'S'], [(C, 6), (C, 7), (C, 8)])
                         , (['r', 'e', 'D'], [(D, 3), (D, 3)])
                         ]


-- | 3.0 Pungs and Kongs

allPungsEx :: Hand
allPungsEx = [ (['n', 'k', 'B', 'S'], [(B, 4), (B, 4), (B, 4), (B, 4)])
               , (['r', 'p', 'C', 'S'], [(C, 7), (C, 7), (C, 7)])
               , (['r', 'p', 'B', 'S'], [(B, 2), (B, 2), (B, 2)])
               , (['r', 'p', '­K', 'S'], [(K, 3), (K, 3), (K, 3)])
               , (['n', 'e', 'D'], [(D, 1), (D, 1)])
               ]

twoConcealedPungsEx :: Hand
twoConcealedPungsEx = [ (['n', 'k', 'B', 'S'], [(B, 4), (B, 4), (B, 4), (B, 4)])
                        , (['n', 'p', 'B', 'S'], [(B, 2), (B, 2), (B, 2)])
                        , (['r', 'p', 'C', 'S'], [(C, 7), (C, 7), (C, 7)])
                        , (['r', 'k', 'K', 'S'], [(K, 3), (K, 3), (K, 3), (K, 3)])
                        , (['n', 'e', 'D'], [(D, 1), (D, 1)])
                        ]

threeConcealedPungsEx :: Hand
threeConcealedPungsEx = [ (['n', 'k', 'B', 'S'], [(B, 4), (B, 4), (B, 4), (B, 4)])
                          , (['n', 'p', 'B', 'S'], [(B, 1), (B, 2), (B, 2)])
                          , (['n', 'k', 'K', 'S'], [(K, 3), (K, 3), (K, 3), (K, 3)])
                          , (['r', 'p', 'C', 'S'], [(C, 7), (C, 7), (C, 7)])
                          , (['r', 'e', 'D'], [(D, 1), (D, 1)])
                          ]

fourConcealedPunsgEx :: Hand
fourConcealedPunsgEx = [ (['n', 'k', 'B', 'S'], [(B, 4), (B, 4), (B, 4), (B, 4)])
                         , (['n', 'p', 'B', 'S'], [(B, 2), (B, 2), (B, 2)])
                         , (['n', 'k', 'K', 'S'], [(K, 3), (K, 3), (K, 3), (K, 3)])
                         , (['n', 'p', 'C', 'S'], [(C, 7), (C, 7), (C, 7)])
                         , (['r', 'e', 'D'], [(D, 1), (D, 1)])
                         , (['b'], [(F, 1), (S, 2)])
                         ]

oneKongEx :: Hand
oneKongEx = [ (['n', 'k', 'B', 'S'], [(B, 4), (B, 4), (B, 4), (B, 4)])
              , (['r', 'c', 'C', 'T'], [(C, 7), (C, 8), (C, 9)])
              , (['r', 'c', 'B', 'S'], [(B, 2), (B, 3), (B, 4)])
              , (['r', 'c', 'K', 'T'], [(K, 1), (K, 2), (K, 3)])
              , (['r', 'e', 'D'], [(D, 1), (D, 1)])
              , (['b'], [(F, 1), (S, 2)])
              ]

twoKongsEx :: Hand
twoKongsEx = [ (['n', 'k', 'B', 'S'], [(B, 4), (B, 4), (B, 4), (B, 4)])
               , (['r', 'k', 'C', 'T'], [(C, 7), (C, 7), (C, 7), (C, 7)])
               , (['r', 'c', 'B', 'S'], [(B, 2), (B, 3), (B, 4)])
               , (['r', 'c', 'K', 'S'], [(K, 3), (K, 4), (K, 5)])
               , (['r', 'e', 'D'], [(D, 1), (D, 1)])
               , (['b'], [(F, 1), (S, 2)])
               ]

threeKongsEx :: Hand
threeKongsEx = [ (['n', 'k', 'C', 'S'], [(B, 4), (B, 4), (B, 4), (B, 4)])
                 , (['r', 'k', 'C', 'S'], [(C, 7), (C, 7), (C, 7), (C, 7)])
                 , (['r', 'k', 'B', 'S'], [(B, 2), (B, 2), (B, 2), (B, 2)])
                 , (['r', 'c', 'K', 'S'], [(K, 3), (K, 4), (K, 5)])
                 , (['r', 'e', 'D'], [(D, 1), (D, 1)])
                 , (['b'], [(F, 1), (S, 2)])
                 ]

fourKongsEx :: Hand
fourKongsEx = [ (['n', 'k', 'B', 'S'], [(B, 4), (B, 4), (B, 4), (B, 4)])
                , (['r', 'k', 'C', 'S'], [(C, 7), (C, 7), (C, 7), (C, 7)])
                , (['r', 'k', 'B', 'S'], [(B, 2), (B, 2), (B, 2), (B, 2)])
                , (['r', 'k', 'K', 'S'], [(K, 3), (K, 3), (K, 3), (K, 3)])
                , (['r', 'e', 'D'], [(D, 1), (D, 1)])
                , (['b'], [(F, 1), (S, 2)])
                ]


-- | 4.0 Similar Sets

threeSimilarChowsEx :: Hand
threeSimilarChowsEx = [ (['c', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
                        , (['r', 'c', 'C', 'S'], [(C, 4), (C, 5), (C, 6)])
                        , (['r', 'p', 'B', 'S'], [(B, 2), (B, 2), (B, 2)])
                        , (['r', 'c', 'K', 'S'], [(K, 4), (K, 5), (K, 6)])
                        , (['r', 'e', 'D'], [(D, 1), (D, 1)])
                        ]

littleThreeSimilarPungsEx :: Hand
littleThreeSimilarPungsEx = [ (['n', 'p', 'B', 'S'], [(B, 4), (B, 4), (B, 4)])
                              , (['r', 'p', 'C', 'S'], [(C, 4), (C, 4), (C, 4)])
                              , (['r', 'c', 'B', 'T'], [(B, 7), (B, 8), (B, 9)])
                              , (['n', 'k', 'B', 'S'], [(B, 2), (B, 2), (B, 2), (B, 2)])
                              , (['r', 'e', 'K', 'S'], [(K, 4), (K, 4)])
                              ]

threeSimilarPungsEx :: Hand
threeSimilarPungsEx = [ (['n', 'p', 'B', 'S'], [(B, 4), (B, 4), (B, 4)])
                        , (['r', 'p', 'C', 'S'], [(C, 4), (C, 4), (C, 4)])
                        , (['r', 'c', 'B', 'T'], [(B, 7), (B, 8), (B, 9)])
                        , (['r', 'p', 'K', 'T'], [(K, 4), (K, 4), (K, 4)])
                        , (['n', 'e', 'D'], [(D, 1), (D, 1)])
                        ]


-- | 5.0 Consecutive Sets

nineTileStraightEx :: Hand
nineTileStraightEx = [ (['n', 'p', 'B', 'S'], [(B, 4), (B, 4), (B, 4)])
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

fourConsecutivePungsEx :: Hand
fourConsecutivePungsEx = [ (['n', 'p', 'B', 'S'], [(B, 4), (B, 4), (B, 4)])
                           , (['r', 'p', 'B', 'S'], [(B, 5), (B, 5), (B, 5)])
                           , (['r', 'p', 'B', 'S'], [(B, 6), (B, 6), (B, 6)])
                           , (['r', 'p', 'B', 'S'], [(B, 7), (B, 7), (B, 7)])
                           , (['n', 'e', 'C', 'S'], [(C, 4), (C, 4)])
                           ]

threeMothersEx :: Hand
threeMothersEx = [ (['n', 'p', 'B', 'S'], [(B, 4), (B, 4), (B, 4)])
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

littleTerminalClubEx :: Hand
littleTerminalClubEx = [ (['n', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                         , (['r', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                         , (['r', 'c', 'B', 'T'], [(B, 7), (B, 8), (B, 9)])
                         , (['r', 'c', 'B', 'T'], [(B, 7), (B, 8), (B, 9)])
                         , (['n', 'e', 'S'], [(B, 5), (B, 5)])
                         ]

bigTerminalClubEx :: Hand
bigTerminalClubEx = [ (['n', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                      , (['r', 'p', 'B', 'T'], [(B, 1), (B, 1), (B, 1)])
                      , (['r', 'c', 'B', 'T'], [(B, 7), (B, 8), (B, 9)])
                      , (['r', 'p', 'B', 'T'], [(B, 9), (B, 9), (B, 9)])
                      , (['n', 'e', 'S'], [(B, 5), (B, 5)])
                      ]

nineGatesEx :: Hand
nineGatesEx = [ ( ['r', 'h', 'B', 'T', 'S']
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

mixedLesserTerminalEx :: Hand
mixedLesserTerminalEx = [ (['n', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                          , (['r', 'p', 'C', 'T'], [(C, 1), (C, 1), (C, 1)])
                          , (['r', 'c', 'K', 'T'], [(K, 7), (K, 8), (K, 9)])
                          , (['r', 'p', 'D'], [(D, 2), (D, 2), (D, 2)])
                          , (['n', 'e', 'W'], [(W, 1), (W, 1)])
                          ]

pureLesserTerminalEx :: Hand
pureLesserTerminalEx = [ (['n', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
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

dragonPungEx :: Hand
dragonPungEx = [ (['n', 'p', 'B', 'S'], [(B, 2), (B, 2), (B, 2)])
                 , (['n', 'c', 'K', 'S'], [(K, 3), (K, 4), (K, 5)])
                 , (['r', 'c', 'C', 'T'], [(C, 1), (C, 2), (C, 3)])
                 , (['r', 'p', 'D'], [(D, 1), (D, 1), (D, 1)])
                 , (['r', 'e', 'D'], [(D, 3), (D, 3)])
                 , (['b'], [(F, 4)])
                 ]

seatWindEx :: Hand
seatWindEx = [ (['n', 'p', 'B', 'S'], [(B, 2), (B, 2), (B, 2)])
               , (['n', 'c', 'K', 'S'], [(K, 3), (K, 4), (K, 5)])
               , (['r', 'c', 'C', 'T'], [(C, 1), (C, 2), (C, 3)])
               , (['r', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
               , (['r', 'e', 'D'], [(D, 3), (D, 3)])
               , (['b'], [(F, 4)])
               , ]

littleThreeWindsEx :: Hand
littleThreeWindsEx = [ (['n', 'p', 'C', 'S'], [(C, 2), (C, 2), (C, 2)])
                       , (['n', 'p', 'W'], [(W, 3), (W, 3), (W, 3)])
                       , (['r', 'c', 'K', 'T'], [(K, 1), (K, 2), (K, 3)])
                       , (['r', 'p', 'W'], [(W, 4), (W, 4), (W, 4)])
                       , (['r', 'e', 'W'], [(W, 1), (W, 1)])
                       , (['b'], [(F, 4)])
                       ]

bigThreeWindsEx :: Hand
bigThreeWindsEx = [ (['n', 'p', 'W'], [(W, 2), (W, 2), (W, 2)])
                    , (['n', 'p', 'W'], [(W, 3), (W, 3), (W, 3)])
                    , (['r', 'c', 'K', 'T'], [(K, 1), (K, 2), (K, 3)])
                    , (['r', 'p', 'W'], [(W, 4), (W, 4), (W, 4)])
                    , (['r', 'e', 'C', 'S'], [(C, 4), (C, 4)])
                    , (['b'], [(F, 4)])
                    ]

littleFourWindsEx :: Hand
littleFourWindsEx = [ (['n', 'p', 'W'], [(W, 2), (W, 2), (W, 2)])
                      , (['n', 'c', 'K', 'T'], [(K, 1), (K, 2), (K, 3)])
                      , (['r', 'p', 'W'], [(W, 3), (W, 3), (W, 3)])
                      , (['r', 'p', 'W'], [(W, 4), (W, 4), (W, 4)])
                      , (['r', 'e', 'W'], [(W, 1), (W, 1)])
                      , (['b'], [(F, 4)])
                      ]

bigFourWindsEx :: Hand
bigFourWindsEx = [ (['n', 'p', 'W'], [(W, 2), (W, 2), (W, 2)])
                   , (['r', 'p', 'W'], [(W, 3), (W, 3), (W, 3)])
                   , (['r', 'p', 'W'], [(W, 4), (W, 4), (W, 4)])
                   , (['r', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
                   , (['r', 'e', 'C', 'S'], [(C, 4), (C, 4)])
                   , (['b'], [(F, 4)])
                   ]

littleThreeDragonsEx :: Hand
littleThreeDragonsEx = [ (['n', 'p', 'D'], [(D, 2), (D, 2), (D, 2)])
                         , (['n', 'c', 'K', 'S'], [(K, 3), (K, 4), (K, 5)])
                         , (['r', 'p', 'C', 'T'], [(C, 1), (C, 1), (C, 1)])
                         , (['r', 'p', 'D'], [(D, 1), (D, 1), (D, 1)])
                         , (['r', 'e', 'D'], [(D, 3), (D, 3)])
                         ]

bigThreeDragonsEx :: Hand
bigThreeDragonsEx = [ (['n', 'p', 'D'], [(D, 2), (D, 2), (D, 2)])
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

allHonorPairsEx :: Hand
allHonorPairsEx = [ (['n', 'e', 'W'], [(W, 1), (W, 1)])
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

grandChariotEx :: Hand
grandChariotEx = [ (['n', 'e', 'C', 'S'], [(C, 2), (C, 2)])
                   , (['n', 'e', 'C', 'S'], [(C, 3), (C, 3)])
                   , (['n', 'e', 'C', 'S'], [(C, 4), (C, 4)])
                   , (['n', 'e', 'C', 'S'], [(C, 5), (C, 5)])
                   , (['n', 'e', 'C', 'S'], [(C, 6), (C, 6)])
                   , (['n', 'e', 'C', 'S'], [(C, 7), (C, 7)])
                   , (['r', 'e', 'C', 'S'], [(C, 8), (C, 8)])
                   ]

bambooForestEx :: Hand
bambooForestEx = [ (['n', 'e', 'B', 'S'], [(B, 2), (B, 2)])
                   , (['n', 'e', 'B', 'S'], [(B, 3), (B, 3)])
                   , (['n', 'e', 'B', 'S'], [(B, 4), (B, 4)])
                   , (['n', 'e', 'B', 'S'], [(B, 5), (B, 5)])
                   , (['n', 'e', 'B', 'S'], [(B, 6), (B, 6)])
                   , (['n', 'e', 'B', 'S'], [(B, 7), (B, 7)])
                   , (['r', 'e', 'B', 'S'], [(B, 8), (B, 8)])
                   ]

numberNeighborhoodEx :: Hand
numberNeighborhoodEx = [ (['n', 'e', 'K', 'S'], [(K, 2), (K, 2)])
                         , (['n', 'e', 'K', 'S'], [(K, 3), (K, 3)])
                         , (['n', 'e', 'K', 'S'], [(K, 4), (K, 4)])
                         , (['n', 'e', 'K', 'S'], [(K, 5), (K, 5)])
                         , (['n', 'e', 'K', 'S'], [(K, 6), (K, 6)])
                         , (['n', 'e', 'K', 'S'], [(K, 7), (K, 7)])
                         , (['r', 'e', 'K', 'S'], [(K, 8), (K, 8)])
                         ]


-- | 10.0 Color Hands

allGreenEx :: Hand
allGreenEx = [ (['n', 'p', 'B', 'S'], [(B, 2), (B, 2), (B, 2)])
               , (['r', 'c', 'B', 'S'], [(B, 2), (B, 3), (B, 4)])
               , (['r', 'p', 'B', 'S'], [(B, 6), (B, 6), (B, 6)])
               , (['r', 'p', 'D'], [(D, 2), (D, 2), (D, 2)])
               , (['r', 'e', 'B', 'S'], [(B, 8), (B, 8)])
               ]

allRedEx :: Hand
allRedEx = [ (['n', 'p', 'B', 'S'], [(B, 1), (B, 1), (B, 1)])
             , (['r', 'p', 'B', 'S'], [(B, 5), (B, 5), (B, 5)])
             , (['r', 'p', 'B', 'S'], [(B, 9), (B, 9), (B, 9)])
             , (['r', 'p', 'D'], [(D, 1), (D, 1), (D, 1)])
             , (['r', 'e', 'B', 'S'], [(B, 7), (B, 7)])
             ]

allBlueEx :: Hand
allBlueEx = [ (['n', 'p', 'W'], [(W, 2), (W, 2), (W, 2)])
              , (['r', 'p', 'W'], [(W, 3), (W, 3), (W, 3)])
              , (['r', 'p', 'W'], [(C, 8), (C, 8), (C, 8)])
              , (['r', 'p', 'W'], [(D, 3), (D, 3), (D, 3)])
              , (['r', 'e', 'W'], [(W, 4), (W, 4)])
              , (['b'], [(F, 4)])
              ]


-- | 11.0 Irregular Hands

thirteenOrphanEx :: Hand
thirteenOrphanEx = [ ( ['r', 'h', 'B', 'T', 'W', 'D']
                       , [ (C, 1), (C, 9), (B, 1), (B, 9), (K, 1), (K, 9)
                           (W, 1), (W, [(C, 1), (C, 2), (W, 3), (W, 4)
                           (D, 1), (D, [(C, 1), (C, 2), (D, 2), (D, 3)
                         ]
                       )
                     , (['b'], [(F, 1, [(C, 1), ('C')])
                     ]


-- | 12.0 Incidental Bonuses

finalDrawEx :: Hand []
finalDrawEx = [] 

finalDiscardEx :: Hand []
finalDiscardEx = []

winOnKongEx :: Hand []
winOnKongEx = []

winOnBonusTileEx :: Hand []
winOnBonusTileEx = []

robbingKongEx :: Hand []
robbingKongEx = []

blessingOfHeavenEx :: Hand []
blessingOfHeavenEx = []

blessingOfEarthEx :: Hand []
blessingOfEarthEx = []


-- | 13.0 Bonus Tiles

nonSeatFlowerEx :: Hand
nonSeatFlowerEx = [ (['r', 'c', 'C', 'T'], [(C, 7), (C, 8), (C, 9)])
                    , (['r', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
                    , (['r', 'c', 'K', 'S'], [(K, 1), (K, 2), (K, 3)])
                    , (['r', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
                    , (['c', 'e', 'W'], [(W, 3), (W, 3)])
                    , (['b'], [(F, 3), (S, 2)])
                    ]

nonSeatSeasonEx :: Hand
nonSeatSeasonEx = [ (['r', 'c', 'C', 'T'], [(C, 7), (C, 8), (C, 9)])
                    , (['r', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
                    , (['r', 'c', 'K', 'S'], [(K, 1), (K, 2), (K, 3)])
                    , (['r', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
                    , (['c', 'e', 'W'], [(W, 3), (W, 3)])
                    , (['b'], [(F, 2), (S, 4)])
                    ]

seatFlowerEx :: Hand
seatFlowerEx = [ (['r', 'c', 'C', 'T'], [(C, 7), (C, 8), (C, 9)])
                 , (['r', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
                 , (['r', 'c', 'K', 'S'], [(K, 1), (K, 2), (K, 3)])
                 , (['r', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
                 , (['c', 'e', 'W'], [(W, 3), (W, 3)])
                 , (['b'], [(F, 1), (S, 2)])
                 ]

seatSeasonEx :: Hand
seatSeasonEx = [ (['r', 'c', 'C', 'T'], [(C, 7), (C, 8), (C, 9)])
                 , (['r', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
                 , (['r', 'c', 'K', 'S'], [(K, 1), (K, 2), (K, 3)])
                 , (['r', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
                 , (['c', 'e', 'W'], [(W, 3), (W, 3)])
                 , (['b'], [(F, 2), (S, 1)])
                 ]

fourFlowersEx :: Hand
fourFlowersEx = [ (['r', 'c', 'C', 'T'], [(C, 7), (C, 8), (C, 9)])
                  , (['r', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
                  , (['r', 'c', 'K', 'S'], [(K, 1), (K, 2), (K, 3)])
                  , (['r', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
                  , (['c', 'e', 'W'], [(W, 3), (W, 3)])
                  , (['b'], [(F, 1), (F, 2), (F, 3), (F, 4)])
                  ]

fourSeasonsEx :: Hand
fourSeasonsEx = [ (['r', 'c', 'C', 'T'], [(C, 7), (C, 8), (C, 9)])
                  , (['r', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
                  , (['r', 'c', 'K', 'S'], [(K, 1), (K, 2), (K, 3)])
                  , (['r', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
                  , (['c', 'e', 'W'], [(W, 3), (W, 3)])
                  , (['b'], [(S, 1), (S, 2), (S, 3), (S, 4)])
                  ]

allBonusTileEx :: Hand
allBonusTileEx = [ (['r', 'c', 'C', 'T'], [(C, 7), (C, 8), (C, 9)])
                   , (['r', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
                   , (['r', 'c', 'K', 'S'], [(K, 1), (K, 2), (K, 3)])
                   , (['l'], [(B, 4), (B, 5), (B, 6), (B, 7)])
                   , (['b'], [ (F, 1), (F, 2), (F, 3), (F, 4)
                             , (S, 1), (S, 2), (S, 3), (S, 4)
                             ]
                     )
                   ]
