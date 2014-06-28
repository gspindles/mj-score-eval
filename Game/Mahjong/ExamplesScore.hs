module Game.Mahjong.ExampleHands

where

import Game.Mahjong.Score

-- | Make a list of hands for testing


-- | 1.0 Trivial Patterns

chickenHand :: [Melds]
chickenHand = [ (['r', 'c', 'C', 'T'], [(C, 7), (C, 8), (C, 9)])
              , (['r', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
              , (['r', 'c', 'K', 'S'], [(K, 1), (K, 2), (K, 3)])
              , (['r', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
              , (['c', 'e', 'W'], [(W, 3), (W, 3)])
              , (['b'], [(F, 1), (S, 2)])
              ]

allChowsHand :: [Melds] 
allChowsHand = [ (['n', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
               , (['r', 'c', 'C', 'T'], [(C, 7), (C, 8), (C, 9)])
               , (['r', 'c', 'B', 'S'], [(B, 2), (B, 3), (B, 4)])
               , (['r', 'c', 'K', 'T'], [(K, 1), (K, 2), (K, 3)])
               , (['n', 'e', 'D'], [(D, 1), (D, 1)])
               , (['b'], [(F, 1), (S, 2)])
               ]

concealedHand :: [Melds] 
concealedHand = [ (['n', 'c', 'C', 'T'], [(C, 7), (C, 8), (C, 9)])
                , (['n', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
                , (['n', 'c', 'K', 'S'], [(K, 1), (K, 2), (K, 3)])
                , (['n', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
                , (['c', 'e', 'W'], [(W, 3), (W, 3)])
                , (['b'], [(F, 1), (S, 2)])
                ]

selfDrawnHand :: [Melds]
selfDrawnHand = []

allSimpleHand2 :: [Melds]
allSimpleHand2 = [ (['n', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
                 , (['r', 'c', 'C', 'S'], [(C, 6), (C, 7), (C, 8)])
                 , (['r', 'p', 'B', 'S'], [(B, 3), (B, 3), (B, 3)])
                 , (['r', 'p', 'K', 'S'], [(K, 2), (K, 2), (K, 2)])
                 , (['r', 'e', 'B', 'S'], [(B, 2), (B, 2)])
                 , (['b'], [(F, 1), (S, 2)])
                 ]

allSimpleHand2 = [Melds]
allSimpleHand2 = [ (['n', 'e', 'C', 'S'], [(C, 2), (C, 2)])
                 , (['n', 'e', 'C', 'S'], [(C, 5), (C, 5)])
                 , (['n', 'e', 'C', 'S'], [(C, 8), (C, 8)])
                 , (['n', 'e', 'B', 'S'], [(B, 3), (B, 3)])
                 , (['n', 'e', 'B', 'S'], [(B, 6), (B, 6)])
                 , (['n', 'e', 'B', 'S'], [(B, 8), (B, 8)])
                 , (['r', 'e', 'K', 'S'], [(K, 8), (K, 8)])
                 ]

AllTypesHand :: [Melds]
AllTypesHand = [ (['r', 'c', 'C', 'T'], [(C, 7), (C, 8), (C, 9)])
               , (['r', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
               , (['r', 'c', 'K', 'T'], [(K, 1), (K, 2), (K, 3)])
               , (['r', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
               , (['r', 'e', 'D'], [(D, 3), (D, 3)])
               , (['b'], [(F, 1)])
               ]


-- | 2.0 Identical Chows

twoIdenticalChowsHand :: [Melds]
twoIdenticalChowsHand = [ (['n', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
                        , (['r', 'c', 'C', 'S'], [(C, 6), (C, 7), (C, 8)])
                        , (['r', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
                        , (['r', 'c', 'C', 'S'], [(C, 6), (C, 7), (C, 8)])
                        , (['r', 'e', 'D'], [(D, 3), (D, 3)])
                        ]

twoIdenticalChowsTwiceHand :: [Melds]
twoIdenticalChowsTwiceHand = [ (['n', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
                             , (['r', 'c', 'C', 'S'], [(C, 6), (C, 7), (C, 8)])
                             , (['r', 'p', 'W'], [(B, 4), (B, 5), (B, 6)])
                             , (['r', 'c', 'C', 'S'], [(C, 6), (C, 7), (C, 8)])
                             , (['r', 'e', 'D'], [(D, 3), (D, 3)])
                             ]

threeIdenticalChowsHand :: [Melds]
threeIdenticalChowsHand = [ (['n', 'c', 'C', 'S'], [(C, 6), (C, 7), (C, 8)])
                          , (['r', 'c', 'C', 'S'], [(C, 6), (C, 7), (C, 8)])
                          , (['r', 'p', 'B', 'W'], [(B, 4), (B, 5), (B, 6)])
                          , (['r', 'c', 'C', 'S'], [(C, 6), (C, 7), (C, 8)])
                          , (['r', 'e', 'D'], [(D, 3), (D, 3)])
                          ]

fourIdenticalChowsHand :: [Melds]
fourIdenticalChowsHand = [ (['n', 'c', 'C', 'S'], [(C, 6), (C, 7), (C, 8)])
                         , (['r', 'c', 'C', 'S'], [(C, 6), (C, 7), (C, 8)])
                         , (['r', 'p', 'C', 'W'], [(C, 6), (C, 7), (C, 8)])
                         , (['r', 'c', 'C', 'S'], [(C, 6), (C, 7), (C, 8)])
                         , (['r', 'e', 'D'], [(D, 3), (D, 3)])
                         ]


-- | 3.0 Pungs and Kongs

allPungsHand :: [Melds]
allPungsHand = [ (['n', 'k', 'B', 'S'], [(B, 4), (B, 4), (B, 4), (B, 4)])
               , (['r', 'p', 'C', 'S'], [(C, 7), (C, 7), (C, 7)])
               , (['r', 'p', 'B', 'S'], [(B, 2), (B, 2), (B, 2)])
               , (['r', 'p', '­K', 'S'], [(K, 3), (K, 3), (K, 3)])
               , (['n', 'e', 'D'], [(D, 1), (D, 1)])
               ]

twoConcealedPungsHand :: [Melds]
twoConcealedPungsHand = [ (['n', 'k', 'B', 'S'], [(B, 4), (B, 4), (B, 4), (B, 4)])
                        , (['n', 'p', 'B', 'S'], [(B, 2), (B, 2), (B, 2)])
                        , (['r', 'p', 'C', 'S'], [(C, 7), (C, 7), (C, 7)])
                        , (['r', 'k', 'K', 'S'], [(K, 3), (K, 3), (K, 3), (K, 3)])
                        , (['n', 'e', 'D'], [(D, 1), (D, 1)])
                        ]

threeConcealedPungsHand :: [Melds]
threeConcealedPungsHand = [ (['n', 'k', 'B', 'S'], [(B, 4), (B, 4), (B, 4), (B, 4)])
                          , (['n', 'p', 'B', 'S'], [(B, 1), (B, 2), (B, 2)])
                          , (['n', 'k', 'K', 'S'], [(K, 3), (K, 3), (K, 3), (K, 3)])
                          , (['r', 'p', 'C', 'S'], [(C, 7), (C, 7), (C, 7)])
                          , (['r', 'e', 'D'], [(D, 1), (D, 1)])
                          ]

fourConcealedPunsgHand :: [Melds]
fourConcealedPunsgHand = [ (['n', 'k', 'B', 'S'], [(B, 4), (B, 4), (B, 4), (B, 4)])
                         , (['n', 'p', 'B', 'S'], [(B, 2), (B, 2), (B, 2)])
                         , (['n', 'k', 'K', 'S'], [(K, 3), (K, 3), (K, 3), (K, 3)])
                         , (['n', 'p', 'C', 'S'], [(C, 7), (C, 7), (C, 7)])
                         , (['r', 'e', 'D'], [(D, 1), (D, 1)])
                         , (['b'], [(F, 1), (S, 2)])
                         ]

oneKongHand :: [Melds]
oneKongHand = [ (['n', 'k', 'B', 'S'], [(B, 4), (B, 4), (B, 4), (B, 4)])
              , (['r', 'c', 'C', 'T'], [(C, 7), (C, 8), (C, 9)])
              , (['r', 'c', 'B', 'S'], [(B, 2), (B, 3), (B, 4)])
              , (['r', 'c', 'K', 'T'], [(K, 1), (K, 2), (K, 3)])
              , (['r', 'e', 'D'], [(D, 1), (D, 1)])
              , (['b'], [(F, 1), (S, 2)])
              ]

twoKongsHand :: [Melds]
twoKongsHand = [ (['n', 'k', 'B', 'S'], [(B, 4), (B, 4), (B, 4), (B, 4)])
               , (['r', 'k', 'C', 'T'], [(C, 7), (C, 7), (C, 7), (C, 7)])
               , (['r', 'c', 'B', 'S'], [(B, 2), (B, 3), (B, 4)])
               , (['r', 'c', 'K', 'S'], [(K, 3), (K, 4), (K, 5)])
               , (['r', 'e', 'D'], [(D, 1), (D, 1)])
               , (['b'], [(F, 1), (S, 2)])
               ]

threeKongsHand :: [Melds]
threeKongsHand = [ (['n', 'k', 'C', 'S'], [(B, 4), (B, 4), (B, 4), (B, 4)])
                 , (['r', 'k', 'C', 'S'], [(C, 7), (C, 7), (C, 7), (C, 7)])
                 , (['r', 'k', 'B', 'S'], [(B, 2), (B, 2), (B, 2), (B, 2)])
                 , (['r', 'c', 'K', 'S'], [(K, 3), (K, 4), (K, 5)])
                 , (['r', 'e', 'D'], [(D, 1), (D, 1)])
                 , (['b'], [(F, 1), (S, 2)])
                 ]

fourKongsHand :: [Melds]
fourKongsHand = [ (['n', 'k', 'B', 'S'], [(B, 4), (B, 4), (B, 4), (B, 4)])
                , (['r', 'k', 'C', 'S'], [(C, 7), (C, 7), (C, 7), (C, 7)])
                , (['r', 'k', 'B', 'S'], [(B, 2), (B, 2), (B, 2), (B, 2)])
                , (['r', 'k', 'K', 'S'], [(K, 3), (K, 3), (K, 3), (K, 3)])
                , (['r', 'e', 'D'], [(D, 1), (D, 1)])
                , (['b'], [(F, 1), (S, 2)])
                ]


-- | 4.0 Similar Sets

threeSimilarChowsHand :: [Melds]
threeSimilarChowsHand = [ (['c', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
                        , (['r', 'c', 'C', 'S'], [(C, 4), (C, 5), (C, 6)])
                        , (['r', 'p', 'B', 'S'], [(B, 2), (B, 2), (B, 2)])
                        , (['r', 'c', 'K', 'S'], [(K, 4), (K, 5), (K, 6)])
                        , (['r', 'e', 'D'], [(D, 1), (D, 1)])
                        ]

littleThreeSimilarPungsHand :: [Melds]
littleThreeSimilarPungsHand = [ (['n', 'p', 'B', 'S'], [(B, 4), (B, 4), (B, 4)])
                              , (['r', 'p', 'C', 'S'], [(C, 4), (C, 4), (C, 4)])
                              , (['r', 'c', 'B', 'T'], [(B, 7), (B, 8), (B, 9)])
                              , (['n', 'k', 'B', 'S'], [(B, 2), (B, 2), (B, 2), (B, 2)])
                              , (['r', 'e', 'K', 'S'], [(K, 4), (K, 4)])
                              ]

threeSimilarPungsHand :: [Melds]
threeSimilarPungsHand = [ (['n', 'p', 'B', 'S'], [(B, 4), (B, 4), (B, 4)])
                        , (['r', 'p', 'C', 'S'], [(C, 4), (C, 4), (C, 4)])
                        , (['r', 'c', 'B', 'T'], [(B, 7), (B, 8), (B, 9)])
                        , (['r', 'p', 'K', 'T'], [(K, 4), (K, 4), (K, 4)])
                        , (['n', 'e', 'D'], [(D, 1), (D, 1)])
                        ]


-- | 5.0 Consecutive Sets

nineTileStraightHand :: [Melds]
nineTileStraightHand = [ (['n', 'p', 'B', 'S'], [(B, 4), (B, 4), (B, 4)])
                       , (['n', 'c', 'C', 'T'], [(C, 1), (C, 2), (C, 3)])
                       , (['r', 'c', 'C', 'S'], [(C, 4), (C, 5), (C, 6)])
                       , (['r', 'c', 'C', 'T'], [(C, 7), (C, 8), (C, 9)])
                       , (['n', 'e', 'W'], [(W, 4), (W, 4)])
                       ]

threeConsecutivePungsHand1 :: [Melds]
threeConsecutivePungsHand1 = [ (['n', 'p', 'B', 'S'], [(B, 4), (B, 4), (B, 4)])
                             , (['r', 'p', 'B', 'S'], [(B, 5), (B, 5), (B, 5)])
                             , (['r', 'p', 'B', 'S'], [(B, 6), (B, 6), (B, 6)])
                             , (['r', 'c', 'C', 'T'], [(C, 1), (C, 2), (C, 2)])
                             , (['n', 'e', 'W'], [(W, 4), (W, 4)])
                             ]

threeConsecutivePungsHand2 :: [Melds]
threeConsecutivePungsHand2 = [ (['n', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 2)])
                             , (['n', 'p', 'B', 'S'], [(B, 4), (B, 4), (B, 4)])
                             , (['r', 'p', 'B', 'S'], [(B, 5), (B, 5), (B, 5)])
                             , (['r', 'p', 'B', 'S'], [(B, 6), (B, 6), (B, 6)])
                             , (['n', 'e', 'W'], [(W, 4), (W, 4)])
                             ]

fourConsecutivePungsHand :: [Melds]
fourConsecutivePungsHand = [ (['n', 'p', 'B', 'S'], [(B, 4), (B, 4), (B, 4)])
                           , (['r', 'p', 'B', 'S'], [(B, 5), (B, 5), (B, 5)])
                           , (['r', 'p', 'B', 'S'], [(B, 6), (B, 6), (B, 6)])
                           , (['r', 'p', 'B', 'S'], [(B, 7), (B, 7), (B, 7)])
                           , (['n', 'e', 'C', 'S'], [(C, 4), (C, 4)])
                           ]

threeMothersHand :: [Melds]
threeMothersHand = [ (['n', 'p', 'B', 'S'], [(B, 4), (B, 4), (B, 4)])
                   , (['r', 'p', 'B', 'S'], [(B, 5), (B, 5), (B, 5)])
                   , (['r', 'p', 'B', 'S'], [(B, 6), (B, 6), (B, 6)])
                   , (['r', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
                   , (['n', 'e', 'C', 'S'], [(C, 4), (C, 4)])
                   ]


-- | 6.0 Suit Patterns

mixedOneSuitHand1 :: [Melds]
mixedOneSuitHand1 = [ (['n', 'p', 'B', 'S'], [(B, 3), (B, 3), (B, 3)])
                    , (['r', 'p', 'W'], [(W, 4), (W, 4), (W, 4)])
                    , (['r', 'p', 'D'], [(D, 1), (D, 1), (D, 1)])
                    , (['r', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
                    , (['n', 'e', 'B', 'S'], [(B, 8), (B, 8)])
                    ]

mixedOneSuitHand2 :: [Melds]
mixedOneSuitHand2 = [ (['n', 'e', 'B', 'T'], [(B, 1), (B, 1)])
                    , (['n', 'e', 'B', 'S'], [(B, 3), (B, 3)])
                    , (['n', 'e', 'B', 'S'], [(B, 5), (B, 5)])
                    , (['n', 'e', 'B', 'S'], [(B, 6), (B, 6)])
                    , (['n', 'e', 'W'], [(W, 2), (W, 2)])
                    , (['n', 'e', 'W'], [(W, 4), (W, 4)])
                    , (['r', 'e', 'D'], [(D, 1), (D, 1)])
                    ]

pureOneSuitHand1 :: [Melds]
pureOneSuitHand1 = [ (['n', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                   , (['r', 'p', 'B', 'S'], [(B, 4), (B, 4), (B, 4)])
                   , (['r', 'c', 'B', 'S'], [(B, 6), (B, 7), (B, 8)])
                   , (['r', 'c', 'B', 'S'], [(B, 5), (B, 6), (B, 7)])
                   , (['n', 'e', 'B', 'T'], [(B, 9), (B, 9)])
                   ]

pureOneSuitHand2 :: [Melds]
pureOneSuitHand2 = [ (['n', 'e', 'B', 'T'], [(B, 1), (B, 1)])
                   , (['n', 'e', 'B', 'S'], [(B, 3), (B, 3)])
                   , (['n', 'e', 'B', 'S'], [(B, 4), (B, 4)])
                   , (['n', 'e', 'B', 'S'], [(B, 5), (B, 5)])
                   , (['n', 'e', 'B', 'S'], [(B, 6), (B, 6)])
                   , (['n', 'e', 'B', 'S'], [(B, 8), (B, 8)])
                   , (['r', 'e', 'B', 'T'], [(B, 9), (B, 9)])
                   ]

littleTerminalClubHand :: [Melds]
littleTerminalClubHand = [ (['n', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                         , (['r', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                         , (['r', 'c', 'B', 'T'], [(B, 7), (B, 8), (B, 9)])
                         , (['r', 'c', 'B', 'T'], [(B, 7), (B, 8), (B, 9)])
                         , (['n', 'e', 'S'], [(B, 5), (B, 5)])
                         ]

bigTerminalClubHand :: [Melds]
bigTerminalClubHand = [ (['n', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                      , (['r', 'p', 'B', 'T'], [(B, 1), (B, 1), (B, 1)])
                      , (['r', 'c', 'B', 'T'], [(B, 7), (B, 8), (B, 9)])
                      , (['r', 'p', 'B', 'T'], [(B, 9), (B, 9), (B, 9)])
                      , (['n', 'e', 'S'], [(B, 5), (B, 5)])
                      ]

nineGatesHand :: [Melds]
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

twoTailedTerminalChowsHand1 :: [Melds]
twoTailedTerminalChowsHand1 = [ (['n', 'p', 'C', 'S'], [(C, 3), (C, 3), (C, 3)])
                              , (['n', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                              , (['r', 'c', 'B', 'T'], [(B, 7), (B, 8), (B, 9)])
                              , (['r', 'c', 'K', 'S'], [(K, 5), (K, 6), (K, 7)])
                              , (['n', 'e', 'W'], [(W, 4), (W, 4)])
                              ]

twoTailedTerminalChowsHand2 :: [Melds]
twoTailedTerminalChowsHand2 = [ (['n', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                              , (['n', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                              , (['r', 'c', 'B', 'T'], [(B, 7), (B, 8), (B, 9)])
                              , (['r', 'c', 'K', 'S'], [(K, 5), (K, 6), (K, 7)])
                              , (['n', 'e', 'W'], [(W, 4), (W, 4)])
                              ]

twoTailedTerminalChowsHand3 :: [Melds]
twoTailedTerminalChowsHand3 = [ (['n', 'c', 'C', 'T'], [(C, 1), (C, 2), (C, 3)])
                              , (['n', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                              , (['r', 'c', 'B', 'T'], [(B, 7), (B, 8), (B, 9)])
                              , (['r', 'c', 'C', 'T'], [(C, 7), (C, 8), (C, 9)])
                              , (['n', 'e', 'K'], [(K, 4), (K, 4)])
                              ]

twoTailedTerminalPungsHand1 :: [Melds]
twoTailedTerminalPungsHand1 = [ (['n', 'p', 'C', 'S'], [(C, 3), (C, 3), (C, 3)])
                              , (['n', 'p', 'B', 'T'], [(B, 1), (B, 1), (B, 1)])
                              , (['r', 'p', 'B', 'T'], [(B, 9), (B, 9), (B, 9)])
                              , (['r', 'c', 'K', 'S'], [(K, 5), (K, 6), (K, 7)])
                              , (['n', 'e', 'W'], [(W, 4), (W, 4)])
                              ]

twoTailedTerminalPungsHand2 :: [Melds]
twoTailedTerminalPungsHand2 = [ (['n', 'p', 'C', 'T'], [(C, 1), (C, 1), (C, 1)])
                              , (['n', 'p', 'B', 'T'], [(B, 1), (B, 1), (B, 1)])
                              , (['r', 'p', 'B', 'T'], [(B, 9), (B, 9), (B, 9)])
                              , (['r', 'p', 'C', 'T'], [(C, 9), (C, 9), (C, 9)])
                              , (['n', 'e', 'K'], [(K, 4), (K, 4)])
                              ]

littleBoundlessMountainHand1 :: [Melds]
littleBoundlessMountainHand1 = [ (['n', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                               , (['r', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                               , (['r', 'c', 'B', 'T'], [(B, 7), (B, 8), (B, 9)])
                               , (['r', 'c', 'B', 'T'], [(B, 7), (B, 8), (B, 9)])
                               , (['n', 'e', 'B', 'T'], [(B, 9), (B, 9)])
                               ]

littleBoundlessMountainHand2 :: [Melds]
littleBoundlessMountainHand2 = [ (['n', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                               , (['r', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                               , (['r', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                               , (['r', 'c', 'B', 'T'], [(B, 7), (B, 8), (B, 9)])
                               , (['n', 'e', 'B', 'T'], [(B, 9), (B, 9)])
                               ]

bigBoundlessMountainHand1 :: [Melds]
bigBoundlessMountainHand1 = [ (['n', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                            , (['r', 'p', 'B', 'T'], [(B, 1), (B, 1), (B, 1)])
                            , (['r', 'c', 'B', 'T'], [(B, 7), (B, 8), (B, 9)])
                            , (['r', 'c', 'B', 'T'], [(B, 7), (B, 8), (B, 9)])
                            , (['n', 'e', 'B', 'T'], [(B, 9), (B, 9)])
                            ]

bigBoundlessMountainHand2 :: [Melds]
bigBoundlessMountainHand2 = [ (['n', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                            , (['r', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                            , (['r', 'c', 'B', 'T'], [(B, 7), (B, 8), (B, 9)])
                            , (['r', 'p', 'B', 'T'], [(B, 9), (B, 9), (B, 9)])
                            , (['n', 'e', 'B', 'T'], [(B, 1), (B, 1)])
                            ]

mixedLesserTerminalHand :: [Melds]
mixedLesserTerminalHand = [ (['n', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                          , (['r', 'p', 'C', 'T'], [(C, 1), (C, 1), (C, 1)])
                          , (['r', 'c', 'K', 'T'], [(K, 7), (K, 8), (K, 9)])
                          , (['r', 'p', 'D'], [(D, 2), (D, 2), (D, 2)])
                          , (['n', 'e', 'W'], [(W, 1), (W, 1)])
                          ]

pureLesserTerminalHand :: [Melds]
pureLesserTerminalHand = [ (['n', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                         , (['r', 'p', 'C', 'T'], [(C, 1), (C, 1), (C, 1)])
                         , (['r', 'c', 'K', 'T'], [(K, 7), (K, 8), (K, 9)])
                         , (['r', 'p', 'B', 'T'], [(B, 9), (B, 9), (B, 9)])
                         , (['n', 'e', 'K', 'T'], [(K, 1), (K, 1)])
                         ]

mixedGreaterTerminalHand1 :: [Melds]
mixedGreaterTerminalHand1 = [ (['n', 'p', 'K', 'T'], [(K, 1), (K, 1), (K, 1)])
                            , (['r', 'p', 'C', 'T'], [(C, 9), (C, 9), (C, 9)])
                            , (['r', 'p', 'B', 'T'], [(B, 9), (B, 9), (B, 9)])
                            , (['r', 'p', 'D'], [(D, 2), (D, 2), (D, 2)])
                            , (['n', 'e', 'W'], [(W, 1), (W, 1)])
                            ]

mixedGreaterTerminalHand2 :: [Melds]
mixedGreaterTerminalHand2 = [ (['n', 'e', 'C', 'T'], [(C, 9), (C, 9)])
                            , (['n', 'e', 'B', 'T'], [(B, 1), (B, 1)])
                            , (['n', 'e', 'B', 'T'], [(B, 9), (B, 9)])
                            , (['n', 'e', 'K', 'T'], [(K, 1), (K, 1)])
                            , (['n', 'e', 'W'], [(W, 2), (W, 2)])
                            , (['n', 'e', 'W'], [(W, 4), (W, 4)])
                            , (['r', 'e', 'D'], [(D, 1), (D, 1)])
                            ]

pureGreaterTerminalHand1 :: [Melds]
pureGreaterTerminalHand1 = [ (['n', 'p', 'C', 'T'], [(C, 1), (C, 1), (C, 1)])
                           , (['r', 'p', 'C', 'T'], [(C, 9), (C, 9), (C, 9)])
                           , (['r', 'p', 'B', 'T'], [(B, 1), (B, 1), (B, 1)])
                           , (['r', 'p', 'B', 'T'], [(B, 9), (B, 9), (B, 9)])
                           , (['n', 'e', 'K', 'T'], [(K, 9), (K, 9)])
                           ]

pureGreaterTermimalHand2 :: [Melds]
pureGreaterTermimalHand2 = [ (['n', 'e', 'C', 'T'], [(C, 1), (C, 1)])
                           , (['n', 'e', 'C', 'T'], [(C, 1), (C, 1)])
                           , (['n', 'e', 'C', 'T'], [(C, 9), (C, 9)])
                           , (['n', 'e', 'B', 'T'], [(B, 1), (B, 1)])
                           , (['n', 'e', 'B', 'T'], [(B, 9), (B, 9)])
                           , (['n', 'e', 'K', 'T'], [(K, 1), (K, 1)])
                           , (['r', 'e', 'K', 'T'], [(K, 9), (K, 9)])
                           ]


-- | 8.0 Honor Tiles

dragonPungHand :: [Melds]
dragonPungHand = [ (['n', 'p', 'B', 'S'], [(B, 2), (B, 2), (B, 2)])
                 , (['n', 'c', 'K', 'S'], [(K, 3), (K, 4), (K, 5)])
                 , (['r', 'c', 'C', 'T'], [(C, 1), (C, 2), (C, 3)])
                 , (['r', 'p', 'D'], [(D, 1), (D, 1), (D, 1)])
                 , (['r', 'e', 'D'], [(D, 3), (D, 3)])
                 , (['b'], [(F, 4)])
                 ]

seatWindHand :: [Melds]
seatWindHand = [ (['n', 'p', 'B', 'S'], [(B, 2), (B, 2), (B, 2)])
               , (['n', 'c', 'K', 'S'], [(K, 3), (K, 4), (K, 5)])
               , (['r', 'c', 'C', 'T'], [(C, 1), (C, 2), (C, 3)])
               , (['r', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
               , (['r', 'e', 'D'], [(D, 3), (D, 3)])
               , (['b'], [(F, 4)])
               , ]

littleThreeWindsHand :: [Melds]
littleThreeWindsHand = [ (['n', 'p', 'C', 'S'], [(C, 2), (C, 2), (C, 2)])
                       , (['n', 'p', 'W'], [(W, 3), (W, 3), (W, 3)])
                       , (['r', 'c', 'K', 'T'], [(K, 1), (K, 2), (K, 3)])
                       , (['r', 'p', 'W'], [(W, 4), (W, 4), (W, 4)])
                       , (['r', 'e', 'W'], [(W, 1), (W, 1)])
                       , (['b'], [(F, 4)])
                       ]

bigThreeWindsHand :: [Melds]
bigThreeWindsHand = [ (['n', 'p', 'W'], [(W, 2), (W, 2), (W, 2)])
                    , (['n', 'p', 'W'], [(W, 3), (W, 3), (W, 3)])
                    , (['r', 'c', 'K', 'T'], [(K, 1), (K, 2), (K, 3)])
                    , (['r', 'p', 'W'], [(W, 4), (W, 4), (W, 4)])
                    , (['r', 'e', 'C', 'S'], [(C, 4), (C, 4)])
                    , (['b'], [(F, 4)])
                    ]

littleFourWindsHand :: [Melds]
littleFourWindsHand = [ (['n', 'p', 'W'], [(W, 2), (W, 2), (W, 2)])
                      , (['n', 'c', 'K', 'T'], [(K, 1), (K, 2), (K, 3)])
                      , (['r', 'p', 'W'], [(W, 3), (W, 3), (W, 3)])
                      , (['r', 'p', 'W'], [(W, 4), (W, 4), (W, 4)])
                      , (['r', 'e', 'W'], [(W, 1), (W, 1)])
                      , (['b'], [(F, 4)])
                      ]

bigFourWindsHand :: [Melds]
bigFourWindsHand = [ (['n', 'p', 'W'], [(W, 2), (W, 2), (W, 2)])
                   , (['r', 'p', 'W'], [(W, 3), (W, 3), (W, 3)])
                   , (['r', 'p', 'W'], [(W, 4), (W, 4), (W, 4)])
                   , (['r', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
                   , (['r', 'e', 'C', 'S'], [(C, 4), (C, 4)])
                   , (['b'], [(F, 4)])
                   ]

littleThreeDragonsHand :: [Melds]
littleThreeDragonsHand = [ (['n', 'p', 'D'], [(D, 2), (D, 2), (D, 2)])
                         , (['n', 'c', 'K', 'S'], [(K, 3), (K, 4), (K, 5)])
                         , (['r', 'p', 'C', 'T'], [(C, 1), (C, 1), (C, 1)])
                         , (['r', 'p', 'D'], [(D, 1), (D, 1), (D, 1)])
                         , (['r', 'e', 'D'], [(D, 3), (D, 3)])
                         ]

bigThreeDragonsHand :: [Melds]
bigThreeDragonsHand = [ (['n', 'p', 'D'], [(D, 2), (D, 2), (D, 2)])
                      , (['n', 'c', 'B', 'T'], [(B, 1), (B, 2), (B, 3)])
                      , (['r', 'p', 'D'], [(D, 1), (D, 1), (D, 1)])
                      , (['r', 'p', 'D'], [(D, 3), (D, 3), (D, 3)])
                      , (['r', 'e', 'C', 'S'], [(C, 3), (C, 3)])
                      ]

allHonorsHand1 :: [Melds]
allHonorsHand1 = [ (['n', 'p', 'D'], [(D, 2), (D, 2), (D, 2)])
                 , (['n', 'p', 'W'], [(W, 3), (W, 3), (W, 3)])
                 , (['r', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
                 , (['r', 'p', 'D'], [(D, 1), (D, 1), (D, 1)])
                 , (['r', 'e', 'W'], [(W, 4), (W, 4)])
                 ]

allHonorsHand2 :: [Melds]
allHonorsHand2 = [ (['n', 'e', 'W'], [(W, 1), (W, 1)])
                 , (['n', 'e', 'W'], [(W, 2), (W, 2)])
                 , (['n', 'e', 'W'], [(W, 2), (W, 2)])
                 , (['n', 'e', 'D'], [(D, 1), (D, 1)])
                 , (['n', 'e', 'D'], [(D, 2), (D, 2)])
                 , (['n', 'e', 'D'], [(D, 3), (D, 3)])
                 , (['r', 'e', 'W'], [(W, 4), (W, 4)])
                 ]

allHonorPairsHand :: [Melds]
allHonorPairsHand = [ (['n', 'e', 'W'], [(W, 1), (W, 1)])
                    , (['n', 'e', 'W'], [(W, 2), (W, 2)])
                    , (['n', 'e', 'W'], [(W, 3), (W, 3)])
                    , (['n', 'e', 'D'], [(D, 1), (D, 1)])
                    , (['n', 'e', 'D'], [(D, 2), (D, 2)])
                    , (['n', 'e', 'D'], [(D, 3), (D, 3)])
                    , (['r', 'e', 'W'], [(W, 4), (W, 4)])
                    ]


-- | 9.0 Seven Pairs

sevenPairsHand1 :: [Melds]
sevenPairsHand1 = [ (['n', 'e', 'C', 'T'], [(C, 1), (C, 1)])
                  , (['n', 'e', 'C', 'S'], [(C, 7), (C, 7)])
                  , (['n', 'e', 'B', 'S'], [(B, 3), (B, 3)])
                  , (['n', 'e', 'K', 'S'], [(K, 6), (K, 6)])
                  , (['n', 'e', 'W'], [(W, 4), (W, 4)])
                  , (['n', 'e', 'D'], [(D, 2), (D, 2)])
                  , (['r', 'e', 'D'], [(D, 3), (D, 3)])
                  ]

sevenPairsHand2 :: [Melds]
sevenPairsHand2 = [ (['n', 'e', 'C', 'T'], [(C, 1), (C, 1)])
                  , (['n', 'e', 'C', 'T'], [(C, 1), (C, 1)])
                  , (['n', 'e', 'B', 'S'], [(B, 3), (B, 3)])
                  , (['n', 'e', 'K', 'S'], [(K, 6), (K, 6)])
                  , (['n', 'e', 'W'], [(W, 4), (W, 4)])
                  , (['n', 'e', 'D'], [(D, 2), (D, 2)])
                  , (['r', 'e', 'D'], [(D, 3), (D, 3)])
                  ]

sevenPairsHand3 :: [Melds]
sevenPairsHand3 = [ (['n', 'e', 'C', 'T'], [(C, 1), (C, 1)])
                  , (['n', 'e', 'C', 'T'], [(C, 1), (C, 1)])
                  , (['n', 'e', 'B', 'S'], [(B, 3), (B, 3)])
                  , (['n', 'e', 'B', 'S'], [(B, 3), (B, 3)])
                  , (['n', 'e', 'W'], [(W, 4), (W, 4)])
                  , (['n', 'e', 'D'], [(D, 2), (D, 2)])
                  , (['r', 'e', 'D'], [(D, 3), (D, 3)])
                  ]

sevenPairsHand4 :: [Melds]
sevenPairsHand4 = [ (['n', 'e', 'C', 'T'], [(C, 1), (C, 1)])
                  , (['n', 'e', 'C', 'T'], [(C, 1), (C, 1)])
                  , (['n', 'e', 'B', 'S'], [(B, 3), (B, 3)])
                  , (['n', 'e', 'B', 'S'], [(B, 3), (B, 3)])
                  , (['n', 'e', 'W'], [(W, 4), (W, 4)])
                  , (['n', 'e', 'W'], [(W, 4), (W, 4)])
                  , (['r', 'e', 'D'], [(D, 3), (D, 3)])
                  ]

sevenShiftedPairsHand1 :: [Melds]
sevenShiftedPairsHand1 = [ (['n', 'e', 'C', 'T'], [(C, 1), (C, 1)])
                         , (['n', 'e', 'C', 'S'], [(C, 2), (C, 2)])
                         , (['n', 'e', 'C', 'S'], [(C, 3), (C, 3)])
                         , (['n', 'e', 'C', 'S'], [(C, 4), (C, 4)])
                         , (['n', 'e', 'C', 'S'], [(C, 5), (C, 5)])
                         , (['n', 'e', 'C', 'S'], [(C, 6), (C, 6)])
                         , (['r', 'e', 'C', 'S'], [(C, 7), (C, 7)])
                         ]

sevenShiftedPairsHand2 :: [Melds]
sevenShiftedPairsHand2 = [ (['n', 'e', 'K', 'S'], [(K, 3), (K, 3)])
                         , (['n', 'e', 'K', 'S'], [(K, 4), (K, 4)])
                         , (['n', 'e', 'K', 'S'], [(K, 5), (K, 5)])
                         , (['n', 'e', 'K', 'S'], [(K, 6), (K, 6)])
                         , (['n', 'e', 'K', 'S'], [(K, 7), (K, 7)])
                         , (['n', 'e', 'K', 'S'], [(K, 8), (K, 8)])
                         , (['r', 'e', 'K', 'T'], [(K, 9), (K, 9)])
                         ]

grandChariotHand :: [Melds]
grandChariotHand = [ (['n', 'e', 'C', 'S'], [(C, 2), (C, 2)])
                   , (['n', 'e', 'C', 'S'], [(C, 3), (C, 3)])
                   , (['n', 'e', 'C', 'S'], [(C, 4), (C, 4)])
                   , (['n', 'e', 'C', 'S'], [(C, 5), (C, 5)])
                   , (['n', 'e', 'C', 'S'], [(C, 6), (C, 6)])
                   , (['n', 'e', 'C', 'S'], [(C, 7), (C, 7)])
                   , (['r', 'e', 'C', 'S'], [(C, 8), (C, 8)])
                   ]

bambooForestHand :: [Melds]
bambooForestHand = [ (['n', 'e', 'B', 'S'], [(B, 2), (B, 2)])
                   , (['n', 'e', 'B', 'S'], [(B, 3), (B, 3)])
                   , (['n', 'e', 'B', 'S'], [(B, 4), (B, 4)])
                   , (['n', 'e', 'B', 'S'], [(B, 5), (B, 5)])
                   , (['n', 'e', 'B', 'S'], [(B, 6), (B, 6)])
                   , (['n', 'e', 'B', 'S'], [(B, 7), (B, 7)])
                   , (['r', 'e', 'B', 'S'], [(B, 8), (B, 8)])
                   ]

numberNeighborhoodHand :: [Melds]
numberNeighborhoodHand = [ (['n', 'e', 'K', 'S'], [(K, 2), (K, 2)])
                         , (['n', 'e', 'K', 'S'], [(K, 3), (K, 3)])
                         , (['n', 'e', 'K', 'S'], [(K, 4), (K, 4)])
                         , (['n', 'e', 'K', 'S'], [(K, 5), (K, 5)])
                         , (['n', 'e', 'K', 'S'], [(K, 6), (K, 6)])
                         , (['n', 'e', 'K', 'S'], [(K, 7), (K, 7)])
                         , (['r', 'e', 'K', 'S'], [(K, 8), (K, 8)])
                         ]


-- | 10.0 Color Hands

allGreenHand :: [Melds]
allGreenHand = [ (['n', 'p', 'B', 'S'], [(B, 2), (B, 2), (B, 2)])
               , (['r', 'c', 'B', 'S'], [(B, 2), (B, 3), (B, 4)])
               , (['r', 'p', 'B', 'S'], [(B, 6), (B, 6), (B, 6)])
               , (['r', 'p', 'D'], [(D, 2), (D, 2), (D, 2)])
               , (['r', 'e', 'B', 'S'], [(B, 8), (B, 8)])
               ]

allRedHand :: [Melds]
allRedHand = [ (['n', 'p', 'B', 'S'], [(B, 1), (B, 1), (B, 1)])
             , (['r', 'p', 'B', 'S'], [(B, 5), (B, 5), (B, 5)])
             , (['r', 'p', 'B', 'S'], [(B, 9), (B, 9), (B, 9)])
             , (['r', 'p', 'D'], [(D, 1), (D, 1), (D, 1)])
             , (['r', 'e', 'B', 'S'], [(B, 7), (B, 7)])
             ]

allBlueHand :: [Melds]
allBlueHand = [ (['n', 'p', 'W'], [(W, 2), (W, 2), (W, 2)])
              , (['r', 'p', 'W'], [(W, 3), (W, 3), (W, 3)])
              , (['r', 'p', 'W'], [(C, 8), (C, 8), (C, 8)])
              , (['r', 'p', 'W'], [(D, 3), (D, 3), (D, 3)])
              , (['r', 'e', 'W'], [(W, 4), (W, 4)])
              , (['b'], [(F, 4)])
              ]


-- | 11.0 Irregular Hands

thirteenOrphanHand :: [Melds]
thirteenOrphanHand = [ ( ['r', 'h', 'B', 'T', 'W', 'D']
                       , [ (C, 1), (C, 9), (B, 1), (B, 9), (K, 1), (K, 9)
                           (W, 1), (W, [(C, 1), (C, 2), (W, 3), (W, 4)
                           (D, 1), (D, [(C, 1), (C, 2), (D, 2), (D, 3)
                         ]
                       )
                     , (['b'], [(F, 1, [(C, 1), ('C')])
                     ]


-- | 12.0 Incidental Bonuses

finalDrawHand :: [Melds] []
finalDrawHand = [] 

finalDiscardHand :: [Melds] []
finalDiscardHand = []

winOnKongHand :: [Melds] []
winOnKongHand = []

winOnBonusTileHand :: [Melds] []
winOnBonusTileHand = []

robbingKongHand :: [Melds] []
robbingKongHand = []

blessingOfHeavenHand :: [Melds] []
blessingOfHeavenHand = []

blessingOfEarthHand :: [Melds] []
blessingOfEarthHand = []


-- | 13.0 Bonus Tiles

nonSeatFlowerHand :: [Melds]
nonSeatFlowerHand = [ (['r', 'c', 'C', 'T'], [(C, 7), (C, 8), (C, 9)])
                    , (['r', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
                    , (['r', 'c', 'K', 'S'], [(K, 1), (K, 2), (K, 3)])
                    , (['r', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
                    , (['c', 'e', 'W'], [(W, 3), (W, 3)])
                    , (['b'], [(F, 3), (S, 2)])
                    ]

nonSeatSeasonHand :: [Melds]
nonSeatSeasonHand = [ (['r', 'c', 'C', 'T'], [(C, 7), (C, 8), (C, 9)])
                    , (['r', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
                    , (['r', 'c', 'K', 'S'], [(K, 1), (K, 2), (K, 3)])
                    , (['r', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
                    , (['c', 'e', 'W'], [(W, 3), (W, 3)])
                    , (['b'], [(F, 2), (S, 4)])
                    ]

seatFlowerHand :: [Melds]
seatFlowerHand = [ (['r', 'c', 'C', 'T'], [(C, 7), (C, 8), (C, 9)])
                 , (['r', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
                 , (['r', 'c', 'K', 'S'], [(K, 1), (K, 2), (K, 3)])
                 , (['r', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
                 , (['c', 'e', 'W'], [(W, 3), (W, 3)])
                 , (['b'], [(F, 1), (S, 2)])
                 ]

seatSeasonHand :: [Melds]
seatSeasonHand = [ (['r', 'c', 'C', 'T'], [(C, 7), (C, 8), (C, 9)])
                 , (['r', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
                 , (['r', 'c', 'K', 'S'], [(K, 1), (K, 2), (K, 3)])
                 , (['r', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
                 , (['c', 'e', 'W'], [(W, 3), (W, 3)])
                 , (['b'], [(F, 2), (S, 1)])
                 ]

fourFlowersHand :: [Melds]
fourFlowersHand = [ (['r', 'c', 'C', 'T'], [(C, 7), (C, 8), (C, 9)])
                  , (['r', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
                  , (['r', 'c', 'K', 'S'], [(K, 1), (K, 2), (K, 3)])
                  , (['r', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
                  , (['c', 'e', 'W'], [(W, 3), (W, 3)])
                  , (['b'], [(F, 1), (F, 2), (F, 3), (F, 4)])
                  ]

fourSeasonsHand :: [Melds]
fourSeasonsHand = [ (['r', 'c', 'C', 'T'], [(C, 7), (C, 8), (C, 9)])
                  , (['r', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
                  , (['r', 'c', 'K', 'S'], [(K, 1), (K, 2), (K, 3)])
                  , (['r', 'c', 'B', 'S'], [(B, 4), (B, 5), (B, 6)])
                  , (['c', 'e', 'W'], [(W, 3), (W, 3)])
                  , (['b'], [(S, 1), (S, 2), (S, 3), (S, 4)])
                  ]

allBonusTileHand :: [Melds]
allBonusTileHand = [ (['r', 'c', 'C', 'T'], [(C, 7), (C, 8), (C, 9)])
                   , (['r', 'p', 'W'], [(W, 1), (W, 1), (W, 1)])
                   , (['r', 'c', 'K', 'S'], [(K, 1), (K, 2), (K, 3)])
                   , (['l'], [(B, 4), (B, 5), (B, 6), (B, 7)])
                   , (['b'], [ (F, 1), (F, 2), (F, 3), (F, 4)
                             , (S, 1), (S, 2), (S, 3), (S, 4)
                             ]
                     )
                   ]
