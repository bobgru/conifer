-- A Comparison of Virtual Conifers
--
-- This program draws an array of conifers with variations of parameters.

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
import Conifer
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

-- Main Program
--
-- Run the program with dist/build/comparison/comparison -o comparison.svg -w 800
-- where -o sets the output filename, and -w sets the diagram width.

main = defaultMain (example # withGrid # centerXY # pad 1.2)

-- Selected Comparison
--
-- Uncomment one at a time and recompile with cabal build
-- or ghc --make comparison.lhs.

example = 
--    trunkLength_Whorls
--    trunkLength_trunkBranchAngles
--    whorls_whorlSize
--    branchLength_branchAngle
    centerBranchLength_sideBranchLength

-- Layout
--
-- Create an array of background cells sized to fit the largest of a list
-- of diagrams, where n is the cells per row, and bg is the background.

gridLayout n bg ds = grid # centerXY
    where
        grid    = foldr1 (===) rows
        rows    = map (foldr1 (|||)) boxes
        boxes   = chunk n ds''
        ds''    = zipWith (<>) ds' (repeat box)
        ds'     = map (pad 1.1 . centerXY) ds
        box     = bg # scale boxSize
        boxSize = boxAll ds'
        boxAll  = maximum . map box1
        box1 d  = map (`diameter` d) [unitX, unitY] # maximum

chunk n [] = []
chunk n xs = take n xs : chunk n (drop n xs)

-- An arrangement specialized for all our samples, which are 3 x 3.

withGrid   = gridLayout 3 bg
    where bg = square 1 # lwG 0.1 # lc black # centerXY # pad 1.1

-- Helper Functions
--
-- Draw a tree with the given parameters but overridden with the specified age and whorl phase.

treeAtAge tp a p = tree tp (AgeParams a 0 p) # renderTree

-- Comparisons
--
-- All comparisons will make alterations to the following set of parameters.

tp :: TreeParams
tp = def {
      tpTrunkLengthIncrementPerYear = 2.0
    , tpTrunkBranchLengthRatio      = 0.6
    , tpTrunkBranchAngles           = [tau / 9, tau / 7, tau / 4.8 , tau / 6.5 ]
    , tpTrunkGirth                  = 6.0
    , tpWhorlsPerYear               = 9
    , tpWhorlSize                   = 11
    , tpBranchGirth                 = 1.0
    , tpBranchBranchLengthRatio     = 1.0
    , tpBranchBranchLengthRatio2    = 1.2
    , tpBranchBranchAngle           = 1/8 @@ turn
    }

-- Trunk length vs. whorls per year

trunkLength_Whorls = trees
    where f tp x y = tp { tpTrunkLengthIncrementPerYear = x
                        , tpWhorlsPerYear               = y
                        }
          trees    = [ treeAtAge (f tp x y) 4 (tau/3) | x <- [1.0, 1.5, 2.0]
                                                      , y <- [3, 6, 9] ]

-- Trunk length vs. trunk-branch angles

trunkLength_trunkBranchAngles = trees
    where f tp x y = tp { tpTrunkLengthIncrementPerYear = x
                        , tpTrunkBranchAngles           = y
                        }
          trees = [ treeAtAge (f tp x y) 3 (tau/3)
                      | x <- [ 1.5, 2.0, 2.5 ]
                      , y <- [ [tau / 8, tau / 5.5,  tau / 6.8, tau / 6.2 ]
                             , [tau / 9, tau / 5,    tau / 7.2, tau / 6.2 ]
                             , [tau / 10, tau / 4.5, tau / 6.5, tau / 5.8 ]] ]

-- Whorls per year vs whorl size.

whorls_whorlSize = trees
    where f tp x y = tp { tpTrunkLengthIncrementPerYear = 2.0
                        , tpTrunkBranchAngles           = [ tau / 10,  tau / 4.5
                                                          , tau / 6.5, tau / 5.8 ]
                        , tpWhorlsPerYear               = x
                        , tpWhorlSize                   = y
                        }
          trees = [ treeAtAge (f tp x y) 3 (tau/3) | x <- [ 5, 9, 13 ]
                                                   , y <- [ 5, 8, 11 ] ]

-- Side-subbranch length vs. Side-subbranch angle

branchLength_branchAngle = trees
    where f tp x y = tp { tpTrunkLengthIncrementPerYear = 2.0
                        , tpTrunkBranchAngles           = [ tau / 6.5, tau / 5.8
                                                          , tau / 10,  tau / 4.5 ]
                        , tpWhorlsPerYear               = 13
                        , tpWhorlSize                   = 5
                        , tpBranchBranchLengthRatio2    = x
                        , tpBranchBranchAngle           = y @@ rad
                        }
          trees = [ treeAtAge (f tp x y) 3 (tau/2) | x <- [ 0.8, 1.2, 1.4 ]
                                                   , y <- [ tau/5, tau/8, tau/11 ] ]

-- Center-subbranch length vs. Side-subbranch length

centerBranchLength_sideBranchLength = trees
    where f tp x y = tp { tpTrunkLengthIncrementPerYear = 2.0
                        , tpTrunkBranchAngles           = [ tau / 6.5, tau / 5.8
                                                          , tau / 10,  tau / 4.5 ]
                        , tpWhorlsPerYear               = 13
                        , tpWhorlSize                   = 5
                        , tpBranchBranchLengthRatio     = x
                        , tpBranchBranchLengthRatio2    = y
                        , tpBranchBranchAngle           = tau/5 @@ rad
                        }
          trees = [ treeAtAge (f tp x y) 3 (tau/2) | x <- [ 0.8, 1.0, 1.2 ]
                                                   , y <- [ 0.8, 1.2, 1.4 ] ]
