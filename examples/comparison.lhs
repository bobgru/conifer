A Comparison of Virtual Conifers
================================

This program draws an array of conifers with variations of parameters.

> {-# LANGUAGE NoMonomorphismRestriction #-}
> import Conifer
> import Diagrams.Prelude
> import Diagrams.Backend.SVG.CmdLine
> import Diagrams.ThreeD.Types
> import Data.Default.Class

**Main Program**

Run the program with `dist/build/comparison/comparison -o comparison.svg -w 800` 
where `-o` sets the output filename, and `-w` sets the diagram width.

> main = defaultMain (example # withGrid # centerXY # pad 1.2)

**Selected Comparison**

Uncomment one at a time and recompile with `cabal build`
or `ghc --make comparison.lhs`.

> example = 
> --    trunkLength_Whorls
> --    trunkLength_trunkBranchAngles
> --    whorls_whorlSize
> --    branchLength_branchAngle
>     centerBranchLength_sideBranchLength

**Layout**

Create an array of background cells sized to fit the largest of a list
of diagrams, where _n_ is the cells per row, and _bg_ is the background.

> gridLayout n bg ds = grid # centerXY
>     where
>         grid    = foldr1 (===) rows
>         rows    = map (foldr1 (|||)) boxes
>         boxes   = chunk n ds''
>         ds''    = zipWith (<>) ds' (repeat box)
>         ds'     = map (pad 1.1 . centerXY) ds
>         box     = bg # scale boxSize
>         boxSize = boxAll ds'
>         boxAll  = maximum . map box1
>         box1 d  = map (`diameter` d) [unitX, unitY] # maximum

> chunk n [] = []
> chunk n xs = take n xs : chunk n (drop n xs)

An arrangement specialized for all our samples, which are 3 _x_ 3.

> withGrid   = gridLayout 3 bg
>     where bg = square 1 # lw 0.1 # lc black # centerXY # pad 1.1

**Helper Functions**

Draw a tree with the given parameters but overridden with the specified age.

> treeAtAge tp a = renderTree (tp { tpAge = a })

**Comparisons**

All comparisons will make alterations to the following set of parameters.

> tp :: TreeParams
> tp = def {
>       tpAge                      = 2
>     , tpTrunkLengthIncrement     = 2.0
>     , tpTrunkBranchLengthRatio   = 0.6
>     , tpTrunkBranchAngles        = [tau / 9, tau / 7, tau / 4.8 , tau / 6.5 ]
>     , tpTrunkGirth               = 6.0
>     , tpWhorlsPerYear            = 9
>     , tpWhorlSize                = 11
>     , tpWhorlPhase               = tau / 3
>     , tpBranchGirth              = 1.0
>     , tpBranchBranchLengthRatio  = 1.0
>     , tpBranchBranchLengthRatio2 = 1.2
>     , tpBranchBranchAngle        = tau / 8
>     }

Trunk length vs. whorls per year

> trunkLength_Whorls = trees
>     where f tp x y = tp { tpTrunkLengthIncrement = x
>                         , tpWhorlsPerYear        = y
>                         }
>           trees    = [ treeAtAge (f tp x y) 3 | x <- [1.0, 1.5, 2.0]
>                                               , y <- [3, 6, 9] ]

Trunk length vs. trunk-branch angles

> trunkLength_trunkBranchAngles = trees
>     where f tp x y = tp { tpTrunkLengthIncrement = x
>                         , tpTrunkBranchAngles    = y
>                         }
>           trees = [ treeAtAge (f tp x y) 2 
>                       | x <- [ 1.5, 2.0, 2.5 ]
>                       , y <- [ [tau / 8, tau / 5.5,  tau / 6.8, tau / 6.2 ]
>                              , [tau / 9, tau / 5,    tau / 7.2, tau / 6.2 ]
>                              , [tau / 10, tau / 4.5, tau / 6.5, tau / 5.8 ]] ]

Whorls per year vs whorl size.

> whorls_whorlSize = trees
>     where f tp x y = tp { tpTrunkLengthIncrement = 2.0
>                         , tpTrunkBranchAngles    = [ tau / 10,  tau / 4.5
>                                                    , tau / 6.5, tau / 5.8 ]
>                         , tpWhorlsPerYear        = x
>                         , tpWhorlSize            = y
>                         }
>           trees = [ treeAtAge (f tp x y) 2 | x <- [ 5, 9, 13 ]
>                                            , y <- [ 5, 8, 11 ] ]

Side-subbranch length vs. Side-subbranch angle

> branchLength_branchAngle = trees
>     where f tp x y = tp { tpTrunkLengthIncrement     = 2.0
>                         , tpTrunkBranchAngles        = [ tau / 6.5, tau / 5.8
>                                                        , tau / 10,  tau / 4.5 ]
>                         , tpWhorlsPerYear            = 13
>                         , tpWhorlSize                = 5
>                         , tpWhorlPhase               = tau / 2
>                         , tpBranchBranchLengthRatio2 = x
>                         , tpBranchBranchAngle        = y
>                         }
>           trees = [ treeAtAge (f tp x y) 2 | x <- [ 0.8, 1.2, 1.4 ]
>                                            , y <- [ tau/5, tau/8, tau/11 ] ]

Center-subbranch length vs. Side-subbranch length

> centerBranchLength_sideBranchLength = trees
>     where f tp x y = tp { tpTrunkLengthIncrement     = 2.0
>                         , tpTrunkBranchAngles        = [ tau / 6.5, tau / 5.8
>                                                        , tau / 10,  tau / 4.5 ]
>                         , tpWhorlsPerYear            = 13
>                         , tpWhorlSize                = 5
>                         , tpWhorlPhase               = tau / 2
>                         , tpBranchBranchLengthRatio  = x
>                         , tpBranchBranchLengthRatio2 = y
>                         , tpBranchBranchAngle        = tau/5
>                         }
>           trees = [ treeAtAge (f tp x y) 2 | x <- [ 0.8, 1.0, 1.2 ]
>                                            , y <- [ 0.8, 1.2, 1.4 ] ]
