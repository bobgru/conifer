A Comparison of Virtual Conifers
================================

This program draws an array of conifers with variations of parameters.

> {-# LANGUAGE NoMonomorphismRestriction #-}
> import Conifer
> import Diagrams.Prelude hiding (render)
> import Diagrams.Backend.SVG.CmdLine
> import Diagrams.ThreeD.Types
> import Data.Default.Class

Run the program with `dist/build/comparison/comparison -o comparison.svg -w 800` 
where `-o` sets the output filename, and `-w` sets the diagram width.

> main           = defaultMain (example # centerXY # pad 1.2)
> render         = drawTree origin . projectTreeXZ . toAbsoluteTree origin
> treeAtAge tp a = render (tree (tp { tpAge = a }))

> example = 
> --    trunkLength_Whorls
> --    trunkLength_trunkBranchAngles
> --    whorls_whorlSize
> --    branchLength_branchAngle
>     centerBranchLength_sideBranchLength

A starting point for variations.

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

> gridLayout n e ds = grid # centerXY
>     where
>         grid    = foldr1 (===) rows
>         rows    = map (foldr1 (|||)) boxes
>         boxes   = chunk n ds''
>         ds''    = zipWith (<>) ds' (repeat box)
>         ds'     = map (pad 1.1 . centerXY) ds
>         box     = e # scale boxSize
>         boxSize = boxAll ds'
>         boxAll  = maximum . (map box1)
>         box1 d  = map (flip diameter d) [unitX, unitY] # maximum

> chunk n [] = []
> chunk n xs = take n xs : chunk n (drop n xs)

> background = square 1 # lw 0.1 # lc black # centerXY # pad 1.1

A sample of comparisons.

Trunk length vs. whorls per year

> trunkLength_Whorls = trees # gridLayout 3 background
>     where f tp x y = tp { tpTrunkLengthIncrement = x
>                         , tpWhorlsPerYear        = y
>                         }
>           trees    = [ treeAtAge (f tp x y) 3 | x <- [1.0, 1.5, 2.0]
>                                               , y <- [3, 6, 9] ]

Trunk length vs. trunk-branch angles

> trunkLength_trunkBranchAngles = trees # gridLayout 3 background
>     where f tp x y = tp { tpTrunkLengthIncrement = x
>                         , tpTrunkBranchAngles    = y
>                         }
>           trees = [ treeAtAge (f tp x y) 2 
>                       | x <- [ 1.5, 2.0, 2.5 ], 
>                         y <- [ [tau / 8, tau / 5.5,  tau / 6.8, tau / 6.2 ]
>                              , [tau / 9, tau / 5,    tau / 7.2, tau / 6.2 ]
>                              , [tau / 10, tau / 4.5, tau / 6.5, tau / 5.8 ]] ]

Whorls per year vs whorl size.

> whorls_whorlSize = trees # gridLayout 3 background
>     where f tp x y = tp { tpTrunkLengthIncrement = 2.0
>                         , tpTrunkBranchAngles    = [ tau / 10,  tau / 4.5
>                                                    , tau / 6.5, tau / 5.8 ]
>                         , tpWhorlsPerYear        = x
>                         , tpWhorlSize            = y
>                         }
>           trees = [ treeAtAge (f tp x y) 2 | x <- [ 5, 9, 13 ]
>                                            , y <- [ 5, 8, 11 ] ]

Side-subbranch length vs. Side-subbranch angle

> branchLength_branchAngle = trees # gridLayout 3 background
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

> centerBranchLength_sideBranchLength = trees # gridLayout 3 background
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
