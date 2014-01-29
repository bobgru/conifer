An Individual Conifer
=====================

This program draws the conifer with the specified values.

> {-# LANGUAGE NoMonomorphismRestriction #-}
> import Conifer
> import Diagrams.Prelude
> import Diagrams.Backend.SVG.CmdLine
> import Data.Default.Class

Run the program with `dist/build/individual/individual -o individual.svg -w 400` 
where `-o` sets the output filename, and `-w` sets the diagram width.

> main   = defaultMain (tree tp # render' # centerXY # pad 1.2)

> render' = if withNeedles 
>               then renderTreeWithNeedles needlePolicy
>               else renderTree

Change `withNeedles` to `False` to draw the tree without needles.

> withNeedles = True

> needlePolicy :: Double -> Bool
> needlePolicy age = age <= 2.0

> tp = def {
>       tpAge                         = 3
>     , tpTrunkLengthIncrementPerYear = 1.4
>     , tpTrunkBranchLengthRatio      = 0.6
>     , tpTrunkBranchAngles           = [tau / 9, tau / 7, tau / 4.8 , tau / 6.5 ]
>     , tpTrunkGirth                  = 5.0
>     , tpWhorlsPerYear               = 9
>     , tpWhorlSize                   = 7
>     , tpWhorlPhase                  = tau / 3
>     , tpBranchGirth                 = 1.0
>     , tpBranchBranchLengthRatio     = 1.0
>     , tpBranchBranchLengthRatio2    = 1.0
>     , tpBranchBranchAngle           = tau / 6
>     }
