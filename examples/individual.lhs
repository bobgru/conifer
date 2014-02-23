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

> main   = defaultMain (tree tp ap # render' # centerXY # pad 1.2)

> render' = if withNeedles 
>               then renderTreeWithNeedles needlePolicy np
>               else renderTree

Change `withNeedles` to `False` to draw the tree without needles.

> withNeedles = True

> needlePolicy :: Double -> Bool
> needlePolicy age = age <= 2.0

> np :: NeedleParams
> np = def {
>       needleLength = 0.05
>     , needleAngle  = tau / 10
>     , needleIncr   = 0.05
>     }


> tp :: TreeParams
> tp = def {
>       tpTrunkLengthIncrementPerYear = 1.4
>     , tpTrunkBranchLengthRatio      = 0.6
>     , tpTrunkBranchAngles           = [tau / 9, tau / 7, tau / 4.8 , tau / 6.5 ]
>     , tpTrunkGirth                  = 5.0
>     , tpWhorlsPerYear               = 9
>     , tpWhorlSize                   = 7
>     , tpBranchGirth                 = 1.0
>     , tpBranchBranchLengthRatio     = 1.0
>     , tpBranchBranchLengthRatio2    = 1.0
>     , tpBranchBranchAngle           = tau / 6
>     }

> ap :: AgeParams
> ap = AgeParams 3 0 (tau / 3)
