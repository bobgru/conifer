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

> main   = defaultMain (renderTree tp # centerXY # pad 1.2)

> tp = def {
>       tpAge                      = 2
>     , tpTrunkLengthIncrement     = 1.4
>     , tpTrunkBranchLengthRatio   = 0.6
>     , tpTrunkBranchAngles        = [tau / 9, tau / 7, tau / 4.8 , tau / 6.5 ]
>     , tpTrunkGirth               = 5.0
>     , tpWhorlsPerYear            = 9
>     , tpWhorlSize                = 7
>     , tpWhorlPhase               = tau / 3
>     , tpBranchGirth              = 1.0
>     , tpBranchBranchLengthRatio  = 1.0
>     , tpBranchBranchLengthRatio2 = 1.0
>     , tpBranchBranchAngle        = tau / 6
>     }
