A Legend for the Virtual Conifer
================================

This program draws a diagram explaining some of the tree
parameters for the virtual conifer.

> {-# LANGUAGE NoMonomorphismRestriction #-}
> import Diagrams.Prelude
> import Diagrams.Backend.SVG.CmdLine
> import Diagrams.ThreeD.Types
> import Diagrams.ThreeD.Transform as T3D
> import Diagrams.ThreeD.Vector as T3V
> import Data.Default.Class

Run the program with `dist/build/one-conifer/legend -o legend.svg -w 400` 
where `-o` sets the output filename, and `-w` sets the diagram width.

> main = defaultMain (legend theta phi # centerXY # pad 1.2)

> legend theta phi =  legendPlanes theta phi
>                  <> legendTrunk theta phi
>                  <> legendBranches theta phi

> legendTrunk :: Rad -> Rad -> Diagram B R2
> legendTrunk theta phi = (drawTrunk girth0 girth1 . flatten . spin theta phi) trunk

> legendBranches :: Rad -> Rad -> Diagram B R2
> legendBranches theta phi = (mconcat . map drawTip . flatten . spin theta phi) branches

> legendPlanes :: Rad -> Rad -> Diagram B R2
> legendPlanes theta phi = (drawPlanes . flatten . spin theta phi) planes

> planes :: [[P3]]
> planes = [ branchRect, vertRect, horizRect ]

> baseWidth       = 1.5
> baseLength      = 3.5
> trunkHeight     = 2
> branchTipHeight = trunkHeight / 2
> theta           = pi * (9.1/10) :: Rad
> phi             = pi * (2.2/5) :: Rad

> trunk :: [[P3]]
> trunk = [[ origin, p3 (0, 0, trunkHeight) ]]

> girth0 = 0.1
> girth1 = 0.05

> branches :: [[P3]]
> branches = [ center, left, right ]
>    where v = r3 (baseLength, 0, branchTipHeight)
>          v' = v # scale (2/3)
>          center = [ origin, origin .+^ v # scale 1.2 ]
>          left   = [ (origin .+^ v'), p3 (baseLength, -baseWidth/2, branchTipHeight)  ]
>          right  = [ (origin .+^ v'), p3 (baseLength,  baseWidth/2, branchTipHeight)  ]

> horizRect :: [P3]
> horizRect = [ p3 (          0, -baseWidth/2, 0)
>             , p3 ( baseLength, -baseWidth/2, 0)
>             , p3 ( baseLength,  baseWidth/2, 0)
>             , p3 (          0,  baseWidth/2, 0)
>             ] 

> vertRect :: [P3]
> vertRect = [ p3 (          0, 0,           0)
>            , p3 (          0, 0, trunkHeight)
>            , p3 ( baseLength, 0, trunkHeight)
>            , p3 ( baseLength, 0,           0)
>            ]

> branchRect :: [P3]
> branchRect = [ p3 (          0, -baseWidth/2,               0)
>              , p3 ( baseLength, -baseWidth/2, branchTipHeight)
>              , p3 ( baseLength,  baseWidth/2, branchTipHeight)
>              , p3 (          0,  baseWidth/2,               0)
>              ]

> spin :: Rad -> Rad -> [[P3]] -> [[P3]]
> spin theta phi = map (map m) 
>     where m = rotateAboutX (phi - pi) . rotateAboutZ (pi/2 - theta)

> rotateAboutZ :: Rad -> P3 -> P3
> rotateAboutZ theta = transform (aboutZ theta)

> rotateAboutX :: Rad -> P3 -> P3
> rotateAboutX theta = transform (aboutX theta)

> flatten :: [[P3]] -> [[P2]]
> flatten  = map (map projectXY) 

> projectXY :: P3 -> P2
> projectXY p = p2 (x, y) where (x, y, _) = unp3 p

> drawPlanes :: [[P2]] -> Diagram B R2
> drawPlanes pss = (mconcat . map drawRect) pss

> drawRect :: [P2] -> Diagram B R2
> drawRect ps = fromVertices (ps ++ [head ps]) # lw 0.01 # dashing [0.03, 0.03] 0

> drawTrunk :: Double -> Double -> [[P2]] -> Diagram B R2
> drawTrunk g0 g1 [[p0, p1]] = place trunk p0
>     where trunk = (closeLine . lineFromVertices) [ p0, a, b, c, d ]
>                   # strokeLoop 
>                   # fc black 
>                   # lw 0.01 
>           n = (p1 .-. p0) # rotateBy (1/4) # normalized
>           g0_2 = g0 / 2
>           g1_2 = g1 / 2
>           a = p0 .-^ (g0_2 *^ n)
>           b = p1 .-^ (g1_2 *^ n)
>           c = p1 .+^ (g1_2 *^ n)
>           d = p0 .+^ (g0_2 *^ n)

> drawTip :: [P2] -> Diagram B R2 
> drawTip [p0, p1] = position [(p0, fromOffsets [ p1 .-. p0 ] # lw 0.01)]
