A Legend for the Virtual Conifer
================================

This program draws a diagram explaining some of the tree
parameters for the virtual conifer.

> {-# LANGUAGE NoMonomorphismRestriction #-}
> module Main where
> import Diagrams.Prelude as P
> import Diagrams.Backend.SVG.CmdLine
> import Diagrams.ThreeD.Types
> import Diagrams.ThreeD.Transform as T3D
> import Diagrams.ThreeD.Vector as T3V
> import Data.Default.Class
> import Data.List (minimumBy)
> import Conics(ellipseFromPoints, drawEllipticalArc)

Run the program with `dist/build/legend/legend -o legend.svg -w 400` 
where `-o` sets the output filename, and `-w` sets the diagram width.

**The Arbitrary Parameters**

The values `theta` (radians counterclockwise rotation around unitZ) and `phi`
(radians clockwise rotation about new unitX) determine the viewing angle and were
found by experimentation. 

> theta = pi * (9.15/10) :: Rad
> phi   = pi * (2.2/5) :: Rad

**The Model-to-Screen Transformation**

Spin the model so that a projection of the new XY-plane is trivial.

> modelToScreen :: [[P3]] -> [[P2]]
> modelToScreen = flatten . spin theta phi

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

**The Model**

The following values describe the 3D model: a rectangular base plane with a vertical
plane rising from its long bisector, and a third plane rising at an incline from one
end of the base. A tapered section of trunk is at the front of the vertical plane,
that is, the end where the inclined plane meets the base. Two levels of branching
are drawn in the inclined plane. The trunk-branch angle and the branch-branch angle
are indicated, as are the trunk length increment and the ratios of branch to trunk,
straight branch to branch, and side branch to branch. These ratios are all implicitly
determined by the intersections of the planes.

Determine the corners of the three intersecting planes:

> baseWidth       = 1.5
> baseLength      = 4.0
> trunkHeight     = 2.5
> branchTipHeight = trunkHeight / 2

Place the section of trunk and determine the taper:

> trunk :: [[P3]]
> trunk = [[ origin, p3 (0, 0, trunkHeight) ]]

> girth0 = 0.05
> girth1 = 0.02

Build up the planes and branches and the points that will be used to determine
the elliptical arcs for indicating angles:

> planes :: [[P3]]
> planes = [ branchRect, vertRect, horizRect ]

> horizRect :: [P3]
> horizRect = [ 
>               p3 (          0,  baseWidth/2, 0)
>             , p3 (          0, -baseWidth/2, 0)
>             , p3 ( baseLength, -baseWidth/2, 0)
>             , p3 ( baseLength,  baseWidth/2, 0)
>             ] 

> vertRect :: [P3]
> vertRect = [ 
>              p3 (          0, 0,           0)
>            , p3 (          0, 0, trunkHeight)
>            , p3 ( baseLength, 0, trunkHeight)
>            , p3 ( baseLength, 0,           0)
>            ]

> branchRect :: [P3]
> branchRect = [ 
>                p3 (          0,  baseWidth/2,               0)
>              , p3 (          0, -baseWidth/2,               0)
>              , p3 ( baseLength, -baseWidth/2, branchTipHeight)
>              , p3 ( baseLength,  baseWidth/2, branchTipHeight)
>              ]

> branches :: [[P3]]
> branches = [ center, left, right ]
>    where v = r3 (baseLength, 0, branchTipHeight)
>          v' = v # scale (2/3) -- pick the branching point
>          center = [ origin, origin .+^ v # scale 1.2 ]
>          left   = [ (origin .+^ v'), p3 (baseLength, -baseWidth/2, branchTipHeight)  ]
>          right  = [ (origin .+^ v'), p3 (baseLength,  baseWidth/2, branchTipHeight)  ]

> trunkAnglePts :: [[P3]]
> trunkAnglePts = [ map (origin .+^) [T3V.unitX, T3V.unit_X, T3V.unitZ, T3V.unit_Z, 
>                                  r3(baseLength, 0, branchTipHeight) # normalized] ]

> trunkBranchAngle :: Rad
> trunkBranchAngle = T3V.angleBetween T3V.unitX (p .-. origin) 
>     where p = last (last trunkAnglePts)

**The Main Program**

Assemble the components of the diagram.

> main = defaultMain (legend theta phi # centerXY # pad 1.2)

> legend theta phi =  legendTrunkAngle
>                  <> legendPlanes
>                  <> legendTrunk
>                  <> legendBranches

> legendTrunk :: Diagram B R2
> legendTrunk = (drawTrunk girth0 girth1 . modelToScreen) trunk

> legendBranches :: Diagram B R2
> legendBranches = (mconcat . map drawTip . modelToScreen) branches

> legendPlanes :: Diagram B R2
> legendPlanes = (drawPlanes . modelToScreen) planes

> legendTrunkAngle :: Diagram B R2
> legendTrunkAngle = drawEllipticalArc ei a1' a2' # scale r
>     where a1            = trunkBranchAngle
>           a2            = P.direction P.unitY :: Rad
>           a1'           = a1 + angleFudge
>           a2'           = a2 + angleFudge
>           r             = 0.5
>           [ps]           = modelToScreen trunkAnglePts
>           ei             = ellipseFromPoints ps
>           angleFudge     = 0.15::Rad	-- compensate for rounding error in drawEllipticalArc

> drawPlanes :: [[P2]] -> Diagram B R2
> drawPlanes = mconcat . map drawRect

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
