-- A Legend for the Virtual Conifer
--
-- This program draws a diagram explaining some of the tree
-- parameters for the virtual conifer.
--
-- Run the program with dist/build/legend/legend -o legend.svg -w 400
-- where -o sets the output filename, and -w sets the diagram width.
--
-- The Euler angles can also be set from the command line with the following
-- syntax: ... -a "Rad 0.85" -b "Rad -1" -c "Rad 0". For any that is omitted,
-- the default value is used.
--
-- The diagram is modeled in 3D. The planes are projected onto a display plane
-- determined by the Euler angles. The arcs for angle parameters are drawn as
-- elliptical arcs, determined by the ultimate transformation of points on a
-- circle. The labels are overlaid at manually-determined positions.

{-# LANGUAGE FlexibleInstances, NoMonomorphismRestriction, TypeFamilies #-}
module Main where

-- The diagrams prelude provides the 2D functions we need, but for 3D we need to
-- import a few more, aliased because of the conflicting names. Some prelude
-- functions are hidden because of conflicts with Options.Applicative.

import Diagrams.Prelude as P hiding ((<>), option, value)
import Diagrams.ThreeD.Types
import Diagrams.ThreeD.Transform as T3D
import Diagrams.ThreeD.Vector as T3V

-- We need to draw arcs on transformed 3D planes, which we do by sampling points
-- of a circle and calculating the ellipse that the viewer sees.

import Conics(ellipseFromPoints, drawEllipticalArc)

-- We use the default command line processing from diagrams but add a few more
-- options, so import the necessary support.

import Diagrams.Backend.CmdLine
import Diagrams.Backend.SVG.CmdLine
import Options.Applicative
import Safe (readMay)
import Data.Default.Class

-- The Model-to-Screen Transformation
--
-- The Euler angles determine the viewing angle. The default values were found by
-- experimentation. Angles a, b, and c are rotations about the Z axis,
-- new X axis, then new Z axis, respectively.

data EulerAngles = EA Rad Rad Rad deriving (Show, Eq)

instance Default EulerAngles where
    def = EA 1.315 (-1.39) 0

-- Rotate the model according to the Euler angles and project onto the XY-plane.

modelToScreen :: [[P3]] -> [[P2]]
modelToScreen = modelToScreen' def

modelToScreen' :: EulerAngles -> [[P3]] -> [[P2]]
modelToScreen' ea = flatten . spin ea

-- Spin the model so that a projection of the new XY-plane is trivial.

spin :: EulerAngles -> [[P3]] -> [[P3]]
spin (EA a b c) = map (map m) 
    where m = rotateAboutZ c
            . rotateAboutX b
            . rotateAboutZ a

rotateAboutZ :: Rad -> P3 -> P3
rotateAboutZ theta = transform (aboutZ theta)

rotateAboutX :: Rad -> P3 -> P3
rotateAboutX theta = transform (aboutX theta)

flatten :: [[P3]] -> [[P2]]
flatten  = map (map projectXY) 

projectXY :: P3 -> P2
projectXY p = p2 (x, y) where (x, y, _) = unp3 p

-- The Model
--
-- The following values describe the 3D model: a rectangular base plane with a vertical
-- plane rising from its long bisector, and a third plane rising at an incline from one
-- end of the base. A tapered section of trunk is at the front of the vertical plane,
-- that is, the end where the inclined plane meets the base. Two levels of branching
-- are drawn in the inclined plane. The trunk-branch angle and the branch-branch angle
-- are indicated, as are the trunk length increment and the ratios of branch to trunk,
-- straight branch to branch, and side branch to branch. These ratios are all implicitly
-- determined by the intersections of the planes.
--
-- Determine the corners of the three intersecting planes:

baseWidth       = 1.5
baseLength      = 4.0
trunkHeight     = 2.5
branchTipHeight = trunkHeight / 2

-- Place the section of trunk and determine the taper:

trunk :: [[P3]]
trunk = [[ origin, p3 (0, 0, trunkHeight) ]]

girth0 = 0.05
girth1 = 0.02

-- Build up the planes and branches and the points that will be used to determine
-- the elliptical arcs for indicating angles:

planes :: [[P3]]
planes = [ branchRect, vertRect, horizRect ]

horizRect :: [P3]
horizRect = [ 
              p3 (          0,  baseWidth/2, 0)
            , p3 (          0, -baseWidth/2, 0)
            , p3 ( baseLength, -baseWidth/2, 0)
            , p3 ( baseLength,  baseWidth/2, 0)
            ] 

vertRect :: [P3]
vertRect = [ 
             p3 (          0, 0,           0)
           , p3 (          0, 0, trunkHeight)
           , p3 ( baseLength, 0, trunkHeight)
           , p3 ( baseLength, 0,           0)
           ]

branchRect :: [P3]
branchRect = [ 
               p3 (          0,  baseWidth/2,               0)
             , p3 (          0, -baseWidth/2,               0)
             , p3 ( baseLength, -baseWidth/2, branchTipHeight)
             , p3 ( baseLength,  baseWidth/2, branchTipHeight)
             ]

branches :: [[P3]]
branches = [ center, left, right ]
   where center = [ origin, branchPoint # scale 1.8 ]
         left   = [ branchPoint, p3 (baseLength, -baseWidth/2, branchTipHeight)  ]
         right  = [ branchPoint, p3 (baseLength,  baseWidth/2, branchTipHeight)  ]

branchPoint = p3 (baseLength, 0, branchTipHeight) # scale (2/3)

trunkAnglePts :: [[P3]]
trunkAnglePts = [ map (origin .+^) [T3V.unitX, T3V.unit_X, T3V.unitZ, T3V.unit_Z, 
                                 r3(baseLength, 0, branchTipHeight) # normalized] ]

trunkBranchAngle :: Rad
trunkBranchAngle = T3V.angleBetween T3V.unitX (p .-. origin) 
    where p = last (last trunkAnglePts)

-- Define enough points on a circle to draw an elliptical arc between the side branches.

branchAnglePts :: [[P3]]
branchAnglePts = [[
                    branchPoint .+^ ((pt1 .-. branchPoint)    # normalized # scale r)
                 ,  branchPoint .+^ ((pt2 .-. branchPoint)    # normalized # scale r)
                 ,  branchPoint .+^ ((branchPoint .-. pt1)    # normalized # scale r)
                 ,  branchPoint .+^ ((branchPoint .-. pt2)    # normalized # scale r)
                 ,  branchPoint .+^ ((branchPoint .-. origin) # normalized # scale r)
                 ]]
    where pt1 = p3 (baseLength, -baseWidth/2, branchTipHeight)
          pt2 = p3 (baseLength,  baseWidth/2, branchTipHeight)
          r   = s * magnitude (branchPoint .-. pt1)
          s   = 0.3

-- Add a few more points for intuition about the circle.

moreBranchAnglePts :: [[P3]]
moreBranchAnglePts = [[
                    branchPoint .+^ ((pt3 .-. branchPoint)    # normalized # scale r)
                 ,  branchPoint .+^ ((branchPoint .-. pt3)    # normalized # scale r)
                 ,  branchPoint .+^ ((origin .-. branchPoint) # normalized # scale r)
                 ]]
    where pt1     = p3 (baseLength, -baseWidth/2, branchTipHeight)
          (x,y,z) = unp3 branchPoint
          pt3     = p3 (x, r, z)
          r       = s * magnitude (branchPoint .-. pt1)
          s       = 0.3

-- The Main Program
--
-- Define instances of Parseable for Rad and EulerAngles so we can read the latter
-- from the command line. This makes the process of tuning the viewing angle much faster.

instance Parseable Rad where
  parser = argument readMay mempty

instance Parseable EulerAngles where
  parser =   EA
         <$> option (long "eulerA" <> short 'a' <> value 1.315   <> help "Euler angle a")
         <*> option (long "eulerB" <> short 'b' <> value (-1.39) <> help "Euler angle b")
         <*> option (long "eulerC" <> short 'c' <> value 0       <> help "Euler angle c")

main = mainWith (\ea@(EA a b c) -> legend ea # centerXY # pad 1.2)

-- Assemble the components of the diagram.

legend ea =  legendLabels
          <> legendTrunkAngle ea
          <> legendBranchAngles ea
        --  <> legendBranchAnglePoints ea
        --  <> legendMoreBranchAnglePoints ea
          <> legendPlanes ea
          <> legendTrunk ea
          <> legendBranches ea

legendTrunk :: EulerAngles -> Diagram B R2
legendTrunk ea = (drawTrunk girth0 girth1 . modelToScreen' ea) trunk

legendBranches :: EulerAngles -> Diagram B R2
legendBranches ea = (mconcat . map drawTip . modelToScreen' ea) branches

legendPlanes :: EulerAngles -> Diagram B R2
legendPlanes ea = (drawPlanes . modelToScreen' ea) planes

-- TODO Fix the angle fudging; scale the original points rather than the arc

legendTrunkAngle :: EulerAngles -> Diagram B R2
legendTrunkAngle ea = drawEllipticalArc ei a1' a2' # scale r
    where a1            = trunkBranchAngle
          a2            = P.direction P.unitY :: Rad
          a1'           = a1 + angleFudge
          a2'           = a2 + angleFudge
          r             = 0.5
          [ps]           = modelToScreen' ea trunkAnglePts
          ei             = ellipseFromPoints ps
          angleFudge     = 0.15::Rad	-- compensate for rounding error in drawEllipticalArc

-- TODO Fix the angle calculation

legendBranchAngles :: EulerAngles -> Diagram B R2
legendBranchAngles ea = drawEllipticalArc ei a1' a2'
    where a1   = trunkBranchAngle
          a2   = P.direction P.unitY :: Rad
          a1'  = a1 + (0.57::Rad)
          a2'  = a2 + (0.29::Rad)
          [ps] = modelToScreen' ea branchAnglePts
          ei   = ellipseFromPoints ps

-- Show the points involved in drawing the elliptical arc between the branches,
-- for debugging.

legendBranchAnglePoints :: EulerAngles -> Diagram B R2
legendBranchAnglePoints ea = position (zip ps (repeat dot))
    where dot = circle 0.02 # lw 0 # fc blue
          [ps] = branchAnglePts # modelToScreen' ea

-- Show more points on the circle around the branch point in the inclined plane.
-- These points aren't used to calculate the elliptical arc, but are used to
-- give confidence that the arc looks right, being part of the transformed circle.

legendMoreBranchAnglePoints :: EulerAngles -> Diagram B R2
legendMoreBranchAnglePoints ea = position (zip ps (repeat dot))
    where dot = circle 0.02 # lw 0 # fc yellow
          [ps] = moreBranchAnglePts # modelToScreen' ea

-- Label the angles and ratios represented in the diagram. The positions were
-- determined experimentally, as were the scale factors.

legendLabels :: Diagram B R2
legendLabels =  position angles
             <> position ratios
           --  <> position (zip ps   (repeat (dot # fc blue)))       -- alignment helpers
    where dot  = circle 0.02 # lw 0
          ps     = [p2(0,1), p2(1,0), p2(1,1), p2(0,0)]
          angles = [ (p2(0.15,0.5), dPhi)
                   , (p2(0.7,1.618), dTheta)
                   , (p2(0.95,1.6), dTheta)]
          ratios = [ (p2(0.11,1.02),   dRatio 1)
                   , (p2(0.44,1),      dRatio 2)
                   , (p2(0.87,1.8),    dRatio 3)
                   , (p2(0.45, 1.545), dRatio 4)
                   , (p2(1.2, 1.5),    dRatio 4)]
          thetaSymbol = toEnum 0X3B8 :: Char
          phiSymbol   = toEnum 0X3D5 :: Char
          dTheta      = text [thetaSymbol] # italic # fontSize 1 # scale 0.15
          dPhi        = text [phiSymbol]   # italic # fontSize 1 # scale 0.15
          dRatio n    = text "r" # italic # fontSize 1 # scale 0.15
                                 # withSubscript (show n) 0.075

-- For the purposes of this diagram, the following produced a perfect subscript.

withSubscript :: String -> Double -> Diagram B R2 -> Diagram B R2
withSubscript t s d = d <> topLeftText t # scale s

-- Drawing Functions

drawPlanes :: [[P2]] -> Diagram B R2
drawPlanes = mconcat . map drawRect

drawRect :: [P2] -> Diagram B R2
drawRect ps = fromVertices (ps ++ [head ps]) # lw 0.01 # dashing [0.03, 0.03] 0

drawTrunk :: Double -> Double -> [[P2]] -> Diagram B R2
drawTrunk g0 g1 [[p0, p1]] = place trunk p0
    where trunk = (closeLine . lineFromVertices) [ p0, a, b, c, d ]
                  # strokeLoop 
                  # fc black 
                  # lw 0.01 
          n = (p1 .-. p0) # rotateBy (1/4) # normalized
          g0_2 = g0 / 2
          g1_2 = g1 / 2
          a = p0 .-^ (g0_2 *^ n)
          b = p1 .-^ (g1_2 *^ n)
          c = p1 .+^ (g1_2 *^ n)
          d = p0 .+^ (g0_2 *^ n)

drawTip :: [P2] -> Diagram B R2 
drawTip [p0, p1] = position [(p0, fromOffsets [ p1 .-. p0 ] # lw 0.01)]
