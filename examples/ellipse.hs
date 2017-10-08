-- Drawing an Elliptical Arc
--
-- Introduction
--
-- This program shows how to deduce the transformation from
-- circle to ellipse in order to draw an elliptical arc.
--
-- The original motivation was to draw a circular arc defined in 3D but projected to 2D,
-- which became an elliptical arc in the process. Neither the formula for the ellipse nor
-- the effective 2D affine transformation that created it were known, but as many points
-- as desired could be sampled from the original 3D circle projected onto 2D.

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where
import Data.Monoid ((<>))
import Diagrams.Prelude hiding ((<>))
import Diagrams.Backend.SVG.CmdLine
import Conics
import Matrix

main = defaultMain (example # centerXY # pad 1.2)

example = ( ellipticalArc # lc orange
         <> ellipseAxes   # lc orange
         <> circularArc   # lc yellow
         <> ellip         # lc black
         <> circ          # lc lightgray
          ) # scale 10

-- The Circle
--
-- Draw a circle, but help the viewer's intuition by adding a grid with axes and
-- some sample points.

-- circ
--  :: (Semigroup a, TrailLike a, Alignable a, Transformable a,
--      HasStyle a, Juxtaposable a, HasOrigin a, Monoid a, V a ~ R2) =>
--     a
circ =  circleDots # fc blue
     <> unitCircle # lwG 0.2
     <> circleAxes # lwG 0.1
     <> grid 20 20

-- circleDots
--  :: (Semigroup a, TrailLike a, Transformable a, HasStyle a,
--      HasOrigin a, Monoid a, V a ~ R2) =>
--     a
circleDots = position $ zip (origin : circlePoints) (repeat dot)
    where dot = circle 0.03 # lw none

circlePoints :: [P2 Double]
circlePoints = [origin .+^ v | v <- [unitX # rotateBy (i/16) | i <- [0..4]]]

-- circleAxes :: (Semigroup a, TrailLike a, V a ~ R2) => a
circleAxes =  fromOffsets [unit_X, unitX ^* 2]
           <> fromOffsets [unit_Y, unitY ^* 2]

-- circularArc :: (TrailLike b, HasStyle b, V b ~ R2) => b
circularArc = arc (angleDir beginAngle) endAngle # lwG 0.2

beginAngle = (rotate (1/8 @@ turn) (unitX :: V2 Double)) ^. _theta
endAngle   = 1/8 @@ turn :: Angle Double

-- grid
--  :: (Semigroup b, TrailLike b, Alignable b, Transformable b,
--      HasStyle b, Juxtaposable b, HasOrigin b, Monoid b, V b ~ R2) =>
--     Int -> Int -> b
grid m n = matrix 
         # scaleX (2 / fromIntegral n)
         # scaleY (2 / fromIntegral m)
         # centerXY
         # lwG 0.02
    where matrix = foldr (===) mempty (replicate m row)
          row    = foldr (|||) mempty (replicate n cell)
          cell   = square 1

-- The Ellipse
--
-- An ellipse is a circle under an affine transformation. As we are creating the
-- ellipse, we know the transformation, here named transform1.
--
-- If we didn't know the transformation but had enough points on the ellipse, we
-- could determine the transformation, which we will call transform2.
--
-- As a demonstration of the accuracy of the technique, we will draw the ellipse
-- created with transform1 and the elliptical arc created with transform2, which
-- should perfectly coincide. To illustrate the analysis, we will also display the
-- major and minor axes of the ellipse.

-- ellip
--  :: (Semigroup b, TrailLike b, Alignable b, Transformable b,
--      HasStyle b, Juxtaposable b, HasOrigin b, Monoid b, V b ~ R2) =>
--     b
ellip         =  circ # transform1

ei :: EllipseInfo
ei = ellipseFromPoints ellipsePoints

ellipsePoints :: [P2 Double]
ellipsePoints = map transform1 circlePoints

ellipticalArc :: Diagram B
ellipticalArc = drawEllipticalArc ei beginAngle endAngle # lwG 0.2

-- The ellipse axes are the eigenvectors scaled precisely according to the corresponding
-- eigenvalues, all of which come from spectral analysis of the ellipse's formula.

-- ellipseAxes
--  :: (Semigroup b, TrailLike b, Transformable b, HasStyle b,
--      V b ~ R2) =>
--     b
ellipseAxes = (fromOffsets [bv1 ^* s1] <> fromOffsets [bv2 ^* s2])
             # lwG 0.2 # translate dXY

s1, s2 :: Double
bv1, bv2 :: V2 Double
dXY :: V2 Double
Just (s1, s2)   = scaleXY ei
Just (bv1, bv2) = eigenVectors ei
Just dXY        = offsetXY ei

-- For transform1 we specify the transformation directly in terms of scale factors
-- and directions, plus a translation.

-- transform1 :: (Transformable c, V c ~ R2) => c -> c
transform1 = translate (r2(dx, dy))
           . transformBy dr2 dtheta2
           . transformBy dr1 dtheta1
dr1         = 0.4
dtheta1     = negated $ (1/4 - 1/10) @@ turn :: Angle Double
dr2         = 0.8
dtheta2     = negated $ 1/20 @@ turn :: Angle Double
dx         = 0.5
dy         = 0.25

-- For transform2 we build up the transformation by scaling appropriately along the
-- eigenvectors, then translating, if necessary. The values e1 and e2 are the
-- eigenvalues calculated below, and bv1 and bv2 the corresponding orthonormal
-- basis vectors.

-- transform2 :: (Transformable c, V c ~ R2) => c -> c
transform2 = transformByEllipseInfo ei

-- Our general transformation is scaling in a certain direction. A general affine
-- transformation would add a translation. We apply translations in a separate step.
--
-- Note that the rotation by theta is undone, the scaling is applied, then the
-- rotation by theta is reapplied, with the net effect of having done the scaling
-- in the direction of theta.

-- transformBy :: (Transformable a, V a ~ R2) => Double -> Angle Double -> a -> a
transformBy r theta = scaleX r `underT` rotation (negated theta)
