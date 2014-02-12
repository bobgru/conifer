Drawing an Elliptical Arc
=========================

**Introduction**

This program shows how to deduce the transformation from
circle to ellipse in order to draw an elliptical arc.

The original motivation was to draw a circular arc defined in 3D but projected to 2D,
which became an elliptical arc in the process. Neither the formula for the ellipse nor
the effective 2D affine transformation that created it were known, but as many points
as desired could be sampled from the original 3D circle projected onto 2D.

> {-# LANGUAGE NoMonomorphismRestriction, TypeFamilies #-}
> module Conics (EllipseInfo(..)
>               ,ellipseFromPoints
>               ,drawEllipse, drawEllipticalArc
>               ,transformByEllipseInfo)
> where
> import Diagrams.Prelude
> import Diagrams.Backend.SVG.CmdLine
> import Data.Default.Class
> import Matrix

**The Ellipse**

An ellipse is a circle under an affine transformation. The `diagrams` package
includes two functions to draw ellipses, given the ... or the eccentricity.

If all you have are five sample points, this library will provide the other pertinent
information about the ellipse through those points.

> data EllipseInfo a = EI {
>       samplePoints :: Maybe [Point a]  -- there must be 5 points
>     , coefficients :: Maybe [Double]    -- the coefficients
>     , eigenValues  :: Maybe (Double, Double)
>     , eigenVectors :: Maybe (a, a)
>     , scaleXY      :: Maybe (Double, Double)
>     , offsetXY     :: Maybe a
> }

> drawEllipse :: EllipseInfo R2 -> Diagram B R2
> drawEllipse ei = unitCircle # transformByEllipseInfo ei

> drawEllipticalArc :: EllipseInfo R2 -> Rad -> Rad -> Diagram B R2
> drawEllipticalArc ei a1 a2 = arc a1 a2 # transformByEllipseInfo ei

> transformByEllipseInfo :: (Transformable c, V c ~ R2) => EllipseInfo R2 -> c -> c
> transformByEllipseInfo ei = translate dXY
>                           . transformBy s1 (direction bv1 :: Rad)
>                           . transformBy s2 (direction bv2 :: Rad)
>     where Just (e1, e2)   = eigenValues ei
>           Just (bv1, bv2) = eigenVectors ei
>           Just (s1, s2)   = scaleXY ei
>           Just dXY        = offsetXY ei

Our general transformation is scaling in a certain direction. A general affine
transformation would add a translation. We apply translations in a separate step.

Note that the rotation by `theta` is undone, the scaling is applied, then the 
rotation by `theta` is reapplied, with the net effect of having done the scaling
in the direction of `theta`.

> transformBy :: (Angle a1, Transformable a, V a ~ R2) => Double -> a1 -> a -> a
> transformBy r theta = scaleX r `under` rotation (-theta)

**Analysis of the Ellipse**

The general form of the equation of an ellipse is

_ax^2 + bxy + cy^2 + dx + ey = 1_

The general quadratic equation is usually given with a constant term _f_ and 0 on 
the right hand side. By subtracting _f_ then dividing by _-f_ on both sides, the
above form of the equation emerges.

We have five unknowns—the coefficients—which are the same for any point
on the ellipse. If we substitute _x_ and _y_ from five different points on
the ellipse, we get a system of linear equations, which we can solve by 
row-reducing the matrix for the system.

We already have points on the circle, so transform them to get points on the ellipse.

**TODO** Error handling is rude

> ellipseFromPoints :: [Point R2] -> EllipseInfo R2
> ellipseFromPoints ps = EI {
>       samplePoints = Just ps
>     , coefficients = Just coeffs
>     , eigenValues  = Just (e1, e2)
>     , eigenVectors = Just (b1, b2)
>     , scaleXY      = Just (s1, s2)
>     , offsetXY     = Just dXY
>     }
>     where coeffs             = ellipseCoefficients ps
>           [a,b,c,d,e]        = coeffs
>           [(e1,b1), (e2,b2)] = solveQuadraticForm a b c
>           (s1, s2, dXY)      = calcScaleAndOffsetXY (e1, e2) (b1, b2) d e

Create a matrix for the system of equations, where the variables are
_a_, _b_, _c_, _d_, and _e_, and the coefficients are _x_^2, _xy_, _y_^2, _x_, and _y_.

> quadraticFactors :: [Point R2] -> Matrix Double
> quadraticFactors ps = Matrix (map mkRow ps)

> mkRow :: P2 -> [Double]
> mkRow p = case unp2 p of (x, y) -> [x*x, x*y, y*y, x, y, 1]

Reduce the matrix to row-echelon form by gaussian elimination and solve
for the variable values.

> ellipseCoefficients :: [Point R2] -> [Double]
> ellipseCoefficients ps = case solve (quadraticFactors ps) of
>                              None   -> error "No solution" 
>                              Many   -> error "Many solutions"
>                              One xs -> xs

We now have the equation of the ellipse. If there is a cross term (i.e. _b_ ≠ 0),
we rotate the axes to eliminate it. If there are linear terms remaining, we translate
the axes to put the ellipse into standard position.

The ellipse in standard position gives us the lengths of its major and minor axes, and
if it was originally rotated, the orthonormal basis vectors give us their directions.
See "spectral analysis" in any linear algebra textbook for the details.

The basis vectors are chosen to be in quadrants I or IV, i.e. pointing to the right,
so that the `transformBy` function will work.

> solveQuadraticForm :: Double -> Double -> Double -> [(Double, R2)]
> solveQuadraticForm a b c
>     | equivZero b = [(a, unitX), (c, unitY)]
>     | otherwise   = checkSwap [(eigen1, bv eigen1), (eigen2, bv eigen2)]
>     where eigen1 = ((a+c) + sqrt ((a-c)^2 + b^2)) / 2
>           eigen2 = ((a+c) - sqrt ((a-c)^2 + b^2)) / 2
>           bv x   = if b/(x-a) > 0
>                        then r2 ( (b/2)/(x-a),  1) # normalized
>                        else r2 (-(b/2)/(x-a), -1) # normalized

As we are producing the matrix of a rotation, it must have a determinant of 1.
If the determinant is negative, then swap the columns and associated eigenvalues.
We don't need to check that the determinant has absolute value 1 because the column
vectors have been normalized.

> checkSwap :: [(Double, R2)] -> [(Double, R2)]
> checkSwap [p1@(e1, bv1), p2@(e2, bv2)] = if det bv1 bv2 < 0 then [p2,p1] else [p1,p2]

Calculate the determinant of a 2 x 2 matrix given the column vectors.

> det :: R2 -> R2 -> Double
> det v1 v2 = x11 * x22 - x12 * x21
>     where (x11, x21) = unr2 v1
>           (x12, x22) = unr2 v2

The translation is calculated in terms of the rotated coordinate system--that is, we
eliminate the cross-term first, then translate--so we must rotate the translation offset
back to the original coordinates before applying.

> calcScaleAndOffsetXY
>   :: (Double, Double) -> (R2, R2) -> Double -> Double -> (Double, Double, R2) 
> calcScaleAndOffsetXY (e1, e2) (b1, b2) d e = (s1, s2, dXY)
>     where (p11, p21) = unr2 b1
>           (p12, p22) = unr2 b2
>           (d', e') = (d * p11 + e * p21, d * p12 + e * p22)
>           offsetXY' = r2(-d'/(2*e1), -e'/(2*e2))
>           dXY       = rotate (direction b1::Rad) offsetXY'
>           s1        = evScale e1
>           s2        = evScale e2

>           evScale :: Double -> Double
>           evScale ev = sqrt ((1/ev) * (1 + d'*d'/(4*e1) + e'*e'/(4*e2)))

