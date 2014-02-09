Drawing an Elliptical Arc
=========================

**Introduction**

This program shows how to deduce the transformation from
circle to ellipse in order to draw an elliptical arc.

The original motivation was to draw a circular arc defined in 3D but projected to 2D,
which became an elliptical arc in the process. Neither the formula for the ellipse nor
the effective 2D affine transformation that created it were known, but as many points
as desired could be sampled from the original 3D circle projected onto 2D.

> {-# LANGUAGE NoMonomorphismRestriction #-}
> module Main where
> import Diagrams.Prelude
> import Diagrams.Backend.SVG.CmdLine
> import Data.Default.Class
> import Matrix

> main = defaultMain (example # centerXY # pad 1.2)

![Ellipse](https://github.com/bobgru/conifer/blob/master/images/ellipse.png?raw=true "Ellipse")

> example = ( ellipticalArc # lc orange
>          <> circularArc   # lc yellow
>          <> ellip         # lc black
>          <> circ          # lc lightgray
>           ) # scale 10

**The Circle**

Draw a circle, but help the viewer's intuition by adding a grid with axes and
some sample points.

> circ =  circleDots # fc blue
>      <> unitCircle # lw 0.2
>      <> circleAxes # lw 0.1
>      <> grid 20 20

> circleDots = position $ zip (origin : circlePoints) (repeat dot)
>     where dot = circle 0.03 # lw 0

> circlePoints = [origin .+^ v | v <- [unitX # rotateBy (i/16) | i <- [0..4]]]

> circleAxes =  fromOffsets [unit_X, unitX ^* 2]
>            <> fromOffsets [unit_Y, unitY ^* 2]

> circularArc = arc beginAngle endAngle # lw 0.2

> beginAngle = tau/8 :: Rad
> endAngle   = tau/4 :: Rad

> grid m n = matrix 
>          # scaleX (2 / fromIntegral n)
>          # scaleY (2 / fromIntegral m)
>          # centerXY
>          # lw 0.02
>     where matrix = foldl (===) mempty (replicate m row)
>           row    = foldl (|||) mempty (replicate n cell)
>           cell   = square 1

**The Ellipse**

An ellipse is a circle under an affine transformation. As we are creating the
ellipse, we know the transformation, here named `transform1`.

If we didn't know the transformation but had enough points on the ellipse, we
could determine the transformation, which we will call `transform2`.

As a demonstration of the accuracy of the technique, we will draw the ellipse
created with `transform1` and the elliptical arc created with `transform2`, which
should perfectly coincide. To illustrate the analysis, we will also display the
major and minor axes of the ellipse.

> ellip         =  ellipseAxes # translate offsetXY
>               <> circ # transform1

> ellipticalArc = arc beginAngle endAngle # lw 0.2 # transform2

The ellipse axes are the eigenvectors scaled precisely according to the corresponding
eigenvalues, all of which come from spectral analysis of the ellipse's formula.

> ellipseAxes =  fromOffsets [bv1 ^* evScale e1] # lw 0.2 # lc orange
>             <> fromOffsets [bv2 ^* evScale e2] # lw 0.2 # lc orange

For `transform1` we specify the transformation directly in terms of scale factors
and directions, plus a translation.

> transform1 = translate (r2(dx, dy))
>            . transformBy dr2 dtheta2
>            . transformBy dr1 dtheta1
> dr1         = 0.4
> dtheta1     = -(tau / 4 - tau / 10) :: Rad
> dr2         = 0.8
> dtheta2     = -tau / 20 :: Rad
> dx         = 0.5
> dy         = 0.25

For `transform2` we build up the transformation by scaling appropriately along the
eigenvectors, then translating, if necessary. The values `e1` and `e2` are the
eigenvalues calculated below, and `bv1` and `bv2` the corresponding orthonormal
basis vectors.

> transform2 = translate offsetXY
>            . transformBy (evScale e1) (direction bv1 :: Rad)
>            . transformBy (evScale e2) (direction bv2 :: Rad)

Our general transformation is scaling in a certain direction. A general affine
transformation would add a translation. We apply translations in a separate step.

Note that the rotation by `theta` is undone, the scaling is applied, then the 
rotation by `theta` is reapplied, with the net effect of having done the scaling
in the direction of `theta`.

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

> ellipsePoints = map transform1 circlePoints

Create a matrix for the system of equations, where the variables are
_a_, _b_, _c_, _d_, and _e_, and the coefficients are _x_^2, _xy_, _y_^2, _x_, and _y_.

> quadraticFactors :: Matrix Double
> quadraticFactors = Matrix (map mkRow ellipsePoints)

> mkRow :: P2 -> [Double]
> mkRow p = case unp2 p of (x, y) -> [x*x, x*y, y*y, x, y, 1]

Reduce the matrix to row-echelon form by gaussian elimination and solve
for the variable values.

> ellipseCoefficients :: Solution Double
> ellipseCoefficients = solve quadraticFactors
> [a,b,c,d,e] = case ellipseCoefficients of
>                      None   -> error "No solution" 
>                      Many   -> error "Many solutions"
>                      One xs -> xs

The left hand side of the quadratic equation should evaluate to 1 for every
point on the ellipse, so try a few from the GHCI prompt for confidence.
Remember that vectors are translation-invariant, so we have to convert to
points.

> q r = case unp2 r of (x,y) -> a*x*x + b*x*y + c*y*y + d*x + e*y
> qPts = [(origin .+^ unitX) # rotateBy (i/20) # transform1 | i <- [0..20]]
> qs = map q qPts

We now have the equation of the ellipse. If there is a cross term (i.e. _b_ ≠ 0),
we rotate the axes to eliminate it. If there are linear terms remaining, we translate
the axes to put the ellipse into standard position.

The ellipse in standard position gives us the lengths of its major and minor axes, and
if it was originally rotated, the orthonormal basis vectors give us their directions.
See "spectral analysis" in any linear algebra textbook for the details.

The basis vectors are chosen to be in quadrants I or IV, i.e. pointing to the right,
so that the `transformBy` function will work.

> [(e1, bv1), (e2, bv2)] = solveQuadraticForm a b c

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

> offsetXY = rotate (direction bv1::Rad) (r2(-d'/(2*e1), -e'/(2*e2)))
> (d', e') = (d * p11 + e * p21, d * p12 + e * p22)
>     where (p11, p21) = unr2 bv1
>           (p12, p22) = unr2 bv2

An eigenvalue needs to be converted to a half-axis length.

> evScale :: Double -> Double
> evScale ev = sqrt ((1/ev) * (1 + d'*d'/(4*e1) + e'*e'/(4*e2)))
