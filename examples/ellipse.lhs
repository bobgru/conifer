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

> main = defaultMain (example # centerXY # pad 1.2)

![Ellipse](https://github.com/bobgru/conifer/blob/master/images/ellipse.png?raw=true "Ellipse")

> example = ( ellipticalArc # lc yellow
>          <> circularArc   # lc yellow
>          <> circ          # lc black
>          <> ellip         # lc lightgray
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

> ellip         =  ellipseAxes
>               <> circ # transform1

> ellipticalArc = arc beginAngle endAngle # lw 0.2 # transform2

The ellipse axes are the eigenvectors scaled precisely according to the corresponding
eigenvalues, all of which come from spectral analysis of the ellipse's formula.

> ellipseAxes =  fromOffsets [bv1 ^* ((-1) * evScale e1)] # lw 0.2 # lc orange
>             <> fromOffsets [bv2 ^*         evScale e2]  # lw 0.2 # lc orange

For `transform1` we specify the transformation directly in terms of the scale factor
and its direction.

> transform1 = transformBy r theta
> r          = 0.4
> theta      = tau / 20 :: Rad

For `transform2` we build up the transformation by scaling appropriately along the
eigenvectors. The values `e1` and `e2` are the eigenvalues calculated below, and
`bv1` and `bv2` the corresponding orthonormal basis vectors.

> transform2 = transformBy (evScale e1) (- (direction bv1) :: Rad)
>            . transformBy (evScale e2)    (direction bv2  :: Rad)

Our general transformation is scaling in a certain direction. A general affine
transformation would add a translation (which we may do in future).

> transformBy r theta = scaleX r `under` rotation theta

The eigenvalue needs to be converted to a half-axis length. Note that this
formula does not account for translation, i.e. the center of the ellipse
must be at the origin of the vector space. The derivation of this function
is explained below (**TODO**).

> evScale :: Floating a => a -> a
> evScale ev = sqrt (1/ev)

**Analysis of the Ellipse**

The general form of the equation of an ellipse is

_ax^2 + bxy + cy^2 + dx + ey + f = 0_

or

_ax^2 + bxy + cy^2 + dx + ey = -f_

or

_(-a/f)x^2 + (-b/f)xy + (-c/f)y^2 + (-d/f)x + (-e/f)y = 1_

Renaming the coefficients, we have

_a'x^2 + b'xy + c'y^2 + d'x + e'y = 1_

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
for the variable (i.e. coefficients of ellipse formula) values.

> ellipseCoefficients :: Solution Double
> ellipseCoefficients = (solve . gaussianReduce) quadraticFactors
> [a,b,c,d,e] = case ellipseCoefficients of
>                      None   -> error "No solution" 
>                      Many   -> error "Many solutions"
>                      One xs -> xs

The left hand side of the quadratic equation should evaluate to 1 for every
point on the ellipse, so try a few from the GHCI prompt for confidence.

> q r = case unr2 r of (x,y) -> a*x*x + b*x*y + c*y*y + d*x + e*y
> qPts = [unitX # rotateBy (i/20) # transform1 | i <- [0..19]]
> qs = map q qPts

We now have the equation of the ellipse. If there is a cross term (i.e. _b_ ≠ 0),
we rotate the axes to eliminate it. If there are linear terms remaining, we translate
the axes to put the ellipse into standard position.
(**TODO** translation, general case of testing b)

The ellipse in standard position gives us the lengths of its major and minor axes, and
if it was originally rotated, the orthonormal basis vectors give us their directions.
See "spectral analysis" in any linear algebra textbook for the details.

> [(e1, bv1), (e2, bv2)] = solveQuadraticForm a b c

> solveQuadraticForm :: Double -> Double -> Double -> [(Double, R2)]
> solveQuadraticForm a b c = [(eigen1, basisVector eigen1), (eigen2, basisVector eigen2)]
>     where eigen1 = ((a+c) + sqrt ((a-c)^2 + b^2)) / 2
>           eigen2 = ((a+c) - sqrt ((a-c)^2 + b^2)) / 2
>           basisVector x = r2 ((b/2)/(x-a), 1) # normalized

**Matrix Row Reduction**

This definition of `Matrix` comes from the ICFP '13 paper "Fun with Semirings",
in which gaussian elimination is discussed over semirings with closure. I found it
useful for straightforward row-reduction over the field of `Double`.

The `BlockMatrix` isolates the upper left entry of a matrix and the lower right
submatrix consisting of one fewer row and column. Our row-reduction algorithm
recurses over submatrices.

> data Matrix a = Matrix [[a]] deriving Show

> type BlockMatrix a = (Matrix a, Matrix a,
>                       Matrix a, Matrix a)

> mjoin :: BlockMatrix a -> Matrix a
> mjoin (Matrix a, Matrix b,
>        Matrix c, Matrix d) =
>       Matrix ((a `hcat` b) ++ (c `hcat` d))
>     where hcat = zipWith (++)

> msplit :: Matrix a -> BlockMatrix a
> msplit (Matrix (row:rows)) =
>     (Matrix [[first]], Matrix [top],
>      Matrix left,      Matrix rest)
>     where (first:top) = row
>           (left, rest) = unzip (map (\(x:xs) -> ([x],xs)) rows)

Gaussian elimination consists of repeating the following until every
row has been processed:
* Find the first row _i_ with a nonzero entry in column 0.
* Swap rows _i_ and 0 if they are different. The entry in row 0 column 0 is
  know as the _pivot_.
* Scale row 0 by (1/_a_) where _a_ is the pivot, so the pivot becomes 1.
* Linearly combine all rows _k_ ≠ 0 so that  row_k' = row_k - _a_* row_0 where _a_
  is the entry in row_k in column 0.
* Repeat with the lower right submatrix of the block matrix.

If a column has no non-zero entries, reconstitute the matrix as the lower right
submatrix of the block matrix, but add the tail of row_0 as the new row_0. If we
started with an _n x n+1_ matrix, this will lead to either infinitely many or no
solutions which we don't handle. **TODO**

> gaussianReduce :: (Fractional a, Ord a) => Matrix a -> Matrix a
> gaussianReduce (Matrix [xs]) = Matrix [ normalizeRow xs ]
> gaussianReduce m =
>     case pivotMatrix m of
>              Left _   -> case skipCol m of
>                              Left s -> error ("gaussianReduce failed: " ++ s)
>                              Right m' -> (restoreCol . gaussianReduce) m'
>              Right m' -> (mjoin . gaussianReduceBlock . msplit . reduceLeftCol) m'

> skipCol :: Matrix a -> Either String (Matrix a)
> skipCol (Matrix m) =
>     if null m || 1 == length (head m)
>         then Left "Can't skipCol"
>         else Right (Matrix (map tail m))

> restoreCol :: Num a => Matrix a -> Matrix a
> restoreCol (Matrix m) = Matrix (map (0:) m)

Fix the pivot by floating the row with the first non-zero value in the first column
to the top, then dividing that row by its first entry. The result will be a 1 in the
first column of the first row.

If there is no row with a non-zero first column, return an error indication.

> pivotMatrix :: (Fractional a, Ord a) => Matrix a -> Either String (Matrix a)
> pivotMatrix (Matrix m) = if null test then Left "No pivot" else Right (Matrix m3)
>     where (r:rs) = m
>           test = filter (\(i,r)->(not . equivZero) (head r)) (zip [0..] m)
>           (i,r0) = head test
>           m2 = if i==0 then m else swapListElem m 0 i
>           m3 = normalizeRow (head m2) : tail m2

> reduceLeftCol :: (Fractional a, Ord a) => Matrix a -> Matrix a
> reduceLeftCol (Matrix [x:xs]) = Matrix [x:xs]
> reduceLeftCol (Matrix (r:rs)) = Matrix (r : map (reduceRowBy r) rs)

> reduceRowBy :: Num a => [a] -> [a] -> [a]
> reduceRowBy r1 r2 = r2 `addRow` scaleRow ((-1) * head r2) r1

> addRow :: Num a => [a] -> [a] -> [a]
> r1 `addRow` r2 = zipWith (+) r1 r2

> swapListElem []  _ _    = []
> swapListElem [r] _ _    = [r]
> swapListElem rs i j
>     | i >= length rs || j >= length rs = 
>         error ("swapListElem: bad list index " ++ show i ++ " or " ++ show j)
>     | i == j    = rs
>     | i > j     = swapListElem rs j i
>     | otherwise = left ++ rs_j ++ middle ++ rs_i ++ right
>           where (left, rs_i, middle, rs_j, right) = divList rs i j

> divList :: [a] -> Int -> Int -> ([a], [a], [a], [a], [a])
> divList rs i j = ( take (i-1) rs
>                  , [rs !! i]
>                  , take (j-i-1) (drop (i+1) rs)
>                  , [rs !! j]
>                  , drop (j+1) rs)

> gaussianReduceBlock :: (Fractional a, Ord a) => BlockMatrix a -> BlockMatrix a
> gaussianReduceBlock (a, b, c, rest) = (a, b, c, gaussianReduce rest)

> normalizeRow :: (Fractional a, Ord a) => [a] -> [a]
> normalizeRow [] = []
> normalizeRow r@(x:xs) = if equivZero x then x : normalizeRow xs else scaleRow (1/x) r

> equivZero :: (Fractional a, Ord a) => a -> Bool
> equivZero x = abs x < 1e-20

> scaleRow :: Num a => a -> [a] -> [a]
> scaleRow a = map (*a)

A system of linear equations can have zero, one, or infinitely many solutions.
We can tell the difference by examining the last row of a reduced matrix.

* If it has nonzero entries in the rightmost two columns, there is one solution
  which can be found by back-substitution.
* If it has a single nonzero entry in the rightmost column, there are no solutions,
  because the translation back to a system of equations includes the contradiction
  0 = _c_ for some nonzero value _c_.
* If it has all zeroes, there are infinitely many solutions. **TODO**

> data Solution a = One [a] | None | Many deriving (Show)

> solve :: (Fractional a, Ord a) => Matrix a -> Solution a
> solve (Matrix m)
>     | all equivZero r           = Many
>     | equivZero (last (init r)) = None
>     | otherwise                 = One (solveOne m) 
>     where r = last m

Assuming there is one solution, the last row gives the value of `e` directly.
The previous row gives `d` in terms of `e` and a constant, so we can substitute
the know value of `e`. Continuing up the matrix, we substitute known values in
each equation to determine the next unknown until we know all values.

> solveOne :: Num a => [[a]] -> [a]
> solveOne = reverse . foldl accumSolution [] . reverse

> accumSolution :: Num a => [a] -> [a] -> [a]
> accumSolution sln r = sln ++ nextValue sln r

> nextValue :: Num a => [a] -> [a] -> [a]
> nextValue sln r = [last r - sum (zipWith (*) (reverse (init r)) sln)]
