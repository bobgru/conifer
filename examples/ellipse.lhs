An ellipse as a transformed circle
==================================

This program draws a circle on a grid, then the same diagram
under a transformation, with an analysis of the quadratic form
of the ellipse.

> {-# LANGUAGE NoMonomorphismRestriction #-}
> module Main where
> import Diagrams.Prelude
> import Diagrams.Backend.SVG.CmdLine
> import Data.Default.Class

> main = defaultMain (example # centerXY # pad 1.2)

> example0 = d1 # centerX === strutY 0.5 === d2 # centerX
>     where d1 = circ ||| strutX 0.5 ||| ellip
>           d2 = circ # lc black <> ellip # lc lightgray

> example1 = d2 # centerX
>     where d2 = circ # lc black <> ellip # lc lightgray

> example = d2 # centerX
>     where d2 =  arc a1 a2 # lw 0.2 # lc yellow # t'' # scaleX 10 # scaleY 10
>              <> arc a1 a2 # lw 0.2 # lc yellow # scaleX 10 # scaleY 10
>              <> circ # lc black
>              <> ellip # lc lightgray
>           d3 = arc a1 a2 

> a1 = tau/8 :: Rad
> a2 = tau/4 :: Rad

> t'' = t (evScale e1) (- (direction bv1) :: Rad) . t (evScale e2) (direction bv2 :: Rad)

> evScale x = sqrt (1/x)

**The Circle**

> circ = (dots <> unitCircle # lw 0.2 <> axes # lw 0.1 <> grid 20 20) # scaleX 10 # scaleY 10

> dots = position $ zip (origin : ps) (repeat dot)
>     where dot = circle 0.03 # lw 0 # fc blue
> -- ps   = [origin .+^ v | v <- [unitX, unitY, unit_X, unit_Y, fromDirection theta]]
>     -- # map (rotateBy (1/6))
> ps   = [origin .+^ v | v <- [unitX # rotateBy (i/16) | i <- [0..4]]]

> axes =  fromOffsets [unit_X, unitX ^* 2]
>      <> fromOffsets [unit_Y, unitY ^* 2]

> grid m n = matrix 
>          # scaleX (2/ fromIntegral n)
>          # scaleY (2/ fromIntegral m)
>          # centerXY
>          # lw 0.02
>     where matrix = foldl (===) mempty (replicate m row)
>           row    = foldl (|||) mempty (replicate n cell)
>           cell   = square 1

**The Ellipse**

An ellipse is a circle under an affine transformation, thus:

> ellip     =  eigenvectors
>           <> circ # t'

> eigenvectors =  fromOffsets [bv1 ^* (-10)] # lw 0.2 # lc orange
>              <> fromOffsets [bv2 ^* 10] # lw 0.2 # lc orange

> t'        = t r theta
> t r theta = scaleX r `under` rotation theta

> r         = 0.4
> theta     = tau / 20 :: Rad

**Analysis of the Ellipse**

The general form of the equation of an ellipse is

    _ax^2 + bxy + cy^2 + dx + ey + f = 0_

or

_ax^2 + bxy + cy^2 + dx + ey = -f_

or

_(-a/f)x^2 + (-b/f)xy + (-c/f)y^2 + (-d/f)x + (-e/f)y = 1_

Renaming the coefficients, we have

_a'x^2 + b'xy + c'y^2 + d'x + e'y = 1_

We have five unknowns--the coefficients--which are the same for any point
on the ellipse. We can therefore find the coefficients by substituting _x_
and _y_ from five different points and row-reducing the matrix for the system.

Transform points on the circle to put them on the ellipse, then create a matrix
from them:

> ps' = map t' ps

> m :: Matrix Double
> m = Matrix (map mkRow ps')

> mkRow :: P2 -> [Double]
> mkRow p = case unp2 p of (x, y) -> [x*x, x*y, y*y, x, y, 1]

Reduce the matrix to row-echelon form by gaussian elimination.

> sln = (solve . gaussianReduce) m
> m' = case sln of
>          None   -> error "No solution" 
>          Many   -> error "Many solutions"
>          One xs -> xs

> [a,b,c,d,e] = m'

The left hand side of the quadratic equation should evaluate to 1 for every
point on the ellipse, so try a few for confidence.

> q r = case unr2 r of (x,y) -> a*x*x + b*x*y + c*y*y + d*x + e*y
> qPts = [unitX # rotateBy (i/20) # t' | i <- [0..19]]
> qs = map q qPts

We now have the equation of the ellipse. If there is a cross term (i.e. b /= 0), we rotate
the axes to eliminate it by way of spectral analysis. If there are linear terms remaining,
we translate the axes to put the ellipse into standard position by way of "completing the
squares".

The ellipse in standard position gives us the lengths of its major and minor axes, and
if it was originally rotated, the orthonormal basis vectors we obtained from spectral
analysis give us their directions.

> [(e1, bv1), (e2, bv2)] = solveQuadraticForm a b c

Given the coefficients for the quadratic form of an ellipse, find the
eigenvalues and orthonormal basis vectors of the ellipse rotated
into standard form.

> solveQuadraticForm :: Double -> Double -> Double -> [(Double, R2)]
> solveQuadraticForm a b c = [(eigen1, basisVector eigen1), (eigen2, basisVector eigen2)]
>     where eigen1 = ((a+c) + sqrt ((a-c)^2 + b^2)) / 2
>           eigen2 = ((a+c) - sqrt ((a-c)^2 + b^2)) / 2
>           basisVector x = r2 ((b/2)/(x-a), 1) # normalized



This definition of Matrix comes from the ICFP '13 paper on "Fun with Semirings".

> data Matrix a = Scalar a
>               | Matrix [[a]]
>     deriving Show

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
* Find the first row i with a nonzero entry in column 0.
* Swap rows i and 0 if they are different.
* Scale row 0 by (1/a) where a is the entry in row 0 in column 0.
* Linearly combine all rows k /= 0 so that  row_k' = (-a)* row_0 + row_k where a
  is the entry in row_k in column 0.
* Repeat with the lower right submatrix of the block matrix.

If a column has no non-zero entries, reconstitute the matrix as the lower right
submatrix of the block matrix, but add the tail of row_0 as the new row_0.

> gaussianReduce :: (Fractional a, Ord a) => Matrix a -> Matrix a
> gaussianReduce (Scalar a)    = Scalar a
> gaussianReduce (Matrix [xs]) = Matrix [ normalizeRow xs ]
> gaussianReduce m =
>     case pivotMatrix m of
>              Left _   -> case skipCol m of
>                              Left s -> error ("gaussianReduce failed: " ++ s)
>                              Right m' -> (restoreCol . gaussianReduce) m'
>              Right m' -> (mjoin . gaussianReduceBlock . msplit . reduceLeftCol) m'

> skipCol :: Matrix a -> Either String (Matrix a)
> skipCol (Scalar a) = Left "Can't skipCol of Scalar"
> skipCol (Matrix m) =
>     if null m || 1 == length (head m)
>         then Left "Can't skipCol"
>         else Right (Matrix (map tail m))

> restoreCol :: Num a => Matrix a -> Matrix a
> restoreCol (Scalar a) = error "restoreCol applied to Scalar"
> restoreCol (Matrix m) = Matrix (map (0:) m)

Fix the pivot by floating the row with the first non-zero value in the first column
to the top, then dividing that row by its first entry. The result will be a 1 in the
first column of the first row.

If there is no row with a non-zero first column, return an error indication.

> pivotMatrix :: (Fractional a, Ord a) => Matrix a -> Either String (Matrix a)
> pivotMatrix (Scalar a) = Right (Scalar a)
> pivotMatrix (Matrix m) = if null test then Left "No pivot" else Right (Matrix m3)
>     where (r:rs) = m
>           test = filter (\(i,r)->(not . equivZero) (head r)) (zip [0..] m)
>           (i,r0) = head test
>           m2 = if i==0 then m else swapListElem m 0 i
>           m3 = normalizeRow (head m2) : tail m2

> reduceLeftCol :: (Fractional a, Ord a) => Matrix a -> Matrix a
> reduceLeftCol (Scalar a) = Scalar a
> reduceLeftCol (Matrix [x:xs]) = Matrix [x:xs]
> reduceLeftCol (Matrix (r:rs)) = Matrix (r : map (reduceRowBy r) rs)

> reduceRowBy :: Num a => [a] -> [a] -> [a]
> reduceRowBy r1 r2 = r2 `addRow` (scaleRow ((-1) * head r2) r1)

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

> divList rs i j = ( take (i-1) rs
>                  , [rs !! i]
>                  , take (j-i-1) (drop (i+1) rs)
>                  , [rs !! j]
>                  , drop (j+1) rs)

> gaussianReduceBlock :: (Fractional a, Ord a) => BlockMatrix a -> BlockMatrix a
> gaussianReduceBlock (a, b, c, rest) = (a, b, c, gaussianReduce rest)

> normalizeRow :: (Fractional a, Ord a) => [a] -> [a]
> normalizeRow [] = []
> normalizeRow r@(x:xs) =
>     case equivZero x of
>         True  -> (x : normalizeRow xs)
>         False -> scaleRow (1/x) r

> equivZero x = abs x < 1e-20

> scaleRow a r = map (*a) r

A system of linear equations can have zero, one, or infinitely many solutions.
We can tell the difference by examining the last row of a reduced matrix.

If it has nonzero entries in the rightmost two columns, there is one solution
which can be found by back-substitution.

If it has a single nonzero entry in the rightmost column, there are no solutions.

If it has all zeroes, there are infinitely many solutions.

> data Solution a = One [a] | None | Many deriving (Show)

> solve :: (Fractional a, Ord a) => Matrix a -> Solution a
> solve (Scalar a) = None
> solve (Matrix m) = if all equivZero r
>                         then Many
>                         else if equivZero (last (init r))
>                             then None
>                             else One (solveOne m) 
>     where r = last m

> solveOne :: Num a => [[a]] -> [a]
> solveOne m = reverse (solveOne' [] (reverse m))

> solveOne' :: Num a => [a] -> [[a]] -> [a]
> solveOne' sln [] = sln
> solveOne' sln (r:rs) = solveOne' sln' rs
>     where sln' = sln ++ next sln r

> next :: Num a => [a] -> [a] -> [a]
> next sln r = [last r - sum (zipWith (*) (reverse (init r)) sln)]


> -- sln = solve m

> Matrix mm = m

> -- solveOne mm
> -- solveOne m = reverse (solveOne' [] (reverse m))

> mmm = reverse mm

> -- solveOne' [] mmm
> -- solveOne' sln (r:rs) = solveOne' (sln ++ next sln r) rs


> sln0 = []
> mr0   = last mmm
> mrs0  = init mmm

> sln1 = sln0 ++ next sln0 mr0
> mr1   = last mrs0
> mrs1  = init mrs0

> sln2 = sln1 ++ next sln1 mr1
> mr2   = last mrs1
> mrs2  = init mrs1

> sln3 = sln2 ++ next sln2 mr2
> mr3   = last mrs2
> mrs3  = init mrs2

> sln4 = sln3 ++ next sln3 mr3
> mr4   = last mrs3
> mrs4  = init mrs3

> sln5 = sln4 ++ next sln4 mr4
> mr5   = last mrs4
> mrs5  = init mrs4
