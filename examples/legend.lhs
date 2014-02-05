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

Run the program with `dist/build/legend/legend -o legend.svg -w 400` 
where `-o` sets the output filename, and `-w` sets the diagram width.

> main = defaultMain (legend theta phi # centerXY # pad 1.2)

> legend theta phi =  mempty
>                  <> legendTrunkAngle theta phi -- # lc yellow
>              --    <> fromOffsets [P.unitX] # lc orange
>              --    <> fromOffsets [P.unitY] # lc orange
>                  <> legendPlanes theta phi
>                  <> legendTrunk theta phi
>                  <> legendBranches theta phi

> legendTrunk :: Rad -> Rad -> Diagram B R2
> legendTrunk theta phi = (drawTrunk girth0 girth1 . flatten . spin theta phi) trunk

> legendBranches :: Rad -> Rad -> Diagram B R2
> legendBranches theta phi = (mconcat . map drawTip . flatten . spin theta phi) branches

> legendPlanes :: Rad -> Rad -> Diagram B R2
> legendPlanes theta phi = (drawPlanes . flatten . spin theta phi) planes

> legendTrunkAngle :: Rad -> Rad -> Diagram B R2
> legendTrunkAngle theta phi = arc a1 a2 # scale r # transformEllipse ellipseParams eigenBasis
>     where a1            = trunkBranchAngle
>           a2            = P.direction P.unitY :: Rad
>           r             = 0.75
>           pss           = (flatten . spin theta phi) trunkAnglePts
>           [a,b,c,d,e]   = ellipseFromPoints pss
>           eigenBasis    = solveQuadraticForm a b c
>       --    ellipseParams = calcEllipseHalfAxes eigenBasis d e
>           [(scale1,offset1), (scale2,offset2)] = calcEllipseHalfAxes eigenBasis d e
>           ellipseParams = [(scale1, offset1'), (scale2, offset2')]

**TODO**

These manually determined offsets are tuned for `theta = pi * (9.15/10) :: Rad`
and `phi = pi * (2.2/5) :: Rad`. What's wrong with the calculation of offsets?

>           [[(offset1', offset2')]] = [[(-0.025, 0.07)]]

Calculate the coefficients A through F of a system of 5 equations in the variables of 
x^2, xy, y^2, x, and y, by substituting the 5 ellipse points into the
equation: Ax^2 + Bxy + Cy^2 + Dx + Ey = -F.

> ellipseFromPoints :: [[P2]] -> [Double]
> ellipseFromPoints [ps]  = [a,b,c,d,e]
>     where m :: Matrix Double
>           m = Matrix ((map mkRow) ps)
>           One [a,b,c,d,e] = (solve . gaussianReduce) m

> mkRow p = case unp2 p of (x, y) -> [x*x, x*y, y*y, x, y, 1]

Calculate half the lengths of the major and minor axes.

> calcEllipseHalfAxes :: [(Double, R2)] -> Double -> Double -> [(Double, Double)]
> calcEllipseHalfAxes [(e1, v1), (e2, v2)] d e = [(scaleEv e1, offset1), (scaleEv e2, offset2)]
>     where scaleEv x = sqrt (k / x)
>           k = 1 + (d'*d'/(4*e1)) + (e'*e'/(4*e2))
>           offset1 = d'/2
>           offset2 = e'/2
>           d' = d * p11 + e * p12
>           e' = d * p21 + e * p22
>           (p11,p12) = unr2 v1
>           (p21,p22) = unr2 v2

Scale by the major and minor axes, given the orthonormal basis vectors.

> transformEllipse :: [(Double, Double)] -> [(Double, R2)] -> Diagram B R2 -> Diagram B R2
> transformEllipse [(scale1, offset1), (scale2,offset2)]  [(e1, v1), (e2, v2)] =
>     translateX offset1 . translateY offset2 .
>     (P.scaleX scale1 `under` rotation ((-P.direction v1)::Rad)) .
>     (P.scaleY scale2 `under` rotation ((P.direction v2)::Rad))

Given the coefficients for the quadratic form of an ellipse, find the
eigenvalues and orthonormal basis vectors of the ellipse rotated
into standard form.

> solveQuadraticForm :: Double -> Double -> Double -> [(Double, R2)]
> solveQuadraticForm a b c = [(eigen1, basisVector eigen1), (eigen2, basisVector eigen2)]
>     where eigen1 = ((a+c) + sqrt ((a-c)^2 + b^2)) / 2
>           eigen2 = ((a+c) - sqrt ((a-c)^2 + b^2)) / 2
>           basisVector x = r2 ((b/2)/(x-a), 1) # normalized

> planes :: [[P3]]
> planes = [ branchRect, vertRect, horizRect ]

> baseWidth       = 1.5
> baseLength      = 4.0
> trunkHeight     = 2.5
> branchTipHeight = trunkHeight / 2
> theta           = pi * (9.15/10) :: Rad
> phi             = pi * (2.2/5) :: Rad

> trunk :: [[P3]]
> trunk = [[ origin, p3 (0, 0, trunkHeight) ]]

> girth0 = 0.05
> girth1 = 0.02

> branches :: [[P3]]
> branches = [ center, left, right ]
>    where v = r3 (baseLength, 0, branchTipHeight)
>          v' = v # scale (2/3)
>          center = [ origin, origin .+^ v # scale 1.2 ]
>          left   = [ (origin .+^ v'), p3 (baseLength, -baseWidth/2, branchTipHeight)  ]
>          right  = [ (origin .+^ v'), p3 (baseLength,  baseWidth/2, branchTipHeight)  ]

> trunkAnglePts :: [[P3]]
> trunkAnglePts = [ map (origin .+^) [T3V.unitX, T3V.unit_X, T3V.unitZ, T3V.unit_Z, 
>                                  r3(baseLength, 0, branchTipHeight) # normalized] ]

> trunkBranchAngle :: Rad
> trunkBranchAngle = T3V.angleBetween T3V.unitX (p .-. origin) 
>     where p = last (last trunkAnglePts)

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

> spin :: Rad -> Rad -> [[P3]] -> [[P3]]
> spin theta phi = map (map m) 
>     where m = rotateAboutX (phi - pi) . rotateAboutZ (pi/2 - theta)

> rotateAboutZ :: Rad -> P3 -> P3
> rotateAboutZ theta = transform (aboutZ theta)

> rotateAboutX :: Rad -> P3 -> P3
> rotateAboutX theta = transform (aboutX theta)

> flatten :: [[P3]] -> [[P2]]
> flatten  = map (map projectXY) 

> projectXZ :: P3 -> P2
> projectXZ p = p2 (x, z) where (x, _, z) = unp3 p

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

> drawArc :: [P2] -> Diagram B R2
> drawArc = undefined

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
