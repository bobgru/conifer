**Matrix Row Reduction**

This definition of `Matrix` comes from the ICFP '13 paper "Fun with Semirings",
in which gaussian elimination is discussed over semirings with closure. I found it
useful for straightforward row-reduction over the field of `Double`.

The `BlockMatrix` isolates the upper left entry of a matrix and the lower right
submatrix consisting of one fewer row and column. Our row-reduction algorithm
recurses over submatrices.

> module Matrix
> where
> import Data.List(sort,foldl')

> data Matrix a = Matrix [[a]] deriving Show

> type BlockMatrix a = (Matrix a, Matrix a,
>                       Matrix a, Matrix a)

> instance Functor Matrix where
>     fmap f (Matrix xs) = Matrix (fmap (fmap f) xs)

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

The method of gaussian elimination known as _pivot condensation_ consists of 
repeating the following until every row has been processed:
* Find the row _i_ with largest absolute value in column 0.
* Swap rows _i_ and 0 if they are different. The entry in row 0 column 0 is
  know as the _pivot_.
* Scale row 0 by (1/_a_) where _a_ is the pivot, so the pivot becomes 1.
* Linearly combine all rows _k_ ≠ 0 so that  row_k' = row_k - _a_* row_0 where _a_
  is the entry in row_k in column 0.
* Repeat with the lower right submatrix of the block matrix until the
  pivot is in the last row.

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
>                              Right m' -> error "gaussianReduce skipCol"-- (restoreCol . gaussianReduce) m'
>              Right m' -> (mjoin . gaussianReduceBlock . msplit . reduceLeftCol) m'

> skipCol :: Matrix a -> Either String (Matrix a)
> skipCol (Matrix m) =
>     if null m || 1 == length (head m)
>         then Left "Can't skipCol"
>         else Right (Matrix (map tail m))

> restoreCol :: Num a => Matrix a -> Matrix a
> restoreCol (Matrix m) = Matrix (map (0:) m)

Fix the pivot by exchanging row 0, if necessary, with the row containing the
largest absolute value in column 0, then dividing the new row 0 by its first entry.
The result will be a 1 in the first column of the first row.

If there is no row with a non-zero first column, return an error indication.

> pivotMatrix :: (Fractional a, Ord a) => Matrix a -> Either String (Matrix a)
> pivotMatrix (Matrix m) = if null test then Left "No pivot" else Right (Matrix m3)
>     where (r:rs) = m
>           test = filter (\(i,r)->(not . equivZero) (head r)) (zip [0..] m)
>           ps   = (reverse . sort) [(abs (head r), i) | (i, r) <- test ]
>           i    = (snd . head . sort . filter (\(x,i)->x==fst(head ps))) ps
>           m2   = if i==0 then m else swapRowsByIndex m 0 i
>           m3   = normalizeRow (head m2) : tail m2

> reduceLeftCol :: (Fractional a, Ord a) => Matrix a -> Matrix a
> reduceLeftCol (Matrix [x:xs]) = Matrix [x:xs]
> reduceLeftCol (Matrix (r:rs)) = Matrix (r : map (reduceRowBy r) rs)

> reduceRowBy :: Num a => [a] -> [a] -> [a]
> reduceRowBy r1 r2 = combineRows (-(head r2)) r1 r2

> gaussianReduceBlock :: (Fractional a, Ord a) => BlockMatrix a -> BlockMatrix a
> gaussianReduceBlock (a, b, c, rest) = (a, b, c, gaussianReduce rest)

> normalizeRow :: (Fractional a, Ord a) => [a] -> [a]
> normalizeRow [] = []
> normalizeRow r@(x:xs) = if equivZero x then 0 : normalizeRow xs else scaleRow (1/x) r

> equivZero :: (Fractional a, Ord a) => a -> Bool
> equivZero x = abs x < 1e-20

The three elementary row operations on a matrix are scale, linearly combine,
and swap.

> scaleRow :: Num a => a -> [a] -> [a]
> scaleRow a = map (*a)

> combineRows :: Num a => a -> [a] -> [a] -> [a]
> combineRows a r1 = addRows (scaleRow a r1)

> swapRowsByIndex :: [a] -> Int -> Int -> [a]
> swapRowsByIndex = swapListElem

Additional helper functions:

> addRows :: Num a => [a] -> [a] -> [a]
> addRows = zipWith (+)

> swapListElem :: [a] -> Int -> Int -> [a]
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
> divList rs i j = ( take i rs
>                  , [rs !! i]
>                  , take (j-i-1) (drop (i+1) rs)
>                  , [rs !! j]
>                  , drop (j+1) rs)

A system of linear equations can have zero, one, or infinitely many solutions.
We can tell the difference by examining the last row of a reduced matrix.

* If it has nonzero entries in the rightmost two columns, there is one solution
  which can be found by back-substitution.
* If it has a single nonzero entry in the rightmost column, there are no solutions,
  because the translation back to a system of equations includes the contradiction
  0 = _c_ for some nonzero value _c_.
* If it has all zeroes, there are infinitely many solutions. **TODO**

> data Solution a = One [a] | None | Many deriving (Show)

> instance (Fractional a, Ord a) => Eq (Solution a) where
>     Many == Many = True
>     None == None = True
>     (One xs) == (One ys) =  (null xs && null ys)
>                          || (length xs == length ys 
>                             && all (\(x,y)-> equivZero (x-y)) (zip xs ys))
>     _ == _ = False

> solve :: (Fractional a, Ord a) => Matrix a -> Solution a
> solve = solve' . gaussianReduce

> solve' :: (Fractional a, Ord a) => Matrix a -> Solution a
> solve' (Matrix m)
>     | all equivZero r           = Many
>     | equivZero (last (init r)) = None
>     | otherwise                 = One (solveOne m) 
>     where r = last m

Assuming there is one solution, the last row gives the value of `e` directly.
The previous row gives `d` in terms of `e` and a constant, so we can substitute
the know value of `e`. Continuing up the matrix, we substitute known values in
each equation to determine the next unknown until we know all values.

> solveOne :: Num a => [[a]] -> [a]
> solveOne = reverse . foldl' accumSolution [] . reverse

> accumSolution :: Num a => [a] -> [a] -> [a]
> accumSolution sln r = sln ++ nextValue sln r

> nextValue :: Num a => [a] -> [a] -> [a]
> nextValue sln r = [last r - sum (zipWith (*) (reverse (init r)) sln)]

> matrixEqual :: (Fractional a, Ord a) => Matrix a -> Matrix a -> Bool
> matrixEqual (Matrix m) (Matrix n) =  length m == length n
>                                   && and (zipWith rowEqual m n)

> rowEqual :: (Fractional a, Ord a) => [a] -> [a] -> Bool
> rowEqual xs ys =  length xs == length ys 
>                && all (\(x,y)->equivZero (x-y)) (zip xs ys)
