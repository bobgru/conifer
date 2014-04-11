> module Main where
> import Control.Monad (when)
> import Matrix
> import System.Exit (exitFailure)
> import Test.HUnit

> main = do 
>     Counts c t e f <- runTestTT tests
>     when (e > 0 || f > 0) exitFailure
> 

> tests = TestList [
>       TestLabel "nextValue_lastRowEmptySln"        $ TestCase  nextValue_lastRowEmptySln
>     , TestLabel "nextValue_lastRowNonEmptySln"     $ TestCase  nextValue_lastRowNonEmptySln

>     , TestLabel "accumSolution_lastRowEmptySln"    $ TestCase accumSolution_lastRowEmptySln
>     , TestLabel "accumSolution_lastRowNonEmptySln" $ TestCase accumSolution_lastRowNonEmptySln

>     , TestLabel "solveOne_identity1"    $ TestCase solveOne_identity1
>     , TestLabel "solveOne_identity2"    $ TestCase solveOne_identity2
>     , TestLabel "solveOne_identity3"    $ TestCase solveOne_identity3
>     , TestLabel "solveOne_nonIdentity2" $ TestCase solveOne_nonIdentity2

>     , TestLabel "solve'_Many1" $ TestCase solve'_Many1
>     , TestLabel "solve'_Many2" $ TestCase solve'_Many2
>     , TestLabel "solve'_None1" $ TestCase solve'_None1
>     , TestLabel "solve'_None2" $ TestCase solve'_None2
>     , TestLabel "solve'_OneNonIdentity3" $ TestCase solve'_OneNonIdentity3

>     , TestLabel "slnEqMany"      $ TestCase slnEqMany
>     , TestLabel "slnEqNone"      $ TestCase slnEqNone
>     , TestLabel "slnNeqManyNone" $ TestCase slnNeqManyNone
>     , TestLabel "slnNeqNoneMany" $ TestCase slnNeqNoneMany
>     , TestLabel "slnEqOneEmpty"  $ TestCase slnEqOneEmpty
>     , TestLabel "slnEqOne1"      $ TestCase slnEqOne1
>     , TestLabel "slnEqOne2"      $ TestCase slnEqOne2
>     , TestLabel "slnEqOne3"      $ TestCase slnEqOne3
>     , TestLabel "slnNeqOneEmpty" $ TestCase slnNeqOneEmpty
>     , TestLabel "slnNeqOne1"     $ TestCase slnNeqOne1
>     , TestLabel "slnNeqOne2"     $ TestCase slnNeqOne2
>     , TestLabel "slnNeqOne3"     $ TestCase slnNeqOne3

>     , TestLabel "divList1" $ TestCase divList1
>     , TestLabel "divList2" $ TestCase divList2
>     , TestLabel "divList3" $ TestCase divList3
>     , TestLabel "divList4" $ TestCase divList4
>     , TestLabel "divList5" $ TestCase divList5
>     , TestLabel "divList6" $ TestCase divList6
>     , TestLabel "divList7" $ TestCase divList7

>     , TestLabel "swapListElem2" $ TestCase swapListElem2
>     , TestLabel "swapListElem3" $ TestCase swapListElem3
>     , TestLabel "swapListElem4" $ TestCase swapListElem4
>     , TestLabel "swapListElem5" $ TestCase swapListElem5
>     , TestLabel "swapListElem6" $ TestCase swapListElem6
>     , TestLabel "swapListElem7" $ TestCase swapListElem7
>     , TestLabel "swapListElem8" $ TestCase swapListElem8

>     , TestLabel "addRows1" $ TestCase addRows1
>     , TestLabel "addRows2" $ TestCase addRows2
>     , TestLabel "addRows3" $ TestCase addRows3

>     , TestLabel "combineRows1" $ TestCase combineRows1
>     , TestLabel "combineRows2" $ TestCase combineRows2
>     , TestLabel "combineRows3" $ TestCase combineRows3

>     , TestLabel "equivZero1" $ TestCase equivZero1
>     , TestLabel "equivZero2" $ TestCase equivZero2

>     , TestLabel "normalizeRow1" $ TestCase normalizeRow1
>     , TestLabel "normalizeRow2" $ TestCase normalizeRow2
>     , TestLabel "normalizeRow3" $ TestCase normalizeRow3
>     , TestLabel "normalizeRow4" $ TestCase normalizeRow4

>     , TestLabel "reduceRowBy1" $ TestCase reduceRowBy1

>     , TestLabel "rowEqual1" $ TestCase rowEqual1
>     , TestLabel "rowEqual2" $ TestCase rowEqual2
>     , TestLabel "rowEqual3" $ TestCase rowEqual3
>     , TestLabel "rowEqual4" $ TestCase rowEqual4

>     , TestLabel "matrixEqual1" $ TestCase matrixEqual1
>     , TestLabel "matrixEqual2" $ TestCase matrixEqual2
>     , TestLabel "matrixEqual3" $ TestCase matrixEqual3
>     , TestLabel "matrixEqual4" $ TestCase matrixEqual4
>     , TestLabel "matrixEqual5" $ TestCase matrixEqual5

>     , TestLabel "pivotMatrix1" $ TestCase pivotMatrix1
>     , TestLabel "pivotMatrix2" $ TestCase pivotMatrix2
>     , TestLabel "pivotMatrix3" $ TestCase pivotMatrix3
>     , TestLabel "pivotMatrix4" $ TestCase pivotMatrix4
>     , TestLabel "pivotMatrix5" $ TestCase pivotMatrix5
>     , TestLabel "pivotMatrix6" $ TestCase pivotMatrix6

>     , TestLabel "reduceLeftCol1" $ TestCase reduceLeftCol1
>     , TestLabel "reduceLeftCol2" $ TestCase reduceLeftCol2
>     , TestLabel "reduceLeftCol3" $ TestCase reduceLeftCol3

>     , TestLabel "gaussianReduce1" $ TestCase gaussianReduce1
>     , TestLabel "gaussianReduce2" $ TestCase gaussianReduce2
>     , TestLabel "gaussianReduce3" $ TestCase gaussianReduce3
>   ]

> nextValue_lastRowEmptySln    = (nextValue [] [2]) `compare` [2] @?= EQ
> nextValue_lastRowNonEmptySln = (nextValue [3] [4,2]) `compare` [-10] @?= EQ

> accumSolution_lastRowEmptySln    = (accumSolution [] [2]) `compare` [2] @?= EQ
> accumSolution_lastRowNonEmptySln = (accumSolution [3] [4,2]) `compare` [3,-10] @?= EQ

> solveOne_identity1 = (solveOne [[1,1]]) `compare` [1] @?= EQ
> solveOne_identity2 = (solveOne [[1,0,2],[0,1,1]]) `compare` [2,1] @?= EQ
> solveOne_identity3 = (solveOne [[1,0,0,3],[0,1,0,2],[0,0,1,1]]) `compare` [3,2,1] @?= EQ
> solveOne_nonIdentity2 = (solveOne [[1,4,2],[0,1,1]]) `compare` [-2,1] @?= EQ
> solveOne_nonIdentity3 = (solveOne [[1,2,1,3],[0,1,4,2],[0,0,1,1]]) `compare` [6,-2,1] @?= EQ

> solve'_Many1 = (solve' (Matrix [[0,0]]))           @?= Many
> solve'_Many2 = (solve' (Matrix [[1,2,3],[0,0,0]])) @?= Many
> solve'_None1 = (solve' (Matrix [[0,1]]))           @?= None
> solve'_None2 = (solve' (Matrix [[1,2,3],[0,0,1]])) @?= None
> solve'_OneNonIdentity3 = (solve' (Matrix [[1,2,1,3],[0,1,4,2],[0,0,1,1]])) @?= (One [6,-2,1])

Eq instance of Solution

> slnEqMany   = (Many == Many) `compare` True  @?= EQ
> slnEqNone   = (None == None) `compare` True  @?= EQ
> slnNeqManyNone = (Many == None) `compare` False @?= EQ
> slnNeqNoneMany = (None == Many) `compare` False @?= EQ
> slnEqOneEmpty = (One [] == One []) `compare` True @?= EQ
> slnEqOne1 = (One [1] == One [1]) `compare` True @?= EQ
> slnEqOne2 = (One [1,2] == One [1-(1e-20), 2+(1e-20)]) `compare` True @?= EQ
> slnEqOne3 = (One [1,2,3] == One [1-(1e-20), 2+(1e-20), 3-(1e-20)]) `compare` True @?= EQ
> slnNeqOneEmpty = (One [] == One [1]) `compare` False @?= EQ
> slnNeqOne1 = (One [1] == One [2]) `compare` False @?= EQ
> slnNeqOne2 = (One [1,2] == One [1-(1e-6), 2]) `compare` False @?= EQ
> slnNeqOne3 = (One [1,2,3] == One [1-(1e-6), 2+(1e-6), 3]) `compare` False @?= EQ

> divList1 = (divList [0,1] 0 1) `compare` ([],[0],[],[1],[]) @?= EQ
> divList2 = (divList [0,1,2] 0 1) `compare` ([],[0],[],[1],[2]) @?= EQ
> divList3 = (divList [0,1,2] 1 2) `compare` ([0],[1],[],[2],[]) @?= EQ
> divList4 = (divList [0..3] 1 2) `compare` ([0],[1],[],[2],[3]) @?= EQ
> divList5 = (divList [0..4] 1 2) `compare` ([0],[1],[],[2],[3,4]) @?= EQ
> divList6 = (divList [0..4] 1 3) `compare` ([0],[1],[2],[3],[4]) @?= EQ
> divList7 = (divList [0..7] 2 5) `compare` ([0,1],[2],[3,4],[5],[6,7]) @?= EQ

> swapListElem2 = (swapListElem [0] 0 0) `compare` [0] @?= EQ
> swapListElem3 = (swapListElem [0,1] 0 0) `compare` [0,1] @?= EQ
> swapListElem4 = (swapListElem [0,1] 0 1) `compare` [1,0] @?= EQ
> swapListElem5 = (swapListElem [0,1,2] 0 1) `compare` [1,0,2] @?= EQ
> swapListElem6 = (swapListElem [0,1,2] 0 2) `compare` [2,1,0] @?= EQ
> swapListElem7 = (swapListElem [0,1,2] 1 2) `compare` [0,2,1] @?= EQ
> swapListElem8 = (swapListElem [0,1,2,3,4,5,6] 3 5) `compare` [0,1,2,5,4,3,6] @?= EQ

> addRows1 = (addRows [0,0,0,0] [0,2,4,6]) `compare` [0,2,4,6] @?= EQ
> addRows2 = (addRows [0,1,2,3] [0,1,2,3]) `compare` [0,2,4,6] @?= EQ
> addRows3 = (addRows [0,1,2,3] [0,-1,-2,-3]) `compare` [0,0,0,0] @?= EQ

> combineRows1 = (combineRows 2 [0,0,0,0] [0,2,4,6]) `compare` [0,2,4,6] @?= EQ
> combineRows2 = (combineRows 2 [0,1,2,3] [0,0,0,0]) `compare` [0,2,4,6] @?= EQ
> combineRows3 = (combineRows (-1) [0,1,2,3] [0,1,2,3]) `compare` [0,0,0,0] @?= EQ

> equivZero1 = (equivZero 1e-21) `compare` True  @?= EQ
> equivZero2 = (equivZero 1e-19) `compare` False @?= EQ

> normalizeRow1 = (normalizeRow [2,2,2]) `compare` [1,1,1] @?= EQ
> normalizeRow2 = (normalizeRow [0,2,2]) `compare` [0,1,1] @?= EQ
> normalizeRow3 = (normalizeRow [0,0,2]) `compare` [0,0,1] @?= EQ
> normalizeRow4 = (normalizeRow [1e-21,0,2]) `compare` [0,0,1] @?= EQ

> reduceRowBy1 = (reduceRowBy [1,1,1] [2,4,6]) `compare` [0,2,4] @?= EQ

> rowEqual1 = (rowEqual [] [1]) `compare` False @?= EQ
> rowEqual2 = (rowEqual [0] [1]) `compare` False @?= EQ
> rowEqual3 = (rowEqual [1] [1]) `compare` True @?= EQ
> rowEqual4 = (rowEqual [1] [1+1e-21]) `compare` True @?= EQ

> matrixEqual1 = (matrixEqual m n) `compare` False @?= EQ
>     where m = Matrix [[1,1],[1,1]]
>           n = Matrix [[1,1],[1,1],[1,1]]

> matrixEqual2 = (matrixEqual m n) `compare` False @?= EQ
>     where m = Matrix [[1,1,1],[1,1,1]]
>           n = Matrix [[1,1],[1,1]]

> matrixEqual3 = (matrixEqual m n) `compare` False @?= EQ
>     where m = Matrix [[1,1,1],[1,1,1]]
>           n = Matrix [[1,1,1],[1,1,0]]

> matrixEqual4 = (matrixEqual m n) `compare` True @?= EQ
>     where m = Matrix [[1,1,1],[1,1,1]]
>           n = Matrix [[1,1,1],[1,1,1]]

> matrixEqual5 = (matrixEqual m n) `compare` True @?= EQ
>     where m = Matrix [[1,1,1],[1,1,1-1e-20]]
>           n = Matrix [[1,1,1],[1,1,1]]

> pivotMatrix1 = (matrixEqual expected actual) `compare` True @?= EQ
>     where expected     = Matrix [[1,0,0,0], [0,1,0,0], [0,0,1,0]]
>           Right actual = pivotMatrix expected

> pivotMatrix2 = (matrixEqual expected actual) `compare` True @?= EQ
>     where expected     = Matrix [[1,2,0,3], [0,1,0,0], [0,0,1,0]]
>           Right actual = pivotMatrix (Matrix [[2,4,0,6], [0,1,0,0], [0,0,1,0]])

> pivotMatrix3 = (matrixEqual expected actual) `compare` True @?= EQ
>     where expected     = Matrix [[1,0,0,0], [0,1,0,0], [0,0,1,0]]
>           Right actual = pivotMatrix (Matrix [[0,1,0,0], [2,0,0,0], [0,0,1,0]])

> pivotMatrix4 = (matrixEqual expected actual) `compare` True @?= EQ
>     where expected     = Matrix [[1,0,-2,0], [1,1,0,0], [0,0,1,0]]
>           Right actual = pivotMatrix (Matrix [[1,1,0,0], [-2,0,4,0], [0,0,1,0]])

> pivotMatrix5 = s `compare` "No pivot" @?= EQ
>     where Left s = pivotMatrix (Matrix [[0,1,0,0], [0,0,4,0], [0,0,1,0]])

> pivotMatrix6 = (matrixEqual expected actual) `compare` True @?= EQ
>     where expected     = Matrix [[1,1,0,0], [1,0,4,0], [0,0,1,0]]
>           Right actual = pivotMatrix (Matrix [[1,1,0,0], [1,0,4,0], [0,0,1,0]])

> reduceLeftCol1 = (matrixEqual expected actual) `compare` True @?= EQ
>     where expected = Matrix [[1,2]]
>           actual   = reduceLeftCol (Matrix [[1,2]])

> reduceLeftCol2 = (matrixEqual expected actual) `compare` True @?= EQ
>     where expected = Matrix [[1,2], [0,1]]
>           actual   = reduceLeftCol (Matrix [[1,2], [1,3]])

> reduceLeftCol3 = (matrixEqual expected actual) `compare` True @?= EQ
>     where expected = Matrix [[1,2,3,4], [0,-1,-2,-3], [0,-2,-4,-6]]
>           actual   = reduceLeftCol (Matrix [[1,2,3,4], [2,3,4,5], [3,4,5,6]])

> gaussianReduce1 = (matrixEqual expected actual) `compare` True @?= EQ
>     where expected = Matrix [[1,2,3]]
>           actual   = gaussianReduce (Matrix [[2,4,6]])

> gaussianReduce2 = (matrixEqual expected actual) `compare` True @?= EQ
>     where expected = Matrix [[1,2], [0,1]]
>           actual   = gaussianReduce (Matrix [[1,2], [1,3]])

> gaussianReduce3 = (matrixEqual expected actual) `compare` True @?= EQ
>     where expected = Matrix [[1,2,3,4], [0,1,2,3], [0,0,0,0]]
>           actual   = gaussianReduce (Matrix [[1,3,5,7], [2,3,4,5], [3,6,9,12]])

> q0 = Matrix [
>     [1.3597283605654433,0.6721972072470472,0.33230834814891136
>     ,1.1660739087062377,0.5764619225490192,1.0]

>     ,[1.4846603828656724,0.9577054623474371,0.6177842173169268
>     ,1.2184664061293082,0.7859925046187952,1.0]

>     ,[1.3490332493927968,1.061502386965913,0.8352554083018345
>     ,1.1614789061333817,0.9139230866445133,1.0]

>     ,[1.0075888211789694,0.9443403460552621,0.8850621110943956
>     ,1.0037872389998637,0.9407773972063719,1.0]

>     ,[0.5919741003840879,0.6635809286982955,0.7438495174812357
>     ,0.7693985315713099,0.8624671109562588,1.0]
>     ]

> -- q1 = pivotMatrix q0
> q1 = Matrix [
>     [1.0,0.6450670290661938,0.4161114719882857,0.8207037920533988
>     ,0.5294089568832453,0.6735547142908285]

>     ,[1.3597283605654433,0.6721972072470472,0.33230834814891136
>     ,1.1660739087062377,0.5764619225490192,1.0]

>     ,[1.3490332493927968,1.061502386965913,0.8352554083018345
>     ,1.1614789061333817,0.9139230866445133,1.0]

>     ,[1.0075888211789694,0.9443403460552621,0.8850621110943956
>     ,1.0037872389998637,0.9407773972063719,1.0]

>     ,[0.5919741003840879,0.6635809286982955,0.7438495174812357
>     ,0.7693985315713099,0.8624671109562588,1.0]
>     ]

> -- q2 = reduceLeftCol q1
> q2 = Matrix [
>     [1.0,0.6450670290661938,0.4161114719882857,0.8207037920533988
>     ,0.5294089568832453,0.6735547142908285]

>     ,[0.0,-0.2049187266399497,-0.23349022147019377,5.013968702762717e-2
>     ,-0.14339045046249743,8.414855258620624e-2]

>     ,[0.0,0.19128551666858784,0.27390719713585776,5.432220275059496e-2
>     ,0.1997328012826578,9.135229513640686e-2]

>     ,[0.0,0.29437801865703583,0.46579284355467304,0.17685527262766954
>     ,0.40735085041879493,0.32133379942816664]

>     ,[0.0,0.2817179544793991,0.4975223031914717,0.28356314258868953
>     ,0.5490707199700213,0.6012730539482254]
>     ]

> -- q3 = lower right submatrix returned by msplit q2
> q3 = Matrix [
>     [-0.2049187266399497,-0.23349022147019377,5.013968702762717e-2
>     ,-0.14339045046249743,8.414855258620624e-2]

>     ,[0.19128551666858784,0.27390719713585776,5.432220275059496e-2
>     ,0.1997328012826578,9.135229513640686e-2]

>     ,[0.29437801865703583,0.46579284355467304,0.17685527262766954
>     ,0.40735085041879493,0.32133379942816664]

>     ,[0.2817179544793991,0.4975223031914717,0.28356314258868953
>     ,0.5490707199700213,0.6012730539482254]
>     ]

> -- q4 = pivotMatrix q3
> q4 = Matrix [
>     [1.0,1.5822949202513097,0.6007760818368514
>     ,1.3837678923078076,1.09156859229539]

>     ,[0.19128551666858784,0.27390719713585776,5.432220275059496e-2
>     ,0.1997328012826578,9.135229513640686e-2]

>     ,[-0.2049187266399497,-0.23349022147019377,5.013968702762717e-2
>     ,-0.14339045046249743,8.414855258620624e-2]

>     ,[0.2817179544793991,0.4975223031914717,0.28356314258868953
>     ,0.5490707199700213,0.6012730539482254]
>     ]

> -- q5 = reduceLeftCol q4
> q5 = Matrix [
>     [1.0,1.5822949202513097,0.6007760818368514,1.3837678923078076,1.09156859229539]

>     ,[0.0,-2.876290420649602e-2,-6.059756046569696e-2,-6.496195494684398e-2,-0.11744896702001992]

>     ,[0.0,9.075163875656539e-2,0.17324995671337295,0.14016950399446554,0.30783139855953995]

>     ,[0.0,5.176141487512881e-2,0.11431373371346368,0.1592384598747963,0.29375858295281093]
>     ]

> -- q6 = lower right submatrix returned by msplit q5
> q6 = Matrix [
>     [-2.876290420649602e-2,-6.059756046569696e-2,-6.496195494684398e-2,-0.11744896702001992]
>     ,[9.075163875656539e-2,0.17324995671337295,0.14016950399446554,0.30783139855953995]
>     ,[5.176141487512881e-2,0.11431373371346368,0.1592384598747963,0.29375858295281093]
>     ]

> -- q7 = pivotMatrix q6
> q7 = Matrix [
>     [1.0,1.9090559585166638,1.5445396459501954,3.392020273983978]
>     ,[-2.876290420649602e-2,-6.059756046569696e-2,-6.496195494684398e-2,-0.11744896702001992]
>     ,[5.176141487512881e-2,0.11431373371346368,0.1592384598747963,0.29375858295281093]
>     ]

> -- q8 = reduceLeftCol q7
> q8 = Matrix [
>     [1.0,1.9090559585166638,1.5445396459501954,3.392020273983978]
>     ,[0.0,-5.6875668060417245e-3,-2.0536509067243233e-2,-1.988461281292636e-2]
>     ,[0.0,1.5498296224845956e-2,7.929090246968366e-2,0.11818281428627814]
>     ]

> -- q9 = lower right submatrix returned by msplit q8
> q9 = Matrix [
>     [-5.6875668060417245e-3,-2.0536509067243233e-2,-1.988461281292636e-2]
>     ,[1.5498296224845956e-2,7.929090246968366e-2,0.11818281428627814]
>     ]

> -- q10 = pivotMatrix q9
> q10 = Matrix [
>      [0.9999999999999999,5.1161044620226805,7.625535902250623]
>      ,[-5.6875668060417245e-3,-2.0536509067243233e-2,-1.988461281292636e-2]
>      ]

> -- q11 = reduceLeftCol q10
> q11 = Matrix [
>      [0.9999999999999999,5.1161044620226805,7.625535902250623]
>      ,[-8.673617379884035e-19,8.56167684719892e-3,2.3486132062993714e-2]
>      ]

> -- q12 = lower right submatrix returned by msplit q11
> q12 = Matrix [[8.56167684719892e-3,2.3486132062993714e-2]]

> -- q13 = gaussianReduce q12
> q13 = Matrix [[1.0,2.7431696479735224]]

> -- q' = gaussianReduce q0
> q' = Matrix [
>     [1.0,0.6450670290661938,0.4161114719882857,0.8207037920533988
>     ,0.5294089568832453,0.6735547142908285]

>     ,[0.0,1.0,1.5822949202513097,0.6007760818368514,1.3837678923078076,1.09156859229539]

>     ,[0.0,0.0,1.0,1.9090559585166638,1.5445396459501954,3.392020273983978]

>     ,[0.0,0.0,0.0,0.9999999999999999,5.1161044620226805,7.625535902250623]

>     ,[0.0,0.0,0.0,-8.673617379884035e-19,1.0,2.7431696479735224]
>     ]

