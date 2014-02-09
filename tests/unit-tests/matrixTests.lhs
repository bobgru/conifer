> module Main where
> import Matrix
> import TestFramework
> import System.Exit (exitFailure, exitSuccess)

> main = do
>     let (details, summary) = runTests pureTests
>     putStrLn details
>     (ioDetails, ioSummary) <- runTestsIO ioTests
>     putStrLn ioDetails

>     if not (summary && ioSummary) then exitFailure else exitSuccess

> pureTests = [
>       ("nextValue_lastRowEmptySln",        nextValue_lastRowEmptySln)
>     , ("nextValue_lastRowNonEmptySln",     nextValue_lastRowNonEmptySln)

>     , ("accumSolution_lastRowEmptySln",    accumSolution_lastRowEmptySln)
>     , ("accumSolution_lastRowNonEmptySln", accumSolution_lastRowNonEmptySln)

>     , ("solveOne_identity1", solveOne_identity1)
>     , ("solveOne_identity2", solveOne_identity2)
>     , ("solveOne_identity3", solveOne_identity3)
>     , ("solveOne_nonIdentity2", solveOne_nonIdentity2)

>     , ("solve'_Many1", solve'_Many1)
>     , ("solve'_Many2", solve'_Many2)
>     , ("solve'_None1", solve'_None1)
>     , ("solve'_None2", solve'_None2)
>     , ("solve'_OneNonIdentity3", solve'_OneNonIdentity3)

>     , ("slnEqMany", slnEqMany)
>     , ("slnEqNone", slnEqNone)
>     , ("slnNeqManyNone", slnNeqManyNone)
>     , ("slnNeqNoneMany", slnNeqNoneMany)
>     , ("slnEqOneEmpty", slnEqOneEmpty)
>     , ("slnEqOne1", slnEqOne1)
>     , ("slnEqOne2", slnEqOne2)
>     , ("slnEqOne3", slnEqOne3)
>     , ("slnNeqOneEmpty", slnNeqOneEmpty)
>     , ("slnNeqOne1", slnNeqOne1)
>     , ("slnNeqOne2", slnNeqOne2)
>     , ("slnNeqOne3", slnNeqOne3)

>     , ("divList1", divList1)
>     , ("divList2", divList2)
>     , ("divList3", divList3)
>     , ("divList4", divList4)
>     , ("divList5", divList5)
>     , ("divList6", divList6)
>     , ("divList7", divList7)

>     , ("swapListElem2", swapListElem2)
>     , ("swapListElem3", swapListElem3)
>     , ("swapListElem4", swapListElem4)
>     , ("swapListElem5", swapListElem5)
>     , ("swapListElem6", swapListElem6)
>     , ("swapListElem7", swapListElem7)
>     , ("swapListElem8", swapListElem8)

>     , ("addRows1", addRows1)
>     , ("addRows2", addRows2)
>     , ("addRows3", addRows3)

>     , ("combineRows1", combineRows1)
>     , ("combineRows2", combineRows2)
>     , ("combineRows3", combineRows3)

>     , ("equivZero1", equivZero1)
>     , ("equivZero2", equivZero2)

>     , ("normalizeRow1", normalizeRow1)
>     , ("normalizeRow2", normalizeRow2)
>     , ("normalizeRow3", normalizeRow3)
>     , ("normalizeRow4", normalizeRow4)

>     , ("reduceRowBy1", reduceRowBy1)

>     , ("rowEqual1", rowEqual1)
>     , ("rowEqual2", rowEqual2)
>     , ("rowEqual3", rowEqual3)
>     , ("rowEqual4", rowEqual4)

>     , ("matrixEqual1", matrixEqual1)
>     , ("matrixEqual2", matrixEqual2)
>     , ("matrixEqual3", matrixEqual3)
>     , ("matrixEqual4", matrixEqual4)
>     , ("matrixEqual5", matrixEqual5)

>     , ("pivotMatrix1", pivotMatrix1)
>     , ("pivotMatrix2", pivotMatrix2)
>     , ("pivotMatrix3", pivotMatrix3)
>     , ("pivotMatrix4", pivotMatrix4)
>     , ("pivotMatrix5", pivotMatrix5)
>     , ("pivotMatrix6", pivotMatrix6)

>     , ("reduceLeftCol1", reduceLeftCol1)
>     , ("reduceLeftCol2", reduceLeftCol2)
>     , ("reduceLeftCol3", reduceLeftCol3)

>     , ("gaussianReduce1", gaussianReduce1)
>     , ("gaussianReduce2", gaussianReduce2)
>     , ("gaussianReduce3", gaussianReduce3)

>     -- END OF TESTS (command-U to add a test from selected text)
>   ]

> ioTests = []

> {-  seqs_test_1 = assertAreEqual expected (seqs 20 2 [a,b,c,d])
>     where
>         expected = [[ b, c ]]
>         a = mkEx 100  90 0 True
>         b = mkEx 120 110 1 True
>         c = mkEx 100  90 2 False
>         d = mkEx 140 130 3 False -}

> nextValue_lastRowEmptySln    = assertAreEqual [2] (nextValue [] [2])
> nextValue_lastRowNonEmptySln = assertAreEqual [-10] (nextValue [3] [4,2])

> accumSolution_lastRowEmptySln = assertAreEqual [2] (accumSolution [] [2])
> accumSolution_lastRowNonEmptySln = assertAreEqual [3,-10] (accumSolution [3] [4,2])

> solveOne_identity1 = assertAreEqual [1] (solveOne [[1,1]])
> solveOne_identity2 = assertAreEqual [2,1] (solveOne [[1,0,2],[0,1,1]])
> solveOne_identity3 = assertAreEqual [3,2,1] (solveOne [[1,0,0,3],[0,1,0,2],[0,0,1,1]])
> solveOne_nonIdentity2 = assertAreEqual [-2,1] (solveOne [[1,4,2],[0,1,1]])
> solveOne_nonIdentity3 = assertAreEqual [6,-2,1] (solveOne [[1,2,1,3],[0,1,4,2],[0,0,1,1]])

> solve'_Many1 = assertAreEqual Many (solve' (Matrix [[0,0]]))
> solve'_Many2 = assertAreEqual Many (solve' (Matrix [[1,2,3],[0,0,0]]))
> solve'_None1 = assertAreEqual None (solve' (Matrix [[0,1]]))
> solve'_None2 = assertAreEqual None (solve' (Matrix [[1,2,3],[0,0,1]]))
> solve'_OneNonIdentity3 = assertAreEqual (One [6,-2,1])
>                                         (solve' (Matrix [[1,2,1,3],[0,1,4,2],[0,0,1,1]]))

Eq instance of Solution

> slnEqMany   = assertAreEqual True  (Many == Many)
> slnEqNone   = assertAreEqual True  (None == None)
> slnNeqManyNone = assertAreEqual False (Many == None)
> slnNeqNoneMany = assertAreEqual False (None == Many)
> slnEqOneEmpty = assertAreEqual True (One [] == One [])
> slnEqOne1 = assertAreEqual True (One [1] == One [1])
> slnEqOne2 = assertAreEqual True (One [1,2] == One [1-(1e-20), 2+(1e-20)])
> slnEqOne3 = assertAreEqual True (One [1,2,3] == One [1-(1e-20), 2+(1e-20), 3-(1e-20)])
> slnNeqOneEmpty = assertAreEqual False (One [] == One [1])
> slnNeqOne1 = assertAreEqual False (One [1] == One [2])
> slnNeqOne2 = assertAreEqual False (One [1,2] == One [1-(1e-6), 2])
> slnNeqOne3 = assertAreEqual False (One [1,2,3] == One [1-(1e-6), 2+(1e-6), 3])

> divList1 = assertAreEqual ([],[0],[],[1],[]) (divList [0,1] 0 1)
> divList2 = assertAreEqual ([],[0],[],[1],[2]) (divList [0,1,2] 0 1)
> divList3 = assertAreEqual ([0],[1],[],[2],[]) (divList [0,1,2] 1 2)
> divList4 = assertAreEqual ([0],[1],[],[2],[3]) (divList [0..3] 1 2)
> divList5 = assertAreEqual ([0],[1],[],[2],[3,4]) (divList [0..4] 1 2)
> divList6 = assertAreEqual ([0],[1],[2],[3],[4]) (divList [0..4] 1 3)
> divList7 = assertAreEqual ([0,1],[2],[3,4],[5],[6,7]) (divList [0..7] 2 5)

> swapListElem2 = assertAreEqual [0] (swapListElem [0] 0 0)
> swapListElem3 = assertAreEqual [0,1] (swapListElem [0,1] 0 0)
> swapListElem4 = assertAreEqual [1,0] (swapListElem [0,1] 0 1)
> swapListElem5 = assertAreEqual [1,0,2] (swapListElem [0,1,2] 0 1)
> swapListElem6 = assertAreEqual [2,1,0] (swapListElem [0,1,2] 0 2)
> swapListElem7 = assertAreEqual [0,2,1] (swapListElem [0,1,2] 1 2)
> swapListElem8 = assertAreEqual [0,1,2,5,4,3,6] (swapListElem [0,1,2,3,4,5,6] 3 5)

> addRows1 = assertAreEqual [0,2,4,6] (addRows [0,0,0,0] [0,2,4,6])
> addRows2 = assertAreEqual [0,2,4,6] (addRows [0,1,2,3] [0,1,2,3])
> addRows3 = assertAreEqual [0,0,0,0] (addRows [0,1,2,3] [0,-1,-2,-3])

> combineRows1 = assertAreEqual [0,2,4,6] (combineRows 2 [0,0,0,0] [0,2,4,6])
> combineRows2 = assertAreEqual [0,2,4,6] (combineRows 2 [0,1,2,3] [0,0,0,0])
> combineRows3 = assertAreEqual [0,0,0,0] (combineRows (-1) [0,1,2,3] [0,1,2,3])

> equivZero1 = assertAreEqual True  (equivZero 1e-21)
> equivZero2 = assertAreEqual False (equivZero 1e-19)

> normalizeRow1 = assertAreEqual [1,1,1] (normalizeRow [2,2,2])
> normalizeRow2 = assertAreEqual [0,1,1] (normalizeRow [0,2,2])
> normalizeRow3 = assertAreEqual [0,0,1] (normalizeRow [0,0,2])
> normalizeRow4 = assertAreEqual [0,0,1] (normalizeRow [1e-21,0,2])

> reduceRowBy1 = assertAreEqual [0,2,4] (reduceRowBy [1,1,1] [2,4,6])

> rowEqual1 = assertAreEqual False (rowEqual [] [1])
> rowEqual2 = assertAreEqual False (rowEqual [0] [1])
> rowEqual3 = assertAreEqual True (rowEqual [1] [1])
> rowEqual4 = assertAreEqual True (rowEqual [1] [1+1e-21])

> matrixEqual1 = assertAreEqual False (matrixEqual m n)
>     where m = Matrix [[1,1],[1,1]]
>           n = Matrix [[1,1],[1,1],[1,1]]

> matrixEqual2 = assertAreEqual False (matrixEqual m n)
>     where m = Matrix [[1,1,1],[1,1,1]]
>           n = Matrix [[1,1],[1,1]]

> matrixEqual3 = assertAreEqual False (matrixEqual m n)
>     where m = Matrix [[1,1,1],[1,1,1]]
>           n = Matrix [[1,1,1],[1,1,0]]

> matrixEqual4 = assertAreEqual True (matrixEqual m n)
>     where m = Matrix [[1,1,1],[1,1,1]]
>           n = Matrix [[1,1,1],[1,1,1]]

> matrixEqual5 = assertAreEqual True (matrixEqual m n)
>     where m = Matrix [[1,1,1],[1,1,1-1e-20]]
>           n = Matrix [[1,1,1],[1,1,1]]

> pivotMatrix1 = assertAreEqual True (matrixEqual expected actual)
>     where expected     = Matrix [[1,0,0,0], [0,1,0,0], [0,0,1,0]]
>           Right actual = pivotMatrix expected

> pivotMatrix2 = assertAreEqual True (matrixEqual expected actual)
>     where expected     = Matrix [[1,2,0,3], [0,1,0,0], [0,0,1,0]]
>           Right actual = pivotMatrix (Matrix [[2,4,0,6], [0,1,0,0], [0,0,1,0]])

> pivotMatrix3 = assertAreEqual True (matrixEqual expected actual)
>     where expected     = Matrix [[1,0,0,0], [0,1,0,0], [0,0,1,0]]
>           Right actual = pivotMatrix (Matrix [[0,1,0,0], [2,0,0,0], [0,0,1,0]])

> pivotMatrix4 = assertAreEqual True (matrixEqual expected actual)
>     where expected     = Matrix [[1,0,-2,0], [1,1,0,0], [0,0,1,0]]
>           Right actual = pivotMatrix (Matrix [[1,1,0,0], [-2,0,4,0], [0,0,1,0]])

> pivotMatrix5 = assertAreEqual "No pivot" s
>     where Left s = pivotMatrix (Matrix [[0,1,0,0], [0,0,4,0], [0,0,1,0]])

> pivotMatrix6 = assertAreEqual True (matrixEqual expected actual)
>     where expected     = Matrix [[1,1,0,0], [1,0,4,0], [0,0,1,0]]
>           Right actual = pivotMatrix (Matrix [[1,1,0,0], [1,0,4,0], [0,0,1,0]])

> reduceLeftCol1 = assertAreEqual True (matrixEqual expected actual)
>     where expected = Matrix [[1,2]]
>           actual   = reduceLeftCol (Matrix [[1,2]])

> reduceLeftCol2 = assertAreEqual True (matrixEqual expected actual)
>     where expected = Matrix [[1,2], [0,1]]
>           actual   = reduceLeftCol (Matrix [[1,2], [1,3]])

> reduceLeftCol3 = assertAreEqual True (matrixEqual expected actual)
>     where expected = Matrix [[1,2,3,4], [0,-1,-2,-3], [0,-2,-4,-6]]
>           actual   = reduceLeftCol (Matrix [[1,2,3,4], [2,3,4,5], [3,4,5,6]])

> gaussianReduce1 = assertAreEqual True (matrixEqual expected actual)
>     where expected = Matrix [[1,2,3]]
>           actual   = gaussianReduce (Matrix [[2,4,6]])

> gaussianReduce2 = assertAreEqual True (matrixEqual expected actual)
>     where expected = Matrix [[1,2], [0,1]]
>           actual   = gaussianReduce (Matrix [[1,2], [1,3]])

> gaussianReduce3 = assertAreEqual True (matrixEqual expected actual)
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

