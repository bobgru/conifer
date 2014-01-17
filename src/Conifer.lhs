A Virtual Conifer
=================

**Introduction**

This project is to model a conifer as the expression of a set of "genes",
i.e. parameters that control lengths and angles and such.

> {-# LANGUAGE NoMonomorphismRestriction #-}
> module Conifer
> where 

> import Diagrams.Prelude
> import Diagrams.Backend.SVG
> import Diagrams.Coordinates
> import Diagrams.ThreeD.Types
> import Data.Default.Class

**The Input: Tree Parameters**

Our ideal tree will be completely determined by its "genes", the various
parameters in `TreeParams`. The age of the tree is the number of recursive
steps in its growth. As we are modeling a conifer, its structure is a main
trunk that adds some number of whorls of branches each year and another 
length of trunk, meanwhile adding another level of branching to existing branches.

> data TreeParams = TreeParams {
>       tpAge                      :: Int
>     , tpTrunkLengthIncrement     :: Double
>     , tpTrunkBranchLengthRatio   :: Double
>     , tpTrunkBranchAngles        :: [Double]
>     , tpTrunkGirth               :: Double
>     , tpWhorlsPerYear            :: Int
>     , tpWhorlSize                :: Int
>     , tpWhorlPhase               :: Double
>     , tpBranchGirth              :: Double
>     , tpBranchBranchLengthRatio  :: Double
>     , tpBranchBranchLengthRatio2 :: Double
>     , tpBranchBranchAngle        :: Rad
>     } deriving (Show, Eq)

> instance Default TreeParams where
>     def = TreeParams {
>       tpAge                      = 5
>     , tpTrunkLengthIncrement     = 0.9
>     , tpTrunkBranchLengthRatio   = 0.7
>     , tpTrunkBranchAngles        = [tau / 6]
>     , tpTrunkGirth               = 1.0
>     , tpWhorlsPerYear            = 1
>     , tpWhorlSize                = 6
>     , tpWhorlPhase               = 0
>     , tpBranchGirth              = 1.0
>     , tpBranchBranchLengthRatio  = 0.8
>     , tpBranchBranchLengthRatio2 = 0.8
>     , tpBranchBranchAngle        = tau / 6
>     }

**The Data Structure: Tree, Whorl, Branch**

A tree rises from its origin to its `tNode` where there is optionally
another tree, and—every year after the first—a whorl. The tree may
grow additional whorls during a year, which are spaced evenly up the
trunk.

> data Tree a = Tree {
>       tNode     :: a
>     , tAge      :: Int
>     , tGirth    :: Double
>     , tNext     :: Maybe (Tree a)
>     , tWhorls   :: [Whorl a]
>     } deriving (Show, Eq)

A whorl is a collection of branches radiating evenly spaced from 
the trunk but at varying angles relative to the trunk. 
There can be multiple whorls per year, so a whorl records its position 
along that year's segment of trunk and a scale factor, so that older 
whorls can have longer branches than younger ones. 

> data Whorl a = Whorl {
>       wNode     :: a
>     , wScale    :: Double
>     , wBranches :: [Branch a]
>     } deriving (Show, Eq)

A branch shoots out from its origin to its `bNode`, where it branches
into some number, possibly zero, of other branches.

> data Branch a =
>     Branch {
>       bNode       :: a
>     , bAge        :: Int
>     , bPartialAge :: Double
>     , bGirth      :: Double
>     , bBranches   :: Maybe [Branch a]
>     } deriving (Show, Eq)

We can specialize the types for the three phases of tree development.
The tree grows as type `RTree3` (3D tree with relative coordinates), 
is converted to `ATree3` (3D tree with absolute coordinates), and
is projected to `ATree2` (2D tree with absolute coordinates) before 
reduction to tree-primitives. The primitives do not retain the tree
structure.

> type RTree3 = Tree P3
> type ATree3 = Tree P3
> type ATree2 = Tree P2

> type RWhorl3 = Whorl P3
> type AWhorl3 = Whorl P3
> type AWhorl2 = Whorl P2

> type RBranch3 = Branch P3
> type ABranch3 = Branch P3
> type ABranch2 = Branch P2

It will be convenient to be able to apply a function throughout the tree while
preserving its structure.

> instance Functor Tree where
>     fmap = treeMap

> instance Functor Whorl where
>     fmap = whorlMap

> instance Functor Branch where
>     fmap = branchMap

> treeMap :: (a -> b) -> Tree a -> Tree b
> treeMap f (Tree p a g mt ws) = Tree p' a g mt' ws'
>     where p'  = f p
>           mt' = fmap (fmap f) mt
>           ws' = fmap (fmap f) ws

> whorlMap :: (a -> b) -> Whorl a -> Whorl b
> whorlMap f (Whorl p s bs) = Whorl p' s bs'
>     where p'  = f p
>           bs' = fmap (fmap f) bs

> branchMap :: (a -> b) -> Branch a -> Branch b
> branchMap f (Branch p a pa g mbs) = Branch p' a pa g mbs'
>     where p'   = f p
>           mbs' = fmap (fmap (fmap f)) mbs

The tree parts are reduced to diagram primitives which can be folded into a single 
diagram for rendering as an image.
* `Trunk` is a section of trunk between points _p0_ and _p1_, with girth _g0_ at _p0_ and _g1_ at _p1_.
* `Stem` is a section of branch between points _p0_ and _p1_ with girth _g_.
* `Tip` is the tip of a branch, between points _p0_ and _p1_.

> data TreePrim = Trunk { p0::P2, p1::P2, g0::Double, g1::Double }
>               | Stem  { p0::P2, p1::P2, g::Double }
>               | Tip   { p0::P2, p1::P2 }

**Growing the Tree**

We first build a tree with each node in its own coordinate space relative to its 
parent node.

> tree :: TreeParams -> RTree3
> tree tp = Tree trunkTip age girth nextTree whorls
>     where age         = tpAge tp
>           girth       = tpTrunkGirth tp
>           trunkGrowth = tpTrunkLengthIncrement tp

This year's trunk growth simply adds an increment of height relative to the
tip of last year's trunk, with possibly another tree on top of that.

>           trunkTip = p3 (0, 0, trunkGrowth)
>           nextTree = if age == 0 then Nothing else Just (tree tpNextYear)

The tree grows at least one whorl of branches every year after the first, starting
at the tip of last year's trunk. Additionally, it might sprout a number of whorls
during the year, which have an amount of partial growth at the tip proportional
to the age of the whorl. A whorl is given the base height from which it sprouts,
and the ratio of partial growth at its tip. If the whorl is the first of the year, 
then its age is one year less, and its partial growth is 1.0. 

>           numWhorls             = tpWhorlsPerYear tp
>           tipGrowth             = tpBranchBranchLengthRatio tp ^ age
>           whorlHeight a         = trunkGrowth * (1 - a)
>           branchPartialGrowth a = tipGrowth   * a
>           partialAge i          = fromIntegral i / fromIntegral numWhorls
>           initialBranchTips     = [(whorlHeight a, branchPartialGrowth a, a) 
>                                       | i <- [1..numWhorls-1], let a = partialAge (numWhorls-i)]

There is no whorl at the very top of the tree, i.e. when age is 0, there is 
no next year's whorl. The whorls' parameters vary by whorl phase and trunk branch angle, so
`tps` supplies the list. Additionally, we need to identify the parameters for next year, which
entails determining the last parameters for this year so we can properly advance the phase and angle.

>           whorls          = thisYearsWhorls ++ nextYearsWhorl
>           thisYearsWhorls = [whorl tp' (p3 (0, 0, height))  tipGrowth pa
>                                 | (tp', (height, tipGrowth, pa)) <- zip tps initialBranchTips]
>           nextYearsWhorl  = if age == 0 then [] else [whorl tpNextYear trunkTip 1.0 1.0]
>           tps             = take (length initialBranchTips)
>                                  (iterate (advancePhase . advanceTrunkBranchAngle) tp)
>           tpNextYear      =  (subYear . advancePhase . advanceTrunkBranchAngle) tpThisYearsLast
>           tpThisYearsLast = if numWhorls == 1 then tp else last tps

Some useful helper functions for manipulating `TreeParams`:

> subYear :: TreeParams -> TreeParams
> subYear      tp = tp { tpAge = tpAge tp - 1 }

> advancePhase :: TreeParams -> TreeParams
> advancePhase tp = tp { tpWhorlPhase = wp + tau / (ws * wpy * 2) }
>     where wp  = tpWhorlPhase tp
>           ws  = fromIntegral (tpWhorlSize tp)
>           wpy = fromIntegral (tpWhorlsPerYear tp)

> advanceTrunkBranchAngle :: TreeParams -> TreeParams
> advanceTrunkBranchAngle tp = tp { tpTrunkBranchAngles = shiftList (tpTrunkBranchAngles tp) }

> shiftList []       = []
> shiftList (x : xs) = xs ++ [x]

A whorl is some number of branches, evenly spaced but at varying angle
from the vertical. A whorl is rotated by the whorl phase, which changes
from one to the next.

> whorl :: TreeParams -> P3 -> Double -> Double -> RWhorl3
> whorl tp p s pa = Whorl p s [ branch tp (pt i) s pa | i <- [0 .. numBranches - 1]]
>     where pt i = p3 ( initialBranchGrowth * cos (rotation i)
>                     , initialBranchGrowth * sin (rotation i)
>                     , height i)
>           age                 = tpAge tp
>           tblr                = tpTrunkBranchLengthRatio tp
>           phase               = tpWhorlPhase tp
>           numBranches         = tpWhorlSize tp
>           rotation i          = fromIntegral i * tau / fromIntegral numBranches + phase

If the whorl is less than a year old, it will have partial growth of its branches,
which are all tips without subbranches. Otherwise, the initial branch lengths will be
at full growth, and the partial growth information is passed through to the branches
to apply at their tips.

>           initialBranchGrowth = if age == 0 then partialGrowth else fullGrowth
>           partialGrowth       = s * tblr
>           fullGrowth          = tblr
>           height i            = initialBranchGrowth * cos (tbas !! j)
>               where j    = i `mod` length tbas
>                     tbas = tpTrunkBranchAngles tp

A branch shoots forward a certain length, then ends or splits into three branches,
going left, center, or right. Along with the point specifying the tip of the branch,
there is a partial growth distance, which is used when drawing the tip itself.

> branch :: TreeParams -> P3 -> Double -> Double -> RBranch3
> branch tp p s pa = Branch p age pa g mbs
>     where age   = tpAge tp
>           g     = tpBranchGirth tp

Next year's subbranches continue straight, to the left and to the right. The straight
subbranch grows at a possibly different rate from the side subbranches.

>           mbs = if age == 0 then Nothing else Just bs
>           bs     = map mkBr [l, c, r]
>           l      = p # rotateXY   bba  # scale growth2
>           c      = p                   # scale growth
>           r      = p # rotateXY (-bba) # scale growth2
>           bba    = tpBranchBranchAngle tp
>           mkBr p = branch (subYear tp) p s pa

Determine if next year has partial growth.

>           growth      = getGrowth tpBranchBranchLengthRatio
>           growth2     = getGrowth tpBranchBranchLengthRatio2           
>           getGrowth f = if age == 1 then partialGrowth  else fullGrowth
>               where partialGrowth  = s * bblr
>                     fullGrowth     = bblr
>                     bblr           = f tp

> rotateXY :: Rad -> P3 -> P3
> rotateXY a p = p3 (x', y', z)
>     where (x, y, z) = unp3 p
>           (x', y')  = unr2 (rotate a (r2 (x, y)))

**Converting from Relative to Absolute Coordinates**

Convert the tree of relative coordinate spaces into a single coherent absolute
coordinate space, which will make projection onto the _x_-_z_-plane trivial.

> toAbsoluteP3 :: P3 -> P3 -> P3
> toAbsoluteP3 n p = n .+^ (p .-. origin)

> toAbsolute :: RTree3 -> ATree3
> toAbsolute = toAbsoluteTree origin

> toAbsoluteTree :: P3 -> RTree3 -> ATree3
> toAbsoluteTree n (Tree p a g mt ws) = Tree p' a g mt' ws'
>     where p'  = toAbsoluteP3 n p
>           ws' = fmap (toAbsoluteWhorl n) ws
>           mt' = fmap (toAbsoluteTree p') mt

> toAbsoluteWhorl :: P3 -> RWhorl3 -> AWhorl3
> toAbsoluteWhorl n (Whorl p s bs) = Whorl p' s bs'
>     where p'  = toAbsoluteP3 n p
>           bs' = fmap (toAbsoluteBranch p') bs

> toAbsoluteBranch :: P3 -> RBranch3 -> ABranch3
> toAbsoluteBranch n (Branch p a pa g mbs) = Branch p' a pa g mbs'
>     where p'   = toAbsoluteP3 n p
>           mbs' = fmap (fmap (toAbsoluteBranch p')) mbs

**Projecting the Tree onto 2D**

We are rendering the tree from the side, so we simply discard the _y_-coordinate.

> xz :: P3 -> P2
> xz p = p2 (x, z) where (x, _, z) = unp3 p

> projectXZ :: ATree3 -> ATree2
> projectXZ = fmap xz

**Respect the Earth**

Assuming the tree is growing on flat ground, we can't have the branches digging into it.

> aboveGround :: P2 -> P2
> aboveGround p = p2 (x, max 0 z) where (x, z) = unp2 p

> mkAboveGround :: ATree2 -> ATree2
> mkAboveGround = fmap aboveGround

**Reducing the Tree to Primitives from Absolute Coordinates**

> toPrim :: ATree2 -> [TreePrim]
> toPrim = treeToPrim origin

> treeToPrim :: P2 -> ATree2 -> [TreePrim]
> treeToPrim n (Tree p a g mt ws) = trunk ++ whorls ++ nextTree
>     where trunk    = trunkToPrim n p a g
>           whorls   = concatMap whorlToPrim ws
>           nextTree = maybe [] (treeToPrim p) mt

> trunkToPrim :: P2 -> P2 -> Int -> Double -> [TreePrim]
> trunkToPrim n p a g = [Trunk n p (girth a g) (girth (a-1) g)]

> whorlToPrim :: AWhorl2 -> [TreePrim]
> whorlToPrim (Whorl p _ bs) = concatMap (branchToPrim p) bs

> branchToPrim :: P2 -> ABranch2 -> [TreePrim]
> branchToPrim n (Branch p a _ g Nothing)   = [Tip n p]
> branchToPrim n (Branch p a _ g (Just bs)) = Stem n p (girth a g) : concatMap (branchToPrim p) bs

**Drawing the Primitives**

> draw :: [TreePrim] -> Diagram B R2
> draw = mconcat . map drawPrim

> drawPrim :: TreePrim -> Diagram B R2
> drawPrim (Trunk p0 p1 g0 g1) = drawTrunk p0 p1 g0 g1
> drawPrim (Stem p0 p1 g)      = drawStem p0 p1 g
> drawPrim (Tip p0 p1)         = drawTip p0 p1

Draw a section of trunk (implicitly vertical) as a trapezoid with the
correct girths at top and bottom.

> drawTrunk :: P2 -> P2 -> Double -> Double -> Diagram B R2
> drawTrunk p0 p1 g0 g1 = trunk
>     where trunk = (closeLine . lineFromVertices . map p2) [
>                         ( g0/2,  y0)
>                       , ( g1/2,  y0 + y1)
>                       , (-g1/2,  y0 + y1)
>                       , (-g0/2,  y0)
>                       ] 
>                   # strokeLoop # fc black # lw 0.01 # centerX
>           y0 = (snd . unp2) p0
>           y1 = (snd . unp2) p1

> drawStem :: P2 -> P2 -> Double -> Diagram B R2 
> drawStem p0 p1 g = position [(p0, fromOffsets [ p1 .-. p0 ] # lw g)]

> drawTip :: P2 -> P2 -> Diagram B R2 
> drawTip p0 p1 = drawStem p0 p1 0.01

Produce a width based on age and girth characteristic.

> girth :: Int -> Double -> Double
> girth a g = fromIntegral (a+1) * g * 0.01

**Rendering a Tree from Parameters**

> renderTree :: TreeParams -> Diagram B R2
> renderTree = draw . toPrim . mkAboveGround . projectXZ . toAbsolute . tree

