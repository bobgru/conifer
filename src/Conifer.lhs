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
>       tpAge                      :: Double
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

> data Tree a =
>     Leaf { lNode :: a }
>
>   | Tree {
>       tNode     :: a
>     , tAge      :: Double
>     , tGirth    :: Double
>     , tNext     :: Tree a
>     , tBranches :: [Tree a]
>     }

A branch shoots out from its origin to its `bNode`, where it branches
into some number, possibly zero, of other branches.

>   | Branch {
>       bNode       :: a
>     , bAge        :: Double
>     , bGirth      :: Double
>     , bBranches   :: [Tree a]
>     }
>   deriving (Show, Eq)

We can specialize the types for the three phases of tree development.
The tree grows as type `RTree3` (3D tree with relative coordinates), 
is converted to `ATree3` (3D tree with absolute coordinates), and
is projected to `ATree2` (2D tree with absolute coordinates) before 
reduction to tree-primitives. The primitives do not retain the tree
structure.

> type RTree3 = Tree P3
> type ATree3 = Tree P3
> type ATree2 = Tree P2

It will be convenient to be able to apply a function throughout the tree while
preserving its structure.

> instance Functor Tree where
>     fmap = treeMap

> treeMap :: (a -> b) -> Tree a -> Tree b
> treeMap f (Leaf p)          = Leaf (f p)
> treeMap f (Tree p a g t bs) = Tree (f p) a g (treeMap f t) (map (treeMap f) bs)
> treeMap f (Branch p a g bs) = Branch (f p) a g (map (treeMap f) bs)

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
> tree tp = if age < ageIncr 
>               then Leaf trunkTip
>               else Tree trunkTip age girth newTree branches
>     where age         = tpAge tp
>           ageIncr     = 1 / fromIntegral (tpWhorlsPerYear tp)
>           girth       = tpTrunkGirth tp
>           trunkIncr   = tpTrunkLengthIncrement tp * ageIncr
>           trunkTip    = p3 (0, 0, trunkIncr)
>           newTree     = tree tpNext
>           branches    = whorl tpNext
>           tpNext      = (adjustAge (-ageIncr) . advancePhase . advanceTrunkBranchAngle) tp

Some useful helper functions for manipulating `TreeParams`:

> subYear :: TreeParams -> TreeParams
> subYear      tp = tp { tpAge = tpAge tp - 1 }

> adjustAge :: Double -> TreeParams -> TreeParams
> adjustAge da tp = tp { tpAge = tpAge tp + da }

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

> whorl :: TreeParams -> [RTree3]
> whorl tp = [ branch tp (pt i) | i <- [0 .. numBranches - 1] ]
>     where pt i = p3 ( tblr * cos (rotation i)
>                     , tblr * sin (rotation i)
>                     , tblr * cos (tba i))

>           tblr        = tpTrunkBranchLengthRatio tp
>           phase       = tpWhorlPhase tp
>           numBranches = tpWhorlSize tp
>           tbas        = tpTrunkBranchAngles tp
>           n           = length tbas
>           rotation i  = fromIntegral i * tau / fromIntegral numBranches + phase
>           tba i       = tbas !! (i `mod` n)

A branch shoots forward a certain length, then ends or splits into three branches,
going left, center, or right.

> branch :: TreeParams -> P3 -> RTree3
> branch tp p = if age < 1 then Leaf leafNode else Branch p age g bs
>     where age   = tpAge tp
>           g     = tpBranchGirth tp

If the branch is less than a year old, it's a shoot with a leaf. The length
is scaled down by its age.

>           leafNode = p # scale (age * tpBranchBranchLengthRatio tp)

Next year's subbranches continue straight, to the left and to the right. The straight
subbranch grows at a possibly different rate from the side subbranches. Scale the
branches to their full length. If they are leaves, they will be scaled back, as above.

>           bs     = map (branch tp') [l, c, r]
>           tp'    = subYear tp
>           l      = p # rotateXY   bba  # scale bblr2
>           c      = p                   # scale bblr
>           r      = p # rotateXY (-bba) # scale bblr2
>           bba    = tpBranchBranchAngle tp
>           bblr   = tpBranchBranchLengthRatio tp
>           bblr2  = tpBranchBranchLengthRatio2 tp

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
> toAbsoluteTree n (Leaf p) = Leaf (toAbsoluteP3 n p)
> toAbsoluteTree n (Tree p a g t bs) = Tree p' a g t' bs'
>     where p'  = toAbsoluteP3 n p
>           t'  = toAbsoluteTree p' t
>           bs' = map (toAbsoluteTree p') bs
> toAbsoluteTree n (Branch p a g bs) = Branch p' a g bs'
>     where p'  = toAbsoluteP3 n p
>           bs' = map (toAbsoluteTree p') bs

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
> treeToPrim n (Leaf p) = [Tip n p]
> treeToPrim n (Tree p a g t bs) = trunk ++ branches ++ nextTree
>     where trunk    = trunkToPrim n p a g
>           branches = concatMap (branchToPrim p) bs
>           nextTree = treeToPrim p t

> trunkToPrim :: P2 -> P2 -> Double -> Double -> [TreePrim]
> trunkToPrim n p a g = [Trunk n p (girth a g) (girth (a-1) g)]

> branchToPrim :: P2 -> ATree2 -> [TreePrim]
> branchToPrim n (Leaf p)          = [Tip n p]
> branchToPrim n (Branch p a g bs) = Stem n p (girth a g) : concatMap (branchToPrim p) bs

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

> girth :: Double -> Double -> Double
> girth a g = (a+1) * g * 0.01

**Rendering a Tree from Parameters**

> renderTree :: TreeParams -> Diagram B R2
> renderTree = draw . toPrim . mkAboveGround . projectXZ . toAbsolute . tree

