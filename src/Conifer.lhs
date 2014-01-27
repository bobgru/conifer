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
parameters in `TreeParams`. The age of the tree is roughly the number of recursive
steps in its growth—each year corresponds to another level of branching. As we are
modeling a conifer, its structure is a main trunk that adds some number of whorls
of branches each year and another length of trunk, meanwhile adding another level
of branching to existing branches.

> data TreeParams = TreeParams {
>       tpAge                         :: Double
>     , tpTrunkLengthIncrementPerYear :: Double
>     , tpTrunkBranchLengthRatio      :: Double
>     , tpTrunkBranchAngles           :: [Double]
>     , tpTrunkGirth                  :: Double
>     , tpWhorlsPerYear               :: Int
>     , tpWhorlSize                   :: Int
>     , tpWhorlPhase                  :: Double
>     , tpBranchGirth                 :: Double
>     , tpBranchBranchLengthRatio     :: Double
>     , tpBranchBranchLengthRatio2    :: Double
>     , tpBranchBranchAngle           :: Rad
>     } deriving (Show, Eq)

> instance Default TreeParams where
>     def = TreeParams {
>       tpAge                         = 5
>     , tpTrunkLengthIncrementPerYear = 0.9
>     , tpTrunkBranchLengthRatio      = 0.7
>     , tpTrunkBranchAngles           = [tau / 6]
>     , tpTrunkGirth                  = 1.0
>     , tpWhorlsPerYear               = 1
>     , tpWhorlSize                   = 6
>     , tpWhorlPhase                  = 0
>     , tpBranchGirth                 = 1.0
>     , tpBranchBranchLengthRatio     = 0.8
>     , tpBranchBranchLengthRatio2    = 0.8
>     , tpBranchBranchAngle           = tau / 6
>     }

**The Tree Data Structure**

A tree is parameterized on payload type.

A tree rises from its origin to a node where there is
another tree, and a whorl of branches. Having age as a continuous
variable allows recursing per year with a remainder of extra growth.
A branch shoots out from its origin to a node, where it branches
into some number, possibly zero, of other branches.

Trunks differ from branches in the composition of their subnodes. A trunk
contains another trunk and a whorl of branches. A branch contains the next
level of branches.

A leaf represents the end of a trunk or branch, containing only its location.

> data Tree a = Leaf a | Node a [Tree a]
>     deriving (Show, Eq)

The payload of a leaf or node is a `TreeInfo` containing the polymorphic
location of the node, and its range of girth (i.e. of the section
of trunk or branch ending at the node).

> type TreeInfo a = (a, Double, Double)

We can specialize the types for the three phases of tree development.
The tree grows as type `RTree3` (3D tree with relative coordinates), 
is converted to `ATree3` (3D tree with absolute coordinates), and
is projected to `ATree2` (2D tree with absolute coordinates) before 
reduction to tree-primitives. The primitives do not retain the tree
structure.

> type RTree3 = Tree (TreeInfo R3)
> type ATree3 = Tree (TreeInfo P3)
> type ATree2 = Tree (TreeInfo P2)

It will be convenient to be able to apply a function throughout the tree while
preserving its structure.

> instance Functor Tree where
>     fmap = treeMap

> treeMap :: (a -> b) -> Tree a -> Tree b
> treeMap f (Leaf a)    = Leaf (f a)
> treeMap f (Node a ns) = Node (f a) (map (treeMap f) ns)

Transforming a tree from relative to absolute coordinates is not an associative operation.
It is a fold of the node value through the tree from root to leaves.

> treeFold :: (b -> a -> b) -> b -> Tree a -> Tree b
> treeFold f a0 (Leaf a)    = Leaf (f a0 a)
> treeFold f a0 (Node a ns) = Node (f a0 a) (map (treeFold f (f a0 a)) ns)

Rendering the tree to drawing primitives likewise isn't associative because it, too, involves
folding a node value through the tree. In this case the result is a flat list, not a tree.

> flattenTree :: (a -> a -> [b]) -> a -> Tree a -> [b]
> flattenTree f a0 (Leaf a)    = f a0 a
> flattenTree f a0 (Node a ns) = f a0 a ++ concatMap (flattenTree f a) ns

The tree parts are reduced to diagram primitives which can be folded into a single 
diagram for rendering as an image.
* `Trunk` is a section of trunk or branch between points _p0_ and _p1_, 
   with girth _g0_ at _p0_ and _g1_ at _p1_.
* `Tip` is the tip of a tree or branch, between points _p0_ and _p1_.

> data TreePrim = Trunk { p0::P2, p1::P2, g0::Double, g1::Double }
>               | Tip   { p0::P2, p1::P2 }

**Growing the Tree**

We first build a tree with each node in its own coordinate space relative to its 
parent node.

> tree :: TreeParams -> RTree3
> tree tp = if age < ageIncr
>               then Leaf (node, -1, -1)
>               else Node (node, girth0, girth1) nodes
>     where age     = tpAge tp
>           g       = tpTrunkGirth tp
>           girth0  = girth age g
>           girth1  = girth (age - ageIncr) g
>           node    = r3 (0, 0, tpTrunkLengthIncrementPerYear tp * ageIncr)
>           nodes   = tree tpNext : whorl tpNext
>           tpNext  = (adjustAge (-ageIncr) . advancePhase . advanceTrunkBranchAngle) tp
>           ageIncr = 1 / fromIntegral (tpWhorlsPerYear tp)

A whorl is some number of branches, evenly spaced but at varying angle
from the vertical. A whorl is rotated by the whorl phase, which changes
from one to the next.

> whorl :: TreeParams -> [RTree3]
> whorl tp = [ branch tp (pt i) | i <- [0 .. numBranches - 1] ]
>     where pt i        = r3 ( tblr * cos (rotation i)
>                            , tblr * sin (rotation i)
>                            , tblr * cos (tba i))
>           tblr        = tpTrunkBranchLengthRatio tp
>           phase       = tpWhorlPhase tp
>           numBranches = tpWhorlSize tp
>           tbas        = tpTrunkBranchAngles tp
>           n           = length tbas
>           rotation i  = fromIntegral i * tau / fromIntegral numBranches + phase
>           tba i       = tbas !! (i `mod` n)

A branch shoots forward a certain length, then ends or splits into three branches,
going left, center, or right.

> branch :: TreeParams -> R3 -> RTree3
> branch tp node = if age < 1
>                      then Leaf (leafNode, -1, -1)
>                      else Node (node, girth0, girth0) nodes
>     where age    = tpAge tp
>           g      = tpBranchGirth tp
>           girth0 = girth (age - 1) g

If the branch is less than a year old, it's a shoot with a leaf. The length
is scaled down by its age.

>           leafNode = node # scale (age * tpBranchBranchLengthRatio tp)

Next year's subbranches continue straight, to the left and to the right. The straight
subbranch grows at a possibly different rate from the side subbranches. Scale the
branches to their full length. If they are leaves, they will be scaled back, as above.

>           nodes  = map (branch tp') [l, c, r]
>           tp'    = subYear tp
>           l      = node # rotateXY   bba  # scale bblr2
>           c      = node                   # scale bblr
>           r      = node # rotateXY (-bba) # scale bblr2
>           bba    = tpBranchBranchAngle tp
>           bblr   = tpBranchBranchLengthRatio tp
>           bblr2  = tpBranchBranchLengthRatio2 tp

Helper functions for building the tree:

> rotateXY :: Rad -> R3 -> R3
> rotateXY a v = r3 (x', y', z)
>     where (x, y, z) = unr3 v
>           (x', y')  = unr2 (rotate a (r2 (x, y)))

> subYear :: TreeParams -> TreeParams
> subYear tp = adjustAge (-1) tp

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

Produce a width based on age and girth characteristic. Don't let the
width go below the minimum.

> girth :: Double -> Double -> Double
> girth a g = 0.01 * max (a * g) 1

**Converting from Relative to Absolute Coordinates**

Convert the tree of relative coordinate spaces into a single coherent absolute
coordinate space, which will make projection onto the _x_-_z_-plane trivial.

> toAbsolute :: RTree3 -> ATree3
> toAbsolute = treeFold infoR3ToP3 (origin, 0, 0)

> infoR3ToP3 :: TreeInfo P3 -> TreeInfo R3 -> TreeInfo P3
> infoR3ToP3 (p, _, _) (v, g0, g1) = (p .+^ v, g0, g1)

**Projecting the Tree onto 2D**

We are rendering the tree from the side, so we simply discard the _y_-coordinate.

> projectXZ :: ATree3 -> ATree2
> projectXZ = fmap infoXZ

> infoXZ :: TreeInfo P3 -> TreeInfo P2
> infoXZ (n, g0, g1) = (xz n, g0, g1)

> xz :: P3 -> P2
> xz p = p2 (x, z) where (x, _, z) = unp3 p

**Respecting the Earth**

Assuming the tree is growing on flat ground, we can't have the branches digging into it.

> mkAboveGround :: ATree2 -> ATree2
> mkAboveGround = fmap infoAboveGround

> infoAboveGround :: TreeInfo P2 -> TreeInfo P2
> infoAboveGround (n, g0, g1) = (aboveGround n, g0, g1)

> aboveGround :: P2 -> P2
> aboveGround p = p2 (x, max 0 z) where (x, z) = unp2 p

**Reducing the Tree to Primitives from Absolute Coordinates**

> toPrim :: ATree2 -> [TreePrim]
> toPrim = flattenTree treeToPrim (origin, 0, 0)

Our flattening function needs the information from a node,
but not the tree structure.

> treeToPrim :: TreeInfo P2 -> TreeInfo P2 -> [TreePrim]
> treeToPrim (n0, _, _) (n, g0, g1) = [if g0 < 0 then tip else node]
>     where tip  = Tip n0 n
>           node = Trunk n0 n g0 g1

**Drawing the Primitives**

> draw :: [TreePrim] -> Diagram B R2
> draw = mconcat . map drawPrim

> drawPrim :: TreePrim -> Diagram B R2
> drawPrim (Trunk p0 p1 g0 g1) = drawTrunk p0 p1 g0 g1
> drawPrim (Tip p0 p1)         = drawTip p0 p1

Draw a section of trunk or branch as a trapezoid with the
correct girths at each end.

> drawTrunk :: P2 -> P2 -> Double -> Double -> Diagram B R2
> drawTrunk p0 p1 g0 g1 = place trunk p0
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

> drawTip :: P2 -> P2 -> Diagram B R2 
> drawTip p0 p1 = position [(p0, fromOffsets [ p1 .-. p0 ] # lw 0.01)]

**Rendering a Tree from Parameters**

> renderTree :: TreeParams -> Diagram B R2
> renderTree = draw . toPrim . mkAboveGround . projectXZ . toAbsolute . tree
