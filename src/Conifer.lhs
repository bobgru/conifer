A Virtual Conifer
=================

**Introduction**

This project is to model a conifer as the expression of a set of "genes",
i.e. parameters that control lengths and angles and such, and to draw
a picture of it.

We'll start with a generic data structure and combinators to apply
functions to tree components in various ways. We'll also establish a
pipeline to produce a drawing of a tree.

The final part is the specification and growth of the kind of tree we want,
a conifer.

> {-# LANGUAGE NoMonomorphismRestriction #-}
> module Conifer ( Tree
>                , renderTree
>                , TreeParams(..)
>                , TreeInfo, RTree3
>                , tree
>                )
> where 

> import Diagrams.Prelude hiding (angleBetween, rotationAbout, direction)
> import Diagrams.Backend.SVG
> import Diagrams.Coordinates
> import Diagrams.ThreeD.Types
> import Diagrams.ThreeD.Transform
> import Diagrams.ThreeD.Vector
> import Data.Default.Class
> import Data.Cross

**The Tree Data Structure**

A `Tree` is a straightforward tree represented by `Leaf` and `Node` constructors,
with a polymorphic payload in each.

> data Tree a = Leaf a | Node a [Tree a]
>     deriving (Show, Eq)

The payload of a leaf or node is a `TreeInfo` parameterized on location
type, and containing the location, the girth at its origin (the location of
which is implicit), the girth at its location, and its age.

> type TreeInfo a = (a, Double, Double, Double)

We specialize the types for the three phases of tree development.
The tree grows as type `RTree3` (3D tree with relative coordinates), 
is converted to `ATree3` (3D tree with absolute coordinates), and
is projected to `ATree2` (2D tree with absolute coordinates) before 
being flattened to a list of primitive drawing elements.

> type RTree3 = Tree (TreeInfo R3)
> type ATree3 = Tree (TreeInfo P3)
> type ATree2 = Tree (TreeInfo P2)

The tree is ultimately converted to context-free drawing instructions
which when carried out produce diagrams.
* `Trunk` is a section of trunk or branch between points _p0_ and _p1_, 
   with girth _g0_ at _p0_ and _g1_ at _p1_.
* `Tip` is the tip of a tree or branch, between points _p0_ and _p1_.

> data TreePrim = Trunk { p0::P2, p1::P2, g0::Double, g1::Double, age::Double }
>               | Tip   { p0::P2, p1::P2, age::Double }

**Tree Combinators**

We need several ways of applying a function throughout a tree, sometimes preserving its
structure, sometimes not.

The `treeMap` function is a functor that applies a function uniformly throughout the tree.

> instance Functor Tree where fmap = treeMap
> treeMap :: (a -> b) -> Tree a -> Tree b
> treeMap f (Leaf a)    = Leaf (f a)
> treeMap f (Node a ns) = Node (f a) (map (treeMap f) ns)

The `treeFold` function preserves tree structure, but iterates a function over all
nodes from root to leaves.

> treeFold :: (b -> a -> b) -> b -> Tree a -> Tree b
> treeFold f a0 (Leaf a)    = Leaf (f a0 a)
> treeFold f a0 (Node a ns) = Node (f a0 a) (map (treeFold f (f a0 a)) ns)

The `flattenTree` function is similar to `treeFold`, but flattens a tree into a list
in the process.

> flattenTree :: (a -> a -> [b]) -> a -> Tree a -> [b]
> flattenTree f a0 (Leaf a)    = f a0 a
> flattenTree f a0 (Node a ns) = f a0 a ++ concatMap (flattenTree f a) ns

**Creating a Drawing of a Tree**

Given a tree, grown by any policy, convert it to a diagram. The pipeline
of functions converts the original `RTree3`—most convenient during construction—to `ATree3`
then to `ATree2`, which are convenient for further processing.

> renderTree :: RTree3 -> Diagram B R2
> renderTree = draw . toPrim . projectXZ . mkAboveGround . toAbsolute

**Converting from Relative to Absolute Coordinates**

Convert the tree of relative coordinate spaces into a single coherent absolute
coordinate space, which will make projection onto the _x_-_z_-plane trivial.

> toAbsolute :: RTree3 -> ATree3
> toAbsolute = treeFold infoR3ToP3 (origin, 0, 0, 0)

> infoR3ToP3 :: TreeInfo P3 -> TreeInfo R3 -> TreeInfo P3
> infoR3ToP3 (p, _, _, _) (v, g0, g1, a) = (p .+^ v, g0, g1, a)

**Respecting the Earth**

Assuming the tree is growing on flat ground, we can't have the branches
digging into it.

> mkAboveGround :: ATree3 -> ATree3
> mkAboveGround = fmap infoAboveGround

> infoAboveGround :: TreeInfo P3 -> TreeInfo P3
> infoAboveGround (n, g0, g1, a) = (aboveGround n, g0, g1, a)

> aboveGround :: P3 -> P3
> aboveGround p = p3 (x, y, max 0 z) where (x, y, z) = unp3 p

**Projecting the Tree onto 2D**

We are rendering the tree from the side, so we simply discard the _y_-coordinate.
We could project onto another plane and the rest of the pipeline would work.

> projectXZ :: ATree3 -> ATree2
> projectXZ = fmap infoXZ

> infoXZ :: TreeInfo P3 -> TreeInfo P2
> infoXZ (n, g0, g1, a) = (xz n, g0, g1, a)

> xz :: P3 -> P2
> xz p = p2 (x, z) where (x, _, z) = unp3 p

**Reducing the Tree to Primitives from Absolute Coordinates**

Given a tree projected onto a plane, convert it to a list
of drawing instructions.

> toPrim :: ATree2 -> [TreePrim]
> toPrim = flattenTree treeToPrim (origin, 0, 0, 0)

> treeToPrim :: TreeInfo P2 -> TreeInfo P2 -> [TreePrim]
> treeToPrim (n0, _, _, _) (n, g0, g1, a) = [if g0 < 0 then tip else node]
>     where tip  = Tip n0 n a
>           node = Trunk n0 n g0 g1 a

**Drawing the Primitives**

Execute the drawing instructions as applications of functions from
the diagrams package, producing a diagram as output.

> draw :: [TreePrim] -> Diagram B R2
> draw = mconcat . map drawPrim

> drawPrim :: TreePrim -> Diagram B R2
> drawPrim (Trunk p0 p1 g0 g1 a) = drawTrunk p0 p1 g0 g1 a
> drawPrim (Tip p0 p1 a)         = drawTip p0 p1 a

Draw a section of trunk or branch as a trapezoid with the
correct girths at each end.

> drawTrunk :: P2 -> P2 -> Double -> Double -> Double -> Diagram B R2
> drawTrunk p0 p1 g0 g1 age = place trunk p0
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

> drawTip :: P2 -> P2 -> Double -> Diagram B R2 
> drawTip p0 p1 age = position [(p0, fromOffsets [ p1 .-. p0 ] # lw 0.01)]

**Specifying a Conifer**

Our ideal tree will be completely determined by its "genes", the various
parameters in `TreeParams`. The age of the tree is roughly the number of recursive
steps in its growth—each year corresponds to another level of branching. As we are
modeling a conifer, its structure is a main trunk that adds some number of whorls
of branches each year and another length of trunk, meanwhile adding another level
of branching to existing branches.

One major concession to arbitrary aesthetics is the list of trunk branch angles,
which led to a fuller and less regular look, important for the original application
of this code. A more realistic approach would be to model random deviations from
the regular growth.

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

**Growing a Conifer**

Our tree rises from its origin to a node where there is another tree, and
a whorl of branches.  A branch shoots out from its origin to a node, where
it branches into some number, possibly zero, of other branches.

Trunks differ from branches in the composition of their subnodes. A trunk
contains another trunk and a whorl of branches. A branch contains the next
level of branches.

A leaf represents the end of a trunk or branch, containing only its location.

Age is represented as a continuous variable which allows recursing once per year,
increasing the level of branching, with a remainder of extra growth, leading to
the pyramidal structure of a real conifer.

We build a tree with each node in its own coordinate space relative to its 
parent node, which is the natural way to use the diagrams package.

> tree :: TreeParams -> RTree3
> tree tp = if age < ageIncr
>               then Leaf (node, -1, -1, 0)
>               else Node (node, girth0, girth1, age) nodes
>     where age     = tpAge tp
>           g       = tpTrunkGirth tp
>           girth0  = girth age g
>           girth1  = girth (age - ageIncr) g
>           node    = r3 (0, 0, tpTrunkLengthIncrementPerYear tp * ageIncr)
>           nodes   = tree tpNext : whorl tpNext
>           tpNext  = (adjustAge (-ageIncr) . advancePhase . advanceTrunkBranchAngle) tp
>           ageIncr = 1 / fromIntegral (tpWhorlsPerYear tp)

A whorl is some number of branches, evenly spaced but at varying angle
from the vertical (an acknowledged hack to "shake up" the otherwise rigidly
regular tree a little). A whorl is rotated by the whorl phase, which changes
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
going left, center, and right.

> branch :: TreeParams -> R3 -> RTree3
> branch tp node = if age < 1
>                      then Leaf (leafNode, -1, -1, 0)
>                      else Node (node, girth0, girth0, age) nodes
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

>           l      = node # transform (t2   bba)  # scale bblr2
>           r      = node # transform (t2 (-bba)) # scale bblr2
>           c      = node                         # scale bblr

>           t2 a   = conjugate t1 (rotationAbout origin zAxis a)
>           t1     = rotationAbout origin nAxis angle
>           zAxis  = (asSpherical . direction) unitZ
>           nAxis  = (asSpherical . direction) (cross3 unitZ node)
>           angle  = 1/4 - ((asTurn . angleBetween unitZ) node)

>           bba    = tpBranchBranchAngle tp
>           bblr   = tpBranchBranchLengthRatio tp
>           bblr2  = tpBranchBranchLengthRatio2 tp

Helper functions for building the tree:

> subYear :: TreeParams -> TreeParams
> subYear = adjustAge (-1)

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
