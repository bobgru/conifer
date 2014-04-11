> {-# LANGUAGE NoMonomorphismRestriction #-}
> module Conifer.Types where

> -- import Control.Monad.Reader
> -- import Data.Cross
> import Data.Default.Class
> -- import Diagrams.Backend.SVG
> import Diagrams.Coordinates
> import Diagrams.Prelude -- hiding (angleBetween, rotationAbout, direction)
> -- import Diagrams.ThreeD.Transform
> import Diagrams.ThreeD.Types
> import Diagrams.ThreeD.Vector
> -- import Diagrams.TwoD.Transform.ScaleInv
> -- import Diagrams.TwoD.Vector (angleBetween)

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
* `Needles` indicates decoration with needles between points _p0_ and _p1_.

> data TreePrim = Trunk   { p0::P2, p1::P2, g0::Double, g1::Double, age::Double }
>               | Tip     { p0::P2, p1::P2, age::Double }
>               | Needles { p0::P2, p1::P2 }

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
>       tpTrunkLengthIncrementPerYear :: Double
>     , tpTrunkBranchLengthRatio      :: Double
>     , tpTrunkBranchAngles           :: [Double]
>     , tpTrunkGirth                  :: Double
>     , tpWhorlsPerYear               :: Int
>     , tpWhorlSize                   :: Int
>     , tpBranchGirth                 :: Double
>     , tpBranchBranchLengthRatio     :: Double
>     , tpBranchBranchLengthRatio2    :: Double
>     , tpBranchBranchAngle           :: Rad
>     } deriving (Show, Eq)

> instance Default TreeParams where
>     def = TreeParams {
>       tpTrunkLengthIncrementPerYear = 0.9
>     , tpTrunkBranchLengthRatio      = 0.7
>     , tpTrunkBranchAngles           = [tau / 6]
>     , tpTrunkGirth                  = 1.0
>     , tpWhorlsPerYear               = 1
>     , tpWhorlSize                   = 6
>     , tpBranchGirth                 = 1.0
>     , tpBranchBranchLengthRatio     = 0.8
>     , tpBranchBranchLengthRatio2    = 0.8
>     , tpBranchBranchAngle           = tau / 6
>     }

The mutable state during a tree's growth consists of its age, the rotational phase of the next
whorl, and the next trunk branch angle to use.

> data AgeParams = AgeParams {
>       apAge                         :: Double
>     , apTrunkBranchAngleIndex       :: Int
>     , apWhorlPhase                  :: Double
>     } deriving (Show, Eq)

The tree can be optionally decorated with needles, in which case the
needles can be customized in various ways.

> data NeedleParams = NeedleParams {
>       needleLength :: Double
>     , needleAngle  :: Rad
>     , needleIncr   :: Double
>     }

> instance Default NeedleParams where
>     def = NeedleParams {
>       needleLength = 0.05
>     , needleAngle  = tau / 10
>     , needleIncr   = 0.05
>     }

