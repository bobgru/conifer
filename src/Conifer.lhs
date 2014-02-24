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

> {-# LANGUAGE NoMonomorphismRestriction, GeneralizedNewtypeDeriving #-}
> module Conifer ( Tree
>                , renderTree, renderTreeWithNeedles
>                , TreeParams(..), AgeParams(..), NeedleParams(..)
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
> import Diagrams.TwoD.Transform.ScaleInv
> import Diagrams.TwoD.Vector (angleBetween)
> import Data.Default.Class
> import Data.Cross
> import Control.Monad.Reader
> import Control.Monad.State
> import Control.Monad.Identity
> import System.Random

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

**Creating a Drawing of a Tree**

Given a tree, grown by any policy, convert it to a diagram. The pipeline
of functions converts the original `RTree3`—most convenient during construction—to `ATree3`
then to `ATree2`, then to `[TreePrim]` which are convenient for further processing. The option
of drawing with or without needles is exposed as different pipelines.

> renderTree :: RTree3 -> Diagram B R2
> renderTree = draw . renderTreeToPrim

> renderTreeWithNeedles :: (Double -> Bool) -> NeedleParams -> RTree3 -> Diagram B R2
> renderTreeWithNeedles f np = draw' np . withNeedles f . renderTreeToPrim

> renderTreeToPrim :: RTree3 -> [TreePrim]
> renderTreeToPrim = toPrim . projectXZ . mkAboveGround . toAbsolute

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

**Decorating with Needles**

After the tree has been converted to primitives, scan them for where to decorate
with needles, according to the predicate supplied by the consumer. Append the
needle-drawing instructions as new primitives.

> withNeedles :: (Double -> Bool) -> [TreePrim] -> [TreePrim]
> withNeedles f ps = ps ++ (map toNeedles . filter (shouldAddNeedles f)) ps

> shouldAddNeedles :: (Double -> Bool) -> TreePrim -> Bool
> shouldAddNeedles f (Trunk _ _ _ _ a) = f a
> shouldAddNeedles f (Tip   _ _     a) = f a
> shouldAddNeedles _ _                 = False

> toNeedles :: TreePrim -> TreePrim
> toNeedles (Trunk p0 p1 _ _ _) = Needles p0 p1
> toNeedles (Tip   p0 p1     _) = Needles p0 p1
> toNeedles p                   = p

**Drawing the Primitives**

Execute the drawing instructions as applications of functions from
the diagrams package, producing a diagram as output.

> draw :: [TreePrim] -> Diagram B R2
> draw = draw' def

> draw' :: NeedleParams -> [TreePrim] -> Diagram B R2
> draw' np = mconcat . map (drawPrim np)

> drawPrim :: NeedleParams -> TreePrim -> Diagram B R2
> drawPrim _  (Trunk p0 p1 g0 g1 _) = drawTrunk   p0 p1 g0 g1
> drawPrim _  (Tip p0 p1 _)         = drawTip     p0 p1
> drawPrim np (Needles p0 p1)       = drawNeedles p0 p1 np

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

> drawNeedles :: P2 -> P2 -> NeedleParams -> Diagram B R2
> drawNeedles p0 p1 np = place (scaleInv ns Diagrams.Prelude.unitX # _scaleInvObj) p0
>     where ns = needles numNeedles (magnitude v) nLength nAngle # rotate (-th)
>           th = Diagrams.TwoD.Vector.angleBetween v Diagrams.Prelude.unitX :: Rad
>           v  = p1 .-. p0
>           numNeedles   = floor (magnitude v / nIncr) :: Int
>           nLength = needleLength np
>           nAngle  = needleAngle np
>           nIncr   = needleIncr np

> needle :: Double -> Rad -> Diagram B R2
> needle l th =  d # rotate th <> d # rotate (-th)
>     where d = fromOffsets [Diagrams.Prelude.unitX ^* l]

> needles :: Int -> Double -> Double -> Rad -> Diagram B R2
> needles n l nl na = position (zip ps (repeat (needle nl na)))
>     where ps = map p2 [(x,0)|x <- [0, dx .. l - dx]]
>           dx = l / fromIntegral (n + 1)

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

**TODO** Putting the age in mutable state as opposed to passing as a function argument was
the worst decision. We now have a bunch of ugly imperative code that saves and restores
state around recursive calls that were formerly clean and simple. We'll keep the mutable
state for the random number generators, which never need to be restored, and change age, etc.,
back to arguments.

> data AgeParams = AgeParams {
>       apAge                         :: Double
>     , apTrunkBranchAngleIndex       :: Int
>     , apWhorlPhase                  :: Double
>     , apSeed                        :: Int
>     , apRndGen                      :: StdGen
>     } deriving (Show, Read)

> instance Default AgeParams where
>     def = AgeParams {
>       apAge = 1
>     , apTrunkBranchAngleIndex = 0
>     , apWhorlPhase = 0
>     , apSeed = s
>     , apRndGen = mkStdGen s
>     }
>         where s = 0

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

We'll give ourselves a new type for the monad stack to come.

> newtype TreeBuilder a = TB { runTB :: ReaderT TreeParams (StateT AgeParams Identity) a }
>     deriving (Monad, MonadReader TreeParams, MonadState AgeParams)

> runTreeBuilder :: TreeBuilder a -> TreeParams -> AgeParams -> (a, AgeParams)
> runTreeBuilder tb tp ap =
>     let rndGen = mkStdGen (apSeed ap)
>         state  = ap { apRndGen = rndGen }
>     in runIdentity (runStateT (runReaderT (runTB tb) tp) state)

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

> tree :: TreeParams -> AgeParams -> (RTree3, AgeParams)
> tree tp ap = runTreeBuilder tree' tp ap

> tree' :: TreeBuilder RTree3
> tree' = do
>     ap <- get
>     let age = apAge ap
>     nb <- whorlsPerYear
>     li <- tli 
>     let ageIncr = 1 / fromIntegral nb
>     let node    = r3 (0, 0, li * ageIncr)
>     if age < ageIncr
>         then return (Leaf (node, -1, -1, 0))
>         else do
>              g <- trunkGirth
>              let girth0 = girth age g
>              let girth1 = girth (age - ageIncr) g

Adjust some parameters for the next level of trunk. We want to continue with the same
values for the whorls, but let other state be modified. For example, the random number
generator should not be reset.

>              tp <- ask
>              let apNext = (adjustAge (-ageIncr) . advancePhase tp . advanceTrunkBranchAngle tp) ap
>              let age'   = apAge apNext
>              let index' = apTrunkBranchAngleIndex apNext
>              let phase' = apWhorlPhase apNext
>              put apNext
>              t  <- tree'

Restore the parts of the state that pertain to the current context.

>              let apNext' = apNext {
>                                     apAge = age'
>                                   , apTrunkBranchAngleIndex = index'
>                                   , apWhorlPhase = phase' }
>              put apNext'
>              ws <- whorl
>              return (Node (node, girth0, girth1, age) (t : ws))

A whorl is some number of branches, evenly spaced but at varying angle
from the vertical (an acknowledged hack to "shake up" the otherwise rigidly
regular tree a little). A whorl is rotated by the whorl phase, which changes
from one to the next.

> whorl :: TreeBuilder [RTree3]
> whorl = do
>     ap <- get
>     let tbai  = apTrunkBranchAngleIndex ap
>     let phase = apWhorlPhase ap

>     lr <- tblr
>     nb <- whorlSize
>     as <- tbas

>     let pt i = r3 (cos (theta i), sin (theta i), cos (phi i)) ^* lr
>          where theta i = fromIntegral i * tau / fromIntegral nb + phase
>                phi i   = as !! ((i + tbai) `mod` (length as))

>     age <- getAge
>     t <- mapM (\i -> do putAge age >> branch (pt i)) [0 .. nb - 1]
>     putAge age
>     return t

A branch shoots forward a certain length, then ends or splits into three branches,
going left, center, and right. If the branch is less than a year old, it's a shoot
with a leaf. The length is scaled down by its age.

> branch :: R3 -> TreeBuilder RTree3
> branch node = do
>     ap <- get
>     let age = apAge ap
>     if age < 1
>         then do
>             lr <- bblr
>             let leafNode = node # scale (age * lr)
>             return (Leaf (leafNode, -1, -1, 0))
>         else do

Set the age for the next branch level. Recursive calls to branch change the age
to 0, so restore it afterwards.

>             let age' = age - 1
>             tp      <- ask
>             let aux v = do putAge age' >> branch v
>             nodes   <- mapM aux (newBranchNodes tp node)

Restore age so the next branch in a whorl grows properly. This is the worst of 
imperative programming! **TODO** Move age out of mutable state and back to an argument

>             putAge age'
>             g       <- branchGirth
>             let g0  = girth (age - 1) g
>             return (Node (node, g0, g0, age) nodes)

Next year's subbranches continue straight, to the left and to the right. The straight
subbranch grows at a possibly different rate from the side subbranches. Scale the
branches to their full length. If they are leaves, they will be scaled back.

> newBranchNodes :: TreeParams -> R3 -> [R3]
> newBranchNodes tp node = [l, c, r]
>     where bba    = tpBranchBranchAngle tp
>           bblr   = tpBranchBranchLengthRatio tp
>           bblr2  = tpBranchBranchLengthRatio2 tp

>           angle  = 1/4 - (asTurn . Diagrams.ThreeD.Vector.angleBetween unitZ) node
>           zAxis  = (asSpherical . direction) unitZ
>           nAxis  = (asSpherical . direction) (cross3 unitZ node)
>           t1     = rotationAbout origin nAxis angle
>           t2 a   = conjugate t1 (rotationAbout origin zAxis a)

>           l      = node # transform (t2   bba)  # scale bblr2
>           r      = node # transform (t2 (-bba)) # scale bblr2
>           c      = node                         # scale bblr

Helper functions for building the tree:

> subYear :: AgeParams -> AgeParams
> subYear = adjustAge (-1)

> adjustAge :: Double -> AgeParams -> AgeParams
> adjustAge da ap = ap { apAge = a' }
>     where a' = apAge ap + da

> advanceTrunkBranchAngle :: TreeParams -> AgeParams -> AgeParams
> advanceTrunkBranchAngle tp ap = ap { apTrunkBranchAngleIndex = i' }
>     where ws  = fromIntegral (tpWhorlSize tp)
>           i'  = (apTrunkBranchAngleIndex ap + 1) `mod` ws

> advancePhase :: TreeParams -> AgeParams -> AgeParams
> advancePhase tp ap = ap { apWhorlPhase = p' }
>     where ws  = fromIntegral (tpWhorlSize tp)
>           wpy = fromIntegral (tpWhorlsPerYear tp)
>           p'  = apWhorlPhase ap + tau / (ws * wpy * 2)

Helpers to pull specific information from the immutable configuration:

> fetch :: (TreeParams -> a) -> TreeBuilder a
> fetch f = do tp <- ask; return (f tp)

> tli           = fetch tpTrunkLengthIncrementPerYear
> tblr          = fetch tpTrunkBranchLengthRatio
> tbas          = fetch tpTrunkBranchAngles
> trunkGirth    = fetch tpTrunkGirth
> whorlsPerYear = fetch tpWhorlsPerYear
> whorlSize     = fetch tpWhorlSize
> branchGirth   = fetch tpBranchGirth
> bblr          = fetch tpBranchBranchLengthRatio
> bblr2         = fetch tpBranchBranchLengthRatio2
> bba           = fetch tpBranchBranchAngle

Helpers to get or set values from mutable state:

> getter :: (AgeParams -> a) -> TreeBuilder a
> getter f = do ap <- get; return (f ap)

> getAge   = getter apAge
> putAge v = do ap <- get; put (ap {apAge = v}); return ()

Produce a width based on age and girth characteristic. Don't let the
width go below the minimum.

> girth :: Double -> Double -> Double
> girth a g = 0.01 * max (a * g) 1
