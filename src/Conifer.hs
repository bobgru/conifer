-- A Virtual Conifer
--
-- Introduction
--
-- This project is to model a conifer as the expression of a set of "genes",
-- i.e. parameters that control lengths and angles and such, and to draw
-- a picture of it.
--
-- We'll start with a generic data structure and combinators to apply
-- functions to tree components in various ways. We'll also establish a
-- pipeline to produce a drawing of a tree.
--
-- The final part is the specification and growth of the kind of tree we want,
-- a conifer.

{-# LANGUAGE NoMonomorphismRestriction #-}
module Conifer ( Tree
               , renderTree, renderTreeWithNeedles
               , TreeParams(..), AgeParams(..), NeedleParams(..)
               , TreeInfo, Tree3
               , tree
               , aboveGround
               )
where 

import Conifer.Types
import Data.Cross
import Data.Default.Class
import Data.Tree(Tree(..), flatten, unfoldTree)
import Diagrams.Backend.SVG
import Diagrams.Coordinates
import Diagrams.Prelude hiding (angleBetween, rotationAbout, direction)
import Diagrams.ThreeD.Transform
import Diagrams.ThreeD.Types
import Diagrams.ThreeD.Vector
import Diagrams.TwoD.Transform.ScaleInv
import Diagrams.TwoD.Vector (angleBetween)

-- Creating a Drawing of a Tree
--
-- Given a tree, grown by any policy, convert it to a diagram. The pipeline
-- of functions converts the original Tree3 to Tree2, then to [TreePrim] which
-- are convenient for further processing. The option of drawing with or without
-- needles is exposed as different pipelines.

renderTree :: Tree3 -> Diagram B R2
renderTree = draw . renderTreeToPrim

renderTreeWithNeedles :: (Double -> Bool) -> NeedleParams -> Tree3 -> Diagram B R2
renderTreeWithNeedles f np = draw' np . withNeedles f . renderTreeToPrim

renderTreeToPrim :: Tree3 -> [TreePrim]
renderTreeToPrim = toPrim . projectXZ

-- Respecting the Earth
--
-- Assuming the tree is growing on flat ground, we can't have the branches
-- digging into it. This adjustment must be applied immediately after the
-- calculation of a new branch tip, so that the modeled nodes propagate
-- correctly.

-- We should never see the base of a branch below ground.
aboveGround :: (P3, R3) -> (P3, R3)
aboveGround (p, v) = 
    if p_z > 0 && p_z' > 0
        then (p, v)                     -- above ground so no change
        else if p_z > 0 && p_z' < 0
                then (p, v')            -- base of branch above, tip below
                else error "Base of branch below ground"
    where
        (p_x, p_y, p_z)    = unp3 p
        (v_x, v_y, v_z)    = unr3 v
        (p_x', p_y', p_z') = unp3 (p .+^ v)
        
        -- Base above ground, tip below.
        -- Rotate branch so that tip rests on ground.
        v'  = r3 (v_x * s, v_y * s, 0)
        s   = c' / c
        c   = sqrt (v_x * v_x + v_y * v_y)
        c'  = sqrt (m_v * m_v - p_z * p_z)
        m_v = magnitude v
        
-- Projecting the Tree onto 2D
--
-- We are rendering the tree from the side, so we simply discard the Y-coordinate.
-- We could project onto another plane and the rest of the pipeline would work.

projectXZ :: Tree3 -> Tree2
projectXZ = fmap infoXZ

infoXZ :: TreeInfo (P3,R3) -> TreeInfo (P2,R2)
infoXZ ((p,v), g0, g1, a, nt) = ((pxz p,rxz v), g0, g1, a, nt)

pxz :: P3 -> P2
pxz p = p2 (x, z) where (x, _, z) = unp3 p

rxz :: R3 -> R2
rxz r = r2 (x, z) where (x, _, z) = unr3 r

-- Reducing the Tree to Primitives
--
-- Given a tree projected onto a plane, convert it to a list
-- of drawing instructions.

toPrim :: Tree2 -> [TreePrim]
toPrim = flatten . fmap treeToPrim

treeToPrim :: TreeInfo (P2,R2) -> TreePrim
treeToPrim ((p,v), g0, g1, a, _) = if g0 < 0 then tip else node
    where tip  = Tip p (p .+^ v) a
          node = Trunk p (p .+^ v) g0 g1 a

-- Decorating with Needles
--
-- After the tree has been converted to primitives, scan them for where to decorate
-- with needles, according to the predicate supplied by the consumer. Append the
-- needle-drawing instructions as new primitives.

withNeedles :: (Age -> Bool) -> [TreePrim] -> [TreePrim]
withNeedles f ps = ps ++ (map toNeedles . filter (shouldAddNeedles f)) ps

shouldAddNeedles :: (Age -> Bool) -> TreePrim -> Bool
shouldAddNeedles f (Trunk _ _ _ _ a) = f a
shouldAddNeedles f (Tip   _ _     a) = f a
shouldAddNeedles _ _                 = False

toNeedles :: TreePrim -> TreePrim
toNeedles (Trunk p0 p1 _ _ _) = Needles p0 p1
toNeedles (Tip   p0 p1     _) = Needles p0 p1
toNeedles p                   = p

-- Drawing the Primitives
--
-- Execute the drawing instructions as applications of functions from
-- the diagrams package, producing a diagram as output.

draw :: [TreePrim] -> Diagram B R2
draw = draw' def

draw' :: NeedleParams -> [TreePrim] -> Diagram B R2
draw' np = mconcat . map (drawPrim np)

drawPrim :: NeedleParams -> TreePrim -> Diagram B R2
drawPrim _  (Trunk p0 p1 g0 g1 _) = drawTrunk   p0 p1 g0 g1
drawPrim _  (Tip p0 p1 _)         = drawTip     p0 p1
drawPrim np (Needles p0 p1)       = drawNeedles p0 p1 np

-- Draw a section of trunk or branch as a trapezoid with the
-- correct girths at each end.

drawTrunk :: P2 -> P2 -> Double -> Double -> Diagram B R2
drawTrunk p0 p1 g0 g1 = place trunk p0
    where trunk = (closeLine . lineFromVertices) [ p0, a, b, c, d ]
                  # strokeLoop 
                  # fc black 
                  # lw 0.01 
          n = (p1 .-. p0) # rotateBy (1/4) # normalized
          g0_2 = g0 / 2
          g1_2 = g1 / 2
          a = p0 .-^ (g0_2 *^ n)
          b = p1 .-^ (g1_2 *^ n)
          c = p1 .+^ (g1_2 *^ n)
          d = p0 .+^ (g0_2 *^ n)

drawTip :: P2 -> P2 -> Diagram B R2 
drawTip p0 p1 = position [(p0, fromOffsets [ p1 .-. p0 ] # lw 0.01)]

drawNeedles :: P2 -> P2 -> NeedleParams -> Diagram B R2
drawNeedles p0 p1 np = place (scaleInv ns Diagrams.Prelude.unitX # _scaleInvObj) p0
    where ns = needles numNeedles (magnitude v) nLength nAngle # rotate (-th)
          th = Diagrams.TwoD.Vector.angleBetween v Diagrams.Prelude.unitX :: Rad
          v  = p1 .-. p0
          numNeedles   = floor (magnitude v / nIncr) :: Int
          nLength = needleLength np
          nAngle  = needleAngle np
          nIncr   = needleIncr np

needle :: Double -> Rad -> Diagram B R2
needle l th =  d # rotate th <> d # rotate (-th)
    where d = fromOffsets [Diagrams.Prelude.unitX ^* l]

needles :: Int -> Double -> Double -> Rad -> Diagram B R2
needles n l nl na = position (zip ps (repeat (needle nl na)))
    where ps = map p2 [(x,0)|x <- [0, dx .. l - dx]]
          dx = l / fromIntegral (n + 1)


-- Growing a conifer by unfolding.
--
-- The tree starts from a seed, representing the first segment of the trunk.
--
-- At the first unfolding, the seed becomes part of the tree, and a new batch of
-- seeds is produced, one for the trunk and one for each branch of a whorl.
--
-- At subsequent unfoldings, the "trunk seed" repeats the process. However, each
-- "branch seed" unfolds as a branch and new branch seeds.

-- Age is represented as a continuous variable which allows recursing once per year,
-- increasing the level of branching, with a remainder of extra growth, leading to
-- the pyramidal structure of a real conifer.

tree :: TreeParams -> AgeParams -> Tree3
tree tp ap = calcGirths a tp . scaleTips a tp . pruneByScale 1e-6 $ pruneByAge a $ tree' tp ap
    where a = apAge ap

-- The tree' function produces an infinite tree which must be pruned to use.

pruneByAge :: Age -> TreeSpec3 -> TreeSpec3
pruneByAge a (Node n@((p, v), _, _, a', _) ns)
    | a > a'  && not (a `equiv` a')  =  Node n (map (pruneByAge a) ns)
    | otherwise                      =  Node n []

pruneByScale :: Double -> TreeSpec3 -> TreeSpec3
pruneByScale s (Node n@((_, v), _, _, _, _) ns)
    | magnitude v > s  =  Node n (map (pruneByScale s) ns)
    | otherwise        =  Node n []

scaleTips :: Age -> TreeParams -> TreeSpec3 -> TreeSpec3
scaleTips a tp = fmap (scaleTip a tp)

-- Tips are recorded as though grown in full-year increments. They
-- are trimmed to partial-year growth here.
scaleTip :: Age -> TreeParams -> SpecInfo (P3, R3) -> SpecInfo (P3, R3)
scaleTip a tp  n@((p, v), _, _, a', TrunkNode) = n'
    where
        n'  = if isTip a a' then (pv, -1, -1, a - a' + 1, TrunkNode) else n
        lpy = tpTrunkLengthIncrementPerYear tp
        ageIncr = 1 / fromIntegral (tpWhorlsPerYear tp)
        s   = max ((a - a' + 1) * lpy * ageIncr) 1e-7
        pv  = (p, v # scale s)
scaleTip a tp n@((p, v), _, _, a', BranchNode) = n'
    where
        n'  = if isTip a a' then (pv, -1, -1, a - a' + 1, BranchNode) else n
        lr  = tpBranchBranchLengthRatio tp
        s   = max ((a - a' + 1) * lr) 1e-7
        pv  = (p, v # scale s)

isTip :: Age -> Age -> Bool
isTip ageTree ageNode = ageTree < ageNode || ageTree `equiv` ageNode

equiv :: Age -> Age -> Bool
equiv x y = abs (x - y) < 1e-6

-- Translate the age and girth specification into a girth to draw.
calcGirths :: Age -> TreeParams -> TreeSpec3 -> Tree3
calcGirths a tp = fixupTipGirths . fmap (calcGirth a tp)

-- Girths are recorded as the ratios to scale by age, which we do here.
calcGirth :: Age -> TreeParams -> SpecInfo (P3, R3) -> TreeInfo (P3, R3)
calcGirth a tp n@(pv, g, g', a', BranchNode)
    | g == (-1) || g' == (-1)  =  n
    | otherwise                =  (pv, gg, gg, a', BranchNode)
    where
        gg  = girth (a - a') g
calcGirth a tp n@(pv, g, g', a', TrunkNode)
    | g == (-1) || g' == (-1)  =  n
    | otherwise                =  (pv, gg, gg', a', TrunkNode)
    where
        gg  = girth (a - a') g
        gg' = girth (a - a') g'

-- TODO might be unnecessary
fixupTipGirths :: Tree3 -> Tree3
fixupTipGirths (Node (pv, _, _, a, t) [])  =  Node (pv, -1, -1, a, t) []
fixupTipGirths n = n
 
-- This tree grows without bound.
tree' :: TreeParams -> AgeParams -> TreeSpec3
tree' tp ap = unfoldTree growTree (seed tp ap)

seed :: TreeParams -> AgeParams -> Seed
seed tp ap = (ti, tp, ap')
    where
        ap'     = ap { apAge = 0 }
        ti      = ((p, v), g, g', apAge ap', TrunkNode)
        (p, v)  = (origin::P3, unitZ ^* (lpy * ageIncr))
        lpy     = tpTrunkLengthIncrementPerYear tp
        ageIncr = 1 / fromIntegral (tpWhorlsPerYear tp)
        tg      = tpTrunkGirth tp
        g       = tg
        g'      = tg * (1 - ageIncr)

-- A node is inserted into the tree unchanged, and a new list of nodes is produced.

-- Given a seed, return a node in the tree and a bunch of new seeds.
-- If the seed is a new sprout (i.e. less than a year old) return it without extra seeds.
-- Seeds can be either for TrunkNodes or BranchNodes. A TrunkNode seed produces a TrunkNode,
-- possibly another TrunkNode seed, and possibly a whorl of BranchNode seeds.

-- Unfold a seed by returning its TreeInfo and new seeds representing upward growth
-- and the next level of branching. If a seed's age is less than one unit of growth,
-- no more seeds are produced by it.

growTree :: Seed -> (SpecInfo (P3, R3), [Seed])
growTree (ti@((p, v), _, _, _, TrunkNode), tp, ap)  = (ti, t:ws)
    where
        ageIncr = 1 / fromIntegral wpy
        wpy     = tpWhorlsPerYear tp

        t       = (((p', v), g, g', age', TrunkNode), tp, apNext)

        p'      = p .+^ v
        apNext  = adjustAge ageIncr .
                  advancePhase wsz wpy .
                  advanceTrunkBranchAngle wsz $ ap
        wsz     = tpWhorlSize tp

        g       = tg
        g'      = tg * (1 - ageIncr)
        tg      = tpTrunkGirth tp
        age'    = apAge apNext
        
        -- TODO The age concept isn't quite right. Branches grow too fast compared
        -- to the trunk. They look better when prematurely aged by a year.
        ws      = whorl p' tp (adjustAge 1 apNext)

growTree (ti@((p, v), _, _, _, BranchNode), tp, ap)  = (ti, ss)
    where
        ss       = [ ((n, g, g, apAge ap', BranchNode), tp, ap') | n <- bs ]
        bs       = newBranchNodes tp (p' .+^ v', v')
        ap'      = adjustAge 1 ap
        (p', v') = aboveGround (p, v) 
        g        = tpBranchGirth tp

whorl :: P3 -> TreeParams -> AgeParams -> [Seed]
whorl p tp ap@(AgeParams age tbai phase) = ss
  where
    lr   = tpTrunkBranchLengthRatio tp
    nb   = tpWhorlSize tp
    as   = tpTrunkBranchAngles tp
    pt :: Int -> R3
    pt i = r3 (cos (theta i), sin (theta i), cos (phi i)) ^* lr
         where theta i = fromIntegral i * tau / fromIntegral nb + phase
               phi i   = as !! ((i + tbai) `mod` (length as))
    g    = tpBranchGirth tp
    ss   = [ (((p, (pt i)), g, g, age, BranchNode), tp, ap) | i <- [0 .. nb - 1]]


-- Next year's subbranches continue straight, to the left and to the right. The straight
-- subbranch grows at a possibly different rate from the side subbranches. Scale the
-- branches to their full length. If they are leaves, they will be scaled back.

newBranchNodes :: TreeParams -> (P3, R3) -> [(P3, R3)]
newBranchNodes tp (p, v) = [(p,l), (p,c), (p,r)]
    where bba    = tpBranchBranchAngle tp
          bblr   = tpBranchBranchLengthRatio tp
          bblr2  = tpBranchBranchLengthRatio2 tp

          angle  = 1/4 - (asTurn . Diagrams.ThreeD.Vector.angleBetween unitZ) v
          zAxis  = (asSpherical . direction) unitZ
          nAxis  = (asSpherical . direction) (cross3 unitZ v)
          t1     = rotationAbout origin nAxis angle
          t2 a   = conjugate t1 (rotationAbout origin zAxis a)

          l      = v # transform (t2   bba)  # scale bblr2
          r      = v # transform (t2 (-bba)) # scale bblr2
          c      = v                         # scale bblr

-- Helper functions for building the tree:

adjustAge :: Double -> AgeParams -> AgeParams
adjustAge da (AgeParams a i p) = AgeParams a' i p 
    where a' = a + da

advanceTrunkBranchAngle :: Int -> AgeParams -> AgeParams
advanceTrunkBranchAngle wsz (AgeParams a i p) = AgeParams a i' p
    where i'  = (i + 1) `mod` wsz

advancePhase :: Int -> Int -> AgeParams -> AgeParams
advancePhase wsz wpy (AgeParams a i p) = AgeParams a i p'
    where p'  = p + tau / (fromIntegral wsz * fromIntegral wpy * 2)

-- Produce a width based on age and girth characteristic. Don't let the
-- width go below the minimum.

girth :: Double -> Double -> Double
girth a g = 0.01 * max (a * g) 1
