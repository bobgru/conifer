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

A tree rises from its origin to its `t3Node` where there is optionally
another tree, and—every year after the first—a whorl. The tree may
grow additional whorls during a year, which are spaced evenly up the
trunk. The tree grows as type `Tree3`, but is projected  to `Tree2`
before rendering as a diagram.

> data Tree3 = Tree3 {
>       t3Node     :: P3
>     , t3Age      :: Int
>     , t3Girth    :: Double
>     , t3Next     :: Maybe Tree3
>     , t3Whorls   :: [Whorl3]
>     } deriving (Show, Eq)

> data Tree2 = Tree2 {
>       t2Node     :: P2
>     , t2Age      :: Int
>     , t2Girth    :: Double
>     , t2Next     :: Maybe Tree2
>     , t2Whorls   :: [Whorl2]
>     } deriving (Show, Eq)

A whorl is a collection of branches radiating evenly spaced from 
the trunk but at varying angles relative to the trunk. 
There can be multiple whorls per year, so a whorl records its position 
along that year's segment of trunk and a scale factor, so that older 
whorls can have longer branches than younger ones. 

> data Whorl3 = Whorl3 {
>       w3Node     :: P3
>     , w3Scale    :: Double
>     , w3Branches :: [Branch3]
>     } deriving (Show, Eq)

> data Whorl2 = Whorl2 {
>       w2Node     :: P2
>     , w2Scale    :: Double
>     , w2Branches :: [Branch2]
>     } deriving (Show, Eq)

A branch shoots out from its origin to its `b3Node`, where it branches
into some number, possibly zero, of other branches.

> data Branch3 = Tip3 P3 Int Double Double |
>     Branch3 {
>       b3Node       :: P3
>     , b3Age        :: Int
>     , b3PartialAge :: Double
>     , b3Girth      :: Double
>     , b3Branches   :: [Branch3]
>     } deriving (Show, Eq)

> data Branch2 = Tip2 P2 Int Double Double |
>     Branch2 {
>       b2Node       :: P2
>     , b2Age        :: Int
>     , b2PartialAge :: Double
>     , b2Girth      :: Double
>     , b2Branches   :: [Branch2]
>     } deriving (Show, Eq)

**Growing the Tree**

We first build a tree with each node is in its own coordinate space relative to its 
parent node.

**TODO** We should have separate types for relative `Tree3` and absolute `Tree3`.

> tree :: TreeParams -> Tree3
> tree tp = if age == 0
>               then Tree3 trunkTip age girth Nothing         whorls
>               else Tree3 trunkTip age girth (Just nextTree) whorls
>     where age         = tpAge tp
>           girth       = tpTrunkGirth tp
>           trunkGrowth = tpTrunkLengthIncrement tp

This year's trunk growth simply adds an increment of height relative to the
tip of last year's trunk, with possibly another tree on top of that.

>           trunkTip  = p3 (0, 0, trunkGrowth)
>           nextTree  = tree tpNextYear

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
no next year's whorl.

>           whorls          = if age == 0 
>                                 then thisYearsWhorls
>                                 else thisYearsWhorls ++ [nextYearsWhorl]
>           thisYearsWhorls = [whorl tp' (p3 (0, 0, height))  tipGrowth pa
>                                 | (tp', (height, tipGrowth, pa)) <- zip tps initialBranchTips]
>           nextYearsWhorl  = whorl tpNextYear trunkTip 1.0 1.0
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

> whorl :: TreeParams -> P3 -> Double -> Double -> Whorl3
> whorl tp p s pa = Whorl3 p s [ branch tp (pt i) s pa | i <- [0 .. numBranches - 1]]
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

> branch :: TreeParams -> P3 -> Double -> Double -> Branch3
> branch tp p s pa = if age == 0
>                        then Tip3    p age pa g
>                        else Branch3 p age pa g bs
>     where age   = tpAge tp
>           g     = tpBranchGirth tp

Next year's subbranches continue straight, to the left and to the right. The straight
subbranch grows at a possibly different rate from the side subbranches.

>           bs    = [l, c, r]
>           l     = branch tp' p_l s pa
>           c     = branch tp' p_c s pa
>           r     = branch tp' p_r s pa
>           tp'   = subYear tp

>           p_l   = p # rotateXY   bba  # scale growth2
>           p_c   = p                   # scale growth
>           p_r   = p # rotateXY (-bba) # scale growth2
>           bba   = tpBranchBranchAngle tp

Determine if next year has partial growth.

>           growth         = if age == 1 then partialGrowth  else fullGrowth
>           partialGrowth  = s * bblr
>           fullGrowth     = bblr
>           bblr           = tpBranchBranchLengthRatio tp

>           growth2        = if age == 1 then partialGrowth2 else fullGrowth2
>           partialGrowth2 = s * bblr2
>           fullGrowth2    = bblr2
>           bblr2          = tpBranchBranchLengthRatio2 tp

> rotateXY :: Rad -> P3 -> P3
> rotateXY a p = p3 (x', y', z)
>     where (x, y, z) = unp3 p
>           (x', y')  = unr2 (rotate a (r2 (x, y)))

**Converting from Relative to Absolute Coordinates**

Convert the tree of relative coordinate spaces into a single coherent absolute
coordinate space, which will make projection onto the _x_-_z_-plane trivial.

> toAbsoluteP3 :: P3 -> P3 -> P3
> toAbsoluteP3 n p = n .+^ (p .-. origin)

> toAbsoluteTree :: P3 -> Tree3 -> Tree3
> toAbsoluteTree n (Tree3 p a g mt ws) =
>     case mt of
>         Nothing -> Tree3 p' a g Nothing ws'
>         Just t  -> Tree3 p' a g (Just (toAbsoluteTree p' t)) ws'
>     where p'  = toAbsoluteP3 n p
>           ws' = map (toAbsoluteWhorl n) ws

> toAbsoluteWhorl :: P3 -> Whorl3 -> Whorl3
> toAbsoluteWhorl n (Whorl3 p s bs) = Whorl3 p' s bs'
>     where p'  = toAbsoluteP3 n p
>           bs' = map (toAbsoluteBranch p') bs

> toAbsoluteBranch :: P3 -> Branch3 -> Branch3
> toAbsoluteBranch n (Tip3 p a pa g) = Tip3 p' a pa g
>     where p'  = toAbsoluteP3 n p
> toAbsoluteBranch n (Branch3 p a pa g bs) = Branch3 p' a pa g bs'
>     where p'  = toAbsoluteP3 n p
>           bs' = map (toAbsoluteBranch p') bs

**Projecting the Tree onto 2D**

We are rendering the tree from the side, so we simply discard the _y_-coordinate.

> xz :: P3 -> P2
> xz p = p2 (x, z) where (x, _, z) = unp3 p

> projectTreeXZ :: Tree3 -> Tree2
> projectTreeXZ (Tree3 p a g mt ws) = case mt of
>     Nothing -> Tree2 p' a g  Nothing                 ws'
>     Just t  -> Tree2 p' a g (Just (projectTreeXZ t)) ws'
>     where p'  = xz p
>           ws' = map projectWhorlXZ ws

> projectWhorlXZ :: Whorl3 -> Whorl2
> projectWhorlXZ (Whorl3 p s bs) = Whorl2 (xz p) s (map projectBranchXZ bs)

> projectBranchXZ :: Branch3 -> Branch2
> projectBranchXZ b = case b of
>     Tip3 p a pa g       -> Tip2    (xz p) a pa g
>     Branch3 p a pa g bs -> Branch2 (xz p) a pa g (map projectBranchXZ bs)

**Drawing the Tree from Absolute Coordinates**

> drawTree :: P2 -> Tree2 -> Diagram B R2
> drawTree n (Tree2 p a g mt ws) =
>     case mt of
>         Nothing -> trunk <> whorls
>         Just t  -> trunk <> whorls <> nextTree t
>     where 
>           trunk      = drawTapered n p a g
>           whorls     = mconcat (map drawWhorl ws)
>           nextTree   = drawTree p

Draw a section of trunk (implicitly vertical) as a trapezoid with the
correct girths at top and bottom.

> drawTapered :: P2 -> P2 -> Int -> Double -> Diagram B R2
> drawTapered n p a g = trunk
>     where trunk    = (closeLine . lineFromVertices . map p2) [
>                              ( w/2,  y0)
>                            , ( w'/2, y0 + y)
>                            , (-w'/2, y0 + y)
>                            , (-w/2,  y0)
>                            ] 
>                      # strokeLoop # fc black # lw 0.01 # centerX
>           (_,y)    = unp2 p
>           (_,y0)   = unp2 n
>           w        = girth a g
>           w'       = girth (a-1) g

> drawWhorl :: Whorl2 -> Diagram B R2
> drawWhorl (Whorl2 p _ bs) = mconcat (map (drawBranch p) bs)

> drawBranch :: P2 -> Branch2 -> Diagram B R2 
> drawBranch n (Tip2 p a _ g)       = d
>     where d   = position [(n, fromOffsets [ p .-. n ] # withGirth a g)]
> drawBranch n (Branch2 p a _ g bs) = d <> bs'
>     where d   = position [(n, fromOffsets [ p .-. n ] # withGirth a g)]
>           bs' = mconcat (map (drawBranch p) bs)

Produce a width based on age and girth characteristic. This can be used directly
as a line width as in `withGirth` or for calculating the top and bottom of a
trapezoid for a tapered trunk segment.

> girth :: Int -> Double -> Double
> girth a g = fromIntegral (a+1) * g * 0.01

> withGirth :: Int -> Double -> Diagram B R2 -> Diagram B R2
> withGirth a = lw . girth a

**Rendering a Tree from Parameters**

> renderTree :: TreeParams -> Diagram B R2
> renderTree = drawTree origin . projectTreeXZ . toAbsoluteTree origin . tree

