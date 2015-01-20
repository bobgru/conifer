{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}
module Conifer.Types where

import Data.Default.Class
import Data.Maybe
import Data.Tree(Tree(..))
import Diagrams.Coordinates
import Diagrams.Prelude -- hiding (angleBetween, rotationAbout, direction)
import Diagrams.ThreeD.Types
import Diagrams.ThreeD.Vector

import Control.Monad (mzero)
import Data.Aeson
import qualified Data.Aeson.Types as DAT
import qualified Data.Attoparsec as P
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.HashMap.Strict as HM
import qualified Data.String as S

-- The Tree Data Structure
--
-- A Tree is the standard tree container represented by a Node constructor,
-- with a polymorphic payload. A leaf is a Node with no children.
--
-- The payload is TreeInfo parameterized on location type, and containing
-- the location, the girth at its origin (the location of which is implicit),
-- the girth at its location, and its age.

data NodeType = TrunkNode | BranchNode deriving (Show)

type Age       = Double
type Girth     = Double
type GirthSpec = Double

type SpecInfo a = (a, GirthSpec, GirthSpec, Age, NodeType)
type TreeInfo a = (a, Girth, Girth, Age, NodeType)

-- We specialize the types for the phases of tree development.
-- The tree grows as type TreeSpec3, in which the nodes have
-- length and direction, but unknown girth. After the tree has
-- been pruned, its trunk and branch girths can be calculated
-- and saved as type Tree3 (nodes in 3D). It can then be projected
-- to Tree2 (nodes in 2D) before being flattened to a list of
-- primitive drawing elements.

type TreeSpec3 = Tree (SpecInfo (P3, R3))
type Tree3     = Tree (TreeInfo (P3, R3))
type Tree2     = Tree (TreeInfo (P2, R2))

-- The tree is ultimately converted to context-free drawing instructions
-- which when carried out produce diagrams.
--
--   Trunk is a section of trunk or branch between points p0 and p1,
--     with girth g0 at p0 and g1 at p1.
--   Tip is the tip of a tree or branch, between points p0 and p1.
--   Needles indicates decoration with needles between points p0 and p1.
--   Trunk and Tip carry age as a hint for when needles should be drawn.

data TreePrim = Trunk   { p0::P2, p1::P2, g0::Double, g1::Double, age::Double }
              | Tip     { p0::P2, p1::P2, age::Double }
              | Needles { p0::P2, p1::P2 }

-- Specifying a Conifer
--
-- Our ideal tree will be completely determined by its "genes", the various
-- parameters in TreeParams. The age of the tree is roughly the number of recursive
-- steps in its growth—each year corresponds to another level of branching. As we are
-- modeling a conifer, its structure is a main trunk that adds some number of whorls
-- of branches each year and another length of trunk, meanwhile adding another level
-- of branching to existing branches.
--
-- One major concession to arbitrary aesthetics is the list of trunk branch angles,
-- which led to a fuller and less regular look, important for the original application
-- of this code. A more realistic approach would be to model random deviations from
-- the regular growth.

data TreeParams = TreeParams {
      tpTrunkLengthIncrementPerYear :: Double
    , tpTrunkBranchLengthRatio      :: Double
    , tpTrunkBranchAngles           :: [Double]
    , tpTrunkGirth                  :: Double
    , tpWhorlsPerYear               :: Int
    , tpWhorlSize                   :: Int
    , tpBranchGirth                 :: Double
    , tpBranchBranchLengthRatio     :: Double
    , tpBranchBranchLengthRatio2    :: Double
    , tpBranchBranchAngle           :: Rad
    } deriving (Show, Eq)

instance Default TreeParams where
    def = TreeParams {
      tpTrunkLengthIncrementPerYear = 0.9
    , tpTrunkBranchLengthRatio      = 0.7
    , tpTrunkBranchAngles           = [tau / 6]
    , tpTrunkGirth                  = 1.0
    , tpWhorlsPerYear               = 1
    , tpWhorlSize                   = 6
    , tpBranchGirth                 = 1.0
    , tpBranchBranchLengthRatio     = 0.8
    , tpBranchBranchLengthRatio2    = 0.8
    , tpBranchBranchAngle           = tau / 6
    }

-- The mutable state during a tree's growth consists of its age, the rotational phase of the next
-- whorl, and the next trunk branch angle to use.

data AgeParams = AgeParams {
      apAge                         :: Age
    , apTrunkBranchAngleIndex       :: Int
    , apWhorlPhase                  :: Double
    } deriving (Show, Eq)

-- A tree is unfolded from a seed.
type Seed = (SpecInfo (P3, R3), TreeParams, AgeParams)

-- The tree can be optionally decorated with needles, in which case the
-- needles can be customized in various ways.

data NeedleParams = NeedleParams {
      needleLength :: Double
    , needleAngle  :: Rad
    , needleIncr   :: Double
    }

instance Default NeedleParams where
    def = NeedleParams {
      needleLength = 0.05
    , needleAngle  = tau / 10
    , needleIncr   = 0.05
    }


-- The UserData type represents the data that can be fed via stdin to configure a tree.

data UserData = UD {
      udAge                         :: Maybe Double
    , udNeedles                     :: Maybe Bool
    , udTrunkLengthIncrementPerYear :: Maybe Double
    , udTrunkBranchLengthRatio      :: Maybe Double
    , udTrunkBranchAngles           :: Maybe [Double]
    , udTrunkGirth                  :: Maybe Double
    , udWhorlsPerYear               :: Maybe Int
    , udWhorlSize                   :: Maybe Int
    , udBranchGirth                 :: Maybe Double
    , udBranchBranchLengthRatio     :: Maybe Double
    , udBranchBranchLengthRatio2    :: Maybe Double
--    , udBranchBranchAngle           :: Maybe Rad
    } deriving (Show, Eq)

instance ToJSON UserData where
    toJSON ud = Object $ HM.fromList $ filter ((/= Null) . snd) [
          ("age", toJSON $ udAge ud) 
        , ("needles", toJSON $ udNeedles ud)
        , ("udTrunkLengthIncrementPerYear", toJSON $ udTrunkLengthIncrementPerYear ud)
        , ("udTrunkBranchLengthRatio",      toJSON $ udTrunkBranchLengthRatio ud)
        , ("udTrunkBranchAngles",           toJSON $ udTrunkBranchAngles ud)
        , ("udTrunkGirth",                  toJSON $ udTrunkGirth ud)
        , ("udWhorlsPerYear",               toJSON $ udWhorlsPerYear ud)
        , ("udWhorlSize",                   toJSON $ udWhorlSize ud)
        , ("udBranchGirth",                 toJSON $ udBranchGirth ud)
        , ("udBranchBranchLengthRatio",     toJSON $ udBranchBranchLengthRatio ud)
        , ("udBranchBranchLengthRatio2",    toJSON $ udBranchBranchLengthRatio2 ud)
--        , ("udBranchBranchAngle",           toJSON $ udBranchBranchAngle ud)
        ]

-- sample data
ud = UD {
      udAge                         = Just 3
    , udNeedles                     = Just False
    , udTrunkLengthIncrementPerYear = Just 1.4
    , udTrunkBranchLengthRatio      = Just 0.6
    , udTrunkBranchAngles           = Just [0.698, 0.898, 1.31 , 0.967]
    , udTrunkGirth                  = Just 5.0
    , udWhorlsPerYear               = Just 9
    , udWhorlSize                   = Just 7
    , udBranchGirth                 = Just 1.0
    , udBranchBranchLengthRatio     = Just 1.0
    , udBranchBranchLengthRatio2    = Just 1.0
--    , udBranchBranchAngle           :: Rad
    }


instance FromJSON UserData where
    parseJSON (Object v)  =  UD
                         <$> v .:? "age"
                         <*> v .:? "needles"
                         <*> v .:? "udTrunkLengthIncrementPerYear"
                         <*> v .:? "udTrunkBranchLengthRatio"
                         <*> v .:? "udTrunkBranchAngles"
                         <*> v .:? "udTrunkGirth"
                         <*> v .:? "udWhorlsPerYear"
                         <*> v .:? "udWhorlSize"
                         <*> v .:? "udBranchGirth"
                         <*> v .:? "udBranchBranchLengthRatio"
                         <*> v .:? "udBranchBranchLengthRatio2"
 --                        <*> v .:? "udBranchBranchAngle"
    parseJSON _           =  mzero

decodeWith :: (Value -> DAT.Parser b) -> String -> Either String b
decodeWith p s = do
  value <- P.eitherResult $ (P.parse json . S.fromString) s
  DAT.parseEither p value

getUserDataFromJSON = decode . B.pack

argsFromInput ud tp ap = (tp', ap', n)
    where tp' = TreeParams
                (upd tpTrunkLengthIncrementPerYear udTrunkLengthIncrementPerYear ud tp)
                (upd tpTrunkBranchLengthRatio udTrunkBranchLengthRatio ud tp)
                (upd tpTrunkBranchAngles udTrunkBranchAngles ud tp)
                (upd tpTrunkGirth udTrunkGirth ud tp)
                (upd tpWhorlsPerYear udWhorlsPerYear ud tp)
                (upd tpWhorlSize udWhorlSize ud tp)
                (upd tpBranchGirth udBranchGirth ud tp)
                (upd tpBranchBranchLengthRatio udBranchBranchLengthRatio ud tp)
                (upd tpBranchBranchLengthRatio2 udBranchBranchLengthRatio2 ud tp)
                (tpBranchBranchAngle tp)

          ap' = AgeParams
                (upd apAge udAge ud ap)
                (apTrunkBranchAngleIndex ap)
                (apWhorlPhase ap)

          n   = maybe False id (udNeedles ud)

upd f_tp f_ud ud tp = maybe (f_tp tp) id (f_ud ud)
