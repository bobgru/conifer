-- An Individual Conifer
--
-- This program draws the conifer with the specified values.
--
-- It can be driven from standard input by specifying -u on the command line.

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Main where
import Conifer
import Conifer.Types
import Data.Default.Class

-- The following all relate to the addition of the -u command line option
-- which allows data to be input from stdin.

import Diagrams.Prelude hiding ((<>), value)
import Diagrams.Backend.CmdLine
import Diagrams.Backend.SVG.CmdLine
import Options.Applicative

-- Run the program with dist/build/individual/individual -o individual.svg -w 400
-- where -o sets the output filename, and -w sets the diagram width.
--
-- Alternatively, by specifying the -u option on the command line, certain
-- parameters can be specified in a JSON object read from stdin. See the UserData
-- type for details.
--
-- Implement UserData Option

newtype FromUserData a = FromUserData a
type TreeArgs = (TreeParams, AgeParams, Bool)
data UserDataOpts = UserDataOpts Bool

instance Parseable UserDataOpts where
    parser  =  UserDataOpts
           <$> switch (long "userdata" 
                    <> short 'u'
                    <> help "Get user data from STDIN")

instance Mainable (FromUserData (TreeArgs -> Diagram SVG R2)) where
    type MainOpts (FromUserData (TreeArgs -> Diagram SVG R2)) =
        (MainOpts (Diagram SVG R2), UserDataOpts)

    mainRender (opts, UserDataOpts b) (FromUserData d) =
        do (tp', ap', n) <- case b of
               False     -> return (tp, ap, False)
               otherwise -> do
                   input <- getContents
                   case getUserDataFromJSON input of
                       Nothing -> error "Failed to parse input"
                       Just ud -> return $ argsFromInput ud tp ap
           return (d (tp', ap', n)) >>= mainRender opts

-- Default Values
--
-- The following values will be used to draw a tree unless overridden with -u.
--
-- Change withNeedles to False to draw the tree without needles.

withNeedles = True

needlePolicy :: Double -> Bool
needlePolicy age = age <= 2.0

np :: NeedleParams
np = def {
      needleLength = 0.05
    , needleAngle  = tau / 10
    , needleIncr   = 0.05
    }

tp :: TreeParams
tp = def {
      tpTrunkLengthIncrementPerYear = 1.4
    , tpTrunkBranchLengthRatio      = 0.6
    , tpTrunkBranchAngles           = [tau / 9, tau / 7, tau / 4.8 , tau / 6.5 ]
    , tpTrunkGirth                  = 5.0
    , tpWhorlsPerYear               = 9
    , tpWhorlSize                   = 7
    , tpBranchGirth                 = 1.0
    , tpBranchBranchLengthRatio     = 1.0
    , tpBranchBranchLengthRatio2    = 1.0
    , tpBranchBranchAngle           = tau / 6
    }

ap :: AgeParams
ap = AgeParams 3 0 (tau / 3)


-- Main Program

main   = mainWith $ FromUserData $ treeFromUserInput

treeFromUserInput (tp, ap, n) = tree tp ap # render' n # centerXY # pad 1.2

render' withNeedles = if withNeedles 
                          then renderTreeWithNeedles needlePolicy np
                          else renderTree
