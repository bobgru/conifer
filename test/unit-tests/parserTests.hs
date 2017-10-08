module Main where
import Conifer.Types
import Control.Monad (when)
import System.Exit (exitFailure)
import Test.HUnit

main = do 
    Counts c t e f <- runTestTT tests
    when (e > 0 || f > 0) exitFailure


tests = TestList [
      TestLabel "getUserDataFromJSON_None"     $ TestCase  getUserDataFromJSON_None
    , TestLabel "getUserDataFromJSON_Age"      $ TestCase  getUserDataFromJSON_Age
    , TestLabel "getUserDataFromJSON_All"      $ TestCase  getUserDataFromJSON_All

    , TestLabel "argsFromInput_NoChange"       $ TestCase  argsFromInput_NoChange
    , TestLabel "argsFromInput_ChangeAll"      $ TestCase  argsFromInput_ChangeAll
  ]

getUserDataFromJSON_None    = actual @?= expected
    where actual   = getUserDataFromJSON json
          expected = Just $ UD {  
                           udAge                         = Nothing
                         , udNeedles                     = Nothing
                         , udTrunkLengthIncrementPerYear = Nothing
                         , udTrunkBranchLengthRatio      = Nothing
                         , udTrunkBranchAngles           = Nothing
                         , udTrunkGirth                  = Nothing
                         , udWhorlsPerYear               = Nothing
                         , udWhorlSize                   = Nothing
                         , udBranchGirth                 = Nothing
                         , udBranchBranchLengthRatio     = Nothing
                         , udBranchBranchLengthRatio2    = Nothing
--                         , udBranchBranchAngle           :: Angle Double
                         }
          json = "\
\{}"

getUserDataFromJSON_Age    = actual @?= expected
    where actual   = getUserDataFromJSON json
          expected = Just $ UD {  
                           udAge                         = Just 3
                         , udNeedles                     = Nothing
                         , udTrunkLengthIncrementPerYear = Nothing
                         , udTrunkBranchLengthRatio      = Nothing
                         , udTrunkBranchAngles           = Nothing
                         , udTrunkGirth                  = Nothing
                         , udWhorlsPerYear               = Nothing
                         , udWhorlSize                   = Nothing
                         , udBranchGirth                 = Nothing
                         , udBranchBranchLengthRatio     = Nothing
                         , udBranchBranchLengthRatio2    = Nothing
--                         , udBranchBranchAngle           :: Angle Double
                         }
          json = "\
\{\"age\":3}"

getUserDataFromJSON_All    = actual @?= expected
    where actual   = getUserDataFromJSON json
          expected = Just $ UD {  
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
--                         , udBranchBranchAngle           :: Angle Double
                         }
          json = "\
\{\"udTrunkGirth\":5,\"udWhorlsPerYear\":9,\"udTrunkBranchAngles\":[0.698,0.898,1.31,0.967],\"udTrunkBranchLengthRatio\":0.6,\"udBranchGirth\":1,\"udWhorlSize\":7,\"udBranchBranchLengthRatio\":1,\"udBranchBranchLengthRatio2\":1,\"age\":3,\"needles\":false,\"udTrunkLengthIncrementPerYear\":1.4}"

argsFromInput_NoChange    = actual @?= expected
    where actual   = argsFromInput ud tp ap
          expected = (tp, ap, False)
          ud = UD {  
                           udAge                         = Nothing
                         , udNeedles                     = Nothing
                         , udTrunkLengthIncrementPerYear = Nothing
                         , udTrunkBranchLengthRatio      = Nothing
                         , udTrunkBranchAngles           = Nothing
                         , udTrunkGirth                  = Nothing
                         , udWhorlsPerYear               = Nothing
                         , udWhorlSize                   = Nothing
                         , udBranchGirth                 = Nothing
                         , udBranchBranchLengthRatio     = Nothing
                         , udBranchBranchLengthRatio2    = Nothing
--                         , udBranchBranchAngle           :: Angle Double
                         }
          tp = TreeParams {
                           tpTrunkLengthIncrementPerYear = 1.4
                         , tpTrunkBranchLengthRatio      = 0.6
                         , tpTrunkBranchAngles           = [0.698, 0.898, 1.31 , 0.967]
                         , tpTrunkGirth                  = 5.0
                         , tpWhorlsPerYear               = 9
                         , tpWhorlSize                   = 7
                         , tpBranchGirth                 = 1.0
                         , tpBranchBranchLengthRatio     = 1.0
                         , tpBranchBranchLengthRatio2    = 1.0
                         , tpBranchBranchAngle           = 0.698
                         }
          ap = AgeParams {
                           apAge = 3
                         , apTrunkBranchAngleIndex = 0
                         , apWhorlPhase = 0
                         }

argsFromInput_ChangeAll    = actual @?= expected
    where actual   = argsFromInput ud tp ap
          expected = (tp', ap', n)
          ud = UD {  
                           udAge                         = Just 5
                         , udNeedles                     = Just True
                         , udTrunkLengthIncrementPerYear = Just 1.5
                         , udTrunkBranchLengthRatio      = Just 0.7
                         , udTrunkBranchAngles           = Just [0.777]
                         , udTrunkGirth                  = Just 6.0
                         , udWhorlsPerYear               = Just 10
                         , udWhorlSize                   = Just 5
                         , udBranchGirth                 = Just 1.2
                         , udBranchBranchLengthRatio     = Just 1.3
                         , udBranchBranchLengthRatio2    = Just 1.4
--                         , udBranchBranchAngle           :: Angle Double
                         }
          tp = TreeParams {
                           tpTrunkLengthIncrementPerYear = 1.4
                         , tpTrunkBranchLengthRatio      = 0.6
                         , tpTrunkBranchAngles           = [0.698, 0.898, 1.31 , 0.967]
                         , tpTrunkGirth                  = 5.0
                         , tpWhorlsPerYear               = 9
                         , tpWhorlSize                   = 7
                         , tpBranchGirth                 = 1.0
                         , tpBranchBranchLengthRatio     = 1.0
                         , tpBranchBranchLengthRatio2    = 1.0
                         , tpBranchBranchAngle           = 0.698
                         }
          ap = AgeParams {
                           apAge = 3
                         , apTrunkBranchAngleIndex = 0
                         , apWhorlPhase = 0
                         }
          tp' = TreeParams {
                           tpTrunkLengthIncrementPerYear = 1.5
                         , tpTrunkBranchLengthRatio      = 0.7
                         , tpTrunkBranchAngles           = [0.777]
                         , tpTrunkGirth                  = 6.0
                         , tpWhorlsPerYear               = 10
                         , tpWhorlSize                   = 5
                         , tpBranchGirth                 = 1.2
                         , tpBranchBranchLengthRatio     = 1.3
                         , tpBranchBranchLengthRatio2    = 1.4
                         , tpBranchBranchAngle           = 0.698
                         }
          ap' = AgeParams {
                           apAge = 5
                         , apTrunkBranchAngleIndex = 0
                         , apWhorlPhase = 0
                         }
          n = True



