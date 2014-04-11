> module Main where
> import Conifer.Types
> import Control.Monad (when)
> import System.Exit (exitFailure)
> import Test.HUnit

> main = do 
>     Counts c t e f <- runTestTT tests
>     when (e > 0 || f > 0) exitFailure
> 

> tests = TestList [
>       TestLabel "getUserDataFromJSON 1"        $ TestCase  getUserDataFromJSON_1
>   ]

> getUserDataFromJSON_1    = actual @?= expected
>     where actual   = getUserDataFromJSON json
>           expected = Just $ UD {  
>                            udAge     = 3
>                          , udNeedles = False
>                          , udTrunkLengthIncrementPerYear = 1.4
>                          , udTrunkBranchLengthRatio      = 0.6
>                          , udTrunkBranchAngles           = [0.698, 0.898, 1.31 , 0.967]
>                          , udTrunkGirth                  = 5.0
>                          , udWhorlsPerYear               = 9
>                          , udWhorlSize                   = 7
>                          , udBranchGirth                 = 1.0
>                          , udBranchBranchLengthRatio     = 1.0
>                          , udBranchBranchLengthRatio2    = 1.0
> --                         , udBranchBranchAngle           :: Rad
>                          }
>           json = "\
>\{\"udTrunkGirth\":5,\"udWhorlsPerYear\":9,\"udTrunkBranchAngles\":[0.698,0.898,1.31,0.967],\"udTrunkBranchLengthRatio\":0.6,\"udBranchGirth\":1,\"udWhorlSize\":7,\"udBranchBranchLengthRatio\":1,\"udBranchBranchLengthRatio2\":1,\"age\":3,\"needles\":false,\"udTrunkLengthIncrementPerYear\":1.4}"
