An Individual Conifer
=====================

This program draws the conifer with the specified values.

It can be driven from standard input by specifying `-u` on the command line.

> {-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings,
>              FlexibleInstances, TypeFamilies 
> #-}
> module Main where
> import Conifer
> import Control.Monad hiding (ap)
> import Data.Aeson
> import qualified Data.Aeson.Types as DAT
> import qualified Data.Attoparsec as P
> import qualified Data.ByteString.Lazy.Char8 as B
> import Data.Default.Class
> import qualified Data.HashMap.Strict as HM
> import qualified Data.String as S
> import qualified Data.Text as T
> import qualified Data.Vector as V
> import Diagrams.Prelude hiding ((<>), value)
> import Diagrams.Backend.CmdLine
> import Diagrams.Backend.SVG.CmdLine
> import Options.Applicative

Run the program with `dist/build/individual/individual -o individual.svg -w 400` 
where `-o` sets the output filename, and `-w` sets the diagram width.

Alternatively, by specifying the `-u` option on the command line, certain
parameters can be specified in a JSON object read from stdin. See the `UserData`
type for details.

> newtype FromUserData a = FromUserData a
> type TreeArgs = (TreeParams, AgeParams, Bool)
> data UserDataOpts = UserDataOpts Bool

> instance Parseable UserDataOpts where
>     parser  =  UserDataOpts
>            <$> switch (long "userdata" 
>                     <> short 'u'
>                     <> help "Get user data from STDIN")

> instance Mainable (FromUserData (TreeArgs -> Diagram SVG R2)) where
>     type MainOpts (FromUserData (TreeArgs -> Diagram SVG R2)) =
>         (MainOpts (Diagram SVG R2), UserDataOpts)

>     mainRender (opts, UserDataOpts b) (FromUserData d) =
>         do (tp', ap', n) <- case b of
>                False     -> return (tp, ap, False)
>                otherwise -> do
>                    input <- getContents
>                    case decode (B.pack input) of
>                        Nothing -> error "Failed to parse input"
>                        Just ud -> return $ argsFromInput ud tp ap
>            return (d (tp', ap', n)) >>= mainRender opts

> main   = mainWith $ FromUserData $ treeFromUserInput

> treeFromUserInput (tp, ap, n) = tree tp ap # render' n # centerXY # pad 1.2

> argsFromInput ud tp ap = (tp', ap', n)
>     where tp' = tp {
>                       tpTrunkLengthIncrementPerYear = udTrunkLengthIncrementPerYear ud
>                     , tpTrunkBranchLengthRatio      = udTrunkBranchLengthRatio ud
>                     , tpTrunkBranchAngles           = udTrunkBranchAngles ud
>                     , tpTrunkGirth                  = udTrunkGirth ud
>                     , tpWhorlsPerYear               = udWhorlsPerYear ud
>                     , tpWhorlSize                   = udWhorlSize ud
>                     , tpBranchGirth                 = udBranchGirth ud
>                     , tpBranchBranchLengthRatio     = udBranchBranchLengthRatio ud
>                     , tpBranchBranchLengthRatio2    = udBranchBranchLengthRatio2 ud
> --                    , tpBranchBranchAngle           = udBranchBranchAngle ud
>                     }
>           ap' = ap { apAge = udAge ud }
>           n   = udNeedles ud

> data UserData = UD {
>       udAge     :: Double
>     , udNeedles :: Bool
>     , udTrunkLengthIncrementPerYear :: Double
>     , udTrunkBranchLengthRatio      :: Double
>     , udTrunkBranchAngles           :: [Double]
>     , udTrunkGirth                  :: Double
>     , udWhorlsPerYear               :: Int
>     , udWhorlSize                   :: Int
>     , udBranchGirth                 :: Double
>     , udBranchBranchLengthRatio     :: Double
>     , udBranchBranchLengthRatio2    :: Double
> --    , udBranchBranchAngle           :: Rad
>     } deriving (Show)

> instance ToJSON UserData where
>     toJSON ud = Object $ HM.fromList [
>           ("age", toJSON $ udAge ud) 
>         , ("needles", toJSON $ udNeedles ud)
>         , ("udTrunkLengthIncrementPerYear", toJSON $ udTrunkLengthIncrementPerYear ud)
>         , ("udTrunkBranchLengthRatio",      toJSON $ udTrunkBranchLengthRatio ud)
>         , ("udTrunkBranchAngles",           toJSON $ udTrunkBranchAngles ud)
>         , ("udTrunkGirth",                  toJSON $ udTrunkGirth ud)
>         , ("udWhorlsPerYear",               toJSON $ udWhorlsPerYear ud)
>         , ("udWhorlSize",                   toJSON $ udWhorlSize ud)
>         , ("udBranchGirth",                 toJSON $ udBranchGirth ud)
>         , ("udBranchBranchLengthRatio",     toJSON $ udBranchBranchLengthRatio ud)
>         , ("udBranchBranchLengthRatio2",    toJSON $ udBranchBranchLengthRatio2 ud)
> --        , ("udBranchBranchAngle",           toJSON $ udBranchBranchAngle ud)
>         ]

> -- sample data
> ud = UD {
>       udAge     = 3
>     , udNeedles = False
>     , udTrunkLengthIncrementPerYear = 1.4
>     , udTrunkBranchLengthRatio      = 0.6
>     , udTrunkBranchAngles           = [0.698, 0.898, 1.31 , 0.967]
>     , udTrunkGirth                  = 5.0
>     , udWhorlsPerYear               = 9
>     , udWhorlSize                   = 7
>     , udBranchGirth                 = 1.0
>     , udBranchBranchLengthRatio     = 1.0
>     , udBranchBranchLengthRatio2    = 1.0
> --    , udBranchBranchAngle           :: Rad
>     }


> instance FromJSON UserData where
>     parseJSON (Object v)  =  UD
>                          <$> v .: "age"
>                          <*> v .: "needles"
>                          <*> v .: "udTrunkLengthIncrementPerYear"
>                          <*> v .: "udTrunkBranchLengthRatio"
>                          <*> v .: "udTrunkBranchAngles"
>                          <*> v .: "udTrunkGirth"
>                          <*> v .: "udWhorlsPerYear"
>                          <*> v .: "udWhorlSize"
>                          <*> v .: "udBranchGirth"
>                          <*> v .: "udBranchBranchLengthRatio"
>                          <*> v .: "udBranchBranchLengthRatio2"
>  --                        <*> v .: "udBranchBranchAngle"
>     parseJSON _           =  mzero

> decodeWith :: (Value -> DAT.Parser b) -> String -> Either String b
> decodeWith p s = do
>   value <- P.eitherResult $ (P.parse json . S.fromString) s
>   DAT.parseEither p value

> render' withNeedles = if withNeedles 
>                           then renderTreeWithNeedles needlePolicy np
>                           else renderTree

Change `withNeedles` to `False` to draw the tree without needles.

> withNeedles = True

> needlePolicy :: Double -> Bool
> needlePolicy age = age <= 2.0

> np :: NeedleParams
> np = def {
>       needleLength = 0.05
>     , needleAngle  = tau / 10
>     , needleIncr   = 0.05
>     }


> tp :: TreeParams
> tp = def {
>       tpTrunkLengthIncrementPerYear = 1.4
>     , tpTrunkBranchLengthRatio      = 0.6
>     , tpTrunkBranchAngles           = [tau / 9, tau / 7, tau / 4.8 , tau / 6.5 ]
>     , tpTrunkGirth                  = 5.0
>     , tpWhorlsPerYear               = 9
>     , tpWhorlSize                   = 7
>     , tpBranchGirth                 = 1.0
>     , tpBranchBranchLengthRatio     = 1.0
>     , tpBranchBranchLengthRatio2    = 1.0
>     , tpBranchBranchAngle           = tau / 6
>     }

> ap :: AgeParams
> ap = AgeParams 3 0 (tau / 3)
