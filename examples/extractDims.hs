-- Extract the original dimensions of a diagram from an SVG file.

{-# LANGUAGE OverloadedStrings #-}
module Main where
import Prelude hiding (take) -- collides with Attoparsec.take
import Control.Applicative ((<|>))
import Control.Monad.Error
import qualified Data.Aeson as A (encode)
import Data.Attoparsec.Text
import qualified Data.ByteString.Lazy.Char8 as B (putStrLn)
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T (getContents)
import System.Exit (exitFailure)

-- Parses events on stdin, and returns their JSON interpretations
-- or a JSON-encoded error message.
main = do
    inp  <- T.getContents
    case filterFunc inp of
        Left e  ->   putStrLn e >> exitFailure
        Right r -> B.putStrLn r

--
-- Called by the command line interface.
filterFunc = setErrorMsg . parseAndEncode

-- Update the error message, but retain the Either monad so we
-- can recognize failure and return the proper exit code.
setErrorMsg = either (\_ -> Left errorMsg) Right
errorMsg    = "{\"error\":\"Cannot parse input\"}"

-- Parser driver that shows the results and all the unconsumed input.
pTest :: Parser a -> Text -> Either String (a, Text)
pTest p = parseOnly $ do
    x <- p
    y <- takeText
    return (x,y)

header :: Parser Text
header = do
    manyTill anyChar (string $ T.pack "transform=\"matrix(")
    numbers  <- takeTill (')' ==)
    return numbers

matrix :: Parser [Number]
matrix = sepBy number (char ',')

parseInput :: Text -> Either String [Number]
parseInput inp = parseOnly header inp >>= parseOnly matrix

--parseAndEncodeFiling :: Text -> Either String Text
parseAndEncode input = parseInput input >>= computeDims >>= return . A.encode

computeDims :: [Number] -> Either String [Number]
computeDims [] = Left "Failed to parse transform matrix: no numbers"
computeDims ns
    | length ns /= 6 = Left $ "Failed to parse transform matrix: wanted 6 numbers, found " ++ show (length ns)
    | otherwise = case ns of
        [_,_,_,D a,D b,D c] -> Right [D (2 * b / a), D (2 * c / a)]


input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
\<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n\
    \\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n\
\<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" version=\"1.1\" width=\"400.0\" height=\"496.07153306070745\" font-size=\"1\" viewBox=\"0 0 400 496\"><g><g stroke=\"rgb(0,0,0)\" stroke-opacity=\"1.0\" fill=\"rgb(0,0,0)\" fill-opacity=\"0.0\" stroke-width=\"1.0e-2\" stroke-linecap=\"butt\" stroke-linejoin=\"miter\" font-size=\"1.0em\" stroke-miterlimit=\"10.0\"><g transform=\"matrix(56.24393798874236,0.0,0.0,56.24393798874236,200.0,248.03576653035373)\"><g><g fill-opacity=\"0.0\" stroke-width=\"1.0e-2\"><path d=\"M 0.7408180424415279,2.6239276999457912 l -0.5247032922923733,-1.7526807501355268e-2 \" /></g><g fill-opacity=\"0.0\" stroke-width=\"1.0e-2\"><path d=\"M 0.7408180424415279,2.6239276999457912 l -0.3257609583052711,-3.5053615002710536e-2 \" /></g><g fill-opacity=\"0.0\" stroke-width=\"1.0e-2\"><path d=\"M 0.7408180424415279,2.6239276999457912 l 0.19894233398710226,-1.7526807501355268e-2 \" /></g><g fill=\"rgb(0,0,0)\" fill-opacity=\"1.0\" stroke-width=\"1.0e-2\"><path d=\"M 1.175165986848556,2.6706658532827383 l"
