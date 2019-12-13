module Main where

import Control.Monad (forM)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List.Split (splitOn)
import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)
import Text.Parsec (parse)
import TerraformParser (tokenizeTF, parseTF, TokOccur(TokOccur), TfToken(TfSep))
import TerraformJson (tfToJson)

main = do
  fileNames <- getArgs
  interpretations <- forM fileNames $ \fileName -> do
    input <- readFile fileName
    hPutStrLn stderr fileName
    return $ parseTF fileName input
  BS.putStrLn . tfToJson $ concat interpretations
