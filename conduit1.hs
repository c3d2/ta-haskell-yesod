module Main (main) where

import Data.Conduit
import qualified Data.Conduit.Binary as CB
import System.Environment (getArgs)


copyFile :: FilePath -> FilePath -> IO ()
copyFile src dest = runResourceT $ CB.sourceFile src $$ CB.sinkFile dest

main = getArgs >>= \[src, dest] ->
       copyFile src dest
