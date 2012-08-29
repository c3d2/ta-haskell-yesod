{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Data.Conduit
import qualified Data.Conduit.Binary as CB
import System.Environment (getArgs)
import Control.Monad.Trans
import qualified Data.ByteString as B


copyFile :: FilePath -> FilePath -> IO ()
copyFile src dest = runResourceT $
                    CB.sourceFile src $=
                    countBytes $$ 
                    CB.sinkFile dest

main = getArgs >>= \[src, dest] ->
       copyFile src dest

countBytes :: MonadIO m => Conduit B.ByteString m B.ByteString
countBytes = 
    let loop !total = 
            do mBuf <- await
               case mBuf of
                 Just buf -> 
                     do yield buf
                        loop $ total + B.length buf
                 Nothing ->
                     do liftIO $ putStrLn $ 
                          "Transferred " ++ 
                          show total ++ 
                          " bytes"
                        return ()
             in loop 0
