module Main where

import System.IO
import Network.Wai.Handler.Warp (runSettings, defaultSettings, 
                                 settingsHost, settingsPort, settingsOnException)
import Data.Conduit.Network (HostPreference (HostIPv6))
import Application


main :: IO ()
main = app >>=
       runSettings (defaultSettings
                    { settingsHost = HostIPv6
                    , settingsPort = 8000
                    , settingsOnException = \e ->
                                            hPrint stdout e
                                            >>
                                            hFlush stdout
                    })
