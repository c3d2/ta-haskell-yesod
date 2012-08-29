{-# LANGUAGE TemplateHaskell #-}

module Main where
import Language.Haskell.TH
import Fibs


main = print $(fibQ 100000)
