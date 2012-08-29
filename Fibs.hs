{-# LANGUAGE TemplateHaskell #-}

module Fibs where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibQ :: Int -> Q Exp
fibQ n = [| $( lift $ fibs !! n) |]
