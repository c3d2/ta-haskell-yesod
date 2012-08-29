{-# LANGUAGE TemplateHaskell #-}
 
-- Import our template "printf"
import PrintF (printf)
 
-- The splice operator $ takes the Haskell source code
-- generated at compile time by "printf" and splices it into
-- the argument of "putStrLn".
main = do
    putStrLn $ $(printf "Hello %s %%x%% %d %%x%%") "World" 12
