{-# LANGUAGE OverloadedStrings #-}

import Math.Stan.Writer
import Math.Stan.AST 

main = putStrLn $ pp $ Program [] [] $ stanModel $ do
   m  <- stoch "m"  $ normal 0 1
   sd <- stoch "sd" $ gamma 1 1
   return ()
