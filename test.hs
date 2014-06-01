{-# LANGUAGE OverloadedStrings #-}

import Math.Stan.Writer
import Math.Stan.AST 

model = do
   m  <- stoch "m"  $ normal (0, 1)
   sd <- stoch "sd" $ gamma (1, 1)
   for 1 100 $ \i -> do
     stoch ("x"!i) $ normal (m, sd)

main = putStrLn $ pp $ estimate model [("x", T (TVector $ EInt 100) (Nothing, Nothing) [])] 
