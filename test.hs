{-# LANGUAGE OverloadedStrings #-}

import Math.Stan.Writer
import Math.Stan.AST 

mymodel = do
   y <- local $ real!100

   m  <- stoch "m"  $ normal (0, 1)
   sd <- stoch "sd" $ gamma (1, 1)

   for 1 100 $ \i -> do

     y!i .= m + sd

     stoch ("x"!i) $ normal (y!1*5, sd)

main = putStrLn $ pp $ estimate mymodel ["x" .: vec 100, 
                                         "y" .: int!100] 
