{-# LANGUAGE OverloadedStrings #-}

import Math.Stan.Writer
import Math.Stan.AST 


mymodel :: Stan (Expr [Double], Expr [Double])
mymodel = do
--   y <- local $ real!100

   m  <- stoch "m"  $ normal (0, 1)
   sd <- stoch "sd" $ gamma (1, 1)

   for 1 100 $ \i -> do

--     y!i .= m + sd
     x <- stoch ("x"!i) $ uniform (0, 1)
     y <- stoch ("y"!i) $ normal (m, sd)

     return (x,y)

main = putStrLn $ pp $ (stan mymodel) { observations = ["x" .: real!100,
                                                        "y" .: real!100],
                                        parameters = ["m"  .: real, 
                                                      "sd" .: real] }
