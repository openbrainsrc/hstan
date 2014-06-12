{-# LANGUAGE OverloadedStrings #-}

import Math.Stan.Writer
import Math.Stan.AST
import Math.Stan.TAST


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

main = do compiledModel <- compile mymodel ["m"  .: real,
                                            "sd" .: real]
          pars <- estimate compiledModel ([1.2,2.3], [3.4, 4.5])
          return ()
