{- |
Monadic writer for Stan programs

-}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FunctionalDependencies, OverloadedStrings #-}

module Math.Stan.Writer where

import Control.Monad.Writer.Strict
import Math.Stan.AST
import Data.String


newtype Expr a = Expr { unExpr :: E }

newtype Prob a = Prob E

newtype Pat a = Pat P

instance IsString (Pat a) where
  fromString nm = Pat (nm,[])


instance IsString (Expr a) where
  fromString nm = Expr $ EVar nm


instance Num a => Num (Expr a) where
   (Expr e1) + (Expr e2) = Expr $ EBin "+" e1 e2
   (Expr e1) - (Expr e2) = Expr $ EBin "-" e1 e2
   (Expr e1) * (Expr e2) = Expr $ EBin "*" e1 e2
   fromInteger i = Expr $ EInt $ fromInteger i


class Indexable a b | a -> b, b -> a where
  (!) :: a -> Expr Int -> b

instance Indexable (Pat [a]) (Pat a) where 
   (Pat (nm, ixs)) ! moreIx = Pat (nm,ixs++[unExpr moreIx])

instance Indexable (Expr [a]) (Expr a) where 
   (Expr e) ! ix = Expr $ EIx e [unExpr ix]

type Stan = Writer [D]

stanModel :: Stan a -> [D]
stanModel = execWriter



normal :: (Expr Double, Expr Double) -> Prob Double
normal (Expr m, Expr sd) = Prob $ EApp "normal" [m, sd]

gamma :: (Expr Double, Expr Double) -> Prob Double
gamma (Expr a, Expr b) = Prob $ EApp "gamma" [a, b]


stoch :: Pat a -> Prob a -> Stan (Expr a)
stoch (Pat p) (Prob dist) = do
  tell [Stoch p dist]
  return $ pToExpr p 

det :: Pat a -> Expr a -> Stan (Expr a)
det (Pat p) (Expr e) = do
  tell [Det p e]
  return $ pToExpr p 


for :: Expr Int -> Expr Int -> (Expr Int -> Stan a) -> Stan ()
for (Expr lo) (Expr hi) body = do
  let ds = execWriter (body "i")
  tell [For "i" lo hi ds]
  return ()
  
data Value = Value

estimate :: Stan a -> [(String, Value)] -> String
estimate model vs = ""

pToExpr :: P -> Expr a
pToExpr (nm,[]) = Expr $ EVar nm
pToExpr (nm,ixs) = Expr $ EIx (EVar nm) ixs


