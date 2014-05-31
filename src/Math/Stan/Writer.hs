{- |
Monadic writer for Stan programs

-}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Math.Stan.Writer where

import Control.Monad.Writer.Strict
import Math.Stan.AST
import Data.String


newtype Expr a = Expr { unExpr :: E }

newtype Prob a = Prob E

newtype Pat a = Pat P

instance IsString (Pat a) where
  fromString nm = Pat (nm,[])


class Indexable a where
  (!) :: a -> [Expr Int] -> a

instance Indexable (Pat a) where 
   (Pat (nm, ixs)) ! moreIxs = Pat (nm,ixs++map unExpr moreIxs)

instance Indexable (Expr a) where 
   (Expr e) ! ixs = Expr $ EIx e $ map unExpr ixs

type Stan = Writer [D]

stanModel :: Stan a -> [D]
stanModel = execWriter



normal :: Expr Double -> Expr Double -> Prob Double
normal (Expr m) (Expr sd) = Prob $ EApp "normal" [m, sd]

gamma :: Expr Double -> Expr Double -> Prob Double
gamma (Expr a) (Expr b) = Prob $ EApp "gamma" [a, b]


stoch :: Pat a -> Prob a -> Stan (Expr a)
stoch (Pat p) (Prob dist) = do
  tell [Stoch p dist]
  return $ pToExpr p 

det :: Pat a -> Expr a -> Stan (Expr a)
det (Pat p) (Expr e) = do
  tell [Det p e]
  return $ pToExpr p 


pToExpr :: P -> Expr a
pToExpr (nm,[]) = Expr $ EVar nm
pToExpr (nm,ixs) = Expr $ EIx (EVar nm) ixs


