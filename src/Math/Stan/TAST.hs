{- |
Typed AST for Stan programs

-}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FunctionalDependencies, OverloadedStrings, UndecidableInstances #-}

module Math.Stan.TAST where

import Control.Monad.State.Strict
import Math.Stan.AST
import Data.String
import Data.List

------------------------------------------------
----  Typed Expressions
-----------------------------------------------



data Expr a = Expr { exprTy:: T,
                     unExpr :: E }
  deriving (Show)

data Prob a = Prob T E
  deriving (Show)

newtype Pat a = Pat P
  deriving (Show)

newtype TyT a = TyT T --typed type
  deriving (Show)

instance IsString (Pat a) where
  fromString nm = Pat (nm,[])

{-instance IsString (Expr a) where
  fromString nm = Expr $ EVar nm -}


instance Num a => Num (Expr a) where
   (Expr t e1) + (Expr _ e2) = Expr t $ EBin "+" e1 e2
   (Expr t e1) - (Expr _ e2) = Expr t $ EBin "-" e1 e2
   (Expr t e1) * (Expr _ e2) = Expr t $ EBin "*" e1 e2
   abs (Expr t e) = Expr t$ EApp "abs" [e]
   fromInteger  = Expr (fromBase TInt) . EInt . fromInteger

instance Fractional a => Fractional (Expr a) where
   (Expr t e1) / (Expr _ e2) = Expr t $ EBin "/" e1 e2
   fromRational = Expr (fromBase TReal) . EReal . fromRational


infixl 7 !

class Indexable a b | a -> b, b -> a where
  (!) :: a -> Expr Int -> b

instance Indexable (Pat [a]) (Pat a) where
   (Pat (nm, ixs)) ! moreIx = Pat (nm,ixs++[unExpr moreIx])

instance Indexable (Expr [a]) (Expr a) where
   (Expr t e) ! ix = Expr (reduceDims t) $ EIx e [unExpr ix]

instance Indexable (TyT a) (TyT [a]) where
   (TyT (T base bnds dims)) ! (Expr _ dime) = TyT $ T base bnds $ dims++[dime]

int :: TyT Int
int = TyT $ T TInt (Nothing, Nothing) []

real :: TyT Double
real = TyT $ T TReal (Nothing, Nothing) []

vec :: Expr Int -> TyT [Double]
vec (Expr t n) = TyT $ T (TVector n) (Nothing, Nothing) []

infixl 1 .:


(.:) :: Id -> TyT a -> (Id,T)
ident .: (TyT t)  = (ident,t)


pToExpr :: P -> T -> Expr a
pToExpr (nm,[]) t = Expr t $ EVar nm
pToExpr (nm,ixs) t = Expr t $ EIx (EVar nm) ixs

class LoopWrap a b | a -> b  where
  loopwrap :: Expr Int -> a -> b

instance LoopWrap (Expr a) (Expr [a]) where
  loopwrap (Expr _ n) (Expr t e) = Expr (addDim n t) e

instance (LoopWrap a b, LoopWrap c d) => LoopWrap (a,c) (b,d) where
  loopwrap e (x,y) = (loopwrap e x, loopwrap e y)
