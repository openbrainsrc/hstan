{- |
Monadic writer for Stan programs

-}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FunctionalDependencies, OverloadedStrings #-}

module Math.Stan.Writer where

import Control.Monad.State.Strict
import Math.Stan.AST
import Data.String

infixl 7 !
infixl 0 .=
infixl 1 .:


------------------------------------------------
----  Typed Expressions
-----------------------------------------------



newtype Expr a = Expr { unExpr :: E }

newtype Prob a = Prob E

newtype Pat a = Pat P

newtype TyT a = TyT T --typed type

instance IsString (Pat a) where
  fromString nm = Pat (nm,[])

instance IsString (Expr a) where
  fromString nm = Expr $ EVar nm 


instance Num a => Num (Expr a) where
   (Expr e1) + (Expr e2) = Expr $ EBin "+" e1 e2
   (Expr e1) - (Expr e2) = Expr $ EBin "-" e1 e2
   (Expr e1) * (Expr e2) = Expr $ EBin "*" e1 e2
   abs (Expr e) = Expr $ EApp "abs" [e]
   fromInteger  = Expr . EInt . fromInteger 

instance Fractional a => Fractional (Expr a) where
   (Expr e1) / (Expr e2) = Expr $ EBin "/" e1 e2
   fromRational = Expr . EReal . fromRational

class Indexable a b | a -> b, b -> a where
  (!) :: a -> Expr Int -> b

instance Indexable (Pat [a]) (Pat a) where 
   (Pat (nm, ixs)) ! moreIx = Pat (nm,ixs++[unExpr moreIx])

instance Indexable (Expr [a]) (Expr a) where 
   (Expr e) ! ix = Expr $ EIx e [unExpr ix]

instance Indexable (TyT a) (TyT [a]) where 
   (TyT (T base bnds dims)) ! (Expr dime) = TyT $ T base bnds $ dims++[dime]

int :: TyT Int
int = TyT $ T TInt (Nothing, Nothing) []

real :: TyT Double
real = TyT $ T TReal (Nothing, Nothing) []

vec :: Expr Int -> TyT [a]
vec (Expr n) = TyT $ T (TVector n) (Nothing, Nothing) []

(.:) :: Id -> TyT a -> (Id,T)
ident .: (TyT t)  = (ident,t)


local :: TyT a -> Stan (Expr a)
local (TyT t) = do
  ident <- fresh
  tell [LocalVar ident t]
  return $ Expr $ EVar ident


------------------------------------------------
----  Stan Monad
-----------------------------------------------


data StanState = StanState { decls :: [D],
                             supply :: Int }


tell :: [D] -> Stan ()
tell ds = modify $ \s -> s { decls = decls s ++ ds }

fresh :: Stan Id
fresh = do
  ix <- fmap supply get
  modify $ \s -> s { supply = ix+1 }
  return $ "i"++show ix

type Stan = State StanState

stanModel :: Stan a -> [D]
stanModel mx= decls $ execState mx (StanState [] 0) 

localDs :: Stan a -> Stan [D]
localDs mx = do
  originalDs <- fmap decls get
  modify $ \s -> s { decls = [] }
  _ <- mx
  newds <- fmap decls get
  modify $ \s -> s { decls = originalDs }
  return newds





normal :: (Expr Double, Expr Double) -> Prob Double
normal (Expr m, Expr sd) = Prob $ EApp "normal" [m, sd]

gamma :: (Expr Double, Expr Double) -> Prob Double
gamma (Expr a, Expr b) = Prob $ EApp "gamma" [a, b]


stoch :: Pat a -> Prob a -> Stan (Expr a)
stoch (Pat p) (Prob dist) = do
  tell [Stoch p dist]
  return $ pToExpr p 

(.=) :: Expr a -> Expr a -> Stan ()
(Expr p) .= (Expr e) = do
  tell [Det p e]
  return ()


for :: Expr Int -> Expr Int -> (Expr Int -> Stan a) -> Stan ()
for (Expr lo) (Expr hi) body = do
  i <- fresh
  ds <- localDs $ body $ Expr (EVar i)
  tell [For i lo hi ds]
  return ()
  
estimate :: Stan a -> [(Id, T)] -> Program
estimate model vs = Program vs [] $ stanModel model

pToExpr :: P -> Expr a
pToExpr (nm,[]) = Expr $ EVar nm
pToExpr (nm,ixs) = Expr $ EIx (EVar nm) ixs 


