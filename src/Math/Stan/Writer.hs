{- |
Monadic writer for Stan programs

-}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FunctionalDependencies, OverloadedStrings, UndecidableInstances #-}

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

newtype Vector = Vector [Double] 

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

{-instance Indexable (Expr Vector) (Expr Double) where 
   (Expr e) ! ix = Expr $ EIx e [unExpr ix] -}

instance Indexable (TyT a) (TyT [a]) where 
   (TyT (T base bnds dims)) ! (Expr dime) = TyT $ T base bnds $ dims++[dime]

int :: TyT Int
int = TyT $ T TInt (Nothing, Nothing) []

real :: TyT Double
real = TyT $ T TReal (Nothing, Nothing) []

vec :: Expr Int -> TyT Vector
vec (Expr n) = TyT $ T (TVector n) (Nothing, Nothing) []

(.:) :: Id -> TyT a -> (Id,T)
ident .: (TyT t)  = (ident,t)


local :: TyT a -> Stan (Expr a)
local (TyT t) = do
  ident <- fresh "v"
  tell [LocalVar ident t]
  return $ Expr $ EVar ident


------------------------------------------------
----  Stan Monad
-----------------------------------------------


data StanState = StanState { decls :: [D],
                             supply :: Int }


tell :: [D] -> Stan ()
tell ds = modify $ \s -> s { decls = decls s ++ ds }

fresh :: Id -> Stan Id
fresh base = do
  ix <- fmap supply get
  modify $ \s -> s { supply = ix+1 }
  return $ base++show ix

type Stan = State StanState

stanModel :: Stan a -> [D]
stanModel mx= decls $ execState mx (StanState [] 0) 

localDs :: Stan a -> Stan ([D], a)
localDs mx = do
  originalDs <- fmap decls get
  modify $ \s -> s { decls = [] }
  x <- mx
  newds <- fmap decls get
  modify $ \s -> s { decls = originalDs }
  return (newds, x)





normal :: (Expr Double, Expr Double) -> Prob Double
normal (Expr m, Expr sd) = Prob $ EApp "normal" [m, sd]

uniform :: (Expr Double, Expr Double) -> Prob Double
uniform (Expr lo, Expr hi) = Prob $ EApp "uniform" [lo, hi]

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


for :: Wrappable a b => Expr Int -> Expr Int -> (Expr Int -> Stan a) -> Stan b
for (Expr lo) (Expr hi) body = do
  i <- fresh "i"
  (ds,x) <- localDs $ body $ Expr (EVar i)
  tell [For i lo hi ds]
  return $ wrap x
  
pToExpr :: P -> Expr a
pToExpr (nm,[]) = Expr $ EVar nm
pToExpr (nm,ixs) = Expr $ EIx (EVar nm) ixs 

stan :: Stan a -> Program
stan model = Program [] [] $ stanModel model

class Wrappable a b | a -> b  where
  wrap :: a -> b

instance Wrappable (Expr a) (Expr [a]) where
  wrap (Expr e) = (Expr e)

instance (Wrappable a b, Wrappable c d) => Wrappable (a,c) (b,d) where
  wrap (x,y) = (wrap x, wrap y)
  
data ProgT a = ProgT { modelT :: Stan a,
                       parametersT :: [(Id, T)] }

data StanExec a = StanExec { execFilePath :: String,
                             execParams :: [(Id, T)] }

compile :: ProgT a -> IO (StanExec a)
compile = undefined

estimate :: StanExec a -> IO [(Id, [Value])]
estimate = undefined
