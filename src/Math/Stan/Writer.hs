{- |
Monadic writer for Stan programs

-}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FunctionalDependencies, OverloadedStrings, UndecidableInstances #-}

module Math.Stan.Writer where

import Control.Monad.State.Strict
import Math.Stan.AST
import Math.Stan.TAST
import Data.String
import Data.List

infixl 0 .=



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

runStanM :: Stan a -> ([D], a)
runStanM mx= let (x, st) = runState mx (StanState [] 0)
              in (decls st, x)

localDs :: Stan a -> Stan ([D], a)
localDs mx = do
  originalDs <- fmap decls get
  modify $ \s -> s { decls = [] }
  x <- mx
  newds <- fmap decls get
  modify $ \s -> s { decls = originalDs }
  return (newds, x)





normal :: (Expr Double, Expr Double) -> Prob Double
normal (Expr _ m, Expr _ sd) = Prob (fromBase TReal) $ EApp "normal" [m, sd]

uniform :: (Expr Double, Expr Double) -> Prob Double
uniform (Expr _ lo, Expr _ hi) = Prob (fromBase TReal) $ EApp "uniform" [lo, hi]

gamma :: (Expr Double, Expr Double) -> Prob Double
gamma (Expr _ a, Expr _  b) = Prob (fromBase TReal) $ EApp "gamma" [a, b]


stoch :: Pat a -> Prob a -> Stan (Expr a)
stoch (Pat p) (Prob t dist) = do
  tell [Stoch p dist]
  return $ pToExpr p t

(.=) :: Expr a -> Expr a -> Stan ()
(Expr _ p) .= (Expr _ e) = do
  tell [Det p e]
  return ()


for :: LoopWrap a b => Expr Int -> Expr Int -> (Expr Int -> Stan a) -> Stan b
for elo@(Expr _ lo) ehi@(Expr _ hi) body = do
  i <- fresh "i"
  (ds,x) <- localDs $ body $ Expr (fromBase TInt) (EVar i)
  tell [For i lo hi ds]
  return $ loopwrap (ehi-elo) x


local :: TyT a -> Stan (Expr a)
local (TyT t) = do
  ident <- fresh "v"
  tell [LocalVar ident t]
  return $ Expr t $ EVar ident


data ProgT a = ProgT { modelT :: Stan a,
                       parametersT :: [(Id, T)] }

data StanExec a = StanExec { execFilePath :: String,
                             execModel :: Stan a,
                             execParams :: [(Id, T)] }

compile :: Eval a b => Stan a -> [(Id, T)] -> IO (StanExec a)
compile m pars = do
  let (ds, obs) = runStanM m

  putStrLn $ pp $ Program (collect obs) pars ds
  return $ StanExec "" m pars

estimate :: (Eval a b) => StanExec a -> b -> IO [(Id, [Value])]
estimate (StanExec pth m params) obsData = do
  let (_, obs) = runStanM m
  putStrLn $ dump obs obsData
  return []

class Eval a b | a -> b where
  collect :: a -> [(Id,T)]

  dump :: a -> b -> String

instance Dump1 a => Eval (Expr a) a where
  collect (Expr t (EIx (EVar ident) _ )) = [(ident, t)]
  collect (Expr t (EVar ident)) = [(ident, t)]

  dump (Expr t (EIx e _)) x = dump (Expr t e) x
  dump (Expr t (EVar ident)) x = ident++"<-"++(dumpAll $ dump1 x)++"\n"

instance (Eval a b, Eval c d) => Eval (a,c) (b,d) where
  collect (e1, e2) = collect e1 ++ collect e2

  dump (e1, e2) (x1,x2)= dump e1 x1 ++ dump e2 x2


class Dump1 a where
  dump1 :: a -> ([String], [Int])

instance Dump1 Double where
    dump1 x = ([show x], [])
instance Dump1 Int where
    dump1 x = ([show x], [])
instance Dump1 Bool where
    dump1 True = (["1"], [])
    dump1 False = (["0"], [])

--dump1c xs = concat $ "c(" : intersperse "," (map dump1 xs) ++[")"]

instance Dump1 a => Dump1 [a] where
    dump1 xs = let xss = map dump1 xs                    
               in (concatMap fst xss, length xss:(snd $ head xss) ) 


dumpAll :: ([String], [Int]) -> String
dumpAll ([s], []) = s
dumpAll (ss, [n]) = concat $ "c(" : intersperse "," ss ++[")"]
dumpAll (ss, ns) = concat $ "structure(c(" : intersperse "," ss ++["), .Dim = c("]
                             ++intersperse "," (map show ns)++[")"]
