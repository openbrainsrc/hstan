{- |
Low-level untyped abstract syntax tree and pretty printer for Stan programs

-}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Math.Stan.AST where

import Data.List
import Data.String

type Id = String

data Program = Program
              {  observations :: [(Id,T)]
              ,  parameters :: [(Id,T)]
              ,  stanModelDs :: [D]
              }

data D = For Id E E [D]
       | Stoch P E
       | Det E E
       | LocalVar Id T
  deriving (Show)

data E = EVar Id
       | EApp Id [E]
       | EBin Id E E
       | EIx E [E]
       | EInt Int
       | EReal Double
  deriving (Show)


data T = T { baseT :: TBase
           , bounds :: (Maybe E, Maybe E)
           , dimensions :: [E]
           }
  deriving (Show)

data TBase = TInt
           | TReal
           | TVector E
  deriving (Show)

fromBase :: TBase -> T
fromBase tb = T tb (Nothing, Nothing) []

reduceDims :: T -> T
reduceDims (T (TVector vdims) bnds []) = T TReal bnds []
reduceDims (T base bnds []) = T base bnds []
reduceDims (T base bnds dims) = T base bnds (init dims)

addDim :: E -> T -> T
addDim e (T base bnds dims) = T base bnds (dims++[e])
type P = (Id, [E]) -- pattern; identifier, indicies


class Pretty a where
  pp :: a -> String

ppIndent :: Pretty a => Int -> a -> String
ppIndent n = unlines . map ((replicate n ' ')++) . lines . pp

instance Pretty E where
  pp (EVar nm) = nm
  pp (EApp nm args) = nm++"("++intercalate "," (map pp args)++")"
  pp (EBin nm l r) = "("++pp l++nm++pp r++")"
  pp (EIx e ixs) = pp e ++ ppIxs ixs
  pp (EInt i) = show i
  pp (EReal x) = show x

instance Pretty P where
  pp (nm, ixs) = nm++ppIxs ixs

instance Pretty TBase where
  pp TInt = "int"
  pp TReal = "real"
  pp (TVector dim) = "vector["++pp dim++"]"

instance Pretty (Id, T) where
  pp (nm, (T base tbounds dims)) = pp base ++ ppBounds tbounds ++ " "++nm++ppIxs dims++";"

instance Pretty D where
  pp (Stoch p e) = pp p ++ " ~ "++ pp e ++";"
  pp (Det p e) = pp p ++ " <- "++ pp e ++";"
  pp (LocalVar nm t) = pp (nm,t)
  pp (For nm from to ds) = inBlock ("for (" ++ nm++" in "++pp from++":"++pp to++")") ds

instance Pretty a => Pretty [a] where
  pp ds = unlines $ map pp ds

instance Pretty Program where
  pp (Program thedata params model) = unlines $ [
                                      inBlock "data" thedata,
                                      inBlock "parameters" params,
                                      inBlock "model" model ]

inBlock :: Pretty a => String -> a -> String
inBlock nm conts = nm ++ " {\n"++ppIndent 2 conts++ "\n}"

ppBounds :: (Maybe E, Maybe E) -> String
ppBounds (Nothing, Nothing) = ""
ppBounds (Just lo, Nothing) = "<lower="++pp lo++">"
ppBounds (Nothing, Just hi) = "<upper="++pp hi++">"
ppBounds (Just lo, Just hi) = "<lower="++pp lo++", upper="++pp hi++">"

ppIxs :: [E] -> String
ppIxs = concatMap (\ix-> "["++pp ix++"]")

compileStan :: Program -> String -> String -> IO ()
compileStan pgm modelName stanDir  = return ()

data Value = VInt Int
           | VReal Double
           | VArray [Value]

instance Num Value where
  fromInteger = VInt . fromInteger

instance Fractional Value where
  fromRational = VReal . fromRational
