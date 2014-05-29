{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Math.Stan where

import Data.List

type Id = String


data Module = Module
              {  moduleData :: [(Id,T)]
              ,  moduleParameters :: [(Id,T)]
              ,  moduleModel :: Model
              }


data Model = Model
             { localVars :: [(Id,T)]
             , declarations :: [D]
             }

data D = For Id E E [D]
       | Sample Pat E
       | Assign Pat E

data E = EVar Id
       | EApp Id [E]
       | EBin Id E E


data T = T { baseT :: TBase
           , bounds :: (Maybe E, Maybe E)
           , dimensions :: [E]
           }

data TBase = TInt
           | TReal
           | TVector E


type Pat = (Id, [E]) -- identifier, indicies

class Pretty a where
  pp :: a -> String
  pp = ppIndent 0

  ppIndent :: Int -> a -> String
  ppIndent n x = unlines $ map ((replicate n ' ')++) $ lines $ pp x

instance Pretty E where
  pp (EVar id) = id
  pp (EApp id args) = id++"("++intercalate "," (map pp args)++")"
  pp (EBin id l r) = "("++pp l++id++pp r++")"

instance Pretty Pat where
  pp (id, ixs) = id++concatMap (\ix-> "["++pp ix++"]") ixs

instance Pretty TBase where
  pp TInt = "int"
  pp TReal = "real"
  pp (TVector dim) = "vector["++pp dim++"]"
