{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Math.Stan where

import Data.List

type Id = String


data Module = Module
              {  moduleData :: [(Id,T)]
              ,  moduleParameters :: [(Id,T)]
              ,  moduleModel :: Model
              }

newtype Model = Model [D]

data D = For Id E E [D]
       | Sample Pat E
       | Assign Pat E
       | LocalVar Id T

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
  pp (EVar nm) = nm
  pp (EApp nm args) = nm++"("++intercalate "," (map pp args)++")"
  pp (EBin nm l r) = "("++pp l++nm++pp r++")"

instance Pretty Pat where
  pp (nm, ixs) = nm++ppIxs ixs

instance Pretty TBase where
  pp TInt = "int"
  pp TReal = "real"
  pp (TVector dim) = "vector["++pp dim++"]"

instance Pretty (Id, T) where
  pp (nm, (T base tbounds dims)) = pp base ++ ppBounds tbounds ++ " "++nm++ppIxs dims++";"

instance Pretty D where
  pp (Sample p e) = pp p ++ " ~ "++ pp e ++";"
  pp (Assign p e) = pp p ++ " <- "++ pp e ++";"
  pp (LocalVar nm t) = pp (nm,t)
  pp (For nm from to ds) = inBlock ("for (" ++ nm++" in "++pp from++":"++pp to++")") ds 

instance Pretty a => Pretty [a] where 
  pp ds = unlines $ map pp ds

instance Pretty Model where
  pp (Model ds) = inBlock "model" ds

instance Pretty Module where
  pp (Module thedata params model) = unlines $ [
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
