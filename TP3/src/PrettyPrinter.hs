module PrettyPrinter
  ( printTerm
  ,     -- pretty printer para terminos
    printType     -- pretty printer para tipos
  )
where

import           Common
import           Text.PrettyPrint.HughesPJ
import           Prelude                 hiding ( (<>) )
-- lista de posibles nombres para variables
vars :: [String]
vars =
  [ c : n
  | n <- "" : map show [(1 :: Integer) ..]
  , c <- ['x', 'y', 'z'] ++ ['a' .. 'w']
  ]

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

-- pretty-printer de términos

pp :: Int -> [String] -> Term -> Doc
pp ii vs (Bound k         ) = text (vs !! (ii - k - 1))
pp _  _  (Free  (Global s)) = text s

pp ii vs (i :@: c         ) = sep
  [ parensIf (isLam i) (pp ii vs i)
  , nest 1 (parensIf (isLam c || isApp c || isLet c || isAs c) (pp ii vs c))
  ]
pp ii vs (Lam t c) =
  text "\\"
    <> text (vs !! ii)
    <> text ":"
    <> printType t
    <> text ". "
    <> pp (ii + 1) vs c
pp ii vs (Let t u) = sep 
  [ text "let "
  , text (vs !! ii)
  , text " = "
  , parensIf (isLam t || isApp t || isLet t || isAs t) (pp ii vs t)
  , text " in "
  , pp (ii + 1) vs u
  ]
pp ii vs (As t tt) = 
  parensIf (isLam t || isApp t || isLet t || isAs t) (pp ii vs t) 
    <> text " as " 
    <> printType tt 
pp _ _ Unit = text "unit"
pp ii vs (Pair t u) = parens $ (pp ii vs t) <> text ", " <> (pp ii vs u) 
pp ii vs (Fst t) = text "fst " <> parensIf (isLam t || isApp t || isLet t || isAs t) (pp ii vs t)
pp ii vs (Snd t) = text "snd " <> parensIf (isLam t || isApp t || isLet t || isAs t) (pp ii vs t)

isLam :: Term -> Bool
isLam (Lam _ _) = True
isLam _         = False

isApp :: Term -> Bool
isApp (_ :@: _) = True
isApp _         = False

isLet :: Term -> Bool
isLet (Let _ _) = True
isLet _         = False

isAs :: Term -> Bool
isAs (As _ _) = True
isAs _        = False

-- pretty-printer de tipos
printType :: Type -> Doc
printType EmptyT = text "E"
printType UnitT = text "Unit"
printType (FunT t1 t2) =
  sep [parensIf (isFun t1) (printType t1), text "->", printType t2]
printType (PairT t1 t2) = 
  parens $ sep [parensIf (isFun t1) (printType t1), text ",", printType t2]


isFun :: Type -> Bool
isFun (FunT _ _) = True
isFun _          = False

fv :: Term -> [String]
fv (Bound _         ) = []
fv (Free  (Global n)) = [n]
fv (t   :@: u       ) = fv t ++ fv u
fv (Lam _   u       ) = fv u
fv (Let t u         ) = fv u
fv (As t _         )  = fv t
fv Unit               = []
fv (Pair t u)         = fv t ++ fv u
fv (Fst t)            = fv t
fv (Snd t)            = fv t

---
printTerm :: Term -> Doc
printTerm t = pp 0 (filter (\v -> not $ elem v (fv t)) vars) t

