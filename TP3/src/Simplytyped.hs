module Simplytyped
  ( conversion
  ,    -- conversion a terminos localmente sin nombre
    eval
  ,          -- evaluador
    infer
  ,         -- inferidor de tipos
    quote          -- valores -> terminos
  )
where

import           Data.List
import           Data.Maybe
import           Prelude                 hiding ( (>>=) )
import           Text.PrettyPrint.HughesPJ      ( render )
import           PrettyPrinter
import           Common

-- conversion a términos localmente sin nombres
conversion :: LamTerm -> Term
conversion = conversion' []

conversion' :: [String] -> LamTerm -> Term
conversion' b (LVar n )  = maybe (Free (Global n)) Bound (n `elemIndex` b)
conversion' b (LApp t u) = conversion' b t :@: conversion' b u
conversion' b (LAbs n t u )  = Lam t (conversion' (n : b) u)
conversion' b (LLet s t1 t2) = Let (conversion' b t1) (conversion' (s:b) t2)
conversion' b (LAs t tt)     = As (conversion' b t) tt
conversion' _ LUnit = Unit
conversion' b (LPair t1 t2) = Pair (conversion' b t1) (conversion' b t2)
conversion' b (LFst t) = Fst (conversion' b t)
conversion' b (LSnd t) = Snd (conversion' b t)
conversion' _ LZero = Zero
conversion' b (LSuc t) = Suc (conversion' b t)
conversion' b (LRec t1 t2 t3) = Rec (conversion' b t1) (conversion' b t2) (conversion' b t3)

-----------------------
--- eval
----------------------- 

sub :: Int -> Term -> Term -> Term
sub i t (Bound j) | i == j    = t
sub _ _ (Bound j) | otherwise = Bound j
sub _ _ (Free n   )           = Free n
sub i t (u   :@: v)           = sub i t u :@: sub i t v
sub i t (Lam t'  u)           = Lam t' (sub (i + 1) t u)
sub i t (Let t1 t2)           = Let t1 (sub (i + 1) t t2)
sub i t (As u tu)             = As (sub i t u) tu
sub _ _ Unit                  = Unit
sub i t (Pair t1 t2)          = Pair (sub i t t1) (sub i t t2)
sub i t (Fst u)               = Fst (sub i t u)
sub i t (Snd u)               = Snd (sub i t u)
sub _ _ Zero                  = Zero
sub i t (Suc u)               = Suc (sub i t u)
sub i t (Rec t1 t2 t3)        = Rec (sub i t t1) (sub i t t2) (sub i t t3)

-- evaluador de términos
eval :: NameEnv Value Type -> Term -> Value
eval _ (Bound _             ) = error "variable ligada inesperada en eval"
eval e (Free  n             ) = fst $ fromJust $ lookup n e
eval _ (Lam      t   u      ) = VLam t u
eval e (Lam _ u  :@: Lam s v) = eval e (sub 0 (Lam s v) u)
eval e (Lam t u1 :@: u2) = let v2 = eval e u2 in eval e (sub 0 (quote v2) u1)
eval e (u        :@: v      ) = case eval e u of
  VLam t u' -> eval e (Lam t u' :@: v)
  _         -> error "Error de tipo en run-time, verificar type checker"
eval e (Let t1 t2) = eval e (sub 0 t1 t2)
eval e (As t tt) = eval e t
eval _ Unit = VUnit
eval e (Pair t1 t2) = VPair (eval e t1) (eval e t2)
eval e (Fst t) = case eval e t of
  VPair v1 _ -> v1
  _          -> error "Error de tipo en run-time, verificar type checker"
eval e (Snd t) = case eval e t of
  VPair _ v2 -> v2
  _          -> error "Error de tipo en run-time, verificar type checker"
eval _ Zero = VNum NZero
eval e (Suc t) = case eval e t of
  VNum n -> VNum (NSuc n)
  _      -> error "Error de tipo en run-time, verificar type checker"
eval e (Rec t1 t2 Zero) = eval e t1
eval e (Rec t1 t2 (Suc t)) = eval e ((t2 :@: Rec t1 t2 t) :@: t)
eval e (Rec t1 t2 t3) = let v3 = eval e t3 in eval e (Rec t1 t2 (quote v3))

-----------------------
--- quoting
-----------------------

quote :: Value -> Term
quote (VLam t f) = Lam t f
quote VUnit = Unit
quote (VPair v1 v2) = Pair (quote v1) (quote v2)
quote (VNum NZero) = Zero
quote (VNum (NSuc n)) = Suc (quote (VNum n))

----------------------
--- type checker
-----------------------

-- type checker
infer :: NameEnv Value Type -> Term -> Either String Type
infer = infer' []

-- definiciones auxiliares
ret :: Type -> Either String Type
ret = Right

err :: String -> Either String Type
err = Left

(>>=)
  :: Either String Type -> (Type -> Either String Type) -> Either String Type
(>>=) v f = either Left f v
-- fcs. de error

matchError :: Type -> Type -> Either String Type
matchError t1 t2 =
  err
    $  "se esperaba "
    ++ render (printType t1)
    ++ ", pero "
    ++ render (printType t2)
    ++ " fue inferido."

notfunError :: Type -> Either String Type
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

notfoundError :: Name -> Either String Type
notfoundError n = err $ show n ++ " no está definida."

infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
infer' c _ (Bound i) = ret (c !! i)
infer' _ e (Free  n) = case lookup n e of
  Nothing     -> notfoundError n
  Just (_, t) -> ret t
infer' c e (t :@: u) = infer' c e t >>= \tt -> infer' c e u >>= \tu ->
  case tt of
    FunT t1 t2 -> if (tu == t1) then ret t2 else matchError t1 tu
    _          -> notfunError tt
infer' c e (Lam t u) = infer' (t : c) e u >>= \tu -> ret $ FunT t tu
infer' c e (Let t u) = infer' c e t >>= \tt -> infer' (tt : c) e u
infer' c e (As t tt) = infer' c e t >>= \tt' -> if tt == tt' then ret tt 
                                                             else matchError tt tt'
infer' _ _ Unit = ret UnitT
infer' c e (Pair t u) = infer' c e t >>= \tt -> infer' c e u >>= \tu -> ret $ PairT tt tu
infer' c e (Fst t) = infer' c e t >>= \tt ->
  case tt of
    PairT tu _ -> ret tu
    _ -> err $ "Se esperaba un par, pero " ++ render (printType tt) ++ " fue inferido."  
infer' c e (Snd t) = infer' c e t >>= \tt ->
  case tt of
    PairT _ tu -> ret tu
    _ -> err $ "Se esperaba un par, pero " ++ render (printType tt) ++ " fue inferido."
infer' c e Zero = ret NatT
infer' c e (Suc t) = infer' c e t >>= \tt -> if tt == NatT then ret tt 
                                                           else matchError NatT tt
infer' c e (Rec t1 t2 t3) = infer' c e t1 >>= \tt1 -> infer' c e t2 >>= \tt2 ->
  if tt2 == (FunT tt1 (FunT NatT tt1)) then (infer' c e t3 >>= \tt3 -> if tt3 == NatT then ret tt1 
                                                                                      else matchError NatT tt3)
                                       else matchError (FunT tt1 (FunT NatT tt1)) tt2

----------------------------------
