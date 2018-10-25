module Iswim where

import Data.Char(toLower)

data Cte = Nat Int      -- Naturales
         | TF Bool      -- Booleanos
           deriving (Eq, Ord)


data Op = IsZero  -- dice si n es cero
        | Sum     -- n + m
        | Prod    -- n * m
        | Pred    -- predecesor de n
        | Not     -- negación booleana
          deriving (Eq, Show, Ord)

data Iswim = IVar Ident                    -- Variables
           | C    Cte                      -- Constantes
           | Lam  Ident Iswim              -- λx.M    
           | App  Iswim Iswim              -- (M N) aplicación
           | Op   Op [Iswim]               -- Operadores primitivos con [operandos]
           | If   Iswim Iswim Iswim        -- If M then N else L
           | Fix  Iswim                    -- Punto fijo para recursión
           | Let  Ident Iswim Iswim        -- let x = N in M 
             deriving (Eq, Ord)

type Ident = String



instance Show Iswim where
    show (Lam "x" (IVar "x")) = " I "
    show (Lam "x" (Lam "y" (IVar "x"))) = " K "
    show (Lam "x" (Lam "y" (Lam "z" ( App (App (IVar "x")  (IVar "z") ) (App (IVar "y")  (IVar "z") )   )))) = " S "
    show (IVar x) = map toLower x
    show (C c) = show c
    show (App a b@App{}) = show a ++ " (" ++ show b ++ ")" 
    show (App a b) = show a ++" "++ show b
    show (Op op xs) = show op ++ " " ++ show xs
    show (Fix e) = "(Fix " ++ show e ++ ")"
    show (Lam x e) = "(\\" ++ x ++ "." ++ show e ++ ")"
    show (Let x a b) = "let " ++ x ++ "=" ++ show a ++ " in " ++ show b
    show (If a b c) = "if " ++ show a ++ " then " ++ show b ++ " else " ++ show c 

instance Show Cte where
    show (Nat n) = show n
    show (TF b) = show b
