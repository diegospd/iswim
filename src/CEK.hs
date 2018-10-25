
module CEK where

import Iswim

import Data.Maybe
import qualified Data.Map as M  

data Closure = Cl Iswim Env  deriving(Eq,Show,Ord)
type Env     = M.Map Ident Closure
env :: Env -> Ident -> (Iswim, Env)
env e x = case M.lookup x e of
    Just (Cl m e') -> (m, e')
    Nothing        -> error $ "Unbound var " ++ x
envM :: Env -> Ident -> Iswim
envM e x = fst $ env e x

envE :: Env -> Ident -> Env
envE e x = snd $ env e x


extend :: Env -> Ident -> Iswim -> Env -> Env
extend e x v e' = M.insert x (Cl v e') e

data Kont = Mt
          | Ar Iswim  Env Kont
          | Fn Lambda Env Kont
          | O [Value] Op [Iswim] Env Kont
          | I Iswim Iswim Env Kont   -- If
          | F Kont                   -- Fix
          deriving (Ord,Show,Eq)

type Lambda = Iswim  
type Value  = Iswim

type CEK = (Iswim, Env, Kont)

step :: CEK -> CEK



isFinal :: CEK -> Bool
isFinal (c,_,k) = isValue c && k == Mt

isValue :: Iswim -> Bool
isValue Lam{}  = True
isValue C{}    = True
isValue _      = False



step  cek | isFinal cek = cek
step (IVar x      ,e, k                 ) = (envM e x, envE e x, k)
step (v           ,e, Ar n e' k         ) | isValue v = (n, e', Fn v e k)
step (v           ,e, Fn (Lam x m) e' k ) | isValue v = (m, extend e' x v e, k)
step (v           ,e, O vs o [] e' k    ) | isValue v = (delta o $ reverse (v:vs), M.empty, k)
step (v           ,e, O vs o (m:ms) e' k) | isValue v = (m, e', O (v:vs) o ms e' k)
step (v           ,e, I n l e' k        ) | isValue v = (if' v n l, e', k)
step (Lam f m     ,e, F k               ) = (m, extend e f (Fix $ Lam f m) e, k)
step (App m n     ,e, k                 ) = (m, e, Ar n e k)
step (Op o (m:ms) ,e, k                 ) = (m, e, O [] o ms e k)
step (If m n l    ,e, k                 ) = (m, e, I n l e k)
step (Fix m       ,e, k                 ) = (m, e, F k)
step (Let x n m   ,e, k                 ) = (App (Lam x m) n, e, k)
step cek = error $ "Error en CEK:step  " ++ show cek

if' :: Value -> Iswim -> Iswim -> Iswim
if' (C (TF b)) n l = if b then n else l

load :: Iswim -> CEK
load m = (m, M.empty, Mt)

unload :: CEK -> Value
unload (m, e, _) = real [] e m

real :: [Ident] -> Env -> Iswim -> Iswim
real bs e (IVar x)  
    | elem x bs       = IVar x
    | otherwise       = real bs (envE e x) (envM e x)
real _  _ c@C{}       = c
real bs e (Lam x m)   = Lam x $ real (x:bs) e m
real bs e (App m n)   = App (real bs e m) (real bs e n)
real bs e (Op op ms)  = Op op $ map (real bs e) ms
real bs e (If m n l)  = If (real bs e m) (real bs e n) (real bs e l)
real bs e (Fix m)     = Fix $ real bs e m
real bs e (Let x n m) = Let x (real bs e n) (real (x:bs) e m)

runCEK :: CEK -> CEK
runCEK cek 
    | step cek == cek = cek
    | otherwise = runCEK $ step cek

eval :: Iswim -> Value
eval = unload . runCEK . load

delta :: Op -> [Value] -> Value
delta IsZero [C (Nat a)]             = C . TF  $ a == 0
delta Sum    [C (Nat a), C (Nat b)]  = C . Nat $ a + b
delta Prod   [C (Nat a), C (Nat b)]  = C . Nat $ a * b
delta Pred   [C (Nat a)]             = C . Nat $ a - 1
delta Not    [C (TF a) ]             = C . TF  $ not a
delta op xs = error $ "Unimplemented operator CEK:delta" ++ show op ++ show xs   
