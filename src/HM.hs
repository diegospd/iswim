module HM where

import Iswim

import Data.List
import Data.Maybe
import Control.Arrow ( (***) )
import Control.Monad
import Control.Monad.State             
import Control.Monad.Except            
import qualified Data.Set as S         
import qualified Data.Map.Strict as M  

import Data.Char


data Type = CType Ident       -- tipos concretos e.g. Nat, Bool
          | Var   TVar        -- variables de tipo 
          | Func  Type  Type  -- tipo función
          deriving (Eq, Ord)

data TScheme = Forall [TVar] Type deriving(Eq,Show,Ord)

newtype TVar = TVar Ident deriving(Eq, Ord)
newtype Gamma = G (M.Map Ident TScheme) deriving (Eq, Show, Ord)
newtype Constraint = R [(Type, Type)] deriving (Eq, Ord)
newtype Substitution = S (M.Map TVar Type)  deriving (Eq, Ord)




class CanSubstitute a where
    subst :: Substitution -> a -> a
instance CanSubstitute Type  where
  subst _ ct@CType{} = ct
  subst s (Func x y) = Func (subst s x) (subst s y)
  subst s (Var tv)   = query tv s
    where
    query :: TVar -> Substitution -> Type
    query tv' (S s')
        | M.member tv' s' = fromJust $ M.lookup tv' s'
        | otherwise = Var tv'
instance CanSubstitute Constraint where
  subst s (R c) =  R $ map  (subst s *** subst s) c

instance CanSubstitute TScheme where
  subst (S s) (Forall tvs t) = Forall tvs (subst s' t)
    where s' = S $ M.difference d s
          d  = M.fromList $ map (\tv -> (tv, tBool)) tvs


instance CanSubstitute Gamma where
  subst s (G g) = G $ M.map (subst s) g



subComp :: Substitution -> Substitution -> Substitution
subComp (S s2) (S s1) = S $ M.map (subst $ S s2) (M.union s1 s2) 


class WithTVars a where
    getTVarsLs :: a -> [Ident]
    getTVars   :: a -> S.Set Ident
    getTVars   = S.fromList . getTVarsLs

instance WithTVars TVar where
    getTVarsLs (TVar v) = [v]

instance WithTVars Constraint where
    getTVarsLs (R c) = concatMap getTVarsLs tvs
        where tvs = foldr (\(a,b) xs -> a:b:xs) [] c

instance WithTVars Gamma where
    getTVarsLs (G g) = M.keys g ++ concatMap getTVarsLs (M.elems g)

instance WithTVars Type where
    getTVarsLs CType{}        = []
    getTVarsLs (Var (TVar x)) = [x]
    getTVarsLs (Func a b)     = getTVarsLs a ++ getTVarsLs b

instance WithTVars Iswim where
    getTVarsLs (IVar _)    = []
    getTVarsLs (C _)       = [] 
    getTVarsLs (Op _ xs)   = concatMap getTVarsLs xs
    getTVarsLs (Fix e)     = getTVarsLs e
    getTVarsLs (App a b)   = getTVarsLs a ++ getTVarsLs b
    getTVarsLs (Lam _ e)   = getTVarsLs e
    getTVarsLs (Let _ a b) = getTVarsLs a ++ getTVarsLs b
    getTVarsLs (If a b c)  = getTVarsLs a ++ getTVarsLs b ++ getTVarsLs c

instance WithTVars TScheme where
    getTVarsLs (Forall vs t) = concatMap getTVarsLs vs ++ getTVarsLs t

type VarsPool = [Ident]

type VGC = (VarsPool, Gamma, Constraint)


type HasArity   = Int
type NeedsArity = Int
data TypeError  = UnboundVar Ident 
                | WrongArity Op HasArity NeedsArity
                | InfiniteType Type Type
                | CannotUnify Type Type
                | UnimplementedOperator Op
                deriving(Eq,Ord)


type GK = StateT VGC (Except TypeError)
   

putPool       :: VarsPool   -> GK ()
putGamma      :: Gamma      -> GK ()
putConstraint :: Constraint -> GK ()
putPool       p = get >>= \(_,g,c) -> put (p,g,c)
putGamma      g = get >>= \(p,_,c) -> put (p,g,c)
putConstraint c = get >>= \(p,g,_) -> put (p,g,c)


getPool       :: GK VarsPool
getGamma      :: GK Gamma
getConstraint :: GK Constraint
getPool       = get >>=  \(p,_,_) -> return p
getGamma      = get >>=  \(_,g,_) -> return g
getConstraint = get >>=  \(_,_,c) -> return c

fresh' :: GK TVar
fresh' = do
    p:ps <- getPool
    putPool ps
    return $ TVar p


fresh :: GK Type
fresh = fresh' >>= return . Var


generalize :: Type -> GK TScheme
generalize t = do
    g      <- getGamma
    let xs =  map TVar $ S.toList $ S.difference (getTVars t) (getTVars g)
    zs     <- mapM (const fresh') xs
    let s  =  S $ M.fromList (zip xs $ map Var zs)
    return $ Forall zs (subst s t)



instantiate :: TScheme -> GK Type
instantiate (Forall xs t) = do
    ys <- mapM (const fresh) xs
    let s = S $ M.fromList (zip xs ys)
    return $ subst s t


extend :: Ident -> TScheme -> GK Gamma
extend x sc = do
    G g <- getGamma
    putGamma (G $ M.insert x sc g)
    return $ G g


addConstraint ::(Type,Type) -> GK ()
addConstraint c = do
    R cs <- getConstraint
    putConstraint $ R (c:cs)

unify :: Constraint -> Either TypeError Substitution
unify (R []) = Right emptySub
unify (R ((s,t):cs))  
    | s == t = unify $ R cs
unify (R ((Func s1 s2, Func t1 t2):cs)) = unify cs'
    where cs' = R $  [(s1,t1), (s2,t2)] ++ cs 
unify (R ((Var x, t):cs))  
    | occursCheck x t = do
        let xt = mkSub x t
        u  <- unify $ subst xt (R cs)
        return $ subComp u xt
    | otherwise = Left $ InfiniteType (Var x) t
unify (R ((t, Var x):cs)) = unify $ R $ (Var x,t):cs
unify (R ((s,t):_))       = Left $ CannotUnify s t


occursCheck :: TVar -> Type -> Bool
occursCheck tv t = tv `notElem` ftv 
    where ftv = map TVar (getTVarsLs t)


getUnifier :: GK Substitution
getUnifier = do
    c <- getConstraint
    case unify c of
        Left  e -> throwError e
        Right u -> return u

infer :: Iswim -> GK Type
infer (IVar x) = do
    G g <- getGamma
    case M.lookup x g of
        Nothing     -> throwError $ UnboundVar x
        Just scheme -> instantiate scheme 
infer (C c) = return $ typeCte c  
infer (Lam x m) = do
    z <- fresh
    g <- extend x (Forall [] z)
    t <- infer m
    putGamma g  -- descarga el contexto
    return $ Func z t
infer (App m n) = do
    z  <- fresh
    t1 <- infer m
    t2 <- infer n
    addConstraint (t1, Func t2 z)
    return z
infer (If m n l) = do
    t1 <- infer m
    t2 <- infer n
    t3 <- infer l
    addConstraint (t1, tBool)
    addConstraint (t2, t3)
    return t2
infer (Fix m) = do
    z <- fresh
    t <- infer m
    addConstraint (t, Func z z)
    return z
infer (Let x n m) = do
    t1'  <- infer n
    u    <- getUnifier
    getGamma >>= \g -> putGamma (subst u g)
    let t1 = subst u t1'
    e     <- generalize t1
    g     <- extend x e
    putConstraint emptyR
    t2    <- infer m
    putGamma g
    return t2
infer (Op op xs) = typeOp op xs

typeCte :: Cte -> Type
typeCte Nat{} = tNat
typeCte TF{}  = tBool

tBool :: Type
tNat  :: Type
tBool =  CType "Bool"
tNat  =  CType "Nat"

typeOp :: Op -> [Iswim] -> GK Type
typeOp IsZero [m] = do
    t <- infer m
    addConstraint (t,tNat)
    return tBool
typeOp Pred [m] = do
    t <- infer m
    addConstraint (t,tNat)
    return tNat
typeOp Prod [m1,m2] = do
    t1 <- infer m1
    t2 <- infer m2
    addConstraint (t1, tNat)
    addConstraint (t2, tNat)
    return tNat
typeOp Sum [m1,m2] = do
    t1 <- infer m1
    t2 <- infer m2
    addConstraint (t1, tNat)
    addConstraint (t2, tNat)
    return tNat
typeOp Not [m]  = do
    t <- infer m
    addConstraint(t,tBool)
    return tBool
typeOp m xs = arityError m xs

arityError :: Op -> [Iswim] -> GK Type
arityError op@IsZero xs = throwError $ WrongArity op (length xs) 1
arityError op@Pred   xs = throwError $ WrongArity op (length xs) 1
arityError op@Prod   xs = throwError $ WrongArity op (length xs) 2
arityError op@Sum    xs = throwError $ WrongArity op (length xs) 2
arityError op@Not    xs = throwError $ WrongArity op (length xs) 1
arityError op        xs = throwError $ UnimplementedOperator op



principalType' :: Iswim -> GK Type
principalType' m = do 
    t <- infer m
    u <- getUnifier
    return $ shift $ subst u t

shift :: Type -> Type
shift t = subst s t
    where ks = map TVar $ S.toList $ getTVars t
          vs = map (Var . TVar) $ take (length ks) allVars
          s  = S $ M.fromList (zip ks vs)

principalType :: Iswim -> Either TypeError Type
principalType m = runExcept $ evalStateT (principalType' m)  initialState

initialState :: VGC
initialState = (allVars, emptyG, emptyR)

allVars :: VarsPool
allVars = [1..] >>= flip replicateM ['a'..'z']








emptySub :: Substitution
emptySub = S M.empty

mkSub :: TVar -> Type -> Substitution
mkSub x t = S $ M.singleton x t

emptyG :: Gamma
emptyG = G  M.empty

emptyR :: Constraint
emptyR = R []


-- unifier :: Iswim -> Either TypeError Substitution

unifier m = runExcept $ evalStateT (unifier' m)  initialState

unifier' m = do
  t <- infer m
  u <- getUnifier
  return (t,u)


instance Show TVar where
    show (TVar x) = map toUpper x

instance Show Type where
    show (CType (x:xs)) = toUpper x : map toLower xs 
    show (CType [])     = "[NoIdent]"
    show (Var v)        = show v  
    show (Func a@Func{} b) = "(" ++ show a ++ ")→" ++ show b
    show (Func a b)     = show a ++ "→" ++ show b

instance Show Constraint where
    show (R cs)    = "[" ++ cs' ++ "]"
        where f (a,b) = show a ++ " = " ++ show b
              cs'     = intercalate ", " (map f cs)

instance Show Substitution where
    show (S m) = "[" ++ intercalate ", " (zipWith f (M.keys m) (M.elems m)) ++ "]"
        where f k x =  show k ++ "↦" ++ show x         

instance Show TypeError where
    show (UnboundVar x) = "UnboundVariable " ++ x 
    show (WrongArity op has needs) = "WrongArity: Operator " ++ show op ++ " needs " ++ show needs ++ " arguments, but has " ++ show has
    show (UnimplementedOperator op) = "UnimplementedOperator: " ++ show op
    show (InfiniteType t1 t2) = "InfiniteType: " ++ show (CannotUnify t1 t2)
    show (CannotUnify t1 t2 ) = "CannotUnify " ++ show t1 ++ " = " ++ show t2                



