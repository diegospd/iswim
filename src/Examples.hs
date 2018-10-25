import Iswim
import HM
import CEK

import Prelude hiding(id, const, pred, (.))



[c0, c1, c2, c3, c4, c5, c6, c7, c8, c9] = map f [0..9]
    where f = \x -> C $ Nat x


[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z] = map f ['a'..'z']
    where f = \x -> IVar [x]

true  = C $ TF True
false = C $ TF False


(.) :: Iswim -> Iswim -> Iswim
(.) = App



idF = Lam "x" x
id = IVar "id"


let1 = (Lam "id" letIf) . idF
let2 = Let "id" idF letIf  
letIf = If (id . true) (id . c1) c2


fact' = Lam  "f" $ Lam "n" $ If (isZero n) c1  (mult n (f . (pred n)))
isZero m = Op IsZero [m]
mult m n = Op Prod [m, n]
pred m   = Op Pred [m]


fact = Fix fact'
fix = Lam "f" $ fix' . fix'
    where fix' = Lam "x" $ f . (x . x)
fact2 = fix . fact'
fixV = Lam "f" $ fix' . fix'
    where fix' = Lam "y" $ f . (Lam "z" $ y . y . z)

fact3 = fixV . fact'


aa = Var $ TVar "a"
bb = Var $ TVar "b"
cc = Var $ TVar "c"
zz = Var $ TVar "z"
t1 = Var $ TVar "t1"
t2 = Var $ TVar "t2"


fixId = Fix idF
chafa = If true fixId fixId

pierce = Lam "f" $ Lam "x" $ Let "g" f (g . x)

pierce2 = (Lam "f" $ Lam "x" $ Let "g" f (g . c0)) . (Lam "x" $ If x true false) . true


tonto  = Lam "x" $ Let "p" (Op Pred [x]) x

alo = (Lam "x" $ Lam "x" x) . c7
alo2 = (Lam "y" $ Lam "x" y) . c7

