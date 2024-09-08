open Printf
open Terms
open Subst
open Beta
open Booleans
open Church
open YCombinator
open Pairs


let lConsH: term=
Lambda("t1",Lambda("t2",lPair( lFalse)(lPair(Var "t1")(Var "t2"))))

let lCons (t1: term) (t2: term) : term=
App(App(lConsH,t1),t2)

let lNil:term=
  Lambda("l",Var "l")

 

let lHeadH:term=
    Lambda("l",lProj1(lProj2 (Var "l")))
let lHead (t: term) : term=
    App(lHeadH,t)
 
   
let lTailH:term=
   Lambda("l",lProj2(lProj2 (Var "l")))
let lTail (t: term) : term=
       App(lTailH,t)
let lIsNil (t: term) : term=
   lProj1 t


let lLengthH: term =
   Lambda("f", Lambda("l",
   lIte
   (lIsNil(Var "l"))
   (lZero)
   (lAdd (lOne) (App(Var "f",lTail (Var "l"))))
   ))
let lLength (t: term): term =
    App( lLengthH,t)
let lAppend (t: term): term =
   App(lAppendH,t)

let lAppendH: term =
   Lambda("f", Lambda("l1",Lambda("l2",
   lIte
   (lIsNil(Var "l1"))
   (Var "l2")
   (lCons (lHead (Var "l1")) (App(App(Var "f",lTail (Var "l1"),Var"l2")))
   ))))
let lReverse (t: term): term =
    App( lReverseH,t)
 
let lReverseH: term =
  Lambda("f", Lambda("l1",Lambda("l2",
lIte
(lIsNil(Var "l1"))
(Var "l2")
((App(App(Var "f",lTail (Var "l1"),lCons (lHead (Var "l1"))(Var "l2") )))
))))


