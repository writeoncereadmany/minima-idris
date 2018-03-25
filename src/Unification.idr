module Unification

import MinimaTypes
import Bindings
import TypeErrors

%access public export

data UnificationOp = Unify MinimaType MinimaType

infixl 8 |?
(|?) : MinimaType -> MinimaType -> UnificationOp
(|?) = Unify

mutual
  WithBindings UnificationOp (Either TypeErrors MinimaType) where
    (|=>) bindings (Unify a b) = unify bindings a b

  unify : Bindings -> MinimaType -> MinimaType -> Either TypeErrors MinimaType
  -- either x or anything can be anything
  unify b Anything _ = Right Anything
  unify b _ Anything = Right Anything

  -- note: this isn't just a dodge. Nothing is the return type of a recursive
  -- path, so unifying it with a valid return type is something which frequently
  -- does occur. If something either returns a String, or doesn't return, then
  -- every time it returns, it returns a String.
  unify b Nothing y = Right y
  unify b x Nothing = Right x

  -- shortcut the unification if they're the same name
  unify b nx@(Named x) ny@(Named y) = if x == y
    then Right nx
    else b |=> lookupType x b |? ny
  unify b (Named x) y = b |=> lookupType x b |? y
  unify b x (Named y) = b |=> x |? lookupType y b

  unify b (Unbound x) (Unbound y) = Right (Unbound x)
  unify b (Unbound _) y = Right y
  unify b x (Unbound _) = Right x

  unify b (Union xs) (Union ys) = Right $ Union $ sort $ union xs ys
  unify b x@(Union _) y = b |=> x |? Union [y]
  unify b x y@(Union _) = b |=> Union [x] |? y

  unify b dx@(Data x) dy@(Data y) = if x == y
    then Right dx
    else Right $ Union [dx, dy]

  unify b (Function argsa reta) (Function argsb retb) = do returns <- b |=> reta |? retb
                                                           pure $ Function argsa returns
  -- if we get here, the types are disjoint: create a union of the two types
  unify b x y = b |=> Union [x] |? y

updateFirst : (Eq a) => List (a, b) -> a -> b -> List (a, b)
updateFirst [] x y = []
updateFirst ((ok, ov) :: xs) nk nv = if ok == nk
  then (nk, nv) :: xs
  else (ok, ov) :: updateFirst xs nk nv

condense : Bindings -> Either TypeErrors Bindings
condense [] = Right []
condense ((b, t1) :: xs) = case lookup b xs of
   Nothing => do condensed <- condense xs
                 pure ((b, t1) :: condensed)
   -- note here that in order to condense - ie normalise - bindings,
   -- we need bindings...
   -- This could get tricky wrt recursively-typed things.
   (Just t2) => do unified <- [] |=> t1 |? t2
                   condense $ updateFirst xs b unified

unifyBindings : Bindings -> Bindings -> Bindings
unifyBindings x y = case condense (x ++ y) of
  (Left l) => ?do_we_need_this
  (Right r) => r

unifyAll : List Bindings -> Bindings
unifyAll xs = case condense (join xs) of
  (Left l) => ?do_we_need_this_2
  (Right r) => r
