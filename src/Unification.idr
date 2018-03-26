module Unification

import MinimaTypes
import Bindings
import TypeErrors

%access public export

data UnificationOp = Unify MinimaType MinimaType

data IntersectionOp = Intersect MinimaType MinimaType

infixl 8 |?
(|?) : MinimaType -> MinimaType -> UnificationOp
(|?) = Unify

infixl 8 &?
(&?) : MinimaType -> MinimaType -> IntersectionOp
(&?) = Intersect

overlap : (Eq a) => List a -> List a -> List a
overlap [] ys = []
overlap (x :: xs) ys = if elem x ys
  then x :: overlap xs ys
  else overlap xs ys

mutual
  WithBindings UnificationOp MinimaType where
    (|=>) bindings (Unify a b) = unify bindings a b

  unify : Bindings -> MinimaType -> MinimaType -> MinimaType
  -- either x or anything can be anything
  unify b Anything _ = Anything
  unify b _ Anything = Anything

  -- note: this isn't just a dodge. Nothing is the return type of a recursive
  -- path, so unifying it with a valid return type is something which frequently
  -- does occur. If something either returns a String, or doesn't return, then
  -- every time it returns, it returns a String.
  unify b Nothing y = y
  unify b x Nothing = x

  -- shortcut the unification if they're the same name
  unify b nx@(Named x) ny@(Named y) = if x == y
    then nx
    else b |=> lookupType x b |? ny
  unify b (Named x) y = b |=> lookupType x b |? y
  unify b x (Named y) = b |=> x |? lookupType y b

  unify b (Unbound x) (Unbound y) = Unbound x
  unify b (Unbound _) y = y
  unify b x (Unbound _) = x

  unify b (Union xs) (Union ys) = Union $ sort $ union xs ys
  unify b x@(Union _) y = b |=> x |? Union [y]
  unify b x y@(Union _) = b |=> Union [x] |? y

  unify b dx@(Data x) dy@(Data y) = if x == y
    then dx
    else Union [dx, dy]

  unify b (Function argsa reta) (Function argsb retb) = let returns = b |=> reta |? retb
                                                         in Function argsa returns
  -- if we get here, the types are disjoint: create a union of the two types
  unify b x y = b |=> Union [x] |? y

  WithBindings IntersectionOp MinimaType where
    (|=>) bindings (Intersect a b) = intersect bindings a b

  intersect : Bindings -> MinimaType -> MinimaType -> MinimaType

  intersect b Anything y = y
  intersect b x Anything = x

  intersect b Nothing y = Nothing
  intersect b x Nothing = Nothing

  intersect b nx@(Named x) ny@(Named y) = if x == y
    then nx
    else b |=> lookupType x b &? ny
  intersect b (Named x) y = b |=> lookupType x b &? y
  intersect b x (Named y) = b |=> x &? lookupType y b

  intersect b (Union xs) (Union ys) = case overlap xs ys of
    [] => Nothing
    [x] => x
    xs => Union xs

  intersect b dx@(Data x) dy@(Data y) = if x == y
    then dx
    else Nothing

  intersect b (Function args returns) y = ?intersect_rhs_5

  intersect b (Unbound introduction) y = ?intersect_rhs_7

updateFirst : (Eq a) => List (a, b) -> a -> b -> List (a, b)
updateFirst [] x y = []
updateFirst ((ok, ov) :: xs) nk nv = if ok == nk
  then (nk, nv) :: xs
  else (ok, ov) :: updateFirst xs nk nv

condense : Bindings -> Bindings
condense [] = []
condense ((b, t1) :: xs) = case lookup b xs of
   Nothing => ((b, t1) :: condense xs)
   -- note here that in order to condense - ie normalise - bindings,
   -- we need bindings...
   -- This could get tricky wrt recursively-typed things.
   (Just t2) => condense $ updateFirst xs b ([] |=> t1 |? t2)

unifyBindings : Bindings -> Bindings -> Bindings
unifyBindings x y = condense (x ++ y)

unifyAll : List Bindings -> Bindings
unifyAll xs = condense (join xs)
