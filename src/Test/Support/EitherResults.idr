module EitherResults

import Specdris.Spec

%access public export

infixl 4 \@/
(\@/) : (Show a, Show b, Eq b) => Either a b -> b -> SpecResult
(\@/) (Left actual) expected = UnaryFailure actual $ "Got failure: expected success of " ++ show expected
(\@/) (Right actual) expected = actual === expected

infixl 4 >.<
(>.<) : (Show a, Show b, Eq a) => Either a b -> a -> SpecResult
(>.<) (Left actual) expected = actual === expected
(>.<) (Right actual) expected = UnaryFailure actual $ "Got success: expected failure of " ++ show expected
