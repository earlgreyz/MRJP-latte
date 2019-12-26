module Util.Function where

-- Composition of a list of functions.
compose :: [a -> a] -> a -> a
compose = foldr (.) id

-- Finds a function fix point.
fixPoint :: (Eq a) => (a -> a) -> a -> a
fixPoint f x = let y = f x in
  if x == y then x else fixPoint f y
