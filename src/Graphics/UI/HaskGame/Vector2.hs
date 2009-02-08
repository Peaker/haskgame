{-# OPTIONS -Wall -O2 #-}

module Graphics.UI.HaskGame.Vector2
    (Vector2(Vector2)
    ,first,second
    ,fst,snd)
where

import Prelude hiding (fst, snd)
import Control.Applicative(Applicative(..), liftA2)

data Vector2 a = Vector2 !a !a
  -- Note the Ord instance is obviously not a mathematical one
  -- (Vectors aren't ordinals!). Useful to have in a binary search
  -- tree though.
  deriving (Eq, Ord, Show, Read)

type Endo a = a -> a

fst, snd :: Vector2 a -> a
fst (Vector2 x _) = x
snd (Vector2 _ y) = y
first, second :: Endo a -> Endo (Vector2 a)
first f (Vector2 x y) = Vector2 (f x) y
second f (Vector2 x y) = Vector2 x (f y)

instance Functor Vector2 where
  fmap f (Vector2 x y) = Vector2 (f x) (f y)
instance Applicative Vector2 where
  pure x = Vector2 x x
  Vector2 f g <*> Vector2 x y = Vector2 (f x) (g y)

-- An improper Num instance, for convenience
instance (Eq a, Show a, Num a) => Num (Vector2 a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  negate = fmap negate
  signum = fmap signum
  fromInteger = pure . fromInteger
