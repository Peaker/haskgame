{-# OPTIONS -Wall -O2 #-}

module Graphics.UI.HaskGame.Vector2
    (Vector2(Vector2)
    ,vector2
    ,first,second,(***),both
    ,fst,snd)
where

import Prelude hiding (fst, snd)
import Control.Applicative(Applicative(..), liftA2)
import Control.Monad(join)

data Vector2 a = Vector2 !a !a
  -- Note the Ord instance is obviously not a mathematical one
  -- (Vectors aren't ordinals!). Useful to have in a binary search
  -- tree though.
  deriving (Eq, Ord, Show, Read)

type Endo a = a -> a

fst :: Vector2 a -> a
fst (Vector2 x _) = x

snd :: Vector2 a -> a
snd (Vector2 _ y) = y

first :: Endo a -> Endo (Vector2 a)
first f (Vector2 x y) = Vector2 (f x) y

second :: Endo a -> Endo (Vector2 a)
second f (Vector2 x y) = Vector2 x (f y)

(***) :: Endo a -> Endo a -> Endo (Vector2 a)
(f *** g) (Vector2 x y) = Vector2 (f x) (g y)

vector2 :: (a -> a -> b) -> Vector2 a -> b
vector2 f (Vector2 x y) = f x y

both :: Endo a -> Endo (Vector2 a)
both = join (***)

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
