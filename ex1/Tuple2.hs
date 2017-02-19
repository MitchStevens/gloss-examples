module Tuple2
	where

import Data.Monoid

data Tuple2 a = Tuple2 a a

instance (Num n) => Monoid (Tuple2 n) where
	mempty = Tuple2 0 0
	mappend t1 t2 = (pure (+)) <*> t1 <*> t2

instance Functor Tuple2 where
	fmap f (Tuple2 x y) = Tuple2 (f x) (f y)

instance Applicative Tuple2 where
	pure a = Tuple2 a a
	Tuple2 f g <*> Tuple2 x y = Tuple2 (f x) (g y)

inv :: (Num n) => Tuple2 n -> Tuple2 n
inv = fmap (*(-1))

diff :: (Num n) => Tuple2 n -> Tuple2 n -> Tuple2 n
diff t1 t2 = t1 <> (inv t2)
