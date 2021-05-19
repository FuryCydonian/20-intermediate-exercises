-- show Fluffy instances
class Fluffy f where
  furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy [] where
  furry = map

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
  furry _ Nothing = Nothing
  furry f (Just x) = Just (f x)

-- Exercise 3
-- Relative Difficulty: 5
instance Fluffy ((->) t) where
  furry f g = f . g

newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)

-- Exercise 4
-- Relative Difficulty: 5
instance Fluffy (EitherLeft t) where
  furry f (EitherLeft (Left a)) = EitherLeft (Left $ f a)
  furry _ (EitherLeft (Right b)) = EitherLeft (Right b)

-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
  furry _ (EitherRight (Left a)) = EitherRight (Left a)
  furry f (EitherRight (Right b)) = EitherRight (Right $ f b)
-- /show

main = putStrLn "It typechecks!"