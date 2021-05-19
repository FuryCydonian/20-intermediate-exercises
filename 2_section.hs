-- show Misty instances
class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a

-- Exercise 7
-- Relative Difficulty: 2
instance Misty [] where
  banana f = concat . map f
  unicorn a = [a]

-- Exercise 8
-- Relative Difficulty: 2
instance Misty Maybe where
  banana _ Nothing = Nothing
  banana f (Just a) = f a
  unicorn = Just

-- Exercise 9
-- Relative Difficulty: 6
instance Misty ((->) t) where
  banana f g = \t -> f (g t) t
  unicorn a = \_ -> a

newtype EitherLeft b a  = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)

-- Exercise 10
-- Relative Difficulty: 6
instance Misty (EitherLeft t) where
  banana f (EitherLeft (Left a)) = f a
  banana _ (EitherLeft (Right b)) = EitherLeft (Right b)
  unicorn a = EitherLeft (Left a)

-- Exercise 11
-- Relative Difficulty: 6
instance Misty (EitherRight t) where
  banana _ (EitherRight (Left a)) = EitherRight (Left a)
  banana f (EitherRight (Right b)) = f b
  unicorn b = EitherRight (Right b)

-- /show

main = putStrLn "It typechecks!"