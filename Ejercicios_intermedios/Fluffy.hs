-- show Fluffy instances
class Fluffy f where
  furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy [] where
  furry = \f -> map f
{-
Comentarios largos
-}
-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
  furry f (Just a) = Just (f a)
  furry _ Nothing = Nothing

-- Exercise 3
-- Relative Difficulty: 5
instance Fluffy ((->) t) where
  furry f g = error "todo"

newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)

-- Exercise 4
-- Relative Difficulty: 5
instance Fluffy (EitherLeft t) where
  furry = error "todo"

-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
  furry = error "todo"
-- /show

main = let f = furry (+1) [1,2,3] in
           putStrLn f-- "It typechecks!"