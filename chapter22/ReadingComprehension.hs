module ReadingComprehesion where

--1

myLiftA2 :: Applicative f =>
            (a -> b -> c)
          -> f a -> f b -> f c
myLiftA2 f xs ys = f <$> xs <*> ys

--2

newtype Reader r a = Reader { runReader:: r -> a }

asks :: (r -> a) -> Reader r a
asks = Reader
