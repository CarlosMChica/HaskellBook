module ChapterExercises where

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap f (Moi g) = Moi $ \s -> let (x, s') = g s
                              in (f x, s')

instance Applicative (Moi s) where
  pure x          = Moi $ \s -> (x, s)
  Moi f <*> Moi g = Moi $ \s -> let (f', s') = f s
                                    (x, s'')  = g s'
                               in (f' x, s')

instance Monad (Moi s) where
  return = pure
  Moi f >>= g = Moi $ \s -> let (x, s') = f s
                           in runMoi (g x) s'


--1
get :: Moi s s
get = Moi $ \s -> (s, s)

--2
put :: s -> Moi s ()
put x = Moi $ \_ -> ((), x)

--3
exec :: Moi s a -> s -> s
exec sa s = let (_, s') = runMoi sa s in s'

--4
eval :: Moi s a -> s -> a
eval sa s = let (x, _) = runMoi sa s in x

--5
modify :: (s -> s) -> Moi s ()
modify f = Moi $ \s -> ((), f s)

--put' :: s -> Moi s ()
--put' x = modify (const x)
