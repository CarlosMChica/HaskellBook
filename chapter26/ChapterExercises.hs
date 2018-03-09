module ChapterExercises where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Functor.Identity
--1

rDec :: Num a => Reader a a
rDec = ReaderT $ \a -> Identity a - 1

--2

rDec' :: Num a => Reader a a
rDec' = ReaderT $ Identity . subtract 1

--3

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ Identity . show

--4 already in 3

--5

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \a -> (print . concat $ ["Hi: ", show a]) *> (return $ a + 1)
--rPrintAndInc = ReaderT $ \a -> do
--  print $ "Hi: " ++ show a
--  return $ a + 1

--6

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> (print . concat $ ["Hi: ", show s]) *> (return (show s, s +1))
--sPrintIncAccum = StateT $ \a -> do
--  print . concat $ ["Hi: ", show a]
--  return (show a, a + 1)

rPrintWith :: (Num a, Show a) => (a -> b) -> ReaderT a IO b
rPrintWith f = ReaderT $ \a -> (print . concat $ ["Hi: ", show a]) *> (return . f $ a)

rPrintAndInc' :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc' = rPrintWith (+1)

sPrintIncAccum' :: (Num a, Show a) => StateT a IO String
--sPrintIncAccum'= StateT . runReaderT . rPrintWith $ \a -> (show a, a + 1)
--sPrintIncAccum'= StateT . runReaderT . rPrintWith $ (,) <$> show <*> (+1)
sPrintIncAccum'= StateT . runReaderT . rPrintWith $ liftA2 (,) show (+1)
