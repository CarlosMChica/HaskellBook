module WrapItUp where

newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = MaybeT (ExceptT (ReaderT (const (return (Right (Just 1))))))
