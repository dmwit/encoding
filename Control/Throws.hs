{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses #-}
module Control.Throws where

import Control.Exception.Extensible
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State

class Throws e m where
    throwException :: e -> m a

instance Exception e => Throws e Identity where
    throwException = throw

{-instance MonadError e m => Throws e m where
    throwException = throwError-}

instance Throws e (Either e) where
    throwException = Left

instance Exception e => Throws e IO where
    throwException = throw

instance Throws e m => Throws e (StateT s m) where
    throwException x = StateT (\s -> throwException x)

instance Throws e m => Throws e (ReaderT s m) where
    throwException x = ReaderT (\s -> throwException x)