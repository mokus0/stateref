{-
 -      ``Data/StateRef/Instances/STM''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
        CPP,
        MultiParamTypeClasses,
        FlexibleInstances,
        IncoherentInstances
  #-}

-- |This module exports no new symbols of its own.  It defines 
--  basic class instances for creating, reading, and writing 'TVar's and
--  (if available) 'TMVar's, and re-exports the types for which it defines 
--  instances as well as the 'atomically' function, which is indispensible
--  when playing with this stuff in ghci.
--
--  Note that this module declares incoherent instances.  The universe should
--  refrain from imploding on itself as long as you don't define
--  \"instance MonadIO STM\".  However, hugs doesn't seem to support 
--  overlapping instances, so I may have to give up on the dream of MonadIO 
--  everywhere, or introduce some major conditional compilation stuff. (or
--  abandon hugs support)

module Data.StateRef.Instances.STM
        ( STM
        , TVar
#ifdef useTMVar
        , TMVar
#endif
        
        , atomically
        ) where

import Data.StateRef.Types

import Control.Monad.Trans

import Control.Concurrent.STM

-- (STM a) in STM and IO-compatible monads
instance ReadRef (STM a) STM a where
        readRef = id
instance MonadIO m => ReadRef (STM a) m a where
        readRef = liftIO . atomically

-- TVar in STM monad
instance HasRef STM where
    newRef x = do
        sr <- newTVar x
        return (Ref sr)
instance NewRef (TVar a) STM a where
        newReference = newTVar
instance ReadRef (TVar a) STM a where
        readRef = readTVar
instance WriteRef (TVar a) STM a where
        writeRef = writeTVar
instance ModifyRef (TVar a) STM a

-- TVar in IO-compatible monads
instance MonadIO m => NewRef (TVar a) m a where
        newReference = liftIO . newTVarIO
instance MonadIO m => ReadRef (TVar a) m a where
        readRef = liftIO . atomically . readRef
instance MonadIO m => WriteRef (TVar a) m a where
        writeRef ref = liftIO . atomically . writeRef ref
instance MonadIO m => ModifyRef (TVar a) m a where
        modifyRef ref = liftIO . atomically . modifyRef ref

#ifdef useTMVar
-- TMVar in STM monad
instance NewRef (TMVar a) STM (Maybe a) where
	newReference Nothing = newEmptyTMVar
	newReference (Just x) = newTMVar x
instance ReadRef (TMVar a) STM (Maybe a) where
	readRef tmv = fmap Just (readTMVar tmv) `orElse` return Nothing

-- TMVar in IO-compatible monad
instance MonadIO m => NewRef (TMVar a) m (Maybe a) where
	newReference Nothing = liftIO newEmptyTMVarIO
	newReference (Just x) = liftIO (newTMVarIO x)
instance MonadIO m => ReadRef (TMVar a) m (Maybe a) where
	readRef = liftIO . atomically . readRef
#endif
