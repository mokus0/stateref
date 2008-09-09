{-
 -      ``Data/StateRef/Instances/STM''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
        CPP,
        MultiParamTypeClasses,
        FlexibleInstances
  #-}

-- |This module exports no new symbols of its own.  It defines 
--  basic class instances for creating, reading, and writing 'TVar's and
--  (if available) 'TMVar's, and re-exports the types for which it defines 
--  instances as well as the 'atomically' function, which is indispensible
--  when playing with this stuff in ghci.
module Data.StateRef.Instances.STM
        ( STM
        , TVar
#ifdef useTMVar
        , TMVar
#endif
        
        , atomically
        ) where

import Data.StateRef.Classes

import Control.Concurrent.STM

-- TVar in STM monad
instance DefaultStateRef (TVar a) STM a
instance NewRef (TVar a) STM a where
        newRef = newTVar
instance ReadRef (TVar a) STM a where
        readRef = readTVar
instance WriteRef (TVar a) STM a where
        writeRef = writeTVar
instance ModifyRef (TVar a) STM a

-- TVar in IO monad
instance NewRef (TVar a) IO a where
        newRef = newTVarIO
instance ReadRef (TVar a) IO a where
        readRef = atomically . readRef
instance WriteRef (TVar a) IO a where
        writeRef ref = atomically . writeRef ref
instance ModifyRef (TVar a) IO a where
        modifyRef ref = atomically . modifyRef ref

#ifdef useTMVar
-- TMVar in STM monad
instance NewRef (TMVar a) STM (Maybe a) where
	newRef Nothing = newEmptyTMVar
	newRef (Just x) = newTMVar x
instance ReadRef (TMVar a) STM (Maybe a) where
	readRef tmv = fmap Just (readTMVar tmv) `orElse` return Nothing

-- TMVar in IO monad
instance NewRef (TMVar a) IO (Maybe a) where
	newRef Nothing = newEmptyTMVarIO
	newRef (Just x) = newTMVarIO x
instance ReadRef (TMVar a) IO (Maybe a) where
	readRef = atomically . readRef
#endif
