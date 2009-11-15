{-
 -      ``Data/MRef/Instances/STM''
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
module Data.MRef.Instances.STM
        ( STM
#ifdef useTMVar
        , TMVar
#endif
        , TVar
        
        , atomically
        ) where

import Data.MRef.Types
import Data.StateRef (readReference, writeReference, newReference)
import Data.StateRef.Instances.STM ()

import Control.Concurrent.STM

#ifdef useTMVar
--TMVar in STM monad
instance DefaultMRef (TMVar a) STM a
instance NewMRef (TMVar a) STM a where
        newMRef = newTMVar
        newEmptyMRef = newEmptyTMVar

instance TakeMRef (TMVar a) STM a where
	takeMRef = takeTMVar
instance PutMRef (TMVar a) STM a where
	putMRef = putTMVar

-- TMVar in IO monad
instance NewMRef (TMVar a) IO a where
        newMRef = newTMVarIO
        newEmptyMRef = newEmptyTMVarIO
        
instance TakeMRef (TMVar a) IO a where
        takeMRef = atomically . takeMRef
instance PutMRef (TMVar a) IO a where
        putMRef ref = atomically . putMRef ref
#endif

-- incidental instances, which may occasionally be handy in a pinch
-- TVars containing "Maybe" values in STM monad.
-- Also use as default if TMVar isn't available.
#ifndef useTMVar
instance DefaultMRef (TVar (Maybe a)) STM a
#endif
instance NewMRef (TVar (Maybe a)) STM a where
        newMRef = newReference . Just
        newEmptyMRef = newReference Nothing

instance TakeMRef (TVar (Maybe a)) STM a where
        takeMRef ref = do
                x <- readReference ref
                case x of
                        Nothing -> retry
                        Just x -> do
                                writeReference ref Nothing
                                return x
instance PutMRef (TVar (Maybe a)) STM a where
        putMRef ref val = do
                x <- readReference ref
                case x of
                        Nothing -> writeReference ref (Just val)
                        Just x -> retry

-- TVars containing "Maybe" values in IO monad
instance NewMRef (TVar (Maybe a)) IO a where
        newMRef = newReference . Just
        newEmptyMRef = newReference Nothing
instance TakeMRef (TVar (Maybe a)) IO a where
        takeMRef = atomically . takeMRef
instance PutMRef (TVar (Maybe a)) IO a where
        putMRef ref = atomically . putMRef ref
