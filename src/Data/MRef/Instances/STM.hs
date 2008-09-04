{-
 -      ``Data/MRef/Instances/STM''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
        MultiParamTypeClasses,
        FlexibleInstances
  #-}

module Data.MRef.Instances.STM
        ( STM
        , TMVar
        , TVar
        
        , atomically
        ) where

import Data.MRef.Classes
import Data.StateRef (readRef, writeRef, newRef)
import Data.StateRef.Instances.STM ()

import Control.Concurrent.STM

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

-- incidental instances, which may occasionally be handy in a pinch
-- TVars containing "Maybe" values in STM monad
instance NewMRef (TVar (Maybe a)) STM a where
        newMRef = newRef . Just
        newEmptyMRef = newRef Nothing

instance TakeMRef (TVar (Maybe a)) STM a where
        takeMRef ref = do
                x <- readRef ref
                case x of
                        Nothing -> retry
                        Just x -> do
                                writeRef ref Nothing
                                return x
instance PutMRef (TVar (Maybe a)) STM a where
        putMRef ref val = do
                x <- readRef ref
                case x of
                        Nothing -> writeRef ref (Just val)
                        Just x -> retry

-- TVars containing "Maybe" values in IO monad
instance NewMRef (TVar (Maybe a)) IO a where
        newMRef = newRef . Just
        newEmptyMRef = newRef Nothing
instance TakeMRef (TVar (Maybe a)) IO a where
        takeMRef = atomically . takeMRef
instance PutMRef (TVar (Maybe a)) IO a where
        putMRef ref = atomically . putMRef ref
