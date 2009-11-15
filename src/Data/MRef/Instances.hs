{-
 -      ``Data/MRef/Instances''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
        CPP,
        MultiParamTypeClasses,
        FlexibleInstances
  #-}

-- |This module exports no new symbols of its own.  It defines 
--  basic class instances for creating, reading, and writing 'MVar's, and
--  re-exports 'MVar'.
module Data.MRef.Instances
        ( MVar
        , MonadIO(..)

#ifdef useSTM
        , module Data.MRef.Instances.STM
#endif
        ) where

#ifdef useSTM
import Data.MRef.Instances.STM
#endif

import Data.MRef.Types

import Control.Concurrent.MVar
import Control.Monad.Trans

-- preferred instances
-- MVar in IO monad
instance DefaultMRef (MVar a) IO a
instance MonadIO m => NewMRef (MVar a) m a where
        newMRef = liftIO . newMVar
        newEmptyMRef = liftIO newEmptyMVar
instance MonadIO m => TakeMRef (MVar a) m a where
	takeMRef = liftIO . takeMVar
instance MonadIO m => PutMRef (MVar a) m a where
	putMRef r = liftIO . putMVar r
