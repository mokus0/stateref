{-
 -      ``Data/MRef/Instances''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
        CPP,
        MultiParamTypeClasses,
        FlexibleInstances
  #-}

module Data.MRef.Instances where

#ifdef useSTM
import Data.MRef.Instances.STM
#endif

import Data.MRef.Classes

import Control.Concurrent.MVar

-- preferred instances
-- MVar in IO monad
instance DefaultMRef (MVar a) IO a
instance NewMRef (MVar a) IO a where
        newMRef = newMVar
        newEmptyMRef = newEmptyMVar
instance TakeMRef (MVar a) IO a where
	takeMRef = takeMVar
instance PutMRef (MVar a) IO a where
	putMRef = putMVar

