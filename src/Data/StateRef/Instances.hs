{-
 -      ``Data/StateRef/Instances''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
        CPP,
        MultiParamTypeClasses,
        FlexibleInstances
  #-}

module Data.StateRef.Instances where

#ifdef useSTM
import Data.StateRef.Instances.STM
#endif

import Data.StateRef.Classes

import Data.IORef
import Control.Concurrent.MVar

import Control.Monad.ST
import Data.STRef

-- IORef in IO monad
instance DefaultStateRef (IORef a) IO a
instance NewRef (IORef a) IO a where
        newRef = newIORef
instance ReadRef (IORef a) IO a where
        readRef = readIORef
instance WriteRef (IORef a) IO a where
        writeRef = writeIORef
instance ModifyRef (IORef a) IO a where
        modifyRef = modifyIORef

-- (STRef s) in (ST s) monad
instance DefaultStateRef (STRef s a) (ST s) a
instance NewRef (STRef s a) (ST s) a where
        newRef = newSTRef
instance ReadRef (STRef s a) (ST s) a where
        readRef = readSTRef
instance WriteRef (STRef s a) (ST s) a where
        writeRef = writeSTRef
instance ModifyRef (STRef s a) (ST s) a where

-- (STRef RealWorld) in IO monad
instance NewRef (STRef RealWorld a) IO a where
        newRef = stToIO . newRef
instance ReadRef (STRef RealWorld a) IO a where
        readRef = stToIO . readRef
instance WriteRef (STRef RealWorld a) IO a where
        writeRef r = stToIO . writeRef r
instance ModifyRef (STRef RealWorld a) IO a where
        modifyRef r = stToIO . modifyRef r

-- MVar in IO monad (constructable but not usable as a "normal" state ref)
instance NewRef (MVar a) IO (Maybe a) where
	newRef Nothing = newEmptyMVar
	newRef (Just x) = newMVar x

-- this probably should not actually be defined, unless Ptr supports a finalizer.
-- Probably should change to use ForeignPtr
-- instance Storable a => NewRef (Ptr a) IO a where
--         newRef val = do
--                 ptr <- malloc
--                 poke ptr val
--                 return ptr
-- instance Storable a => ReadRef (Ptr a) IO a where
--         readRef = peek
-- instance Storable a => WriteRef (Ptr a) IO a where
--         writeRef = poke
-- instance Storable a => ModifyRef (Ptr a) IO a where



-- Might be nice (maybe put some of these hypotheticals into a separate module)
-- 
-- instance (MonadState s m) => ReadRef (s -> a) m a where
-- 	readRef = gets
-- 
-- instance MonadReader r m => ReadRef (r -> a) m a where
-- 	readRef = asks

-- this is an instance I would like to make, but it opens
-- a big can of worms... it requires incoherent instances, for one.
-- perhaps I ought to give up the abstractness of 'sr' in the class
-- definition; i don't know if that gets me anywhere though... 
--
-- note that as long as only these instances exist, there is no
-- actual overlap.  maybe it's not such a bad idea.  on the other
-- hand, a corresponding instance for Reader would be nice too, and
-- that would be a duplicate instance (because only the context would
-- differ).
--
-- instance (MonadState s1 m,
--           StateRef s2 m a)
--                 => StateRef (s1 -> s2) m a
--         where
--                 readRef f       = do
--                         s1 <- get
--                         readRef (f s1)
--                 writeRef f val  = do
--                         s1 <- get
--                         writeRef (f s1) val
--                 modifyRef f g = do
--                         s1 <- get
--                         modifyRef (f s1) g
