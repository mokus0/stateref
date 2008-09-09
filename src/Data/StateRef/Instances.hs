{-
 -      ``Data/StateRef/Instances''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
        CPP,
        MultiParamTypeClasses,
        FlexibleInstances
  #-}

-- |This module exports no new symbols of its own.  It defines several 
--  basic class instances for creating, reading, and writing standard
--  reference types, and re-exports the types for which it defines instances.
module Data.StateRef.Instances
        ( IORef
        , MVar
        
        , STRef
        , ST
        , RealWorld
        
        , ForeignPtr
        
#ifdef useSTM
        , module Data.StateRef.Instances.STM
#endif
        
        ) where

#ifdef useSTM
import Data.StateRef.Instances.STM
#endif

import Data.StateRef.Classes

import Data.IORef
import Control.Concurrent.MVar

import Control.Monad.ST
import Data.STRef

import qualified Control.Monad.ST.Lazy
import qualified Data.STRef.Lazy

import Foreign.Storable
import Foreign.ForeignPtr

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

#ifdef NotHADDOCK
-- (haddock doesn't like "FlexibleInstances", it seems)
-- (STRef RealWorld) in IO monad
instance NewRef (STRef RealWorld a) IO a where
        newRef = stToIO . newRef
instance ReadRef (STRef RealWorld a) IO a where
        readRef = stToIO . readRef
instance WriteRef (STRef RealWorld a) IO a where
        writeRef r = stToIO . writeRef r
instance ModifyRef (STRef RealWorld a) IO a where
        modifyRef r = stToIO . modifyRef r

-- (STRef s) in lazy (ST s) monad
instance DefaultStateRef (STRef s a) (Control.Monad.ST.Lazy.ST s) a
instance NewRef (STRef s a) (Control.Monad.ST.Lazy.ST s) a where
        newRef = Data.STRef.Lazy.newSTRef
instance ReadRef (STRef s a) (Control.Monad.ST.Lazy.ST s) a where
        readRef = Data.STRef.Lazy.readSTRef
instance WriteRef (STRef s a) (Control.Monad.ST.Lazy.ST s) a where
        writeRef = Data.STRef.Lazy.writeSTRef
instance ModifyRef (STRef s a) (Control.Monad.ST.Lazy.ST s) a where

-- MVar in IO monad (constructable but not usable as a "normal" state ref)
instance NewRef (MVar a) IO (Maybe a) where
	newRef Nothing = newEmptyMVar
	newRef (Just x) = newMVar x
#endif

instance Storable a => NewRef (ForeignPtr a) IO a where
        newRef val = do
                ptr <- mallocForeignPtr
                withForeignPtr ptr (\ptr -> poke ptr val)
                return ptr
instance Storable a => ReadRef (ForeignPtr a) IO a where
        readRef ptr = withForeignPtr ptr peek
instance Storable a => WriteRef (ForeignPtr a) IO a where
        writeRef ptr val = withForeignPtr ptr (\ptr -> poke ptr val)
instance Storable a => ModifyRef (ForeignPtr a) IO a

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
