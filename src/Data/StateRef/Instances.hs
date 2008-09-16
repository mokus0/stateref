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
--  
--  TODO: add millions of SPECIALIZE INSTANCE pragmas, for IO monad at
--  a minimum.
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

import Control.Monad.Trans
import Control.Monad.ST
import Data.STRef

import qualified Control.Monad.ST.Lazy
import qualified Data.STRef.Lazy

import Foreign.Storable
import Foreign.ForeignPtr

-- m a in semi-arbitrary monad m
-- (cannot have instance Monad m => ReadRef (m a) m a, because this activates
-- functional dependencies that would overconstrain totally unrelated instances
-- because of the possibility of the future addition of, e.g., instance Monad TMVar)
instance Monad m => NewRef (IO a) m a where
        newRef ro = return (return ro)
instance MonadIO m => ReadRef (IO a) m a where
        readRef = liftIO

instance Monad m => NewRef (ST s a) m a where
        newRef ro = return (return ro)
instance ReadRef (ST s a) (ST s) a where
        readRef = id
instance MonadIO m => ReadRef (ST RealWorld a) m a where
        readRef = liftIO . stToIO

-- IORef in IO-compatible monads
instance DefaultStateRef (IORef a) IO a
instance MonadIO m => NewRef (IORef a) m a where
        newRef = liftIO . newIORef
instance MonadIO m => ReadRef (IORef a) m a where
        readRef = liftIO . readIORef
instance MonadIO m => WriteRef (IORef a) m a where
        writeRef r = liftIO . writeIORef r
instance MonadIO m => ModifyRef (IORef a) m a where
        atomicModifyRef r = liftIO . atomicModifyIORef r
        modifyRef r = liftIO . modifyIORef r

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
-- (STRef RealWorld) in IO monad (not MonadIO instances, because the m
--  would overlap with (ST s) even though there's no instance MonadIO (ST a))
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

-- MVar in IO-compatible monads (constructable but not usable as a "normal" state ref)
instance MonadIO m => NewRef (MVar a) m (Maybe a) where
	newRef Nothing = liftIO newEmptyMVar
	newRef (Just x) = liftIO (newMVar x)
#endif

-- ForeignPtrs, Ptrs, etc., in IO-compatible monads
instance (Storable a, MonadIO m) => NewRef (ForeignPtr a) m a where
        newRef val = liftIO $ do
                ptr <- mallocForeignPtr
                withForeignPtr ptr (\ptr -> poke ptr val)
                return ptr
instance (Storable a, MonadIO m) => ReadRef (ForeignPtr a) m a where
        readRef ptr = liftIO (withForeignPtr ptr peek)
instance (Storable a, MonadIO m) => WriteRef (ForeignPtr a) m a where
        writeRef ptr val = liftIO (withForeignPtr ptr (\ptr -> poke ptr val))
instance (Storable a, MonadIO m) => ModifyRef (ForeignPtr a) m a

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
