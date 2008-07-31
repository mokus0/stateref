{-
 -      ``StateRef.hs''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE 
    MultiParamTypeClasses,
    FunctionalDependencies,
    FlexibleInstances
  #-}

module Data.StateRef (
        module Data.StateRef,
        
        IORef, STRef, TVar
        ) where

import Control.Monad

import Data.IORef

import Control.Monad.ST
import Data.STRef

import Control.Concurrent.STM

import Foreign

class Monad m => NewRef sr m a | sr -> a where
        newRef :: a -> m sr

class Monad m => WriteRef sr m a | sr -> a where
        writeRef :: sr -> a -> m ()

class Monad m => ReadRef sr m a | sr -> a where
        readRef :: sr -> m a

class (Monad m, ReadRef sr m a, WriteRef sr m a) => ModifyRef sr m a | sr -> a where
        modifyRef :: sr -> (a -> a) -> m ()
        modifyRef ref f = do
                x <- readRef ref
                let x' = f x
                writeRef ref x'
                return ()

class (NewRef sr m a)
       => DefaultStateRef sr m a | m a -> sr where
        newRef' :: a -> m sr
        newRef' = newRef

readRef' :: (DefaultStateRef sr m a, ReadRef sr m a) => sr -> m a
readRef' = readRef

writeRef' :: (DefaultStateRef sr m a, WriteRef sr m a) => sr -> a -> m ()
writeRef' = writeRef

modifyRef' :: (DefaultStateRef sr m a, ModifyRef sr m a) => sr -> (a -> a) -> m ()
modifyRef' = modifyRef

instance DefaultStateRef (IORef a) IO a
instance NewRef (IORef a) IO a where
        newRef = newIORef
instance ReadRef (IORef a) IO a where
        readRef = readIORef
instance WriteRef (IORef a) IO a where
        writeRef = writeIORef
instance ModifyRef (IORef a) IO a where
        modifyRef = modifyIORef

instance DefaultStateRef (STRef s a) (ST s) a
instance NewRef (STRef s a) (ST s) a where
        newRef = newSTRef
instance ReadRef (STRef s a) (ST s) a where
        readRef = readSTRef
instance WriteRef (STRef s a) (ST s) a where
        writeRef = writeSTRef
instance ModifyRef (STRef s a) (ST s) a where

instance NewRef (STRef RealWorld a) IO a where
        newRef = stToIO . newRef
instance ReadRef (STRef RealWorld a) IO a where
        readRef = stToIO . readRef
instance WriteRef (STRef RealWorld a) IO a where
        writeRef r = stToIO . writeRef r
instance ModifyRef (STRef RealWorld a) IO a where
        modifyRef r = stToIO . modifyRef r

instance DefaultStateRef (TVar a) STM a
instance NewRef (TVar a) STM a where
        newRef = newTVar
instance ReadRef (TVar a) STM a where
        readRef = readTVar
instance WriteRef (TVar a) STM a where
        writeRef = writeTVar
instance ModifyRef (TVar a) STM a where

instance NewRef (TVar a) IO a where
        newRef                  = newTVarIO
instance ReadRef (TVar a) IO a where
        readRef ref             = atomically (readTVar ref)
instance WriteRef (TVar a) IO a where
        writeRef ref val        = atomically (writeTVar ref val)
instance ModifyRef (TVar a) IO a where
        modifyRef ref f         = atomically (modifyRef ref f)

instance Storable a => NewRef (Ptr a) IO a where
        newRef val = do
                ptr <- malloc
                poke ptr val
                return ptr
instance Storable a => ReadRef (Ptr a) IO a where
        readRef = peek
instance Storable a => WriteRef (Ptr a) IO a where
        writeRef = poke
instance Storable a => ModifyRef (Ptr a) IO a where

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
-- that one does have potential overlap.
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

-- can't use fmap, because Functor isn't a superclass of Monad, 
-- despite the fact that every Monad is a functor.
readsRef :: (ReadRef sr m a,
             Monad m) =>
            sr -> (a -> b) -> m b
readsRef r f = do
        x <- readRef r
        return (f x)

newCounter :: (DefaultStateRef sr m1 a,
	       ModifyRef sr m1 a,
               NewRef sr m a,
               Enum a) =>
              a -> m (m1 a)
newCounter n = do
        c <- newRef n
        return $ do
		x <- readRef c
		writeRef' c (succ x)
                return x
