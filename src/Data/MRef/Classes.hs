{-
 -      ``Data/MRef/Classes''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
        MultiParamTypeClasses,
        FunctionalDependencies
  #-}

-- |This module defines the \"MRef\" abstraction, which is a set of
--  type-classes for things that behave like 'Control.Concurrent.MVar.MVar's.
--  See the documentation there for more info.  
--
--  This interface may be subject to future expansion.  Presently, rather 
--  than providing something like 'Control.Concurrent.MVar.tryTakeMVar',
--  instances for \"'Data.StateRef.Classes.ReadRef' sr m ('Maybe' a)\" are
--  provided, giving 'Data.StateRef.Classes.readRef' the same type 
--  tryTakeMRef would have if it existed.  There is currently nothing like
--  'Control.Concurrent.MVar.tryPutMVar', though.  Perhaps there should be.
--  Or, perhaps this is the sort of thing the weird (to me) signature of
--  'Data.IORef.atomicModifyIORef' is for, and an argument for a similar
--  signature for 'Data.StateRef.Classes.modifyStateRef' or the addition of
--  a new atomicModifyStateRef function.
--
--  I would like to resolve these questions in version 0.3 of this package.
module Data.MRef.Classes where

class Monad m => NewMRef sr m a | sr -> a where
        -- |See 'Control.Concurrent.MVar.newMVar'
        newMRef :: a -> m sr
        -- |See 'Control.Concurrent.MVar.newEmptyMVar'
        newEmptyMRef :: m sr
class Monad m => TakeMRef sr m a | sr -> a where
        -- |See 'Control.Concurrent.MVar.takeMVar'
	takeMRef :: sr -> m a
class Monad m => PutMRef sr m a | sr -> a where
        -- |See 'Control.Concurrent.MVar.putMVar'
	putMRef :: sr -> a -> m ()

class Monad m => DefaultMRef sr m a | sr -> a, m a -> sr
