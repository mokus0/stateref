{-# LANGUAGE 
    MultiParamTypeClasses,
    FunctionalDependencies,
    GADTs
  #-}

module Data.StateRef.Types where

data Ref m a where
    Ref :: ModifyRef sr m a => sr -> Ref m a

class Monad m => WriteRef sr m a | sr -> a where
        -- |Replace the existing value of the given reference with the provided value.
        writeReference :: sr -> a -> m ()

class Monad m => ReadRef sr m a | sr -> a where
        -- |Get the current value referenced by the given state reference.
        readReference :: sr -> m a

-- consider whether there needs to be something along the lines of
-- 'atomicModifyIORef' and/or 'modifyMVar' (specifically, what is the
-- motivation for the signature of atomicModifyIORef?  and is there a
-- downside to providing a signature like that to modifyReference?)

class (Monad m, ReadRef sr m a, WriteRef sr m a) => ModifyRef sr m a | sr -> a where
        -- |Atomically modify the contents of a reference.  This is
        -- implemented in a separate class (rather than a function with
        -- context (ReadRef sr m a, WriteRef sr m a)) because in most
        -- cases the default implementation cannot act atomically.
        atomicModifyReference :: sr -> (a -> (a,b)) -> m b
        atomicModifyReference ref f = do
                x <- readReference ref
                let (x', b) = f x
                writeReference ref x'
                return b
        
        -- |Same thing, but don't thread out the extra return.  Could perhaps
        -- be implemented slightly more efficiently than 'atomicModifyReference' in many cases.
        -- Note that implementations are expected to be atomic, if at all possible,
        -- but not strictly required to be.
        modifyReference :: sr -> (a -> a) -> m ()
        modifyReference ref f = do
                x <- readReference ref
                let x' = f x
                writeReference ref x'
                return ()

class Monad m => NewRef sr m a | sr -> a where
    -- |Construct a new mutable reference containing the provided value.
    newReference :: a -> m sr

class Monad m => HasRef m where
    -- |Construct a new mutable reference containing the provided value.
    newRef :: a -> m (Ref m a)
