{-
 -      ``Data/StateRef/Classes''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE 
    MultiParamTypeClasses,
    FunctionalDependencies
  #-}

module Data.StateRef.Classes where

class Monad m => NewRef sr m a | sr -> a where
        -- |Construct a new mutable reference containing the provided value.
        newRef :: a -> m sr

class Monad m => WriteRef sr m a | sr -> a where
        -- |Replace the existing value of the given reference with the provided value.
        writeRef :: sr -> a -> m ()

class Monad m => ReadRef sr m a | sr -> a where
        -- |Get the current value referenced by the given state reference.
        readRef :: sr -> m a

-- consider whether there needs to be something along the lines of
-- 'atomicModifyIORef' and/or 'modifyMVar' (specifically, what is the
-- motivation for the signature of atomicModifyIORef?  and is there a
-- downside to providing a signature like that to modifyRef?)

class (Monad m, ReadRef sr m a, WriteRef sr m a) => ModifyRef sr m a | sr -> a where
        -- |Atomically modify the contents of a reference.  This is
        -- implemented in a separate class (rather than a function with
        -- context (ReadRef sr m a, WriteRef sr m a)) because in most
        -- cases the default implementation cannot act atomically.
        modifyRef :: sr -> (a -> a) -> m ()
        modifyRef ref f = do
                x <- readRef ref
                let x' = f x
                writeRef ref x'
                return ()

-- |The 'DefaultStateRef' and 'Data.MRef.Classes.DefaultMRef' are used to 
-- internally constrain types that do not escape an expression, so that the 
-- compiler may choose an instance for the reference type (which it otherwise
-- would not, and maybe not even tell you until you tried to use your
-- function).  For an example, see the source for 'Data.StateRef.newCounter'.
-- See also 'Data.MRef.Classes.DefaultMRef'.
-- 
-- The sole purpose for these classes' existence is as a carrier for an
-- altered set of functional dependencies, which constrain the reference
-- type to be uniquely determined by the monad and the contained type.
class Monad m => DefaultStateRef sr m a | sr -> a, m a -> sr

--
-- in the absence of type families, it'd be nice to be able to say 
-- something like:
--
-- type StateRef m a = 
--         ( DefaultStateRef sr m a
--         , ReadRef sr m a
--         , WriteRef sr m a
--         ) => sr
--
-- this would ease the transition to type families later, assuming
-- they catch on.
--
