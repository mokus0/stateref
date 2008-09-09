{-
 -      ``StateRef.hs''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

-- |This module provides classes and instances for mutable state
-- references.  Various implementation exist in common usage, but
-- no way (until now ;-) to define functions using state references
-- which don't depend on the specific monad or reference type in use.
-- 
-- These modules use several language extensions, including multi-parameter
-- type classes and functional dependencies.
module Data.StateRef
        ( module Data.StateRef
        , module Data.StateRef.Classes
        , module Data.StateRef.Instances
        ) where

import Data.StateRef.Classes
import Data.StateRef.Instances

-- |Create a reference and constrain its type to be the default reference type
-- for the monad in which it is being created.  See 'newRef'.
newDefaultRef :: (DefaultStateRef sr m a) => a -> m sr
newDefaultRef = newRef

-- |Read a reference and constrain its type to be the default reference type
-- for the monad in which it is being read.  See 'readRef'.
readDefaultRef :: (DefaultStateRef sr m a, ReadRef sr m a) => sr -> m a
readDefaultRef = readRef

-- |Write a reference and constrain its type to be the default reference type
-- for the monad in which it is being written.  See 'writeRef'
writeDefaultRef :: (DefaultStateRef sr m a, WriteRef sr m a) => sr -> a -> m ()
writeDefaultRef = writeRef

-- |Modify a reference and constrain its type to be the default reference type
-- for the monad in which it is being modified.  See 'modifyRef'.
modifyDefaultRef :: (DefaultStateRef sr m a, ModifyRef sr m a) => sr -> (a -> a) -> m ()
modifyDefaultRef = modifyRef


-- |Essentially the same concept as 'Control.Monad.State.gets',
-- 'Control.Monad.State.asks', et al. Typically useful to read a field of
-- a referenced ADT by passing a record selector as the second argument.
readsRef :: (ReadRef sr m a,
             Monad m) =>
            sr -> (a -> b) -> m b
readsRef r f = do
        x <- readRef r
        return (f x)

-- |Construct a counter - a monadic value which, each time it is
-- evaluated, returns the 'succ' of the previous value returned.
newCounter :: (DefaultStateRef sr m1 a,
	       ModifyRef sr m1 a,
               NewRef sr m a,
               Enum a) =>
              a -> m (m1 a)
newCounter n = do
        c <- newRef n
        return $ do
		x <- readDefaultRef c
		writeDefaultRef c (succ x)
                return x
