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
        , module Data.StateRef.Types
        , module Data.StateRef.Instances
        , module Data.Accessor
        ) where

import Data.StateRef.Types
import Data.StateRef.Instances
import Data.Accessor

-- |Create a reference and constrain its type to be the default reference type
-- for the monad in which it is being created.  See 'newRef'.
newDefaultRef :: HasRef m => a -> m (Ref m a)
newDefaultRef = newRef

-- |Read a reference and constrain its type to be the default reference type
-- for the monad in which it is being read.  See 'readRef'.
readDefaultRef :: HasRef m => Ref m a -> m a
readDefaultRef = readRef

-- |Write a reference and constrain its type to be the default reference type
-- for the monad in which it is being written.  See 'writeRef'
writeDefaultRef :: HasRef m => Ref m a -> a -> m ()
writeDefaultRef = writeRef

-- |Modify a reference and constrain its type to be the default reference type
-- for the monad in which it is being modified.  See 'modifyRef'.
atomicModifyDefaultRef :: HasRef m => Ref m a -> (a -> (a,b)) -> m b
atomicModifyDefaultRef = atomicModifyRef


-- |Modify a reference and constrain its type to be the default reference type
-- for the monad in which it is being modified.  See 'modifyRef'.
modifyDefaultRef :: HasRef m => Ref m a -> (a -> a) -> m ()
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
newCounter n = do
        c <- newRef n
        return $ do
            x <- readDefaultRef c
            writeDefaultRef c (succ x)
            return x

-- |Create a \"lapse reader\" (suggestions for better terminology are more 
-- than welcome), a sort of a time-lapse of the variable.  The first 
-- motivating instance for this operation was a clock in a simple simulation
-- application.  Given a 'TVar' 'Double' called \"clock\", a useful
-- value \"dT\" is yielded by the expression: 'mkLapseReader' clock (-)
-- 
-- note that there's a unification ghc missed here:
-- the fundep sr -> a on NewRef and DefaultStateRef should cause a and a1 
-- to be unified, because of the 2 constraints:
--      NewRef sr1 m a
--      DefaultStateRef sr1 m1 a1
-- this isn't a \"bug\" because the type is still valid, but it seems like
-- something ghc \"ought\" to do, since a and a1 are doomed to unification
-- anyway.
mkLapseReader var f = do
        startVal <- readRef var
        prevRef <- newRef startVal
        
        return $ do
                newVal <- readRef var
                prevVal <- readDefaultRef prevRef
                
                writeRef prevRef newVal
                
                return (f newVal prevVal)
