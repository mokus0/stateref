{-
 -      ``Data/MRef''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module Data.MRef 
        ( module Data.MRef
        , module Data.MRef.Classes
        , module Data.MRef.Instances
        ) where

import Data.MRef.Classes
import Data.MRef.Instances

-- |Create a m-reference and constrain its type to be the default reference
-- type for the monad in which it is being created.  See 'newMRef'.
newDefaultMRef :: (DefaultMRef sr m a, NewMRef sr m a) => a -> m sr
newDefaultMRef = newMRef

-- |Create an empty m-reference and constrain its type to be the default
-- reference type for the monad in which it is being created.  See 'newMRef'.
newDefaultEmptyMRef :: (DefaultMRef sr m a, NewMRef sr m a) => m sr
newDefaultEmptyMRef = newEmptyMRef

-- |See 'takeMRef'.
takeDefaultMRef :: (DefaultMRef sr m a, TakeMRef sr m a) => sr -> m a
takeDefaultMRef = takeMRef

-- |See 'putMRef'.
putDefaultMRef :: (DefaultMRef sr m a, PutMRef sr m a) => sr -> a -> m ()
putDefaultMRef = putMRef
