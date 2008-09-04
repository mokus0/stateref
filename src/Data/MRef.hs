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

newDefaultMRef :: (DefaultMRef sr m a) => a -> m sr
newDefaultMRef = newMRef

newDefaultEmptyMRef :: (DefaultMRef sr m a) => m sr
newDefaultEmptyMRef = newEmptyMRef

