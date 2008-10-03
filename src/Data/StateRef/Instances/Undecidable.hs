{-
 -      ``Data/StateRef/Instances/Undecidable''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
        MultiParamTypeClasses,
        FlexibleInstances,
        UndecidableInstances
  #-}

module Data.StateRef.Instances.Undecidable where

import Data.StateRef.Classes

-- |Wrap a state reference that supports reading and writing, and add a
-- potentially thread-unsafe 'ModifyRef' instance.
newtype UnsafeModifyRef sr = UnsafeModifyRef sr

instance ReadRef sr m a => ReadRef (UnsafeModifyRef sr) m a where
        readRef (UnsafeModifyRef sr) = readRef sr
instance WriteRef sr m a => WriteRef (UnsafeModifyRef sr) m a where
        writeRef (UnsafeModifyRef sr) = writeRef sr
instance (ReadRef sr m a, WriteRef sr m a) => ModifyRef (UnsafeModifyRef sr) m a

