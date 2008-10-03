{-
 -      ``Data/Accessor''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
        MultiParamTypeClasses,
        FlexibleInstances
  #-}

module Data.Accessor where

import Data.StateRef.Classes

newtype Getter m a = Getter (m a)
newtype Setter m a = Setter (a -> m ())
newtype Accessor m a = Accessor (Getter m a, Setter m a)

instance Monad m => ReadRef (Getter m a) m a where
        readRef (Getter x) = x

instance Monad m => WriteRef (Setter m a) m a where
        writeRef (Setter f) = f

instance Monad m => ReadRef (Accessor m a) m a where
        readRef (Accessor (Getter x, _)) = x
instance Monad m => WriteRef (Accessor m a) m a where
        writeRef (Accessor (_, Setter f)) = f

