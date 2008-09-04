{-
 -      ``Data/MRef/Classes''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
        MultiParamTypeClasses,
        FunctionalDependencies
  #-}

module Data.MRef.Classes where

class Monad m => NewMRef sr m a | sr -> a where
        newMRef :: a -> m sr
        newEmptyMRef :: m sr
class Monad m => TakeMRef sr m a | sr -> a where
	takeMRef :: sr -> m a
class Monad m => PutMRef sr m a | sr -> a where
	putMRef :: sr -> a -> m ()

class (NewMRef sr m a)
       => DefaultMRef sr m a | m a -> sr

newDefaultMRef :: (DefaultMRef sr m a) => a -> m sr
newDefaultMRef = newMRef

newDefaultEmptyMRef :: (DefaultMRef sr m a) => m sr
newDefaultEmptyMRef = newEmptyMRef

