{-
 -      ``StateRef.hs''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module Data.StateRef
        ( module Data.StateRef
        , module Data.StateRef.Classes
        ) where

import Data.StateRef.Classes
import Data.StateRef.Instances

newDefaultRef :: (DefaultStateRef sr m a) => a -> m sr
newDefaultRef = newRef

readDefaultRef :: (DefaultStateRef sr m a, ReadRef sr m a) => sr -> m a
readDefaultRef = readRef

writeDefaultRef :: (DefaultStateRef sr m a, WriteRef sr m a) => sr -> a -> m ()
writeDefaultRef = writeRef

modifyDefaultRef :: (DefaultStateRef sr m a, ModifyRef sr m a) => sr -> (a -> a) -> m ()
modifyDefaultRef = modifyRef


-- can't use fmap, because Functor isn't a superclass of Monad, 
-- despite the fact that every Monad is a functor.
readsRef :: (ReadRef sr m a,
             Monad m) =>
            sr -> (a -> b) -> m b
readsRef r f = do
        x <- readRef r
        return (f x)

-- a useful thing, as well as an example of why we want the "DefaultStateRef" class
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
