{-
 -      ``StateRef.hs''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE 
    MultiParamTypeClasses,
    FunctionalDependencies
  #-}

module Data.StateRef where

class Monad m => NewRef sr m a | sr -> a where
        newRef :: a -> m sr

class Monad m => WriteRef sr m a | sr -> a where
        writeRef :: sr -> a -> m ()

class Monad m => ReadRef sr m a | sr -> a where
        readRef :: sr -> m a

-- consider whether there needs to be something along the lines of
-- 'atomicModifyIORef' and/or 'modifyMVar'
class (Monad m, ReadRef sr m a, WriteRef sr m a) => ModifyRef sr m a | sr -> a where
        modifyRef :: sr -> (a -> a) -> m ()
        modifyRef ref f = do
                x <- readRef ref
                let x' = f x
                writeRef ref x'
                return ()

-- the sole real purpose for the "Default" classes' existence is as a
-- carrier for their fundeps.
class (NewRef sr m a)
       => DefaultStateRef sr m a | m a -> sr

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
