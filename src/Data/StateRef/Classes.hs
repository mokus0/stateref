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
