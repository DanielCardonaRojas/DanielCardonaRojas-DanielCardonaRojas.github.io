---
title: Writing a rwst monad transformer
date: 2016-07-05
---
#Writing a Monad Transformer

Typical examples of monads are Reader, Writer, Maybe, Either, State. Today I want to recap my part of my understanding
of monads through the RWST monad. Also make it a monad transformer for the exercise.

```haskell

import Control.Monad.Trans hiding (RWST)
import Control.Monad.State.Class
```

This how you encode the rwst monad. It has to be a function that reads both a read only value r and some state s.
The RWST provides the following functionality, read a configuration value, thread a mutable state value, and accumulate a monoidal value.
Stating it in other words the RWST monad provides a computational context where one have state, have a read only configuration, acumulate a value.

``` haskell
newtype RWST r w s m a = RWST {runRWST :: r -> s -> m (a, s, w) }
```
``` haskell 
instance (Monad m, Monoid w) => Monad (RWST r w s m) where
    return a = RWST $ \r s -> return (a,s,mempty) 
    m >>= k =
       RWST $ \r s -> do
          (x, st, w) <- (runRWST m) r s
          (x', st',w') <- (runRWST (k x)) r st
          return (x', st', w `mappend` w')


instance (Monad m, Monoid w) => Applicative (RWST r w s m) where
    pure a = RWST $ \r s -> return (a,s,mempty)
    (RWST mf) <*> (RWST v) = 
     RWST $ \r s -> do
         (f,st,w) <- mf r s
         (a, st',w') <- v r st
         return (f a, st', w `mappend` w')
      

```
``` haskell         
instance (Functor m, Monoid w) => Functor (RWST r w s m) where
    fmap f (RWST v) = RWST $ \r s -> fmap (\(x,st,w) -> (f x,st,w)) (v r s) 
```
MonadTrans typeclass is used for defining how a monad transformer talks to its inner monad.
Or how the inner monad gets lifted into the context a the wrapper monad. 
```haskell
instance (Monoid w) => MonadTrans (RWST r w s) where
    lift m = RWST $ \r s -> do
        a <- m
        return $ (a,s,mempty)


instance (Monoid w, MonadIO m) => MonadIO (RWST r w s m) where
    liftIO = lift . liftIO 

```
