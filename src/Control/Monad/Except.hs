{-# LANGUAGE PackageImports #-}

module Control.Monad.Except
  ( tryError,
    withError,
    handleError,
    mapError,
    Ext.runExceptT,
    module Ext,
  )
where

import qualified "mtl" Control.Monad.Except as Ext
import Prelude

-- | 'MonadError' analogue to the 'Control.Exception.try' function.
tryError :: Ext.MonadError e m => m a -> m (Either e a)
tryError action = (Right <$> action) `Ext.catchError` (pure . Left)

-- | 'MonadError' analogue to the 'withExceptT' function.
-- Modify the value (but not the type) of an error.  The type is
-- fixed because of the functional dependency @m -> e@.  If you need
-- to change the type of @e@ use 'mapError'.
withError :: Ext.MonadError e m => (e -> e) -> m a -> m a
withError f action = tryError action >>= either (Ext.throwError . f) pure

-- | As 'handle' is flipped 'Control.Exception.catch', 'handleError'
-- is flipped 'catchError'.
handleError :: Ext.MonadError e m => (e -> m a) -> m a -> m a
handleError = flip Ext.catchError

-- | 'MonadError' analogue of the 'mapExceptT' function.  The
-- computation is unwrapped, a function is applied to the @Either@, and
-- the result is lifted into the second 'MonadError' instance.
mapError :: (Ext.MonadError e m, Ext.MonadError e' n) => (m (Either e a) -> n (Either e' b)) -> m a -> n b
mapError f action = f (tryError action) >>= Ext.liftEither