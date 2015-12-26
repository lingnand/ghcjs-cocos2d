{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module JavaScript.Cocos2d.Utils where

import Control.Monad
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Foreign.Callback
import JavaScript.Cocos2d.Class

convCallback :: Cocos2d m => (listener -> Callback (IO ()) -> IO ()) -> listener -> IO () -> m (IO ())
convCallback ffi l h = liftIO $ do
        cb <- syncCallback ContinueAsync h
        ffi l cb
        return $ releaseCallback cb

convCallback1 :: (FromJSVal a, Cocos2d m) => (listener -> Callback (JSVal -> IO ()) -> IO ()) -> listener -> (a -> IO ()) -> m (IO ())
convCallback1 ffi l h = liftIO $ do
        cb <- syncCallback1 ContinueAsync $ h <=< fromJSValUnchecked
        ffi l cb
        return $ releaseCallback cb

convCallback2 :: (FromJSVal a, FromJSVal b, Cocos2d m) => (listener -> Callback (JSVal -> JSVal -> IO ()) -> IO ()) -> listener -> (a -> b -> IO ()) -> m (IO ())
convCallback2 ffi l h = liftIO $ do
        cb <- syncCallback2 ContinueAsync $ \a b -> join $ h <$> fromJSValUnchecked a <*> fromJSValUnchecked b
        ffi l cb
        return $ releaseCallback cb
