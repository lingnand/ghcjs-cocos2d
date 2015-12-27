{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module JavaScript.Cocos2d.Utils where

import Control.Monad
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Foreign.Callback
import JavaScript.Cocos2d.Class

convCallback :: Cocos2d m => (Callback (IO ()) -> IO ()) -> IO () -> m (IO ())
convCallback ffi h = liftIO $ do
        cb <- syncCallback ContinueAsync h
        ffi cb
        return $ releaseCallback cb

convCallback1 :: (FromJSVal a, Cocos2d m) => (Callback (JSVal -> IO ()) -> IO ()) -> (a -> IO ()) -> m (IO ())
convCallback1 ffi h = liftIO $ do
        cb <- syncCallback1 ContinueAsync $ h <=< fromJSValUnchecked
        ffi cb
        return $ releaseCallback cb

convCallback2 :: (FromJSVal a, FromJSVal b, Cocos2d m) => (Callback (JSVal -> JSVal -> IO ()) -> IO ()) -> (a -> b -> IO ()) -> m (IO ())
convCallback2 ffi h = liftIO $ do
        cb <- syncCallback2 ContinueAsync $ \a b -> join $ h <$> fromJSValUnchecked a <*> fromJSValUnchecked b
        ffi cb
        return $ releaseCallback cb
