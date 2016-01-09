{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module JavaScript.Cocos2d.Utils where

import Control.Monad
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Foreign.Callback

convCallback :: (Callback (IO ()) -> IO ()) -> IO () -> IO (IO ())
convCallback ffi h = snd <$> convCallbackWithReturn ffi h

convCallbackWithReturn :: (Callback (IO ()) -> IO a) -> IO () -> IO (a, IO ())
convCallbackWithReturn ffi h = do
    cb <- syncCallback ContinueAsync h
    a <- ffi cb
    return (a, releaseCallback cb)

convCallback1 :: FromJSVal a => (Callback (JSVal -> IO ()) -> IO ()) -> (a -> IO ()) -> IO (IO ())
convCallback1 ffi h = do
    cb <- syncCallback1 ContinueAsync $ mapM_ h <=< fromJSVal
    ffi cb
    return $ releaseCallback cb

convCallback1WithReturn :: FromJSVal a => (Callback (JSVal -> IO ()) -> IO b) -> (a -> IO ()) -> IO (b, IO ())
convCallback1WithReturn ffi h = do
    cb <- syncCallback1 ContinueAsync $ mapM_ h <=< fromJSVal
    b <- ffi cb
    return (b, releaseCallback cb)

convCallback2 :: (FromJSVal a, FromJSVal b) => (Callback (JSVal -> JSVal -> IO ()) -> IO ()) -> (a -> b -> IO ()) -> IO (IO ())
convCallback2 ffi h = do
    cb <- syncCallback2 ContinueAsync $ \a b -> join . fmap sequence_ $ liftM2 h <$> fromJSVal a <*> fromJSVal b
    ffi cb
    return $ releaseCallback cb
