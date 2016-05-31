{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module JavaScript.Cocos2d.Utils where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Marshal.Pure
import GHCJS.Foreign.Callback
import Control.Exception

convCallback :: (Callback (IO ()) -> IO ()) -> IO () -> IO (IO ())
convCallback ffi h = snd <$> convCallbackWithReturn ffi h

convCallbackWithReturn :: (Callback (IO ()) -> IO a) -> IO () -> IO (a, IO ())
convCallbackWithReturn ffi h = do
    cb <- syncCallback ThrowWouldBlock h
    a <- ffi cb
    return (a, releaseCallback cb)

convCallback1 :: FromJSVal a => (Callback (JSVal -> IO ()) -> IO ()) -> (a -> IO ()) -> IO (IO ())
convCallback1 ffi h = do
    cb <- syncCallback1 ThrowWouldBlock $ mapM_ h <=< fromJSVal
    ffi cb
    return $ releaseCallback cb

convCallback1WithReturn :: FromJSVal a => (Callback (JSVal -> IO ()) -> IO b) -> (a -> IO ()) -> IO (b, IO ())
convCallback1WithReturn ffi h = do
    cb <- syncCallback1 ThrowWouldBlock $ mapM_ h <=< fromJSVal
    b <- ffi cb
    return (b, releaseCallback cb)

convCallback2 :: (FromJSVal a, FromJSVal b) => (Callback (JSVal -> JSVal -> IO ()) -> IO ()) -> (a -> b -> IO ()) -> IO (IO ())
convCallback2 ffi h = do
    cb <- syncCallback2 ThrowWouldBlock $ \a b -> join . fmap sequence_ $ liftM2 h <$> fromJSVal a <*> fromJSVal b
    ffi cb
    return $ releaseCallback cb

-- convert a JSVal from an Enum, Bounded instance by trying values one by one
enumFromJSValByTryAll :: (Enum a, Bounded a, ToJSVal a) => JSVal -> IO (Maybe a)
enumFromJSValByTryAll v = runMaybeT . msum . flip fmap [minBound .. maxBound] $ \x -> do
    lift (toJSVal x) >>= guard . js_eq v
    return x

enumFromJSValByInt :: Enum a => JSVal -> IO (Maybe a)
enumFromJSValByInt = flip catch handle . fmap Just . evaluate . toEnum . pFromJSVal
  where handle :: ErrorCall -> IO (Maybe a)
        handle e = print e >> return Nothing

jsNullOrUndefinedMaybe :: (JSVal -> a) -> JSVal -> Maybe a
jsNullOrUndefinedMaybe _ r | isNull r || isUndefined r = Nothing
jsNullOrUndefinedMaybe f r = Just (f r)

foreign import javascript unsafe "$1===$2" js_eq :: JSVal -> JSVal -> Bool
