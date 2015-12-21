{-# LANGUAGE JavaScriptFFI #-}

module JavaScript.Cocos2d.EventListener where

import Control.Monad
import Linear
import GHCJS.Types
import GHCJS.Foreign.Callback
import JavaScript.Cocos2d.Touch
import JavaScript.Cocos2d.Event
import JavaScript.Cocos2d.Geometry

newtype TouchOneByOneEventListener = TouchOneByOneEventListener JSVal

foreign import javascript unsafe "new cc._EventListenerTouchOneByOne()" createTouchOneByOneEventListener :: IO TouchOneByOneEventListener
foreign import javascript unsafe "$1.onTouchBegan = $2" cc_setOnTouchBegan :: TouchOneByOneEventListener -> Callback a -> IO ()
foreign import javascript unsafe "$1.onTouchEnded = $2" cc_setOnTouchEnded :: TouchOneByOneEventListener -> Callback a -> IO ()
foreign import javascript unsafe "$1.onTouchMoved = $2" cc_setOnTouchMoved :: TouchOneByOneEventListener -> Callback a -> IO ()
foreign import javascript unsafe "$1.onTouchCancelled = $2" cc_setOnTouchCancelled :: TouchOneByOneEventListener -> Callback a -> IO ()

-- return an IO action that releases the callback
setOnTouchBegan :: TouchOneByOneEventListener -> (Touch -> EventTouch -> IO ()) -> IO (IO ())
setOnTouchBegan l h = do
    cb <- syncCallback2 ContinueAsync $ \t e -> h (Touch t) (EventTouch e)
    cc_setOnTouchBegan l cb
    return $ releaseCallback cb

setOnTouchEnded :: TouchOneByOneEventListener -> (Touch -> EventTouch -> IO ()) -> IO (IO ())
setOnTouchEnded l h = do
    cb <- syncCallback2 ContinueAsync $ \t e -> h (Touch t) (EventTouch e)
    cc_setOnTouchEnded l cb
    return $ releaseCallback cb

setOnTouchMoved :: TouchOneByOneEventListener -> (Touch -> EventTouch -> IO ()) -> IO (IO ())
setOnTouchMoved l h = do
    cb <- syncCallback2 ContinueAsync $ \t e -> h (Touch t) (EventTouch e)
    cc_setOnTouchMoved l cb
    return $ releaseCallback cb

setOnTouchCancelled :: TouchOneByOneEventListener -> (V2 Double -> IO ()) -> IO (IO ())
setOnTouchCancelled l h = do
    cb <- syncCallback1 ContinueAsync $ h <=< pointToV2 . Point
    cc_setOnTouchCancelled l cb
    return $ releaseCallback cb
