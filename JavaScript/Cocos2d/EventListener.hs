{-# LANGUAGE JavaScriptFFI #-}

module JavaScript.Cocos2d.EventListener where

import Control.Monad
import Linear
import GHCJS.Types
import GHCJS.Foreign.Callback
import JavaScript.Cocos2d.Touch
import JavaScript.Cocos2d.Event
import JavaScript.Cocos2d.Geometry
import JavaScript.Cocos2d.Utils

newtype TouchOneByOneEventListener = TouchOneByOneEventListener JSVal

foreign import javascript unsafe "new cc._EventListenerTouchOneByOne()" createTouchOneByOneEventListener :: IO TouchOneByOneEventListener
foreign import javascript unsafe "$1.onTouchBegan = $2" cc_setOnTouchBegan :: TouchOneByOneEventListener -> Callback a -> IO ()
foreign import javascript unsafe "$1.onTouchEnded = $2" cc_setOnTouchEnded :: TouchOneByOneEventListener -> Callback a -> IO ()
foreign import javascript unsafe "$1.onTouchMoved = $2" cc_setOnTouchMoved :: TouchOneByOneEventListener -> Callback a -> IO ()
foreign import javascript unsafe "$1.onTouchCancelled = $2" cc_setOnTouchCancelled :: TouchOneByOneEventListener -> Callback a -> IO ()

-- return an IO action that releases the callback
setOnTouchBegan :: TouchOneByOneEventListener -> (Touch -> EventTouch -> IO ()) -> IO (IO ())
setOnTouchBegan = setCallback2 cc_setOnTouchBegan 
setOnTouchEnded :: TouchOneByOneEventListener -> (Touch -> EventTouch -> IO ()) -> IO (IO ())
setOnTouchEnded = setCallback2 cc_setOnTouchEnded
setOnTouchMoved :: TouchOneByOneEventListener -> (Touch -> EventTouch -> IO ()) -> IO (IO ())
setOnTouchMoved = setCallback2 cc_setOnTouchMoved

newtype TouchAllAtOnceEventListener = TouchAllAtOnceEventListener JSVal

-- setOnTouchCancelled :: TouchOneByOneEventListener -> (V2 Double -> IO ()) -> IO (IO ())
-- setOnTouchCancelled l h = do
--     cb <- syncCallback1 ContinueAsync $ h <=< pointToV2 . Point
--     cc_setOnTouchCancelled l cb
--     return $ releaseCallback cb
--
-- foreign import javascript unsafe "new cc._EventListenerTouchAllAtOnce()" createTouchAllAtOnceEventListener :: IO TouchAllAtOnceEventListener
foreign import javascript unsafe "$1.onTouchesesBegan = $2" cc_setOnTouchesBegan :: TouchAllAtOnceEventListener -> Callback a -> IO ()
-- foreign import javascript unsafe "$1.onTouchesesEnded = $2" cc_setOnTouchesEnded :: TouchAllAtOnceEventListener -> Callback a -> IO ()
-- foreign import javascript unsafe "$1.onTouchesesMoved = $2" cc_setOnTouchesMoved :: TouchAllAtOnceEventListener -> Callback a -> IO ()
-- foreign import javascript unsafe "$1.onTouchesesCancelled = $2" cc_setOnTouchesCancelled :: TouchAllAtOnceEventListener -> Callback a -> IO ()
--
setOnTouchesBegan :: TouchAllAtOnceEventListener -> ([Touch] -> EventTouch -> IO ()) -> IO (IO ())
setOnTouchesBegan = setCallback2 cc_setOnTouchesBegan
