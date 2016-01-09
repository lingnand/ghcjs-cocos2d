{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module JavaScript.Cocos2d.EventListener where

import Control.Monad.IO.Class
import GHCJS.Types
import GHCJS.Marshal.Internal
import GHCJS.Foreign.Callback
import JavaScript.Cocos2d.Types
import JavaScript.Cocos2d.Widget
import JavaScript.Cocos2d.Node
import JavaScript.Cocos2d.Utils

class IsEventListener a where
    toEventListener :: a -> EventListener
    fromEventListener :: EventListener -> a

newtype EventListener = EventListener JSVal
instance IsEventListener EventListener where
    toEventListener = id
    fromEventListener = id

clone :: (IsEventListener l, MonadIO m) => l -> m l
clone = liftIO . fmap fromEventListener . cc_clone . toEventListener

isEnabled :: (IsEventListener l, MonadIO m) => l -> m Bool
isEnabled = liftIO . cc_isEnabled . toEventListener

setEnabled :: (IsEventListener l, MonadIO m) => l -> Bool -> m ()
setEnabled l = liftIO . cc_setEnabled (toEventListener l)

-- | add listener at scene graph priority - all listeners at scene graph
-- priority will be called according to their bound nodes in front to back
-- order (reverse scene graph drawing order)
addListener :: (IsNode n, IsEventListener l, MonadIO m) => l -> n -> m ()
addListener l n = let Node v = toNode n in liftIO $ cc_addListener (toEventListener l) v

-- | < 0: before scene graph, == 0: scene graph, > 0: after scene graph
addListener' :: (IsEventListener l, MonadIO m) => l -> Int -> m ()
addListener' l priority = liftIO $ cc_addListener (toEventListener l) (pToJSVal priority)

removeListener :: (IsEventListener l, MonadIO m) => l -> m ()
removeListener = liftIO . cc_removeListener . toEventListener

-- TouchOneByOneEventListener is not included because its 'state'-bound
-- semantics involving claiming/swallowing Touches does play well with
-- haskell

newtype TouchAllAtOnceEventListener = TouchAllAtOnceEventListener JSVal
instance IsEventListener TouchAllAtOnceEventListener where
    toEventListener (TouchAllAtOnceEventListener a) = EventListener a
    fromEventListener (EventListener a) = TouchAllAtOnceEventListener a

createTouchAllAtOnceEventListener :: MonadIO m => m TouchAllAtOnceEventListener 
createTouchAllAtOnceEventListener = liftIO cc_createTouchAllAtOnceEventListener

-- XXX: these callbacks ignore the second TouchEvent argument passed into
-- the callback - since they are not really useful
setOnTouchesBegan :: MonadIO m => TouchAllAtOnceEventListener -> ([Touch] -> IO ()) -> m (IO ())
setOnTouchesBegan l = liftIO . convCallback1 (cc_setOnTouchesBegan l)
setOnTouchesEnded :: MonadIO m => TouchAllAtOnceEventListener -> ([Touch] -> IO ()) -> m (IO ())
setOnTouchesEnded l = liftIO . convCallback1 (cc_setOnTouchesEnded l)
setOnTouchesMoved :: MonadIO m => TouchAllAtOnceEventListener -> ([Touch] -> IO ()) -> m (IO ())
setOnTouchesMoved l = liftIO . convCallback1 (cc_setOnTouchesMoved l)
setOnTouchesCancelled :: MonadIO m => TouchAllAtOnceEventListener -> ([Touch] -> IO ()) -> m (IO ())
setOnTouchesCancelled l = liftIO . convCallback1 (cc_setOnTouchesCancelled l)

newtype MouseEventListener = MouseEventListener JSVal
instance IsEventListener MouseEventListener where
    toEventListener (MouseEventListener a) = EventListener a
    fromEventListener (EventListener a) = MouseEventListener a

createMouseEventListener :: MonadIO m => m MouseEventListener
createMouseEventListener = liftIO cc_createMouseEventListener

setOnMouseDown :: MonadIO m => MouseEventListener -> (MouseEvent -> IO ()) -> m (IO ())
setOnMouseDown l = liftIO . convCallback1 (cc_setOnMouseDown l)
setOnMouseUp :: MonadIO m => MouseEventListener -> (MouseEvent -> IO ()) -> m (IO ())
setOnMouseUp l = liftIO . convCallback1 (cc_setOnMouseUp l)
setOnMouseMove :: MonadIO m => MouseEventListener -> (MouseEvent -> IO ()) -> m (IO ())
setOnMouseMove l = liftIO . convCallback1 (cc_setOnMouseMove l)
setOnMouseScroll :: MonadIO m => MouseEventListener -> (MouseEvent -> IO ()) -> m (IO ())
setOnMouseScroll l = liftIO . convCallback1 (cc_setOnMouseScroll l)

newtype KeyboardEventListener = KeyboardEventListener JSVal
instance IsEventListener KeyboardEventListener where
    toEventListener (KeyboardEventListener a) = EventListener a
    fromEventListener (EventListener a) = KeyboardEventListener a

createKeyboardEventListener :: MonadIO m => m KeyboardEventListener
createKeyboardEventListener = liftIO cc_createKeyboardEventListener

-- XXX: these callbacks ignore the second KeyboardEvent argument passed into
-- the callback - since they are not really useful
setOnKeyPressed :: MonadIO m => KeyboardEventListener -> (Key -> IO ()) -> m (IO ())
setOnKeyPressed l = liftIO . convCallback1 (cc_setOnKeyPressed l)
setOnKeyReleased :: MonadIO m => KeyboardEventListener -> (Key -> IO ()) -> m (IO ())
setOnKeyReleased l = liftIO . convCallback1 (cc_setOnKeyReleased l)

newtype AccelerationEventListener = AccelerationEventListener JSVal
instance IsEventListener AccelerationEventListener where
    toEventListener (AccelerationEventListener a) = EventListener a
    fromEventListener (EventListener a) = AccelerationEventListener a

createAccelerationEventListener :: MonadIO m => m AccelerationEventListener
createAccelerationEventListener = liftIO cc_createAccelerationEventListener

-- XXX: these callbacks ignore the second AccelerationEvent argument passed into
-- the callback - since they are not really useful
setOnAccelerationEvent :: MonadIO m => AccelerationEventListener -> (Acceleration -> IO ()) -> m (IO ())
setOnAccelerationEvent l = liftIO . convCallback1 (cc_setOnAccelerationEvent l)

newtype FocusEventListener = FocusEventListener JSVal
instance IsEventListener FocusEventListener where
    toEventListener (FocusEventListener a) = EventListener a
    fromEventListener (EventListener a) = FocusEventListener a

createFocusEventListener :: MonadIO m => m FocusEventListener
createFocusEventListener = liftIO cc_createFocusEventListener

-- widgetLoseFocus -> widgetGetFocus -> action
setOnFocusChanged :: MonadIO m => FocusEventListener -> (Widget -> Widget -> IO ()) -> m (IO ())
setOnFocusChanged l = liftIO . convCallback2 (cc_setOnFocusChanged l)

-- internal foreign import
foreign import javascript unsafe "cc.EventListener.create({ event: cc.EventListener.TOUCH_ALL_AT_ONCE })" cc_createTouchAllAtOnceEventListener :: IO TouchAllAtOnceEventListener
foreign import javascript unsafe "cc.EventListener.create({ event: cc.EventListener.MOUSE })" cc_createMouseEventListener :: IO MouseEventListener
foreign import javascript unsafe "cc.EventListener.create({ event: cc.EventListener.KEYBOARD })" cc_createKeyboardEventListener :: IO KeyboardEventListener
foreign import javascript unsafe "cc.EventListener.create({ event: cc.EventListener.ACCELERATION })" cc_createAccelerationEventListener :: IO AccelerationEventListener
foreign import javascript unsafe "new cc._EventListenerFocus()" cc_createFocusEventListener :: IO FocusEventListener
foreign import javascript unsafe "$1.clone()" cc_clone :: EventListener -> IO EventListener
foreign import javascript unsafe "$1.isEnabled()" cc_isEnabled :: EventListener -> IO Bool
foreign import javascript unsafe "$1.setEnabled($2)" cc_setEnabled :: EventListener -> Bool -> IO ()
foreign import javascript unsafe "cc.eventManager.addListener($1, $2)" cc_addListener :: EventListener -> JSVal -> IO ()
foreign import javascript unsafe "cc.eventManager.removeListener($1)" cc_removeListener :: EventListener -> IO ()
foreign import javascript unsafe "$1.onTouchesBegan = $2" cc_setOnTouchesBegan :: TouchAllAtOnceEventListener -> Callback a -> IO ()
foreign import javascript unsafe "$1.onTouchesEnded = $2" cc_setOnTouchesEnded :: TouchAllAtOnceEventListener -> Callback a -> IO ()
foreign import javascript unsafe "$1.onTouchesMoved = $2" cc_setOnTouchesMoved :: TouchAllAtOnceEventListener -> Callback a -> IO ()
foreign import javascript unsafe "$1.onTouchesCancelled = $2" cc_setOnTouchesCancelled :: TouchAllAtOnceEventListener -> Callback a -> IO ()
foreign import javascript unsafe "$1.onMouseDown = $2" cc_setOnMouseDown :: MouseEventListener -> Callback a -> IO ()
foreign import javascript unsafe "$1.onMouseUp = $2" cc_setOnMouseUp :: MouseEventListener -> Callback a -> IO ()
foreign import javascript unsafe "$1.onMouseMove = $2" cc_setOnMouseMove :: MouseEventListener -> Callback a -> IO ()
foreign import javascript unsafe "$1.onMouseScroll = $2" cc_setOnMouseScroll :: MouseEventListener -> Callback a -> IO ()
foreign import javascript unsafe "$1.onKeyPressed = $2" cc_setOnKeyPressed :: KeyboardEventListener -> Callback a -> IO ()
foreign import javascript unsafe "$1.onKeyReleased = $2" cc_setOnKeyReleased :: KeyboardEventListener -> Callback a -> IO ()
foreign import javascript unsafe "$1.onAccelerationEvent = $2" cc_setOnAccelerationEvent :: AccelerationEventListener -> Callback a -> IO ()
foreign import javascript unsafe "$1.onFocusChanged = $2" cc_setOnFocusChanged :: FocusEventListener -> Callback a -> IO ()
