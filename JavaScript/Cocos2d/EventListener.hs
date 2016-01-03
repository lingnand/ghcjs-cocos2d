{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module JavaScript.Cocos2d.EventListener where

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

clone :: IsEventListener l => l -> IO l
clone = fmap fromEventListener . cc_clone . toEventListener

isEnabled :: IsEventListener l => l -> IO Bool
isEnabled = cc_isEnabled . toEventListener

setEnabled :: IsEventListener l => l -> Bool -> IO ()
setEnabled l e = cc_setEnabled (toEventListener l) e

-- | add listener at scene graph priority - all listeners at scene graph
-- priority will be called according to their bound nodes in front to back
-- order (reverse scene graph drawing order)
addListener :: (IsNode n, IsEventListener l) => l -> n -> IO ()
addListener l n = let Node v = toNode n in cc_addListener (toEventListener l) v

-- | < 0: before scene graph, == 0: scene graph, > 0: after scene graph
addListener' :: IsEventListener l => l -> Int -> IO ()
addListener' l priority = cc_addListener (toEventListener l) (pToJSVal priority)

removeListener :: IsEventListener l => l -> IO ()
removeListener l = cc_removeListener (toEventListener l)

-- TouchOneByOneEventListener is not included because its 'state'-bound
-- semantics involving claiming/swallowing Touches does play well with
-- haskell

newtype TouchAllAtOnceEventListener = TouchAllAtOnceEventListener JSVal
instance IsEventListener TouchAllAtOnceEventListener where
    toEventListener (TouchAllAtOnceEventListener a) = EventListener a
    fromEventListener (EventListener a) = TouchAllAtOnceEventListener a

foreign import javascript unsafe "cc.EventListener.create({ event: cc.EventListener.TOUCH_ALL_AT_ONCE })"
    createTouchAllAtOnceEventListener :: IO TouchAllAtOnceEventListener
-- XXX: these callbacks ignore the second TouchEvent argument passed into
-- the callback - since they are not really useful
setOnTouchesBegan :: TouchAllAtOnceEventListener -> ([Touch] -> IO ()) -> IO (IO ())
setOnTouchesBegan = convCallback1 . cc_setOnTouchesBegan
setOnTouchesEnded :: TouchAllAtOnceEventListener -> ([Touch] -> IO ()) -> IO (IO ())
setOnTouchesEnded = convCallback1 . cc_setOnTouchesEnded
setOnTouchesMoved :: TouchAllAtOnceEventListener -> ([Touch] -> IO ()) -> IO (IO ())
setOnTouchesMoved = convCallback1 . cc_setOnTouchesMoved
setOnTouchesCancelled :: TouchAllAtOnceEventListener -> ([Touch] -> IO ()) -> IO (IO ())
setOnTouchesCancelled = convCallback1 . cc_setOnTouchesCancelled

newtype MouseEventListener = MouseEventListener JSVal
instance IsEventListener MouseEventListener where
    toEventListener (MouseEventListener a) = EventListener a
    fromEventListener (EventListener a) = MouseEventListener a

foreign import javascript unsafe "cc.EventListener.create({ event: cc.EventListener.MOUSE })"
    createMouseEventListener :: IO MouseEventListener
setOnMouseDown :: MouseEventListener -> (MouseEvent -> IO ()) -> IO (IO ())
setOnMouseDown = convCallback1 . cc_setOnMouseDown
setOnMouseUp :: MouseEventListener -> (MouseEvent -> IO ()) -> IO (IO ())
setOnMouseUp = convCallback1 . cc_setOnMouseUp
setOnMouseMove :: MouseEventListener -> (MouseEvent -> IO ()) -> IO (IO ())
setOnMouseMove = convCallback1 . cc_setOnMouseMove
setOnMouseScroll :: MouseEventListener -> (MouseEvent -> IO ()) -> IO (IO ())
setOnMouseScroll = convCallback1 . cc_setOnMouseScroll

newtype KeyboardEventListener = KeyboardEventListener JSVal
instance IsEventListener KeyboardEventListener where
    toEventListener (KeyboardEventListener a) = EventListener a
    fromEventListener (EventListener a) = KeyboardEventListener a

foreign import javascript unsafe "cc.EventListener.create({ event: cc.EventListener.KEYBOARD })"
    createKeyboardEventListener :: IO KeyboardEventListener
-- XXX: these callbacks ignore the second KeyboardEvent argument passed into
-- the callback - since they are not really useful
setOnKeyPressed :: KeyboardEventListener -> (Key -> IO ()) -> IO (IO ())
setOnKeyPressed = convCallback1 . cc_setOnKeyPressed
setOnKeyReleased :: KeyboardEventListener -> (Key -> IO ()) -> IO (IO ())
setOnKeyReleased = convCallback1 . cc_setOnKeyReleased

newtype AccelerationEventListener = AccelerationEventListener JSVal
instance IsEventListener AccelerationEventListener where
    toEventListener (AccelerationEventListener a) = EventListener a
    fromEventListener (EventListener a) = AccelerationEventListener a

foreign import javascript unsafe "cc.EventListener.create({ event: cc.EventListener.ACCELERATION })"
    createAccelerationEventListener :: IO AccelerationEventListener
-- XXX: these callbacks ignore the second AccelerationEvent argument passed into
-- the callback - since they are not really useful
setOnAccelerationEvent :: AccelerationEventListener -> (Acceleration -> IO ()) -> IO (IO ())
setOnAccelerationEvent = convCallback1 . cc_setOnAccelerationEvent

newtype FocusEventListener = FocusEventListener JSVal
instance IsEventListener FocusEventListener where
    toEventListener (FocusEventListener a) = EventListener a
    fromEventListener (EventListener a) = FocusEventListener a

foreign import javascript unsafe "new cc._EventListenerFocus()" createFocusEventListener :: IO FocusEventListener
-- widgetLoseFocus -> widgetGetFocus -> action
setOnFocusChanged :: FocusEventListener -> (Widget -> Widget -> IO ()) -> IO (IO ())
setOnFocusChanged = convCallback2 . cc_setOnFocusChanged

-- internal foreign import
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
