{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module JavaScript.Cocos2d.EventListener where

import Control.Monad
import Linear
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Foreign.Callback
import JavaScript.Cocos2d.Touch
import JavaScript.Cocos2d.Event
import JavaScript.Cocos2d.Types
import JavaScript.Cocos2d.Widget
import JavaScript.Cocos2d.Utils

class IsEventListener a where
    toEventListener :: a -> EventListener
    fromEventListener :: EventListener -> a

newtype EventListener = EventListener JSVal deriving (FromJSVal, ToJSVal)
instance IsEventListener EventListener where
    toEventListener = id
    fromEventListener = id

clone :: IsEventListener l => l -> IO l
clone = fmap fromEventListener . cc_clone . toEventListener

isEnabled :: IsEventListener l => l -> IO Bool
isEnabled = cc_isEnabled . toEventListener

setEnabled :: IsEventListener l => l -> Bool -> IO ()
setEnabled l e = cc_setEnabled (toEventListener l) e

newtype TouchOneByOneEventListener = TouchOneByOneEventListener JSVal deriving (FromJSVal, ToJSVal)
instance IsEventListener TouchOneByOneEventListener where
    toEventListener (TouchOneByOneEventListener a) = EventListener a
    fromEventListener (EventListener a) = TouchOneByOneEventListener a

foreign import javascript unsafe "new cc._EventListenerTouchOneByOne()" createTouchOneByOneEventListener :: IO TouchOneByOneEventListener

setOnTouchBegan :: TouchOneByOneEventListener -> (Touch -> TouchEvent -> IO ()) -> IO (IO ())
setOnTouchBegan = convCallback2 cc_setOnTouchBegan
setOnTouchEnded :: TouchOneByOneEventListener -> (Touch -> TouchEvent -> IO ()) -> IO (IO ())
setOnTouchEnded = convCallback2 cc_setOnTouchEnded
setOnTouchMoved :: TouchOneByOneEventListener -> (Touch -> TouchEvent -> IO ()) -> IO (IO ())
setOnTouchMoved = convCallback2 cc_setOnTouchMoved
setOnTouchCancelled :: TouchOneByOneEventListener -> (V2 Double -> IO ()) -> IO (IO ())
setOnTouchCancelled = convCallback1 cc_setOnTouchCancelled

newtype TouchAllAtOnceEventListener = TouchAllAtOnceEventListener JSVal deriving (FromJSVal, ToJSVal)
instance IsEventListener TouchAllAtOnceEventListener where
    toEventListener (TouchAllAtOnceEventListener a) = EventListener a
    fromEventListener (EventListener a) = TouchAllAtOnceEventListener a

foreign import javascript unsafe "new cc._EventListenerTouchAllAtOnce()" createTouchAllAtOnceEventListener :: IO TouchAllAtOnceEventListener

setOnTouchesBegan :: TouchAllAtOnceEventListener -> ([Touch] -> TouchEvent -> IO ()) -> IO (IO ())
setOnTouchesBegan = convCallback2 cc_setOnTouchesBegan
setOnTouchesEnded :: TouchAllAtOnceEventListener -> ([Touch] -> TouchEvent -> IO ()) -> IO (IO ())
setOnTouchesEnded = convCallback2 cc_setOnTouchesEnded
setOnTouchesMoved :: TouchAllAtOnceEventListener -> ([Touch] -> TouchEvent -> IO ()) -> IO (IO ())
setOnTouchesMoved = convCallback2 cc_setOnTouchesMoved
setOnTouchesCancelled :: TouchAllAtOnceEventListener -> ([Touch] -> TouchEvent -> IO ()) -> IO (IO ())
setOnTouchesCancelled = convCallback2 cc_setOnTouchesCancelled

newtype MouseEventListener = MouseEventListener JSVal deriving (FromJSVal, ToJSVal)
instance IsEventListener MouseEventListener where
    toEventListener (MouseEventListener a) = EventListener a
    fromEventListener (EventListener a) = MouseEventListener a

foreign import javascript unsafe "new cc._EventListenerMouse()" createMouseEventListener :: IO MouseEventListener

setOnMouseDown :: MouseEventListener -> (MouseEvent -> IO ()) -> IO (IO ())
setOnMouseDown = convCallback1 cc_setOnMouseDown
setOnMouseUp :: MouseEventListener -> (MouseEvent -> IO ()) -> IO (IO ())
setOnMouseUp = convCallback1 cc_setOnMouseUp
setOnMouseMove :: MouseEventListener -> (MouseEvent -> IO ()) -> IO (IO ())
setOnMouseMove = convCallback1 cc_setOnMouseMove
setOnMouseScroll :: MouseEventListener -> (MouseEvent -> IO ()) -> IO (IO ())
setOnMouseScroll = convCallback1 cc_setOnMouseScroll

newtype KeyboardEventListener = KeyboardEventListener JSVal deriving (FromJSVal, ToJSVal)
instance IsEventListener KeyboardEventListener where
    toEventListener (KeyboardEventListener a) = EventListener a
    fromEventListener (EventListener a) = KeyboardEventListener a

foreign import javascript unsafe "new cc._EventListenerKeyboard()" createKeyboardEventListener :: IO KeyboardEventListener

-- pass along the keycode in an Int
setOnKeyPressed :: KeyboardEventListener -> (Int -> KeyboardEvent -> IO ()) -> IO (IO ())
setOnKeyPressed = convCallback2 cc_setOnKeyPressed
setOnKeyReleased :: KeyboardEventListener -> (Int -> KeyboardEvent -> IO ()) -> IO (IO ())
setOnKeyReleased = convCallback2 cc_setOnKeyReleased

newtype AccelerationEventListener = AccelerationEventListener JSVal deriving (FromJSVal, ToJSVal)
instance IsEventListener AccelerationEventListener where
    toEventListener (AccelerationEventListener a) = EventListener a
    fromEventListener (EventListener a) = AccelerationEventListener a

foreign import javascript unsafe "new cc._EventListenerAcceleration()" createAccelerationEventListener :: IO AccelerationEventListener

setOnAccelerationEvent :: AccelerationEventListener -> (Acceleration-> AccelerationEvent -> IO ()) -> IO (IO ())
setOnAccelerationEvent = convCallback2 cc_setOnAccelerationEvent

newtype FocusEventListener = FocusEventListener JSVal deriving (FromJSVal, ToJSVal)
instance IsEventListener FocusEventListener where
    toEventListener (FocusEventListener a) = EventListener a
    fromEventListener (EventListener a) = FocusEventListener a

foreign import javascript unsafe "new cc._EventListenerFocus()" createFocusEventListener :: IO FocusEventListener

-- widgetLoseFocus -> widgetGetFocus -> action
setOnFocusChanged :: FocusEventListener -> (Widget -> Widget -> IO ()) -> IO (IO ())
setOnFocusChanged = convCallback2 cc_setOnFocusChanged

-- internal foreign import
foreign import javascript unsafe "$1.clone()" cc_clone :: EventListener -> IO EventListener
foreign import javascript unsafe "$1.isEnabled()" cc_isEnabled :: EventListener -> IO Bool
foreign import javascript unsafe "$1.setEnabled($2)" cc_setEnabled :: EventListener -> Bool -> IO ()
foreign import javascript unsafe "$1.onTouchBegan = $2" cc_setOnTouchBegan :: TouchOneByOneEventListener -> Callback a -> IO ()
foreign import javascript unsafe "$1.onTouchEnded = $2" cc_setOnTouchEnded :: TouchOneByOneEventListener -> Callback a -> IO ()
foreign import javascript unsafe "$1.onTouchMoved = $2" cc_setOnTouchMoved :: TouchOneByOneEventListener -> Callback a -> IO ()
foreign import javascript unsafe "$1.onTouchCancelled = $2" cc_setOnTouchCancelled :: TouchOneByOneEventListener -> Callback a -> IO ()
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
