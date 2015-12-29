{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module JavaScript.Cocos2d.EventListener where

import Linear
import GHCJS.Types
import GHCJS.Foreign.Callback
import JavaScript.Cocos2d.Class
import JavaScript.Cocos2d.Touch
import JavaScript.Cocos2d.Event
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

clone :: (IsEventListener l, Cocos2d m) => l -> m l
clone = fmap fromEventListener . liftIO . cc_clone . toEventListener

isEnabled :: (IsEventListener l, Cocos2d m) => l -> m Bool
isEnabled = liftIO . cc_isEnabled . toEventListener

setEnabled :: (IsEventListener l, Cocos2d m) => l -> Bool -> m ()
setEnabled l e = liftIO $ cc_setEnabled (toEventListener l) e

addListener :: (Cocos2d m, IsNode n, IsEventListener l) => n -> l -> m ()
addListener n l = liftIO $ cc_addListener (toNode n) (toEventListener l)

removeListener :: (Cocos2d m, IsEventListener l) => l -> m ()
removeListener l = liftIO $ cc_removeListener (toEventListener l)

newtype TouchOneByOneEventListener = TouchOneByOneEventListener JSVal
instance IsEventListener TouchOneByOneEventListener where
    toEventListener (TouchOneByOneEventListener a) = EventListener a
    fromEventListener (EventListener a) = TouchOneByOneEventListener a

createTouchOneByOneEventListener :: Cocos2d m => m TouchOneByOneEventListener
createTouchOneByOneEventListener = liftIO cc_createTouchOneByOneEventListener
setOnTouchBegan :: Cocos2d m => TouchOneByOneEventListener -> (Touch -> TouchEvent -> IO ()) -> m (IO ())
setOnTouchBegan = convCallback2 . cc_setOnTouchBegan
setOnTouchEnded :: Cocos2d m => TouchOneByOneEventListener -> (Touch -> TouchEvent -> IO ()) -> m (IO ())
setOnTouchEnded = convCallback2 . cc_setOnTouchEnded
setOnTouchMoved :: Cocos2d m => TouchOneByOneEventListener -> (Touch -> TouchEvent -> IO ()) -> m (IO ())
setOnTouchMoved = convCallback2 . cc_setOnTouchMoved
setOnTouchCancelled :: Cocos2d m => TouchOneByOneEventListener -> (V2 Double -> IO ()) -> m (IO ())
setOnTouchCancelled = convCallback1 . cc_setOnTouchCancelled

newtype TouchAllAtOnceEventListener = TouchAllAtOnceEventListener JSVal
instance IsEventListener TouchAllAtOnceEventListener where
    toEventListener (TouchAllAtOnceEventListener a) = EventListener a
    fromEventListener (EventListener a) = TouchAllAtOnceEventListener a

createTouchAllAtOnceEventListener :: Cocos2d m => m TouchAllAtOnceEventListener
createTouchAllAtOnceEventListener = liftIO cc_createTouchAllAtOnceEventListener
setOnTouchesBegan :: Cocos2d m => TouchAllAtOnceEventListener -> ([Touch] -> TouchEvent -> IO ()) -> m (IO ())
setOnTouchesBegan = convCallback2 . cc_setOnTouchesBegan
setOnTouchesEnded :: Cocos2d m => TouchAllAtOnceEventListener -> ([Touch] -> TouchEvent -> IO ()) -> m (IO ())
setOnTouchesEnded = convCallback2 . cc_setOnTouchesEnded
setOnTouchesMoved :: Cocos2d m => TouchAllAtOnceEventListener -> ([Touch] -> TouchEvent -> IO ()) -> m (IO ())
setOnTouchesMoved = convCallback2 . cc_setOnTouchesMoved
setOnTouchesCancelled :: Cocos2d m => TouchAllAtOnceEventListener -> ([Touch] -> TouchEvent -> IO ()) -> m (IO ())
setOnTouchesCancelled = convCallback2 . cc_setOnTouchesCancelled

newtype MouseEventListener = MouseEventListener JSVal
instance IsEventListener MouseEventListener where
    toEventListener (MouseEventListener a) = EventListener a
    fromEventListener (EventListener a) = MouseEventListener a

createMouseEventListener :: Cocos2d m => m MouseEventListener
createMouseEventListener = liftIO cc_createMouseEventListener
setOnMouseDown :: Cocos2d m => MouseEventListener -> (MouseEvent -> IO ()) -> m (IO ())
setOnMouseDown = convCallback1 . cc_setOnMouseDown
setOnMouseUp :: Cocos2d m => MouseEventListener -> (MouseEvent -> IO ()) -> m (IO ())
setOnMouseUp = convCallback1 . cc_setOnMouseUp
setOnMouseMove :: Cocos2d m => MouseEventListener -> (MouseEvent -> IO ()) -> m (IO ())
setOnMouseMove = convCallback1 . cc_setOnMouseMove
setOnMouseScroll :: Cocos2d m => MouseEventListener -> (MouseEvent -> IO ()) -> m (IO ())
setOnMouseScroll = convCallback1 . cc_setOnMouseScroll

newtype KeyboardEventListener = KeyboardEventListener JSVal
instance IsEventListener KeyboardEventListener where
    toEventListener (KeyboardEventListener a) = EventListener a
    fromEventListener (EventListener a) = KeyboardEventListener a

createKeyboardEventListener :: Cocos2d m => m KeyboardEventListener
createKeyboardEventListener = liftIO cc_createKeyboardEventListener
-- pass along the keycode in an Int
setOnKeyPressed :: Cocos2d m => KeyboardEventListener -> (Int -> KeyboardEvent -> IO ()) -> m (IO ())
setOnKeyPressed = convCallback2 . cc_setOnKeyPressed
setOnKeyReleased :: Cocos2d m => KeyboardEventListener -> (Int -> KeyboardEvent -> IO ()) -> m (IO ())
setOnKeyReleased = convCallback2 . cc_setOnKeyReleased

newtype AccelerationEventListener = AccelerationEventListener JSVal
instance IsEventListener AccelerationEventListener where
    toEventListener (AccelerationEventListener a) = EventListener a
    fromEventListener (EventListener a) = AccelerationEventListener a

createAccelerationEventListener :: Cocos2d m => m AccelerationEventListener
createAccelerationEventListener = liftIO cc_createAccelerationEventListener
setOnAccelerationEvent :: Cocos2d m => AccelerationEventListener -> (Acceleration-> AccelerationEvent -> IO ()) -> m (IO ())
setOnAccelerationEvent = convCallback2 . cc_setOnAccelerationEvent

newtype FocusEventListener = FocusEventListener JSVal
instance IsEventListener FocusEventListener where
    toEventListener (FocusEventListener a) = EventListener a
    fromEventListener (EventListener a) = FocusEventListener a

createFocusEventListener :: Cocos2d m => m FocusEventListener
createFocusEventListener = liftIO cc_createFocusEventListener
-- widgetLoseFocus -> widgetGetFocus -> action
setOnFocusChanged :: Cocos2d m => FocusEventListener -> (Widget -> Widget -> IO ()) -> m (IO ())
setOnFocusChanged = convCallback2 . cc_setOnFocusChanged

-- internal foreign import
foreign import javascript unsafe "$1.clone()" cc_clone :: EventListener -> IO EventListener
foreign import javascript unsafe "$1.isEnabled()" cc_isEnabled :: EventListener -> IO Bool
foreign import javascript unsafe "$1.setEnabled($2)" cc_setEnabled :: EventListener -> Bool -> IO ()
foreign import javascript unsafe "cc.eventManager.addListener($2, $1)" cc_addListener :: Node -> EventListener -> IO ()
foreign import javascript unsafe "cc.eventManager.removeListener($1)" cc_removeListener :: EventListener -> IO ()
foreign import javascript unsafe "new cc._EventListenerTouchOneByOne()" cc_createTouchOneByOneEventListener :: IO TouchOneByOneEventListener
foreign import javascript unsafe "$1.onTouchBegan = $2" cc_setOnTouchBegan :: TouchOneByOneEventListener -> Callback a -> IO ()
foreign import javascript unsafe "$1.onTouchEnded = $2" cc_setOnTouchEnded :: TouchOneByOneEventListener -> Callback a -> IO ()
foreign import javascript unsafe "$1.onTouchMoved = $2" cc_setOnTouchMoved :: TouchOneByOneEventListener -> Callback a -> IO ()
foreign import javascript unsafe "$1.onTouchCancelled = $2" cc_setOnTouchCancelled :: TouchOneByOneEventListener -> Callback a -> IO ()
foreign import javascript unsafe "new cc._EventListenerTouchAllAtOnce()" cc_createTouchAllAtOnceEventListener :: IO TouchAllAtOnceEventListener
foreign import javascript unsafe "$1.onTouchesBegan = $2" cc_setOnTouchesBegan :: TouchAllAtOnceEventListener -> Callback a -> IO ()
foreign import javascript unsafe "$1.onTouchesEnded = $2" cc_setOnTouchesEnded :: TouchAllAtOnceEventListener -> Callback a -> IO ()
foreign import javascript unsafe "$1.onTouchesMoved = $2" cc_setOnTouchesMoved :: TouchAllAtOnceEventListener -> Callback a -> IO ()
foreign import javascript unsafe "$1.onTouchesCancelled = $2" cc_setOnTouchesCancelled :: TouchAllAtOnceEventListener -> Callback a -> IO ()
foreign import javascript unsafe "new cc._EventListenerMouse()" cc_createMouseEventListener :: IO MouseEventListener
foreign import javascript unsafe "$1.onMouseDown = $2" cc_setOnMouseDown :: MouseEventListener -> Callback a -> IO ()
foreign import javascript unsafe "$1.onMouseUp = $2" cc_setOnMouseUp :: MouseEventListener -> Callback a -> IO ()
foreign import javascript unsafe "$1.onMouseMove = $2" cc_setOnMouseMove :: MouseEventListener -> Callback a -> IO ()
foreign import javascript unsafe "$1.onMouseScroll = $2" cc_setOnMouseScroll :: MouseEventListener -> Callback a -> IO ()
foreign import javascript unsafe "new cc._EventListenerKeyboard()" cc_createKeyboardEventListener :: IO KeyboardEventListener
foreign import javascript unsafe "$1.onKeyPressed = $2" cc_setOnKeyPressed :: KeyboardEventListener -> Callback a -> IO ()
foreign import javascript unsafe "$1.onKeyReleased = $2" cc_setOnKeyReleased :: KeyboardEventListener -> Callback a -> IO ()
foreign import javascript unsafe "new cc._EventListenerAcceleration()" cc_createAccelerationEventListener :: IO AccelerationEventListener
foreign import javascript unsafe "$1.onAccelerationEvent = $2" cc_setOnAccelerationEvent :: AccelerationEventListener -> Callback a -> IO ()
foreign import javascript unsafe "new cc._EventListenerFocus()" cc_createFocusEventListener :: IO FocusEventListener
foreign import javascript unsafe "$1.onFocusChanged = $2" cc_setOnFocusChanged :: FocusEventListener -> Callback a -> IO ()
