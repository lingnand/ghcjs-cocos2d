{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | module for loading CCS / CCS UI objects
module JavaScript.Cocos2d.CCS
  (
    Widget
  , IsWidget(..)
  , WidgetTouch(..)
  , setOnWidgetTouch
  , setOnWidgetClick
  , PageView
  , ListView
  , ScrollView
  , Slider
  , TextField
  , PageViewEvent(..)
  , ListViewEvent(..)
  , ScrollViewPos(..)
  , ScrollViewEvent(..)
  , SliderEvent(..)
  , TextFieldEvent(..)
  , setOnPageViewEvent
  , setOnListViewEvent
  , setOnScrollViewEvent
  , setOnSliderEvent
  , setOnTextFieldEvent
  , loadCCS
  , loadCCS'
  , ArmatureFileInfoAsyncListener
  , createArmatureFileInfoAsyncListener
  , setOnArmatureFileInfoAsyncProgress
  , addArmatureFileInfosAsync
  ) where

import Diagrams (P2)
import Control.Monad
import Control.Monad.IO.Class
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Marshal.Pure
import GHCJS.Foreign.Callback
import JavaScript.Cocos2d.Node
import JavaScript.Cocos2d.Action
import JavaScript.Cocos2d.Utils
import JavaScript.Cocos2d.Types()

newtype Widget = Widget JSVal deriving (PFromJSVal, PToJSVal)

class IsWidget a where
  toWidget :: a -> Widget

instance IsWidget Widget where
  toWidget = id

-- XXX: can we remove this in the future?
data WidgetTouch = WidgetTouchBegan (P2 Double)
                 | WidgetTouchMoved (P2 Double)
                 | WidgetTouchEnded (P2 Double)
                 | WidgetTouchCancelled

-- | NOTE: mapping of Widget.TOUCH_{BEGAN,MOVED,ENDED,CANCELED} 0 - 3
convertWidgetTouch :: Widget -> Int -> IO (Maybe WidgetTouch)
convertWidgetTouch w 0 {- TOUCH_BEGAN -} = do
    mp2 <- fromJSVal =<< cc_widget_getTouchBeganPosition w
    return $  WidgetTouchBegan <$> mp2
convertWidgetTouch w 1 {- TOUCH_MOVED -} = do
    mp2 <- fromJSVal =<< cc_widget_getTouchMovePosition w
    return $  WidgetTouchMoved <$> mp2
convertWidgetTouch w 2 {- TOUCH_ENDED -} = do
    mp2 <- fromJSVal =<< cc_widget_getTouchEndPosition w
    return $  WidgetTouchEnded <$> mp2
convertWidgetTouch _ 3 {- TOUCH_CANCELLED -} = return $ Just WidgetTouchCancelled
convertWidgetTouch _ _ = return Nothing

setOnWidgetTouch :: (IsWidget w, MonadIO m) => w -> (WidgetTouch -> IO ()) -> m (IO ())
setOnWidgetTouch w fn =
  let w' = toWidget w
      handle :: JSVal -> Int -> IO ()
      handle _ typ = convertWidgetTouch w' typ >>= mapM_ fn
  in liftIO $ convCallback2 (cc_setOnWidgetTouchEvent w') handle

setOnWidgetClick :: (IsWidget w, MonadIO m) => w -> IO () -> m (IO ())
setOnWidgetClick w = liftIO . convCallback (cc_setOnWidgetClickEvent $ toWidget w)

newtype PageView = PageView JSVal deriving (PFromJSVal, PToJSVal)
newtype ListView = ListView JSVal deriving (PFromJSVal, PToJSVal)
newtype ScrollView = ScrollView JSVal deriving (PFromJSVal, PToJSVal)
newtype Slider = Slider JSVal deriving (PFromJSVal, PToJSVal)
newtype TextField = TextField JSVal deriving (PFromJSVal, PToJSVal)

instance IsWidget PageView where
    toWidget (PageView w) = Widget w

instance IsWidget ListView where
    toWidget (ListView w) = Widget w

instance IsWidget ScrollView where
    toWidget (ScrollView w) = Widget w

instance IsWidget Slider where
    toWidget (Slider w) = Widget w

instance IsWidget TextField where
    toWidget (TextField w) = Widget w

-- | NOTE: mapping from 0 - 0
data PageViewEvent = Turning deriving (Enum)

instance FromJSVal PageViewEvent where
  fromJSVal = enumFromJSValByInt


-- | NOTE: mapping from 0 - 1
data ListViewEvent = SelectedItemStart | SelectedItemEnd deriving (Enum)

instance FromJSVal ListViewEvent where
  fromJSVal = enumFromJSValByInt


data ScrollViewPos = TopPos | BottomPos | LeftPos | RightPos deriving (Enum)

data ScrollViewEvent = To ScrollViewPos | Bounce ScrollViewPos | Scrolling

-- | NOTE: mapping from 0 - 8
instance Enum ScrollViewEvent where
  fromEnum (To TopPos) = 0
  fromEnum (To BottomPos) = 1
  fromEnum (To LeftPos) = 2
  fromEnum (To RightPos) = 3
  fromEnum Scrolling = 4
  fromEnum (Bounce TopPos) = 5
  fromEnum (Bounce BottomPos) = 6
  fromEnum (Bounce LeftPos) = 7
  fromEnum (Bounce RightPos) = 8

  toEnum 0 = To TopPos
  toEnum 1 = To BottomPos
  toEnum 2 = To LeftPos
  toEnum 3 = To RightPos
  toEnum 4 = Scrolling
  toEnum 5 = Bounce TopPos
  toEnum 6 = Bounce BottomPos
  toEnum 7 = Bounce LeftPos
  toEnum 8 = Bounce RightPos
  toEnum k = error $ "JavaScript.Cocos2d.CCS.ScrollViewEvent.toEnum: bad argument " ++ show k

instance FromJSVal ScrollViewEvent where
  fromJSVal = enumFromJSValByInt


-- | NOTE: mapping from 0 - 0
data SliderEvent = PercentChanged deriving (Enum)

instance FromJSVal SliderEvent where
  fromJSVal = enumFromJSValByInt


-- | NOTE: mapping from 0 - 3
data TextFieldEvent = AttachWithIME | DetachWithIME | InsertText | DeleteBackward deriving (Enum)

instance FromJSVal TextFieldEvent where
  fromJSVal = enumFromJSValByInt


setOnPageViewEvent :: MonadIO m => PageView -> (PageViewEvent -> IO ()) -> m (IO ())
setOnPageViewEvent pv = liftIO . convCallback1 (cc_setOnCCSEvent (toWidget pv))

setOnListViewEvent :: MonadIO m => ListView -> (ListViewEvent -> IO ()) -> m (IO ())
setOnListViewEvent lv = liftIO . convCallback1 (cc_setOnCCSEvent (toWidget lv))

setOnScrollViewEvent :: MonadIO m => ScrollView -> (ScrollViewEvent -> IO ()) -> m (IO ())
setOnScrollViewEvent sv = liftIO . convCallback1 (cc_setOnCCSEvent (toWidget sv))

setOnSliderEvent :: MonadIO m => Slider -> (SliderEvent -> IO ()) -> m (IO ())
setOnSliderEvent s = liftIO . convCallback1 (cc_setOnCCSEvent (toWidget s))

setOnTextFieldEvent :: MonadIO m => TextField -> (TextFieldEvent -> IO ()) -> m (IO ())
setOnTextFieldEvent tf = liftIO . convCallback1 (cc_setOnCCSEvent (toWidget tf))

---- loading ccs -----
loadCCS :: (IsNode n, PFromJSVal n, MonadIO m) => String -> m (Maybe n, Maybe Action)
loadCCS ccsFile = liftIO $ unCCS =<< ccs_load (pToJSVal ccsFile)

loadCCS' :: (IsNode n, PFromJSVal n, MonadIO m) => String -> String -> m (Maybe n, Maybe Action)
loadCCS' ccsFile resourcePath = liftIO $ unCCS =<< ccs_load' (pToJSVal ccsFile) (pToJSVal resourcePath)

unCCS :: (IsNode n, PFromJSVal n) => JSVal -> IO (Maybe n, Maybe Action)
unCCS loaded = (,) <$> (jsNullOrUndefinedMaybe pFromJSVal <$> ccs_getNode loaded)
                   <*> (jsNullOrUndefinedMaybe pFromJSVal <$> ccs_getAction loaded)


newtype ArmatureFileInfoAsyncListener = ArmatureFileInfoAsyncListener JSVal

createArmatureFileInfoAsyncListener :: MonadIO m => m ArmatureFileInfoAsyncListener
createArmatureFileInfoAsyncListener = liftIO ccs_createArmatureFileInfoAsyncListener

setOnArmatureFileInfoAsyncProgress :: MonadIO m => ArmatureFileInfoAsyncListener -> (Double -> IO ()) -> m (IO ())
setOnArmatureFileInfoAsyncProgress l = liftIO . convCallback1 (ccs_setOnArmatureFileInfoAsyncProgress l)

-- Armature loading -- using async loading
addArmatureFileInfosAsync :: (MonadIO m) => [String] -- ^ file paths to json files to import
                          -> ArmatureFileInfoAsyncListener -> m ()
addArmatureFileInfosAsync files listener = liftIO $ forM_ files $ \f -> ccs_addArmatureFileInfoAsync (pToJSVal f) listener

foreign import javascript unsafe "$1.addTouchEventListener($2)" cc_setOnWidgetTouchEvent :: Widget -> Callback a -> IO ()
foreign import javascript unsafe "$1.getTouchBeganPosition()" cc_widget_getTouchBeganPosition :: Widget -> IO JSVal
foreign import javascript unsafe "$1.getTouchMovePosition()" cc_widget_getTouchMovePosition :: Widget -> IO JSVal
foreign import javascript unsafe "$1.getTouchEndPosition()" cc_widget_getTouchEndPosition :: Widget -> IO JSVal
foreign import javascript unsafe "$1.addClickEventListener($2)" cc_setOnWidgetClickEvent :: Widget -> Callback a -> IO ()
foreign import javascript unsafe "$1.addCCSEventListener(function(_, t) { $2(t) })" cc_setOnCCSEvent :: Widget -> Callback a -> IO ()
foreign import javascript unsafe "ccs.load($1, $2)" ccs_load' :: JSVal -> JSVal -> IO JSVal
foreign import javascript unsafe "ccs.load($1)" ccs_load :: JSVal -> IO JSVal
foreign import javascript unsafe "$1.node" ccs_getNode :: JSVal -> IO JSVal
foreign import javascript unsafe "$1.action" ccs_getAction :: JSVal -> IO JSVal
foreign import javascript unsafe "ccs.armatureDataManager.addArmatureFileInfoAsync($1, function(percent) { if ($2.progress) { $2.progress(percent); } }, $2)" ccs_addArmatureFileInfoAsync :: JSVal -> ArmatureFileInfoAsyncListener -> IO ()
foreign import javascript unsafe "{}" ccs_createArmatureFileInfoAsyncListener :: IO ArmatureFileInfoAsyncListener
foreign import javascript unsafe "$1.progress = $2" ccs_setOnArmatureFileInfoAsyncProgress :: ArmatureFileInfoAsyncListener -> Callback a -> IO ()
