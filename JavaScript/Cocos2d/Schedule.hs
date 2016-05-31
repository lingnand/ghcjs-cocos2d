module JavaScript.Cocos2d.Schedule
    (
      scheduleUpdate
    , scheduleUpdate'
    ) where

import Data.Time.Clock
import Control.Monad.IO.Class
import GHCJS.Types
import GHCJS.Foreign.Callback
import JavaScript.Cocos2d.Types()
import JavaScript.Cocos2d.Utils

-- schedule a callback run per frame
-- lower priority gets called first
scheduleUpdate :: MonadIO m => Int -> (NominalDiffTime -> IO ()) -> m (IO ())
scheduleUpdate priority f = liftIO $ do
    inst <- cc_createEmptyClass >>= js_createInstance
    release <- convCallback1 (cc_setUpdate inst) f
    cc_scheduleUpdate inst priority
    return $ cc_unscheduleUpdate inst >> release

scheduleUpdate' :: MonadIO m => NominalDiffTime -> Bool -> (NominalDiffTime -> IO ()) -> m (IO ())
scheduleUpdate' delay repeat f = liftIO $ do
    inst <- cc_createEmptyClass >>= js_createInstance
    release <- convCallback1 (cc_setUpdate inst) f
    cc_scheduleUpdate' inst (realToFrac delay) repeat
    return $ cc_unscheduleAll inst >> release

-- we have to extend to get a valid target...
foreign import javascript unsafe "cc.Class.extend()" cc_createEmptyClass :: IO JSVal
foreign import javascript unsafe "new $1()" js_createInstance :: JSVal -> IO JSVal
foreign import javascript unsafe "$1.update = $2" cc_setUpdate :: JSVal -> Callback a -> IO ()
foreign import javascript unsafe "cc.director.getScheduler().scheduleUpdateForTarget($1, $2, false)" cc_scheduleUpdate :: JSVal -> Int -> IO ()
foreign import javascript unsafe "cc.director.getScheduler().scheduleCallbackForTarget($1, $1.update, $2, $3 ? cc.REPEAT_FOREVER : 0, 0, false)" cc_scheduleUpdate' :: JSVal -> Double -> Bool -> IO ()
foreign import javascript unsafe "cc.director.getScheduler().unscheduleUpdateForTarget($1)" cc_unscheduleUpdate :: JSVal -> IO ()
foreign import javascript unsafe "cc.director.getScheduler().unscheduleAllCallbacksForTarget($1)" cc_unscheduleAll :: JSVal -> IO ()
