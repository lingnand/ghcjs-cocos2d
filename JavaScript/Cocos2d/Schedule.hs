module JavaScript.Cocos2d.Schedule
    (
      scheduleUpdate
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
    c <- cc_createEmptyClass
    inst <- js_createInstance c
    release <- convCallback1 (cc_setUpdate inst) f
    cc_scheduleUpdate inst priority
    return $ cc_unscheduleUpdate inst >> release 

-- we have to extend to get a valid target...
foreign import javascript unsafe "cc.Class.extend()" cc_createEmptyClass :: IO JSVal
foreign import javascript unsafe "new $1()" js_createInstance :: JSVal -> IO JSVal
foreign import javascript unsafe "$1.update = $2" cc_setUpdate :: JSVal -> Callback a -> IO ()
foreign import javascript unsafe "cc.director.getScheduler().scheduleUpdateForTarget($1, $2, false)" cc_scheduleUpdate :: JSVal -> Int -> IO ()
foreign import javascript unsafe "cc.director.getScheduler().unscheduleUpdateForTarget($1)" cc_unscheduleUpdate :: JSVal -> IO ()
