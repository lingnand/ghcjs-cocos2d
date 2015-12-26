module JavaScript.Cocos2d.Class (
    Cocos2d(..),
    MonadIO(..)
) where

import Control.Monad.IO.Class

-- an implementation supporting Cocos2d operations
-- right now, any MonadIO would suffice
class MonadIO m => Cocos2d m where

instance Cocos2d IO where
