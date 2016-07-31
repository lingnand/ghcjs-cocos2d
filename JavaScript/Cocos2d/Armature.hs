{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module JavaScript.Cocos2d.Armature
  (
    Armature
  , createArmature
  , getArmatureAnimationSpeedScale
  , setArmatureAnimationSpeedScale
  , playArmatureAnimation
  ) where

import GHCJS.Types
import GHCJS.Marshal.Pure
import JavaScript.Cocos2d.Node
import Control.Monad.IO.Class

newtype Armature = Armature JSVal deriving (PFromJSVal, PToJSVal)

instance IsNode Armature where
  toNode (Armature v) = pFromJSVal v

createArmature :: MonadIO m => String -> m Armature
createArmature name = liftIO $ ccs_createArmature (pToJSVal name)

-- these are all animation functions - they are not put under a new ArmatureAnimation class because
-- the only way to access and use them seems to be through an Armature anyway
getArmatureAnimationSpeedScale :: MonadIO m => Armature -> m Double
getArmatureAnimationSpeedScale = liftIO . ccs_getSpeedScale

setArmatureAnimationSpeedScale :: MonadIO m => Armature -> Double -> m ()
setArmatureAnimationSpeedScale a = liftIO . ccs_setSpeedScale a

playArmatureAnimation :: MonadIO m => Armature
                                   -> String -- ^ Animation name
                                   -> Bool -- ^ Loop or not
                                   -> m ()
playArmatureAnimation armature name = liftIO . ccs_playAnimation armature (pToJSVal name)



foreign import javascript unsafe "new ccs.Armature($1)" ccs_createArmature :: JSVal -> IO Armature
foreign import javascript unsafe "$1.animation.getSpeedScale()" ccs_getSpeedScale :: Armature -> IO Double
foreign import javascript unsafe "$1.animation.setSpeedScale($2)" ccs_setSpeedScale :: Armature -> Double -> IO ()
foreign import javascript unsafe "$1.animation.play($2, -1, $3 ? 1 : 0)" ccs_playAnimation :: Armature -> JSVal -> Bool -> IO ()
