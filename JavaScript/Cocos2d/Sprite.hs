module JavaScript.Cocos2d.Sprite where

import Control.Monad.IO.Class
import GHCJS.Types
import GHCJS.Marshal.Pure
import JavaScript.Cocos2d.Node

class IsNode a => IsSprite a where
    toSprite :: a -> Sprite

newtype Sprite = Sprite JSVal
instance IsNode Sprite where
    toNode (Sprite v) = pFromJSVal v
instance IsSprite Sprite where
    toSprite = id

createSprite :: MonadIO m => m Sprite
createSprite = liftIO cc_createSprite

-- | We use sprite 'Name' as the cannonical way of setting sprites
-- Name can be:
-- * #...: a SpriteFrame name
-- * ...: a file name
setSpriteByName :: (IsSprite s, MonadIO m) => s -> String -> m ()
setSpriteByName s ('#':spriteFrameName) = setSpriteFrame s spriteFrameName
setSpriteByName s fileName = setTexture s fileName

setSpriteFrame :: (IsSprite s, MonadIO m) => s -> String -> m ()
setSpriteFrame s = liftIO . cc_setSpriteFrame (toSprite s) . pToJSVal

setTexture :: (IsSprite s, MonadIO m) => s -> String -> m ()
setTexture s = liftIO . cc_setTexture (toSprite s) . pToJSVal

setFlippedX :: (IsSprite s, MonadIO m) => s -> Bool -> m ()
setFlippedX s = liftIO . cc_setFlippedX (toSprite s)

setFlippedY :: (IsSprite s, MonadIO m) => s -> Bool -> m ()
setFlippedY s = liftIO . cc_setFlippedY (toSprite s)


foreign import javascript unsafe "new cc.Sprite()" cc_createSprite :: IO Sprite
foreign import javascript unsafe "$1.setSpriteFrame($2)" cc_setSpriteFrame :: Sprite -> JSVal -> IO ()
foreign import javascript unsafe "$1.setTexture($2)" cc_setTexture :: Sprite -> JSVal -> IO ()
foreign import javascript unsafe "$1.setFlippedX($2)" cc_setFlippedX :: Sprite -> Bool -> IO ()
foreign import javascript unsafe "$1.setFlippedY($2)" cc_setFlippedY :: Sprite -> Bool -> IO ()
