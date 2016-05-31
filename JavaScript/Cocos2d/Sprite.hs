{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module JavaScript.Cocos2d.Sprite
  (
    IsSprite(..)
  , Sprite
  , createSprite
  , setSpriteByName
  , setSpriteFrame
  , setTexture
  , setFlippedX
  , setFlippedY
  , setFlipped
  , getFlippedX
  , getFlippedY
  , getFlipped
  )where

import Diagrams (V2(..), (^&))
import Control.Monad.IO.Class
import GHCJS.Types
import GHCJS.Marshal.Pure
import JavaScript.Cocos2d.Node
import JavaScript.Cocos2d.Utils

class IsNode a => IsSprite a where
    toSprite :: a -> Sprite

newtype Sprite = Sprite JSVal deriving (PFromJSVal, PToJSVal)
instance IsNode Sprite where
    toNode (Sprite v) = pFromJSVal v
instance IsSprite Sprite where
    toSprite = id

instance Eq Sprite where
    Sprite a == Sprite b = js_eq a b

createSprite :: MonadIO m => m Sprite
createSprite = liftIO cc_createSprite

-- | We use sprite 'Name' as the cannonical way of setting sprites
-- Name can be:
-- * "": do nothing
-- * #...: a SpriteFrame name
-- * ...: a file name
setSpriteByName :: (IsSprite s, MonadIO m) => s -> String -> m ()
setSpriteByName _ [] = return ()
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

setFlipped :: (IsSprite s, MonadIO m) => s -> V2 Bool -> m ()
setFlipped s (V2 x y) = liftIO $ cc_setFlippedX s' x >> cc_setFlippedY s' y
  where s' = toSprite s

getFlippedX :: (IsSprite s, MonadIO m) => s -> m Bool
getFlippedX s = liftIO $ cc_getFlippedX (toSprite s)

getFlippedY :: (IsSprite s, MonadIO m) => s -> m Bool
getFlippedY s = liftIO $ cc_getFlippedY (toSprite s)

getFlipped :: (IsSprite n, MonadIO m) => n -> m (V2 Bool)
getFlipped s = liftIO $ (^&) <$> cc_getFlippedX s' <*> cc_getFlippedY s'
  where s' = toSprite s

foreign import javascript unsafe "new cc.Sprite()" cc_createSprite :: IO Sprite
foreign import javascript unsafe "$1.setSpriteFrame($2)" cc_setSpriteFrame :: Sprite -> JSVal -> IO ()
foreign import javascript unsafe "$1.setTexture($2)" cc_setTexture :: Sprite -> JSVal -> IO ()
foreign import javascript unsafe "$1.flippedX" cc_getFlippedX :: Sprite -> IO Bool
foreign import javascript unsafe "$1.flippedY" cc_getFlippedY :: Sprite -> IO Bool
foreign import javascript unsafe "$1.setFlippedX($2)" cc_setFlippedX :: Sprite -> Bool -> IO ()
foreign import javascript unsafe "$1.setFlippedY($2)" cc_setFlippedY :: Sprite -> Bool -> IO ()
