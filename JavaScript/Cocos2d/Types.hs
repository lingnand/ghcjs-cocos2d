{-# LANGUAGE FlexibleInstances #-}
module JavaScript.Cocos2d.Types where

import Data.Time
import qualified Data.Aeson as A
import qualified Data.JSString as JS
import qualified Data.Text as T
import Data.Word
import Data.Colour as C
import Data.Colour.SRGB
import Linear
import Control.Lens
import Control.Applicative
import GHCJS.Types
import GHCJS.Marshal

----- Helpers
-- copied from JavaScript.JSON.Types.Instances
formatMillis :: (FormatTime t) => t -> String
formatMillis t = take 3 . formatTime defaultTimeLocale "%q" $ t

foreign import javascript unsafe "$1.toISOString()" date_toISOString :: JSVal -> IO JSString
-- Point
foreign import javascript unsafe "$1.x" cc_getX :: JSVal -> IO Double
foreign import javascript unsafe "$1.y" cc_getY :: JSVal -> IO Double
foreign import javascript unsafe "cc.p($1, $2)" cc_p :: Double -> Double -> IO JSVal 
-- Acceleration
foreign import javascript unsafe "$1.z" cc_getZ :: JSVal -> IO Double
foreign import javascript unsafe "$1.timestamp" cc_getTimestamp :: JSVal -> IO JSVal
foreign import javascript unsafe "new cc.Acceleration($1, $2, $3, $4)" cc_createAcceleration :: Double -> Double -> Double -> JSVal -> IO JSVal
-- Color
foreign import javascript unsafe "$1.r" cc_getR :: JSVal -> IO Word8
foreign import javascript unsafe "$1.g" cc_getG :: JSVal -> IO Word8
foreign import javascript unsafe "$1.b" cc_getB :: JSVal -> IO Word8
foreign import javascript unsafe "$1.a" cc_getA :: JSVal -> IO Word8
foreign import javascript unsafe "cc.color($1, $2, $3, $4)" cc_color :: Word8 -> Word8 -> Word8 -> Word8 -> IO JSVal

----- Types
-- V2 Double <-> cc.Point
instance FromJSVal (V2 Double) where
    fromJSVal v = Just <$> (V2 <$> cc_getX v <*> cc_getY v)

instance ToJSVal (V2 Double) where
    toJSVal (V2 x y) = cc_p x y

-- UTCTime <-> Date
instance FromJSVal UTCTime where
    fromJSVal v = do
        jsstr <- date_toISOString v
        case A.fromJSON . A.String .T.pack . JS.unpack $ jsstr of
            A.Success t -> return $ Just t
            _ -> return Nothing

instance ToJSVal UTCTime where
    toJSVal t = toJSVal . JS.pack $ formatTime defaultTimeLocale format t
        where format = "%FT%T." ++ formatMillis t ++ "Z"

-- Acceleration <> cc.Acceleration
data Acceleration = Acceleration { _vec  :: V3 Double
                                 , _time :: UTCTime }
makeLenses ''Acceleration

instance FromJSVal Acceleration where
    fromJSVal v = do
        vec <- V3 <$> cc_getX v <*> cc_getY v <*> cc_getZ v 
        t <- fromJSVal =<< cc_getTimestamp v
        return $ Acceleration vec <$> t

instance ToJSVal Acceleration where
    toJSVal (Acceleration (V3 x y z) t) = cc_createAcceleration x y z =<< toJSVal t

-- AlphaColour Double <> cc.Color
instance FromJSVal (AlphaColour Double) where
    fromJSVal v = do
        a <- cc_getA v
        c <- sRGB24 <$> cc_getR v <*> cc_getG v <*> cc_getB v
        return . Just $ withOpacity c (fromIntegral a / 255)

instance ToJSVal (AlphaColour Double) where
    toJSVal c = cc_color r g b (round $ a * 255)
        where a = alphaChannel c
              pureC | a > 0 = darken (recip a) (c `C.over` black)
                    | otherwise = black
              RGB r g b = toSRGB24 pureC
