{-# LANGUAGE FlexibleInstances #-}
module JavaScript.Cocos2d.Types where

import Data.Time
import qualified Data.Aeson as A
import qualified Data.JSString as JS
import qualified Data.Text as T
import Data.Word
import Data.Colour as C
import Data.Colour.SRGB
import Diagrams
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Marshal.Pure

----- Helpers
-- copied from JavaScript.JSON.Types.Instances
formatMillis :: (FormatTime t) => t -> String
formatMillis t = take 3 . formatTime defaultTimeLocale "%q" $ t

foreign import javascript unsafe "$1.toISOString()" date_toISOString :: JSVal -> IO JSString
-- Point
foreign import javascript unsafe "$1.x" cc_getX :: JSVal -> IO Double
foreign import javascript unsafe "$1.y" cc_getY :: JSVal -> IO Double
foreign import javascript unsafe "cc.p($1, $2)" cc_p :: Double -> Double -> IO JSVal
-- Color
foreign import javascript unsafe "$1.r" cc_getR :: JSVal -> IO Word8
foreign import javascript unsafe "$1.g" cc_getG :: JSVal -> IO Word8
foreign import javascript unsafe "$1.b" cc_getB :: JSVal -> IO Word8
foreign import javascript unsafe "$1.a" cc_getA :: JSVal -> IO Word8
foreign import javascript unsafe "cc.color($1, $2, $3, $4)" cc_color :: Word8 -> Word8 -> Word8 -> Word8 -> IO JSVal

----- Types
-- P2 Double <-> cc.Point
instance FromJSVal (P2 Double) where
    fromJSVal v = Just <$> ((^&) <$> cc_getX v <*> cc_getY v)

instance FromJSVal (V2 Double) where
    fromJSVal v = Just <$> ((^&) <$> cc_getX v <*> cc_getY v)

instance ToJSVal (P2 Double) where
    toJSVal p = cc_p x y
      where (x, y) = unp2 p

instance ToJSVal (V2 Double) where
    toJSVal p = cc_p x y
      where (x, y) = unr2 p

-- NominalDiffTime <> float (in seconds)
instance FromJSVal NominalDiffTime where
    fromJSVal = return . Just . (realToFrac :: Double -> NominalDiffTime) . pFromJSVal

instance ToJSVal NominalDiffTime where
    toJSVal = toJSVal . (realToFrac :: NominalDiffTime -> Double)

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

-- Colour Double/AlphaColour Double <> cc.Color
instance FromJSVal (Colour Double) where
    fromJSVal v = Just <$> (sRGB24 <$> cc_getR v <*> cc_getG v <*> cc_getB v)

instance ToJSVal (Colour Double) where
    toJSVal c = cc_color r g b 255
        where RGB r g b = toSRGB24 c

instance FromJSVal (AlphaColour Double) where
    fromJSVal v = do
        a <- cc_getA v
        Just c <- fromJSVal v
        return . Just $ withOpacity c (fromIntegral a / 255)

instance ToJSVal (AlphaColour Double) where
    toJSVal c = cc_color r g b (round $ a * 255)
        where a = alphaChannel c
              pureC | a > 0 = darken (recip a) (c `C.over` black)
                    | otherwise = black
              RGB r g b = toSRGB24 pureC
