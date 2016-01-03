{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module JavaScript.Cocos2d.Types (
    Acceleration(..),
    Touch,
    Key(..),
    MouseEvent,
    pEnumFromJSVal
) where

import Data.Time
import qualified Data.Aeson as A
import qualified Data.JSString as JS
import qualified Data.Text as T
import Data.Word
import Data.List
import Data.Maybe
import Data.Colour as C
import Data.Colour.SRGB
import Linear
import Control.Monad
import Control.Lens
import Control.Exception
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Marshal.Internal

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

-- Touch
newtype Touch = Touch JSVal deriving (FromJSVal, ToJSVal)

-- Key
data Key =
    -- Android
      AndroidBack
    | AndroidMenu
    -- Common
    | Backspace
    | Tab
    | Enter
    | Shift
    | Ctrl
    | Alt
    | Pause
    | Capslock
    | Escape
    | Space
    | Pageup
    | Pagedown
    | End
    | Home
    | ArrowLeft
    | ArrowUp
    | ArrowRight
    | ArrowDown
    | Select
    | Insert
    | Delete
    | Digit0
    | Digit1
    | Digit2
    | Digit3
    | Digit4
    | Digit5
    | Digit6
    | Digit7
    | Digit8
    | Digit9
    | KeyA
    | KeyB
    | KeyC
    | KeyD
    | KeyE
    | KeyF
    | KeyG
    | KeyH
    | KeyI
    | KeyJ
    | KeyK
    | KeyL
    | KeyM
    | KeyN
    | KeyO
    | KeyP
    | KeyQ
    | KeyR
    | KeyS
    | KeyT
    | KeyU
    | KeyV
    | KeyW
    | KeyX
    | KeyY
    | KeyZ
    | Numpad0
    | Numpad1
    | Numpad2
    | Numpad3
    | Numpad4
    | Numpad5
    | Numpad6
    | Numpad7
    | Numpad8
    | Numpad9
    | NumpadMultiply
    | NumpadPlus
    | NumpadMinus
    | NumpadDecimal
    | NumpadDivide
    | F1
    | F2
    | F3
    | F4
    | F5
    | F6
    | F7
    | F8
    | F9
    | F10
    | F11
    | F12
    | NumLock
    | ScrollLock
    | Semicolon
    | Equal
    | Comma
    | Minus -- dash
    | Period
    | Slash
    | Grave
    | LeftBracket
    | Backslash
    | RightBracket
    | Apostrophe
    -- gamepad control
    | DpadLeft
    | DpadRight
    | DpadUp
    | DpadDown
    | DpadCenter
    deriving (Show, Eq, Ord, Read)

instance Bounded Key where -- does this make sense?
    minBound = AndroidBack
    maxBound = DpadCenter

allKeys :: [Key]
allKeys = [ AndroidBack, AndroidMenu, Backspace, Tab, Enter, Shift, Ctrl, Alt, Pause, Capslock, Escape, Space, Pageup, Pagedown, End, Home, ArrowLeft, ArrowUp, ArrowRight, ArrowDown, Select, Insert, Delete, Digit0, Digit1, Digit2, Digit3, Digit4, Digit5, Digit6, Digit7, Digit8, Digit9, KeyA, KeyB, KeyC, KeyD, KeyE, KeyF, KeyG, KeyH, KeyI, KeyJ, KeyK, KeyL, KeyM, KeyN, KeyO, KeyP, KeyQ, KeyR, KeyS, KeyT, KeyU, KeyV, KeyW, KeyX, KeyY, KeyZ, Numpad0, Numpad1, Numpad2, Numpad3, Numpad4, Numpad5, Numpad6, Numpad7, Numpad8, Numpad9, NumpadMultiply, NumpadPlus, NumpadMinus, NumpadDecimal, NumpadDivide, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, NumLock, ScrollLock, Semicolon, Equal, Comma, Minus, Period, Slash, Grave, LeftBracket, Backslash, RightBracket, Apostrophe, DpadLeft, DpadRight, DpadUp, DpadDown, DpadCenter ]

-- we can do this because both cocos2d-html and jsb use the same key codes
instance Enum Key where
    fromEnum AndroidBack = 6
    fromEnum AndroidMenu = 18
    fromEnum Backspace = 8
    fromEnum Tab = 9
    fromEnum Enter = 13
    fromEnum Shift = 16
    fromEnum Ctrl = 17
    fromEnum Alt = 18
    fromEnum Pause = 19
    fromEnum Capslock = 20
    fromEnum Escape = 27
    fromEnum Space = 32
    fromEnum Pageup = 33
    fromEnum Pagedown = 34
    fromEnum End = 35
    fromEnum Home = 36
    fromEnum ArrowLeft = 37
    fromEnum ArrowUp = 38
    fromEnum ArrowRight = 39
    fromEnum ArrowDown = 40
    fromEnum Select = 41
    fromEnum Insert = 45
    fromEnum Delete = 46
    fromEnum Digit0 = 48
    fromEnum Digit1 = 49
    fromEnum Digit2 = 50
    fromEnum Digit3 = 51
    fromEnum Digit4 = 52
    fromEnum Digit5 = 53
    fromEnum Digit6 = 54
    fromEnum Digit7 = 55
    fromEnum Digit8 = 56
    fromEnum Digit9 = 57
    fromEnum KeyA = 65
    fromEnum KeyB = 66
    fromEnum KeyC = 67
    fromEnum KeyD = 68
    fromEnum KeyE = 69
    fromEnum KeyF = 70
    fromEnum KeyG = 71
    fromEnum KeyH = 72
    fromEnum KeyI = 73
    fromEnum KeyJ = 74
    fromEnum KeyK = 75
    fromEnum KeyL = 76
    fromEnum KeyM = 77
    fromEnum KeyN = 78
    fromEnum KeyO = 79
    fromEnum KeyP = 80
    fromEnum KeyQ = 81
    fromEnum KeyR = 82
    fromEnum KeyS = 83
    fromEnum KeyT = 84
    fromEnum KeyU = 85
    fromEnum KeyV = 86
    fromEnum KeyW = 87
    fromEnum KeyX = 88
    fromEnum KeyY = 89
    fromEnum KeyZ = 90
    fromEnum Numpad0 = 96
    fromEnum Numpad1 = 97
    fromEnum Numpad2 = 98
    fromEnum Numpad3 = 99
    fromEnum Numpad4 = 100
    fromEnum Numpad5 = 101
    fromEnum Numpad6 = 102
    fromEnum Numpad7 = 103
    fromEnum Numpad8 = 104
    fromEnum Numpad9 = 105
    fromEnum NumpadMultiply = 106
    fromEnum NumpadPlus = 107
    fromEnum NumpadMinus = 109
    fromEnum NumpadDecimal = 110
    fromEnum NumpadDivide = 111
    fromEnum F1 = 112
    fromEnum F2 = 113
    fromEnum F3 = 114
    fromEnum F4 = 115
    fromEnum F5 = 116
    fromEnum F6 = 117
    fromEnum F7 = 118
    fromEnum F8 = 119
    fromEnum F9 = 120
    fromEnum F10 = 121
    fromEnum F11 = 122
    fromEnum F12 = 123
    fromEnum NumLock = 144
    fromEnum ScrollLock = 145
    fromEnum Semicolon = 186
    fromEnum Equal = 187
    fromEnum Comma = 188
    fromEnum Minus = 189
    fromEnum Period = 190
    fromEnum Slash = 191
    fromEnum Grave = 192
    fromEnum LeftBracket = 219
    fromEnum Backslash = 220
    fromEnum RightBracket = 221
    fromEnum Apostrophe = 222
    fromEnum DpadLeft = 1000
    fromEnum DpadRight = 1001
    fromEnum DpadUp = 1003
    fromEnum DpadDown = 1004
    fromEnum DpadCenter = 1005

    toEnum 6 = AndroidBack
    -- toEnum 18 = AndroidMenu | - the application should always handle Alt
    toEnum 8 = Backspace
    toEnum 9 = Tab
    toEnum 13 = Enter
    toEnum 16 = Shift
    toEnum 17 = Ctrl
    toEnum 18 = Alt
    toEnum 19 = Pause
    toEnum 20 = Capslock
    toEnum 27 = Escape
    toEnum 32 = Space
    toEnum 33 = Pageup
    toEnum 34 = Pagedown
    toEnum 35 = End
    toEnum 36 = Home
    toEnum 37 = ArrowLeft
    toEnum 38 = ArrowUp
    toEnum 39 = ArrowRight
    toEnum 40 = ArrowDown
    toEnum 41 = Select
    toEnum 45 = Insert
    toEnum 46 = Delete
    toEnum 48 = Digit0
    toEnum 49 = Digit1
    toEnum 50 = Digit2
    toEnum 51 = Digit3
    toEnum 52 = Digit4
    toEnum 53 = Digit5
    toEnum 54 = Digit6
    toEnum 55 = Digit7
    toEnum 56 = Digit8
    toEnum 57 = Digit9
    toEnum 65 = KeyA
    toEnum 66 = KeyB
    toEnum 67 = KeyC
    toEnum 68 = KeyD
    toEnum 69 = KeyE
    toEnum 70 = KeyF
    toEnum 71 = KeyG
    toEnum 72 = KeyH
    toEnum 73 = KeyI
    toEnum 74 = KeyJ
    toEnum 75 = KeyK
    toEnum 76 = KeyL
    toEnum 77 = KeyM
    toEnum 78 = KeyN
    toEnum 79 = KeyO
    toEnum 80 = KeyP
    toEnum 81 = KeyQ
    toEnum 82 = KeyR
    toEnum 83 = KeyS
    toEnum 84 = KeyT
    toEnum 85 = KeyU
    toEnum 86 = KeyV
    toEnum 87 = KeyW
    toEnum 88 = KeyX
    toEnum 89 = KeyY
    toEnum 90 = KeyZ
    toEnum 96 = Numpad0
    toEnum 97 = Numpad1
    toEnum 98 = Numpad2
    toEnum 99 = Numpad3
    toEnum 100 = Numpad4
    toEnum 101 = Numpad5
    toEnum 102 = Numpad6
    toEnum 103 = Numpad7
    toEnum 104 = Numpad8
    toEnum 105 = Numpad9
    toEnum 106 = NumpadMultiply
    toEnum 107 = NumpadPlus
    toEnum 109 = NumpadMinus
    toEnum 110 = NumpadDecimal
    toEnum 111 = NumpadDivide
    toEnum 112 = F1
    toEnum 113 = F2
    toEnum 114 = F3
    toEnum 115 = F4
    toEnum 116 = F5
    toEnum 117 = F6
    toEnum 118 = F7
    toEnum 119 = F8
    toEnum 120 = F9
    toEnum 121 = F10
    toEnum 122 = F11
    toEnum 123 = F12
    toEnum 144 = NumLock
    toEnum 145 = ScrollLock
    toEnum 186 = Semicolon
    toEnum 187 = Equal
    toEnum 188 = Comma
    toEnum 189 = Minus
    toEnum 190 = Period
    toEnum 191 = Slash
    toEnum 192 = Grave
    toEnum 219 = LeftBracket
    toEnum 220 = Backslash
    toEnum 221 = RightBracket
    toEnum 222 = Apostrophe
    toEnum 1000 = DpadLeft
    toEnum 1001 = DpadRight
    toEnum 1003 = DpadUp
    toEnum 1004 = DpadDown
    toEnum 1005 = DpadCenter
    toEnum k = error $ "JavaScript.Cocos2d.Types.Key.toEnum: bad argument " ++ show k
    succ x = (dropWhile (/= x) allKeys) !! 1
    pred x = (dropWhile (/= x) (reverse allKeys)) !! 1
    enumFrom x = enumFromTo x maxBound
    enumFromTo x y | x == maxBound = if y == maxBound then [maxBound] else []
                   | otherwise = enumFromThenTo x (succ x) y
    enumFromThen x y = enumFromThenTo x y (if x <= y then maxBound else minBound)
    -- XXX this doesn't return infinite list on [x,x..] as Prelude does
    enumFromThenTo x1 x2 y | (_:_) <- vs = let (intv, rem) = count (-1) vs
                                               count int [] = (int, [])
                                               count int l@(x:xs) | x == x2 = (int, l)
                                                                  | x == y = (int, [])
                                                                  | otherwise = count (int+1) xs
                                               splice _ [] = []
                                               splice int (x:xs) | x == y = if int == 0 then [x] else []
                                                                 | int == 0 = x:splice intv xs
                                                                 | otherwise = splice (int-1) xs
                                           in x1:splice 0 rem
                           | otherwise = []
        where vs = dropWhile (/= x1) keys
              keys | x1 < x2 && x1 <= y = allKeys
                   | x1 > x2 && x1 >= y = reverse allKeys
                   | otherwise = []

instance FromJSVal Key where
    fromJSVal = flip catch handle . fmap Just . evaluate . toEnum . pFromJSVal
        where handle :: ErrorCall -> IO (Maybe Key)
              handle e = print e >> return Nothing

instance ToJSVal Key where
    toJSVal = toJSVal . fromEnum

-- MouseEvent
newtype MouseEvent = MouseEvent JSVal deriving (FromJSVal, ToJSVal)

-- convert a JSVal from an Enum, Bounded instance by trying values one by one
pEnumFromJSVal :: (Enum a, Bounded a, PToJSVal a) => JSVal -> a
pEnumFromJSVal v = fromMaybe maxBound $ find (js_eq v . pToJSVal) (init [minBound .. maxBound])

foreign import javascript unsafe "$1===$2" js_eq :: JSVal -> JSVal -> Bool
