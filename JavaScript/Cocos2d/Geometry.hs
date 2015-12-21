module JavaScript.Cocos2d.Geometry where

import Control.Applicative
import Linear
import GHCJS.Types

-- provides conversion between CC geometry types and Haskell native
-- geometry types

newtype Point = Point JSVal

foreign import javascript unsafe "$1.x" cc_getX :: Point -> IO Double
foreign import javascript unsafe "$1.y" cc_getY :: Point -> IO Double

pointToV2 :: Point -> IO (V2 Double)
pointToV2 = liftA2 V2 <$> cc_getX <*> cc_getY
