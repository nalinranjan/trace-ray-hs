module PixelOps where

import           Data.Colour.SRGB.Linear
import           Data.ByteString as BS
import           Data.Word


floatToWord8 :: Float -> Word8
floatToWord8 x = fromIntegral (round (x * 255) :: Int) :: Word8

rgbToByteString :: RGB Float -> ByteString
rgbToByteString (RGB r g b) = pack $ Prelude.map floatToWord8 [r,g,b,1.0]

-- replicateBS :: Int -> ByteString -> ByteString
-- replicateBS n bs = BS.concat $ Prelude.replicate n bs 

-- listRgbToByteString :: [RGB Float] -> ByteString
-- listRgbToByteString rgbList = BS.concat $ Prelude.map rgbToByteString rgbList

clamp :: (Ord a) => a -> a -> a -> a
clamp low high = max low . min high

clampRGB :: RGB Float -> RGB Float
clampRGB (RGB r g b) = RGB (clamp' r) (clamp' g) (clamp' b)
  where clamp' = clamp 0.0 1.0

multRGB :: RGB Float -> RGB Float -> RGB Float
multRGB (RGB r g b) (RGB r' g' b') = RGB (r * r') (g * g') (b * b')

scaleRGB :: Float -> RGB Float -> RGB Float
scaleRGB x = multRGB (RGB x x x)

(!+!) :: RGB Float -> RGB Float -> RGB Float
(RGB r g b) !+! (RGB r' g' b') = RGB (r + r') (g + g') (b + b')
