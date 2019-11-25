module PixelOps where

import           Data.Colour.SRGB.Linear
import           Data.ByteString as BS
import           Data.Word


floatToWord8 :: Float -> Word8
floatToWord8 x = fromIntegral (round (x * 255) :: Int) :: Word8

rgbToByteString :: RGB Float -> ByteString
rgbToByteString (RGB r g b) = pack $ Prelude.map floatToWord8 [r,g,b,1.0]

replicateBS :: Int -> ByteString -> ByteString
replicateBS n bs = BS.concat $ Prelude.replicate n bs

listRgbToByteString :: [RGB Float] -> ByteString
listRgbToByteString rgbList = BS.concat $ Prelude.map rgbToByteString rgbList
