{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import qualified Data.ByteString.Lazy.Char8 as ByteString
import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..), ToJSON(..), eitherDecode')
import Data.Text (Text)
import Data.Foldable (traverse_)
import qualified Data.Text as Text
import qualified Data.List as List

import Codec.Picture

type Length = Int
type X = Int
type Y = Int
type Name = Text
type Quantity = Int

data Rectangle = Rectangle {
    rectX :: Length
  , rectY :: Length
  , offX  :: X
  , offY  :: Y
  } deriving (Show, Generic)

instance FromJSON Rectangle

data Wrapping = Wrapping {
    name       :: Name
  , quantity   :: Quantity
  , rectangles :: [ Rectangle ]
  } deriving (Show, Generic)

instance FromJSON Wrapping

printWrapping :: Wrapping -> IO ()
printWrapping wrapping = 
    writePng path $ generateImage pixelRenderer maxX maxY
  where
    -- File to store the template.
    path = Text.unpack (name wrapping) <> ".png"

    -- Render a pixel, since we'll scan all pixels we need some utility function to test
    -- whether a pixel falls in a rectangle or not.
    pixelRenderer x y =
      case findRectangle x y of
        Nothing -> PixelRGB8 (fromIntegral x `mod` 32) (fromIntegral y `mod` 32) 128
        Just _ -> PixelRGB8 0 200 0

    -- Tests whether a pixel should be displayed green or not
    findRectangle x y = List.find (containsPixel x y) (rectangles wrapping)
    containsPixel x y rect =
            let x0 = scale $ offX rect
                x1 = scale $ offX rect + rectX rect
                y0 = scale $ offY rect
                y1 = scale $ offY rect + rectY rect
            in  x0 <= x && x < x1 && y0 <= y && y < y1

    -- bounds and scaling
    maxX = scale $ maximum (0 : [ rectX r + offX r | r <- rectangles wrapping ])
    maxY = scale $ maximum (0 : [ rectY r + offY r | r <- rectangles wrapping ])
    scale t = 10 * t

main :: IO ()
main = do
  wrappings <- eitherDecode' @[Wrapping] <$> ByteString.getContents
  case wrappings of
    Left err -> print err
    Right xs -> traverse_ printWrapping xs
  putStrLn "done!"
