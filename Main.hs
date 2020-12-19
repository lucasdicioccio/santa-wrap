{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveFunctor #-}
module Main where

import qualified Data.ByteString.Lazy.Char8 as ByteString
import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..), ToJSON(..), eitherDecode')
import Data.Text (Text)
import Data.Foldable (traverse_)
import qualified Data.Text as Text
import qualified Data.List as List
import System.Environment (getArgs)

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
  , colorNum   :: Int
  , quantity   :: Quantity
  , rectangles :: [ Rectangle ]
  } deriving (Show, Generic)

instance FromJSON Wrapping

-- | How far a Wrapping goes in the X-direction.
extentX :: Wrapping -> Length
extentX wrapping = maximum (0 : [ rectX r + offX r | r <- rectangles wrapping ])

-- | How far a Wrapping goes in the Y-direction.
extentY :: Wrapping -> Length
extentY wrapping = maximum (0 : [ rectY r + offY r | r <- rectangles wrapping ])


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
        Just idx -> pixelColor idx

    -- Tests whether a pixel should be displayed green or not
    findRectangle x y = List.findIndex (rectangleContainsPixel x y) (rectangles wrapping)
    rectangleContainsPixel x y rect =
            let x0 = scale $ offX rect
                x1 = scale $ offX rect + rectX rect
                y0 = scale $ offY rect
                y1 = scale $ offY rect + rectY rect
            in  x0 <= x && x < x1 && y0 <= y && y < y1

    -- bounds and scaling
    maxX = scale $ extentX wrapping
    maxY = scale $ extentY wrapping
    scale t = 10 * t

data Positioned a = Positioned {
    posX :: X
  , posY :: Y
  , item :: a
  } deriving (Show, Functor)

data Layout = Layout {
    positionedWrappings :: [ Positioned Wrapping ]
  } deriving (Show)


pixelColor :: Int -> PixelRGB8
pixelColor idx
  | idx `mod` 10 == 0 = PixelRGB8 0 200 0
  | idx `mod` 10 == 1 = PixelRGB8 200 0 0
  | idx `mod` 10 == 2 = PixelRGB8 0 0 200
  | idx `mod` 10 == 3 = PixelRGB8 200 200 0
  | idx `mod` 10 == 4 = PixelRGB8 0 200 200
  | idx `mod` 10 == 5 = PixelRGB8 200 0 200
  | idx `mod` 10 == 6 = PixelRGB8 100 100 0
  | idx `mod` 10 == 7 = PixelRGB8 0 100 100
  | idx `mod` 10 == 8 = PixelRGB8 100 0 100
  | idx `mod` 10 == 9 = PixelRGB8 100 100 100


printLayout :: Layout -> IO ()
printLayout layout =
    writePng "layout.png" $ generateImage pixelRenderer maxX maxY
  where
    pixelRenderer :: Int -> Int -> PixelRGB8
    pixelRenderer x y =
      case findWrapping x y of
        Nothing -> PixelRGB8 (fromIntegral x `mod` 32) (fromIntegral y `mod` 32) 128
        Just w -> pixelColor $ colorNum $ item w

    findWrapping :: Int -> Int -> Maybe (Positioned Wrapping)
    findWrapping x y = List.find (wrappingContainsPixel x y) (positionedWrappings layout)

    wrappingContainsPixel :: Int -> Int -> Positioned Wrapping -> Bool
    wrappingContainsPixel x y wrapping =
        List.any
            (rectangleContainsPixel (posX wrapping) (posY wrapping) x y)
            (rectangles $ item wrapping)

    rectangleContainsPixel :: X -> Y -> Int -> Int -> Rectangle -> Bool
    rectangleContainsPixel posx posy x y rect =
            let x0 = scale $ posx + offX rect
                x1 = scale $ posx + offX rect + rectX rect
                y0 = scale $ posy + offY rect
                y1 = scale $ posy + offY rect + rectY rect
            in  x0 <= x && x < x1 && y0 <= y && y < y1

    -- bounds and scaling
    maxX = scale $ maximum (0 : [ extentX (item w) + posX w | w <- positionedWrappings layout ])
    maxY = scale $ maximum (0 : [ extentY (item w) + posY w | w <- positionedWrappings layout ])
    scale t = 10 * t

linearLayout :: [ Wrapping ] -> Layout
linearLayout wrappings = Layout $ List.zipWith (\w (x,y) -> Positioned x y w) wrappings xys
  where
    xys :: [(X,Y)]
    xys = List.scanl placeToTheLeft (0,0) wrappings

    placeToTheLeft :: (X, Y) -> Wrapping -> (X, Y)
    placeToTheLeft (x,y) w = (x + extentX w, 0)

main :: IO ()
main = do
  args <- getArgs
  case args of
     ["parts"] -> mainParts
     ["linear-layout"] -> mainLinearLayout
     _ -> mainUsage
  where
    mainUsage = do
      putStrLn "parts | linear-layout"
    mainParts = do
      wrappings <- eitherDecode' @[Wrapping] <$> ByteString.getContents
      case wrappings of
        Left err -> print err
        Right xs -> traverse_ printWrapping xs
      putStrLn "done!"
    mainLinearLayout = do
      wrappings <- eitherDecode' @[Wrapping] <$> ByteString.getContents
      case wrappings of
        Left err -> print err
        Right xs -> do
          let items = mconcat [ replicate (quantity w) w | w <- xs ]
          let layout = linearLayout $ items
          printLayout layout
      putStrLn "done!"
