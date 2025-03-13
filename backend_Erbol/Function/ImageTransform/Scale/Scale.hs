module ImageTransform.Scale.Scale where

import System.Environment (getArgs)
import Codec.Picture
import Data.Word (Word8)

-- Основная функция для масштабирования
scaleImage :: FilePath -> FilePath -> Float -> IO ()
scaleImage inputPath outputPath scale = do
  eitherImage <- readImage inputPath
  case eitherImage of
    Left err -> putStrLn $ "Ошибка загрузки изображения: " ++ err
    Right image -> do
      let convertedImage = convertRGB8 image
      let scaledImage = scaleImageByFactor convertedImage scale
      savePngImage outputPath (ImageRGB8 scaledImage)

-- Функция для масштабирования изображения
scaleImageByFactor :: Image PixelRGB8 -> Float -> Image PixelRGB8
scaleImageByFactor img scale =
  generateImage pixelMapper newWidth newHeight
  where
    originalWidth = imageWidth img
    originalHeight = imageHeight img
    newWidth = round (fromIntegral originalWidth * scale)
    newHeight = round (fromIntegral originalHeight * scale)

    -- Функция для получения цвета пикселя в новом изображении
    pixelMapper x y =
      let origX = clamp 0 (originalWidth - 1) (round (fromIntegral x / scale))
          origY = clamp 0 (originalHeight - 1) (round (fromIntegral y / scale))
      in pixelAt img origX origY

    -- Функция ограничения значений в диапазоне
    clamp :: Int -> Int -> Int -> Int
    clamp minVal maxVal value = max minVal (min maxVal value)

-- Точка входа программы
main :: IO ()
main = do
  args <- getArgs
  case args of
    [input, output, scaleStr] ->
      case reads scaleStr of
        [(scale, "")] | scale > 0 -> scaleImage input output scale