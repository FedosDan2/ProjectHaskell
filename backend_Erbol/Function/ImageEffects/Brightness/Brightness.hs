module ImageEffects.Brightness.Brightness where

import Codec.Picture
import System.Environment (getArgs)

-- Основная функция обработки
adjustBrightness :: FilePath -> FilePath -> Float -> IO ()
adjustBrightness inputPath outputPath factor = do
  eitherImage <- readImage inputPath
  case eitherImage of
    Left err -> putStrLn $ "Ошибка загрузки изображения: " ++ err
    Right image -> do
      let convertedImage = convertRGB8 image
      let brightnessImage = pixelMap (brightnessFilter factor) convertedImage
      savePngImage outputPath (ImageRGB8 brightnessImage)

-- Фильтр яркости
brightnessFilter :: Float -> PixelRGB8 -> PixelRGB8
brightnessFilter factor (PixelRGB8 r g b) =
  let clamp = min 255 . max 0
      r' = clamp $ truncate (fromIntegral r * factor)
      g' = clamp $ truncate (fromIntegral g * factor)
      b' = clamp $ truncate (fromIntegral b * factor)
  in PixelRGB8 r' g' b'

-- Точка входа программы
main :: IO ()
main = do
  args <- getArgs
  case args of
    [input, output, factorStr] -> 
      case reads factorStr of
        [(factor, "")] -> adjustBrightness input output factor
        _ -> putStrLn "Неверный формат коэффициента яркости"
    _ -> putStrLn "Использование: <входной_файл> <выходной_файл> <коэффициент>"