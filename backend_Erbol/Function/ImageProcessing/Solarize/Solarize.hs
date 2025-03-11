module ImageProcessing.Solarize.Solarize where

import Codec.Picture
import System.Environment (getArgs)

-- Основная функция обработки
applySolarize :: FilePath -> FilePath -> IO ()
applySolarize inputPath outputPath = do
  eitherImage <- readImage inputPath
  case eitherImage of
    Left err -> putStrLn $ "Ошибка загрузки изображения: " ++ err
    Right image -> do
      let convertedImage = convertRGB8 image
      let solarizedImage = pixelMap solarizeFilter convertedImage
      savePngImage outputPath (ImageRGB8 solarizedImage)

-- Фильтр соляризации
solarizeFilter :: PixelRGB8 -> PixelRGB8
solarizeFilter (PixelRGB8 r g b) =
  let solarize c = if c > 128 then 255 - c else c
  in PixelRGB8 (solarize r) (solarize g) (solarize b)

-- Точка входа программы
main :: IO ()
main = do
  args <- getArgs
  case args of
    [input, output] -> applySolarize input output
    _ -> putStrLn "Использование: <входной_файл> <выходной_файл>"