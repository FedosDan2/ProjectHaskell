module Main where

import Codec.Picture
import System.Environment (getArgs)

-- Основная функция для обработки
applyGrayscale :: FilePath -> FilePath -> IO ()
applyGrayscale inputPath outputPath = do
  eitherImage <- readImage inputPath
  case eitherImage of
    Left err -> putStrLn $ "Ошибка загрузки изображения: " ++ err
    Right image -> do
      let convertedImage = convertRGB8 image
      let grayscaleImage = pixelMap grayscaleFilter convertedImage
      savePngImage outputPath (ImageRGB8 grayscaleImage)

-- Фильтр перевода в оттенки серого
grayscaleFilter :: PixelRGB8 -> PixelRGB8
grayscaleFilter (PixelRGB8 r g b) =
  let gray = truncate (0.299 * fromIntegral r + 0.587 * fromIntegral g + 0.114 * fromIntegral b)
  in PixelRGB8 gray gray gray

-- Точка входа программы
main :: IO ()
main = do
  args <- getArgs
  case args of
    [input, output] -> applyGrayscale input output
    _ -> putStrLn "Использование: <входной_файл> <выходной_файл>"