-- Main.hs
module Main where

import Codec.Picture
import System.Environment (getArgs)

-- Основная функция обработки
applyNegative :: FilePath -> FilePath -> IO ()
applyNegative inputPath outputPath = do
  eitherImage <- readImage inputPath
  case eitherImage of
    Left err -> putStrLn $ "Ошибка загрузки изображения: " ++ err
    Right image -> do
      let convertedImage = convertRGB8 image
      let negativeImage = pixelMap negativeFilter convertedImage
      savePngImage outputPath (ImageRGB8 negativeImage)

-- Фильтр негатива
negativeFilter :: PixelRGB8 -> PixelRGB8
negativeFilter (PixelRGB8 r g b) = PixelRGB8 (255 - r) (255 - g) (255 - b)

-- Точка входа программы
main :: IO ()
main = do
  args <- getArgs
  case args of
    [input, output] -> applyNegative input output
    _ -> putStrLn "Использование: <входной_файл> <выходной_файл>"