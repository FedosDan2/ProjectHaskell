-- Main.hs
module Main where

import Codec.Picture
import System.Environment (getArgs)

-- Основная функция обработки
applySepia :: FilePath -> FilePath -> IO ()
applySepia inputPath outputPath = do
  eitherImage <- readImage inputPath
  case eitherImage of
    Left err -> putStrLn $ "Ошибка загрузки изображения: " ++ err
    Right image -> do
      let convertedImage = convertRGB8 image
      let sepiaImage = pixelMap sepiaFilter convertedImage
      savePngImage outputPath (ImageRGB8 sepiaImage)

-- Фильтр сепии
sepiaFilter :: PixelRGB8 -> PixelRGB8
sepiaFilter (PixelRGB8 r g b) =
  let r' = 0.393 * fromIntegral r + 0.769 * fromIntegral g + 0.189 * fromIntegral b
      g' = 0.349 * fromIntegral r + 0.686 * fromIntegral g + 0.168 * fromIntegral b
      b' = 0.272 * fromIntegral r + 0.534 * fromIntegral g + 0.131 * fromIntegral b
      -- Исправленная функция ограничения с округлением
      clamp x = fromIntegral (max 0 (min 255 (round x)))
  in PixelRGB8 (clamp r') (clamp g') (clamp b')

-- Точка входа программы
main :: IO ()
main = do
  args <- getArgs
  case args of
    [input, output] -> applySepia input output
    _ -> putStrLn "Использование: <входной_файл> <выходной_файл>"