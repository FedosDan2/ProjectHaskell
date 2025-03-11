module ImageProcessing.Pixelate.Pixelate where

import Codec.Picture
import System.Environment (getArgs)

-- Основная функция обработки
applyPixelate :: FilePath -> FilePath -> Int -> IO ()
applyPixelate inputPath outputPath blockSize = do
  eitherImage <- readImage inputPath
  case eitherImage of
    Left err -> putStrLn $ "Ошибка загрузки изображения: " ++ err
    Right image -> do
      let convertedImage = convertRGB8 image
      let pixelatedImage = pixelateFilter blockSize convertedImage
      savePngImage outputPath (ImageRGB8 pixelatedImage)

-- Фильтр мозаики
pixelateFilter :: Int -> Image PixelRGB8 -> Image PixelRGB8
pixelateFilter blockSize img = generateImage pixelateFunc (imageWidth img) (imageHeight img)
  where
    pixelateFunc x y =
      let blockX = x - (x `mod` blockSize)
          blockY = y - (y `mod` blockSize)
      in pixelAt img blockX blockY

-- Точка входа программы
main :: IO ()
main = do
  args <- getArgs
  case args of
    [input, output, blockSizeStr] ->
      case reads blockSizeStr of
        [(blockSize, "")] | blockSize > 0 ->
          applyPixelate input output blockSize
        _ -> putStrLn "Ошибка: Размер блока должен быть положительным целым числом"
    _ -> putStrLn "Использование: <входной_файл> <выходной_файл> <размер_блока>"