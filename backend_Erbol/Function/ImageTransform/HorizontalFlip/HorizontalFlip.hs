module ImageTransform.HorizontalFlip.HorizontalFlip where

import Codec.Picture
import System.Environment (getArgs)

-- Основная функция обработки
applyHorizontalFlip :: FilePath -> FilePath -> IO ()
applyHorizontalFlip inputPath outputPath = do
  eitherImage <- readImage inputPath
  case eitherImage of
    Left err -> putStrLn $ "Ошибка загрузки изображения: " ++ err
    Right image -> do
      let convertedImage = convertRGB8 image
      let flippedImage = horizontalFlipFilter convertedImage
      savePngImage outputPath (ImageRGB8 flippedImage)

-- Фильтр горизонтального отражения
horizontalFlipFilter :: Image PixelRGB8 -> Image PixelRGB8
horizontalFlipFilter img = generateImage pixelFunc (imageWidth img) (imageHeight img)
  where
    pixelFunc x y =
      let flippedX = imageWidth img - 1 - x  -- Отражение по горизонтали
      in pixelAt img flippedX y  -- Используем flippedX для отражения

-- Точка входа программы
main :: IO ()
main = do
  args <- getArgs
  case args of
    [input, output] -> applyHorizontalFlip input output
    _ -> putStrLn "Использование: <входной_файл> <выходной_файл>"