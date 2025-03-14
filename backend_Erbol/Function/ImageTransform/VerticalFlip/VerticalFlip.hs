module ImageTransform.VerticalFlip.VerticalFlip where

import Codec.Picture
import System.Environment (getArgs)

-- Основная функция обработки
applyVerticalFlip :: FilePath -> FilePath -> IO ()
applyVerticalFlip inputPath outputPath = do
  eitherImage <- readImage inputPath
  case eitherImage of
    Left err -> putStrLn $ "Ошибка загрузки изображения: " ++ err
    Right image -> do
      let convertedImage = convertRGB8 image
      let flippedImage = verticalFlipFilter convertedImage
      savePngImage outputPath (ImageRGB8 flippedImage)

-- Фильтр вертикального отражения
verticalFlipFilter :: Image PixelRGB8 -> Image PixelRGB8
verticalFlipFilter img = generateImage pixelFunc (imageWidth img) (imageHeight img)
  where
    pixelFunc x y =
      let flippedY = imageHeight img - 1 - y  -- Отражение по вертикали
      in pixelAt img x flippedY

-- Точка входа программы
main :: IO ()
main = do
  args <- getArgs
  case args of
    [input, output] -> applyVerticalFlip input output
    _ -> putStrLn "Использование: <входной_файл> <выходной_файл>"