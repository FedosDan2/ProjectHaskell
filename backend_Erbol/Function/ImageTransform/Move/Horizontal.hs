module ImageTransform.Move.Horizontal where

import Codec.Picture
import System.Environment (getArgs)

-- Основная функция обработки
applyMoveHorizontal :: FilePath -> FilePath -> Int -> IO ()
applyMoveHorizontal inputPath outputPath xOffset = do
  eitherImage <- readImage inputPath
  case eitherImage of
    Left err -> putStrLn $ "Ошибка загрузки изображения: " ++ err
    Right image -> do
      let convertedImage = convertRGB8 image
      let movedImage = moveFilterHorizontal xOffset convertedImage
      savePngImage outputPath (ImageRGB8 movedImage)

-- Фильтр для горизонтального сдвига
moveFilterHorizontal :: Int -> Image PixelRGB8 -> Image PixelRGB8
moveFilterHorizontal xOffset img = generateImage pixelFunc width height
  where
    width = imageWidth img
    height = imageHeight img

    pixelFunc x y =
      let originalX = x - xOffset
      in if originalX >= 0 && originalX < width
         then pixelAt img originalX y
         else PixelRGB8 255 255 255  -- Заполняем пустые области белым цветом

-- Точка входа программы
main :: IO ()
main = do
  args <- getArgs
  case args of
    [input, output, xOffsetStr] ->
      case reads xOffsetStr of
        [(xOffset, "")] -> applyMoveHorizontal input output xOffset
        _ -> putStrLn "Ошибка: Смещение по X должно быть целым числом"
    _ -> putStrLn "Использование: <входной_файл> <выходной_файл> <смещение_x>"
