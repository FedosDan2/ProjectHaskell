module ImageTransform.Move.Vertical where

import Codec.Picture
import System.Environment (getArgs)

-- Основная функция обработки
applyMoveVertical :: FilePath -> FilePath -> Int -> IO ()
applyMoveVertical inputPath outputPath yOffset = do
  eitherImage <- readImage inputPath
  case eitherImage of
    Left err -> putStrLn $ "Ошибка загрузки изображения: " ++ err
    Right image -> do
      let convertedImage = convertRGB8 image
      let movedImage = moveFilterVertical yOffset convertedImage
      savePngImage outputPath (ImageRGB8 movedImage)

-- Фильтр для вертикального сдвига
moveFilterVertical :: Int -> Image PixelRGB8 -> Image PixelRGB8
moveFilterVertical yOffset img = generateImage pixelFunc width height
  where
    width = imageWidth img
    height = imageHeight img

    pixelFunc x y =
      let originalY = y - yOffset
      in if originalY >= 0 && originalY < height
         then pixelAt img x originalY
         else PixelRGB8 255 255 255  -- Заполняем пустые области белым цветом

-- Точка входа программы
main :: IO ()
main = do
  args <- getArgs
  case args of
    [input, output, yOffsetStr] ->
      case reads yOffsetStr of
        [(yOffset, "")] -> applyMoveVertical input output yOffset
        _ -> putStrLn "Ошибка: Смещение по Y должно быть целым числом"
    _ -> putStrLn "Использование: <входной_файл> <выходной_файл> <смещение_y>"
