module ImageTransform.Rotation.Rotation where

import Codec.Picture
import System.Environment (getArgs)
import Data.Fixed (mod')

-- Основная функция обработки
applyRotate :: FilePath -> FilePath -> Double -> IO ()
applyRotate inputPath outputPath angle = do
  eitherImage <- readImage inputPath
  case eitherImage of
    Left err -> putStrLn $ "Ошибка загрузки изображения: " ++ err
    Right image -> do
      let convertedImage = convertRGB8 image
      let rotatedImage = rotateFilter angle convertedImage
      savePngImage outputPath (ImageRGB8 rotatedImage)

-- Фильтр поворота
rotateFilter :: Double -> Image PixelRGB8 -> Image PixelRGB8
rotateFilter angle img = generateImage pixelFunc newWidth newHeight
  where
    -- Исходные размеры изображения
    originalWidth = imageWidth img
    originalHeight = imageHeight img

    -- Угол в радианах
    radians = angle * pi / 180

    -- Новые размеры изображения после поворота
    newWidth = ceiling $ abs (fromIntegral originalWidth * cos radians) + abs (fromIntegral originalHeight * sin radians)
    newHeight = ceiling $ abs (fromIntegral originalWidth * sin radians) + abs (fromIntegral originalHeight * cos radians)

    -- Центр исходного изображения
    originalCenterX = fromIntegral originalWidth / 2
    originalCenterY = fromIntegral originalHeight / 2

    -- Центр нового изображения
    newCenterX = fromIntegral newWidth / 2
    newCenterY = fromIntegral newHeight / 2

    -- Функция для получения пикселя с учетом поворота
    pixelFunc x y =
      let -- Преобразование координат относительно центра нового изображения
          x' = fromIntegral x - newCenterX
          y' = fromIntegral y - newCenterY

          -- Обратное преобразование поворота
          xRotated = x' * cos radians + y' * sin radians
          yRotated = -x' * sin radians + y' * cos radians

          -- Преобразование координат обратно в систему исходного изображения
          originalX = round (xRotated + originalCenterX)
          originalY = round (yRotated + originalCenterY)

      in if originalX >= 0 && originalX < originalWidth && originalY >= 0 && originalY < originalHeight
         then pixelAt img originalX originalY
         else PixelRGB8 255 255 255 -- Белый цвет для областей за пределами исходного изображения

-- Точка входа программы
main :: IO ()
main = do
  args <- getArgs
  case args of
    [input, output, angleStr] ->
      case reads angleStr of
        [(angle, "")] ->
          applyRotate input output angle
        _ -> putStrLn "Ошибка: Угол должен быть числом"
    _ -> putStrLn "Использование: <входной_файл> <выходной_файл> <угол>"