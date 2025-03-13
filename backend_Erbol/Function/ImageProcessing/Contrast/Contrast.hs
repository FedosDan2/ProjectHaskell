module ImageProcessing.Contrast.Contrast where

import Codec.Picture
import System.Environment (getArgs)

-- Основная функция обработки
applyContrast :: FilePath -> FilePath -> Double -> IO ()
applyContrast inputPath outputPath contrastFactor = do
  eitherImage <- readImage inputPath
  case eitherImage of
    Left err -> putStrLn $ "Ошибка загрузки изображения: " ++ err
    Right image -> do
      let convertedImage = convertRGB8 image
      let contrastedImage = contrastFilter contrastFactor convertedImage
      savePngImage outputPath (ImageRGB8 contrastedImage)

-- Фильтр контраста
contrastFilter :: Double -> Image PixelRGB8 -> Image PixelRGB8
contrastFilter contrastFactor img =
  generateImage contrastFunc (imageWidth img) (imageHeight img)
  where
    contrastFunc x y =
      let PixelRGB8 r g b = pixelAt img x y
          adjustComponent c = clamp $ round ((fromIntegral c - 128) * contrastFactor + 128)
          clamp = max 0 . min 255
      in PixelRGB8 (adjustComponent r) (adjustComponent g) (adjustComponent b)

-- Точка входа программы
main :: IO ()
main = do
  args <- getArgs
  case args of
    [input, output, contrastFactorStr] ->
      case reads contrastFactorStr of
        [(contrastFactor, "")] | contrastFactor > 0 ->
          applyContrast input output contrastFactor
        _ -> putStrLn "Ошибка: Фактор контраста должен быть положительным числом"
    _ -> putStrLn "Использование: <входной_файл> <выходной_файл> <фактор_контраста>"