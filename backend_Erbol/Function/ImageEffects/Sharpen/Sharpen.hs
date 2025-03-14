module ImageEffects.Sharpen.Sharpen where

import Codec.Picture
import System.Environment (getArgs)
import Data.Word (Word8)

-- Основная функция обработки
applySharpen :: FilePath -> FilePath -> IO ()
applySharpen inputPath outputPath = do
  eitherImage <- readImage inputPath
  case eitherImage of
    Left err -> putStrLn $ "Ошибка загрузки изображения: " ++ err
    Right image -> do
      let convertedImage = convertRGB8 image
      let sharpenedImage = sharpenFilter convertedImage
      savePngImage outputPath (ImageRGB8 sharpenedImage)

-- Фильтр повышения резкости
sharpenFilter :: Image PixelRGB8 -> Image PixelRGB8
sharpenFilter img = generateImage pixelFunc (imageWidth img) (imageHeight img)
  where
    pixelFunc x y =
      let (r, g, b) = applyKernel img x y sharpenKernel
      in PixelRGB8 (clamp r) (clamp g) (clamp b)

-- Ядро для повышения резкости
sharpenKernel :: [[Int]]
sharpenKernel =
  [ [ 0, -1,  0]
  , [-1,  5, -1]
  , [ 0, -1,  0]
  ]

-- Применение ядра к пикселю
applyKernel :: Image PixelRGB8 -> Int -> Int -> [[Int]] -> (Int, Int, Int)
applyKernel img x y kernel =
  let kernelSize = length kernel
      halfSize = kernelSize `div` 2
      pixels = [ (kernel !! (i) !! (j), pixelAt img (clampX img (x + i - halfSize)) (clampY img (y + j - halfSize))) 
               | i <- [0 .. kernelSize - 1], j <- [0 .. kernelSize - 1] ]
      (totalR, totalG, totalB) = foldl (\(accR, accG, accB) (weight, PixelRGB8 r g b) -> 
                                  (accR + weight * fromIntegral r, accG + weight * fromIntegral g, accB + weight * fromIntegral b)) 
                                (0, 0, 0) pixels
  in (totalR, totalG, totalB)

-- Ограничение координат по ширине изображения
clampX :: Image PixelRGB8 -> Int -> Int
clampX img x = max 0 (min (imageWidth img - 1) x)

-- Ограничение координат по высоте изображения
clampY :: Image PixelRGB8 -> Int -> Int
clampY img y = max 0 (min (imageHeight img - 1) y)

-- Ограничение значения пикселя (0-255)
clamp :: Int -> Word8
clamp value = fromIntegral (max 0 (min 255 value))

-- Точка входа программы
main :: IO ()
main = do
  args <- getArgs
  case args of
    [input, output] -> applySharpen input output
    _ -> putStrLn "Использование: <входной_файл> <выходной_файл>"