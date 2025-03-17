module Main where

import System.Environment (getArgs)
import System.FilePath (takeFileName, replaceExtension, combine)
import ImageEffects.Brightness.Brightness (adjustBrightness)
import ImageProcessing.Grayscale.Grayscale (applyGrayscale)
import ImageProcessing.Negative.Negative (applyNegative)
import ImageProcessing.Sepia.Sepia (applySepia)
import ImageProcessing.Solarize.Solarize (applySolarize)
import ImageTransform.Scale.Scale (scaleImage) 
import ImageEffects.Pixelate.Pixelate (applyPixelate)
import ImageProcessing.Contrast.Contrast (applyContrast)
import ImageTransform.Rotation.Rotation (applyRotate)
import ImageTransform.HorizontalFlip.HorizontalFlip (applyHorizontalFlip)
import ImageTransform.VerticalFlip.VerticalFlip (applyVerticalFlip)
import ImageEffects.Sharpen.Sharpen (applySharpen)
import ImageTransform.Move.Vertical (applyMoveVertical)
import ImageTransform.Move.Horizontal (applyMoveHorizontal)
import Codec.Picture

main :: IO ()
main = do
    -- Получаем аргументы командной строки
    args <- getArgs
    case args of
        [inputPath, filterType, outputDir] -> do
            -- Определяем путь для сохранения обработанного изображения
            let outputFileName = replaceExtension (takeFileName inputPath) ".png"
            let outputPath = combine outputDir outputFileName

            -- Применяем фильтр в зависимости от типа
            case filterType of
                "sepia" -> applySepia inputPath outputPath
                "grayscale" -> applyGrayscale inputPath outputPath
                "negative" -> applyNegative inputPath outputPath
                "brightness" -> adjustBrightness inputPath outputPath 1.5
                "solarize" -> applySolarize inputPath outputPath
                "scale" -> scaleImage inputPath outputPath 1.5
                "contrast" -> applyContrast inputPath outputPath 1.5
                "pixel" -> applyPixelate inputPath outputPath 5
                "rotation" -> applyRotate inputPath outputPath 80
                "vert_flip" -> applyVerticalFlip inputPath outputPath 
                "horiz_flip" -> applyHorizontalFlip inputPath outputPath 

                _ -> putStrLn "Неизвестный тип фильтра."
        _ -> putStrLn "Использование: <input> <filter> <outputDir>"