module Main where

import System.Environment (getArgs)
import System.FilePath (takeFileName, replaceExtension, combine)
import ImageProcessing.Brightness (adjustBrightness)
import ImageProcessing.Grayscale (applyGrayscale)
import ImageProcessing.Negative (applyNegative)
import ImageProcessing.Sepia (applySepia)
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
                _ -> putStrLn "Неизвестный тип фильтра."
        _ -> putStrLn "Использование: <input> <filter> <outputDir>"