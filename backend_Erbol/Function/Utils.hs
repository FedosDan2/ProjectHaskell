module Utils where

import Codec.Picture
import System.FilePath (takeExtension)

-- Сохранение изображения в зависимости от расширения
saveImageWithExtension :: FilePath -> Image PixelRGB8 -> IO ()
saveImageWithExtension path img = do
  let ext = takeExtension path
  case ext of
    ".jpg" -> saveJpgImage 100 path (ImageRGB8 img)
    ".jpeg" -> saveJpgImage 100 path (ImageRGB8 img)
    ".png" -> savePngImage path (ImageRGB8 img)
    _ -> putStrLn "Формат не поддерживается для сохранения." 