import customtkinter as ctk
from PIL import Image
import subprocess
import os
from common import ImageProcces_and_TopMenu  # Импорт из общего модуля

class Helper_Function:
    def __init__(self, func_main):
        self.func_main = func_main

    def _apply_invert_filter(self, image, index):
        """Применяет фильтр инверсии цветов к изображению."""
        image_path = f"temp/multiplProcess/input/picture_inv_{index}.png"
        output_path = f"temp/multiplProcess/output/picture_inv_{index}.png"
        
        # Создаем временную папку, если её нет
        os.makedirs("temp", exist_ok=True)
        
        # Сохраняем изображение для обработки
        image.save(image_path)
        
        try:
            # Вызываем Haskell-программу Negative.hs
            subprocess.run([
                "./backend_Erbol/Function/ImageProcessing/Negative/negative",
                image_path,
                output_path
            ], check=True)
            
            # Загружаем обработанное изображение
            with Image.open(output_path) as img:
                filtered_image = img.copy()
            
            # Удаляем временные файлы
            self.func_main.delete_file(image_path)
            
            return filtered_image
        except subprocess.CalledProcessError as e:
            print(f"Ошибка при вызове Haskell-фильтра: {e}")
            return image  # Возвращаем оригинальное изображение в случае ошибки
        
    def _apply_grayscale_filter(self, image, index):
        """Применяет фильтр черно-белого изображения, используя Haskell (Grayscale.hs)."""
        image_path = f"temp/multiplProcess/input/picture_bw_{index}.png"
        output_path = f"temp/multiplProcess/output/picture_bw_{index}.png"
        
        # Создаем временную папку, если её нет
        os.makedirs("temp", exist_ok=True)
        
        # Сохраняем изображение для обработки
        image.save(image_path)
        
        try:
            # Вызываем Haskell-программу Grayscale.hs
            subprocess.run([
                "./backend_Erbol/Function/ImageProcessing/Grayscale/grayscale",
                image_path,
                output_path
            ], check=True)
            
            # Загружаем обработанное изображение
            with Image.open(output_path) as img:
                filtered_image = img.copy()
            
            # Удаляем временные файлы
            self.func_main.delete_file(image_path)
            
            return filtered_image
        except subprocess.CalledProcessError as e:
            print(f"Ошибка при вызове Haskell-фильтра (Grayscale): {e}")
            return image  # Возвращаем оригинальное изображение в случае ошибки
        

    def _apply_solarize_filter(self, image, index):
        """Применяет фильтр сепии к изображению, используя Haskell (Solarize.hs)."""
        image_path = f"temp/multiplProcess/input/picture_solarize_{index}.png"
        output_path = f"temp/multiplProcess/output/picture_solarize_{index}.png"
        
        # Создаем временную папку, если её нет
        os.makedirs("temp", exist_ok=True)
        
        # Сохраняем изображение для обработки
        image.save(image_path)
        
        try:
            # Вызываем Haskell-программу Solarize.hs
            subprocess.run([
                "./backend_Erbol/Function/ImageProcessing/Solarize/solarize",
                image_path,
                output_path
            ], check=True)
            
            # Загружаем обработанное изображение
            with Image.open(output_path) as img:
                filtered_image = img.copy()
            
            # Удаляем временные файлы
            self.func_main.delete_file(image_path)
            
            return filtered_image
        except subprocess.CalledProcessError as e:
            print(f"Ошибка при вызове Haskell-фильтра (Solarize): {e}")
            return image  # Возвращаем оригинальное изображение в случае ошибки
        
    def _apply_sepia_filter(self, image, index):
        """Применяет фильтр сепии к изображению, используя Haskell (Sepia.hs)."""
        image_path = f"temp/multiplProcess/input/picture_sepia_{index}.png"
        output_path = f"temp/multiplProcess/output/picture_sepia_{index}.png"
        
        # Создаем временную папку, если её нет
        os.makedirs("temp", exist_ok=True)
        
        # Сохраняем изображение для обработки
        image.save(image_path)
        
        try:
            # Вызываем Haskell-программу Sepia.hs
            subprocess.run([
                "./backend_Erbol/Function/ImageProcessing/Sepia/sepia",
                image_path,
                output_path
            ], check=True)
            
            # Загружаем обработанное изображение
            with Image.open(output_path) as img:
                filtered_image = img.copy()
            
            # Удаляем временные файлы
            self.func_main.delete_file(image_path)
            
            return filtered_image
        except subprocess.CalledProcessError as e:
            print(f"Ошибка при вызове Haskell-фильтра (Sepia): {e}")
            return image  # Возвращаем оригинальное изображение в случае ошибки
        
    def _apply_hor_flip_filter(self, image, index):
        """Применяет фильтр горизонтального переворота к изображению, используя Haskell (HorizontalFlip.hs)."""
        image_path = f"temp/multiplProcess/input/picture_hor_flip_{index}.png"
        output_path = f"temp/multiplProcess/output/picture_hor_flip_{index}.png"
        
        # Создаем временную папку, если её нет
        os.makedirs("temp", exist_ok=True)
        
        # Сохраняем изображение для обработки
        image.save(image_path)
        
        try:
            # Вызываем Haskell-программу HorizontalFlip.hs
            subprocess.run([
                "./backend_Erbol/Function/ImageTransform/HorizontalFlip/horizontalFlip",
                image_path,
                output_path
            ], check=True)
            
            # Загружаем обработанное изображение
            with Image.open(output_path) as img:
                filtered_image = img.copy()
            
            # Удаляем временные файлы
            self.func_main.delete_file(image_path)
            
            return filtered_image
        except subprocess.CalledProcessError as e:
            print(f"Ошибка при вызове Haskell-фильтра (HorizontalFlip): {e}")
            return image  # Возвращаем оригинальное изображение в случае ошибки


    def _apply_vert_flip_filter(self, image, index):
        """Применяет фильтр вертикального переворота к изображению, используя Haskell (VerticalFlip.hs)."""
        image_path = f"temp/multiplProcess/input/picture_vert_flip_{index}.png"
        output_path = f"temp/multiplProcess/output/picture_vert_flip_{index}.png"
        
        # Создаем временную папку, если её нет
        os.makedirs("temp", exist_ok=True)
        
        # Сохраняем изображение для обработки
        image.save(image_path)
        
        try:
            # Вызываем Haskell-программу VerticalFlip.hs
            subprocess.run([
                "./backend_Erbol/Function/ImageTransform/VerticalFlip/verticalFlip",
                image_path,
                output_path
            ], check=True)
            
            # Загружаем обработанное изображение
            with Image.open(output_path) as img:
                filtered_image = img.copy()
            
            # Удаляем временные файлы
            self.func_main.delete_file(image_path)
            
            return filtered_image
        except subprocess.CalledProcessError as e:
            print(f"Ошибка при вызове Haskell-фильтра (VerticalFlip): {e}")
            return image  # Возвращаем оригинальное изображение в случае ошибки


    def _apply_sharpen_filter(self, image, index):
        """Применяет фильтр повышения резкости к изображению, используя Haskell (Sharpen.hs)."""
        image_path = f"temp/multiplProcess/input/picture_sharpen_{index}.png"
        output_path = f"temp/multiplProcess/output/picture_sharpen_{index}.png"
        
        # Создаем временную папку, если её нет
        os.makedirs("temp", exist_ok=True)
        
        # Сохраняем изображение для обработки
        image.save(image_path)
        
        try:
            # Вызываем Haskell-программу Sharpen.hs
            subprocess.run([
                "./backend_Erbol/Function/ImageEffects/Sharpen/sharpen",
                image_path,
                output_path
            ], check=True)
            
            # Загружаем обработанное изображение
            with Image.open(output_path) as img:
                filtered_image = img.copy()
            
            # Удаляем временные файлы
            self.func_main.delete_file(image_path)
            
            return filtered_image
        except subprocess.CalledProcessError as e:
            print(f"Ошибка при вызове Haskell-фильтра (Sharpen): {e}")
            return image  # Возвращаем оригинальное изображение в случае ошибки


    def _apply_brightness_filter(self, image, index, factor):
        """Применяет фильтр яркости к изображению, используя Haskell (Brightness.hs)."""
        image_path = f"temp/multiplProcess/input/picture_brightness_{index}.png"
        output_path = f"temp/multiplProcess/output/picture_brightness_{index}.png"
        
        # Создаем временную папку, если её нет
        os.makedirs("temp", exist_ok=True)
        
        # Сохраняем изображение для обработки
        image.save(image_path)
        
        try:
            # Вызываем Haskell-программу Brightness.hs
            subprocess.run([
                "./backend_Erbol/Function/ImageEffects/Brightness/brightness",
                image_path,
                output_path,
                str(factor)
            ], check=True)
            
            # Загружаем обработанное изображение
            with Image.open(output_path) as img:
                filtered_image = img.copy()
            
            # Удаляем временные файлы            
            return filtered_image
        except subprocess.CalledProcessError as e:
            print(f"Ошибка при вызове Haskell-фильтра (Brightness): {e}")
            return image  # Возвращаем оригинальное изображение в случае ошибки
        
    
    def _apply_scale_filter(self, image, index, scale_factor):
        """Применяет фильтр масштабирования к изображению, используя Haskell (Scale.hs)."""
        image_path = f"temp/multiplProcess/input/picture_scale_{index}.png"
        output_path = f"temp/multiplProcess/output/picture_scale_{index}.png"
        
        # Создаем временную папку, если её нет
        os.makedirs("temp", exist_ok=True)
        
        # Сохраняем изображение для обработки
        image.save(image_path)
        
        try:
            # Вызываем Haskell-программу Scale.hs
            subprocess.run([
                "./backend_Erbol/Function/ImageTransform/Scale/scale",
                image_path,
                output_path,
                str(scale_factor)
            ], check=True)
            
            # Загружаем обработанное изображение
            with Image.open(output_path) as img:
                filtered_image = img.copy()
                        
            return filtered_image
        except subprocess.CalledProcessError as e:
            print(f"Ошибка при вызове Haskell-фильтра (Scale): {e}")
            return image  #
        

    def _apply_pixelate_filter(self, image, index, factor):
        """Применяет фильтр мозаики к изображению, используя Haskell (Pixelate.hs)."""
        image_path = f"temp/multiplProcess/input/picture_pixelate_{index}.png"
        output_path = f"temp/multiplProcess/output/picture_pixelate_{index}.png"
        
        # Создаем временную папку, если её нет
        os.makedirs("temp", exist_ok=True)
        
        # Сохраняем изображение для обработки
        image.save(image_path)
        
        try:
            # Вызываем Haskell-программу Pixelate.hs
            subprocess.run([
                "./backend_Erbol/Function/ImageEffects/Pixelate/pixelate",
                image_path,
                output_path,
                str(factor)
            ], check=True)
            
            # Загружаем обработанное изображение
            with Image.open(output_path) as img:
                filtered_image = img.copy()
                        
            return filtered_image
        except subprocess.CalledProcessError as e:
            print(f"Ошибка при вызове Haskell-фильтра (Pixelate): {e}")
            return image  # Возвращаем оригинальное изображение в случае ошибки
        
    def _apply_contrast_filter(self, image, index, factor):
        """Применяет фильтр контрастности к изображению, используя Haskell (Contrast.hs)."""
        image_path = f"temp/multiplProcess/input/picture_contrast_{index}.png"
        output_path = f"temp/multiplProcess/output/picture_contrast_{index}.png"
        
        # Создаем временную папку, если её нет
        os.makedirs("temp", exist_ok=True)
        
        # Сохраняем изображение для обработки
        image.save(image_path)
        
        try:
            # Вызываем Haskell-программу Contrast.hs
            subprocess.run([
                "./backend_Erbol/Function/ImageProcessing/Contrast/contrast",
                image_path,
                output_path,
                str(factor)
            ], check=True)
            
            # Загружаем обработанное изображение
            with Image.open(output_path) as img:
                filtered_image = img.copy()
            
            return filtered_image
        except subprocess.CalledProcessError as e:
            print(f"Ошибка при вызове Haskell-фильтра (Contrast): {e}")
            return image  

    def _apply_rotation_filter(self, image, index, factor):
        """Применяет фильтр поворота к изображению, используя Haskell (Rotation.hs)."""
        image_path = f"temp/multiplProcess/input/picture_rotation_{index}.png"
        output_path = f"temp/multiplProcess/output/picture_rotation_{index}.png"
        
        # Создаем временную папку, если её нет
        os.makedirs("temp", exist_ok=True)
        
        # Сохраняем изображение для обработки
        image.save(image_path)
        
        try:
            # Вызываем Haskell-программу Rotation.hs
            subprocess.run([
                "./backend_Erbol/Function/ImageTransform/Rotation/rotation",
                image_path,
                output_path,
                str(factor)
            ], check=True)
            
            # Загружаем обработанное изображение
            with Image.open(output_path) as img:
                filtered_image = img.copy()
            
            
            return filtered_image
        except subprocess.CalledProcessError as e:
            print(f"Ошибка при вызове Haskell-фильтра (Rotation): {e}")
            return image  # Возвращаем оригинальное изображение в случае ошибки