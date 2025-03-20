from tkinter import filedialog, messagebox
from PIL import Image, ImageTk, ImageEnhance, ImageOps
import subprocess
import os
from function_main_helper import Helper_Function
from common import ImageProcces_and_TopMenu
import customtkinter as ctk

class ImageProcces_and_TopMenu:
    def __init__(self, window):
        self.window = window
        self.layers = []
        self.images = []
        self.current_layer = None
        self.buttons_created = False
        self.mult_process_on = False
        self.func_helper = Helper_Function(self)

    # 💾 Сохранение изображения
    def save_image(self):
        if self.window.edited_image:
            file_path = filedialog.asksaveasfilename(defaultextension=".png", filetypes=[("PNG files", "*.png")])
            if file_path:
                self.window.edited_image.save(file_path)
                messagebox.showinfo("Сохранено", "Изображение успешно сохранено!")
        else:
            messagebox.showwarning("Ошибка", "Сначала откройте изображение!")
            
    # 📂 Открытие изображения
    def open_image(self):
        """Открывает изображение через диалоговое окно и добавляет его как слой."""
        file_path = filedialog.askopenfilename(filetypes=[("Image files", "*.jpg;*.png;*.jpeg")])
        if file_path:
            self.window.image_path = file_path
            self.window.image = Image.open(file_path)
            self.window.edited_image = self.window.image.copy()

            new_width = int(self.window.edited_image.width * 0.6) 
            new_height = int(self.window.edited_image.height * 0.6)  

            # Изменяем размер изображения
            self.window.edited_image = self.window.edited_image.resize((new_width, new_height), Image.Resampling.LANCZOS)

            self.add_layer()
            self.display_image(self.window.edited_image)
        if self.window.image_path:
            self.convert_to_png(self.window.image_path)
        
        self.adjust_brightness(0.7, 1)
        self.save_difference()
        self.current_layer["layer_index"] = len(self.layers)

    # Функция конвертации расширения изображения в .png
    def convert_to_png(self, input_path, output_path=None):
        """Преобразует изображение в формат PNG."""
        try:
            # Открываем изображение напрямую (не через open_image)
            with Image.open(input_path) as img:
                # Если выходной путь не задан, используем то же имя с расширением .png
                if not output_path:
                    base = os.path.splitext(input_path)[0]
                    output_path = f"{base}.png"
                
                # Конвертация в RGB для совместимости
                if img.mode in ("RGBA", "P"):
                    img = img.convert("RGB")
                
                img.save(output_path, format="PNG")
                print(f"✅ Изображение успешно сохранено в {output_path}")
                return output_path

        except Exception as e:
            print(f"❌ Ошибка при конвертации: {e}")
            return None

    # Функция добавления слоя
    def add_layer(self):
        """Создает новый слой и делает его активным"""
        new_layer = {
            "name": f"Слой {len(self.layers) + 1}",
            "image": self.window.edited_image.copy() if self.window.edited_image else None,
            "button": None,
            "brightness_value": 1.0,  # Значение по умолчанию
            "copy": self.window.edited_image.copy() if self.window.edited_image else None,  # Храним оригинал слоя
            "scale_value": 1.0,
            "bright_flag": False,
            "scale_flag": False,
            "scale_frame": False,
            "bright_frame": False,
            "br_flag_saved": True,
            "pixel_value": 1,
            "pixel_flag": False,
            "pixel_frame": False,
            "pixel_flag_saved": True,
            "contrast_value": 1,
            "contrast_flag": False,
            "contrast_frame": False,
            "rotation_value": 0,
            "rotation_frame": False,
            "rotation_flag": False,
            "position_x": 0,
            "position_y": 0            
        }

        new_layer["button"] = ctk.CTkButton(
            self.window.layers_list, text=new_layer["name"], command=lambda l=new_layer: self.select_layer(l)
        )
        new_layer["button"].pack(fill="x", pady=2, padx=5)

        self.layers.append(new_layer)
        self.select_layer(new_layer)  # Делаем новый слой активным
    
    
    # Функция выбора слоя
    def select_layer(self, layer):
        """Выбирает слой и загружает его на canvas."""
        self.current_layer = layer  # Запоминаем текущий слой
        # Меняем цвет кнопок для индикации активного слоя
        for l in self.layers:
            l["button"].configure(fg_color="blue")
        layer["button"].configure(fg_color="red")

        # Отображаем изображение, привязанное к этому слою
        if layer["image"]:
            self.window.edited_image = self.current_layer["image"]   # Загружаем изображение слоя в обработку
            self.display_image(layer["image"])
        
        if self.current_layer["bright_flag"] == True:
            self.window.brightness_frame.destroy()
            self.window.brightness_frame = None
            self.current_layer["bright_flag"] = False
            self.current_layer["bright_frame"] = False

        if (self.current_layer["bright_flag"] == False) and (self.current_layer["bright_frame"] == True):
            self.current_layer["bright_flag"] = True
            self.create_brightness_slider() 

        if self.current_layer["scale_flag"] == True:
            self.window.scale_frame.destroy()
            self.window.scale_frame = None
            self.current_layer["scale_flag"] = False
            self.current_layer["scale_frame"] = False

        if (self.current_layer["scale_flag"] == False) and (self.current_layer["scale_frame"] == True):
            self.current_layer["scale_flag"] = True
            self._create_scale_slider_frame()


    def remove_layer(self):
        """Удаляет текущий слой."""
        answer = messagebox.askyesno("Warning", "Do you want to save picture?")
        if answer:
            self.save_image()

        if not hasattr(self, "layers") or not self.layers:
            print("Ошибка: Нет слоев для удаления.")
            return

        if not hasattr(self, "current_layer"):
            print("Ошибка: Нет активного слоя.")
            return

        # Удаляем текущий слой из списка
        layer_to_remove = self.current_layer
        self.layers.remove(layer_to_remove)

        # Удаляем кнопку слоя из интерфейса
        layer_to_remove["button"].destroy()
        # Если слои остались, выбираем новый текущий слой
        if self.layers:
            # Выбираем предыдущий слой (или первый, если удаленный был первым)
            new_index = max(0, self.layers.index(self.current_layer) - 1)
            self.current_layer = self.layers[new_index]
            self.select_layer(self.current_layer)  # Делаем новый слой активным
        else:
            # Если слоев не осталось, очищаем текущий слой и холст
            self.current_layer = None
            self.window.edited_image = None
            self.window.canvas.delete("all")  # Очищаем холст
            print("Все слои удалены.")


    # Функция отображения изображения на canvas
    def display_image(self, image):
        """Отображает изображение на canvas"""
        if image:
            photo = ImageTk.PhotoImage(image)
            self.window.canvas.delete("all")
            self.window.canvas.create_image(600, 400, image=photo, anchor="center")
            self.window.canvas.image = photo  


     # 🔹 Меню (File)
    def top_menu1(self):
        options = ["📂 Open", "💾 Save", "🚪 Exit"]
        self.menu_combobox1 = ctk.CTkComboBox(self.window.top_frame, values=options, command=self.menu_action1)
        self.menu_combobox1.pack(side="left", padx=2)
        self.menu_combobox1.set("File")

    def menu_action1(self, choice):
        """Обрабатывает выбор из выпадающего списка."""
        if choice == "📂 Open":
            self.open_image()
        elif choice == "💾 Save":
            self.save_image()
        elif choice == "🚪 Exit":
            self.button_active()

    # 🔹 Меню (General)
    def top_menu3(self):
        options = ["Documentation", "About us...", "Future"]
        self.menu_combobox3 = ctk.CTkComboBox(self.window.top_frame, values=options, command=self.menu_action3)
        self.menu_combobox3.pack(side="left", padx=2)
        self.menu_combobox3.set("General")

    def menu_action3(self, choice):
        """Обрабатывает выбор из выпадающего списка."""
        if choice == "Documentation":
            messagebox.showinfo("Документация", "Информация о документации...")
        elif choice == "About us...":
            messagebox.showinfo("О нас", "Разработчик: ...")
        elif choice == "Future":
            messagebox.showinfo("Будущее", "Будущие функции проекта.")

    def button_active(self):
        """Закрывает приложение с предупреждением."""
        answer = messagebox.askokcancel("Warning", "Do you want to quit?")
        if answer:
            self.window.root.destroy()
            #Очищаем папки от временных файлов
            folderinput_path = "temp/inputPath"
            for file_name in os.listdir(folderinput_path):
                file_path = os.path.join(folderinput_path, file_name)
                self.delete_file(file_path)

            folderoutput_path = "temp/outputPath"
            for file_name in os.listdir(folderoutput_path):
                file_path = os.path.join(folderoutput_path, file_name)
                self.delete_file(file_path)

     # 🔹 Меню (Color corrections)
    def color_corrections(self):
        self.buttons_container = ctk.CTkFrame(self.window.tools_frame, fg_color="#535353")
        self.buttons_container.pack(fill="both", expand=True, padx=5, pady=5)
        
        options = ["Negative", "Grayscale", "Solarize", "Sepia", "Contrast"]
        self.menu_combobox1 = ctk.CTkComboBox(self.buttons_container, values=options, font=("TimesNewRoman", 15, "bold"), command=self.menu_color_corrections)
        self.menu_combobox1.pack(fill="both", padx=2)
        self.menu_combobox1.set("Color corrections")

    def menu_color_corrections(self, choice):
        """Обрабатывает выбор из выпадающего списка."""
        if choice == "Negative":
            self.invert_colors()
        elif choice == "Grayscale":
            self.to_black_white()
        elif choice == "Solarize":
            self.to_solarize()
        elif choice == "Sepia":
            self.to_sepia()
        elif choice == "Contrast":
            self.create_contrast_slider()
    
     # 🔹 Меню (Image Transforms)
    def geometry_transform(self):
        self.buttons_container = ctk.CTkFrame(self.window.tools_frame, fg_color="#535353")
        self.buttons_container.pack(fill="both", expand=True, padx=5, pady=5)
        
        options = ["Rotation", "Scaling", "Horizontal flip", "Vertical flip"]
        self.menu_combobox1 = ctk.CTkComboBox(self.buttons_container, values=options, font=("TimesNewRoman", 15, "bold"), command=self.menu_transforms)
        self.menu_combobox1.pack(fill="both", padx=2)
        self.menu_combobox1.set("Geometry Transforms")

    def menu_transforms(self, choice):
        """Обрабатывает выбор из выпадающего списка."""
        if choice == "Rotation":
            self.create_rotation_slider()
        elif choice == "Scaling":
            self.create_scale_slider()
        elif choice == "Horizontal flip":
            self.hor_flip()
        elif choice == "Vertical flip":
            self.vert_flip() 
        
    # 🔹 Меню (Effects)
    def image_effects(self):
        self.buttons_container = ctk.CTkFrame(self.window.tools_frame, fg_color="#535353")
        self.buttons_container.pack(fill="both", expand=True, padx=5, pady=5)
        
        options = ["Brightness", "Pixelate", "Sharpen"]
        self.menu_combobox1 = ctk.CTkComboBox(self.buttons_container, values=options, font=("TimesNewRoman", 15, "bold"), command=self.menu_effects)
        self.menu_combobox1.pack(fill="both", padx=2)
        self.menu_combobox1.set("Image Effects")

    def menu_effects(self, choice):
        """Обрабатывает выбор из выпадающего списка."""
        if choice == "Brightness":
            self.create_brightness_slider()
        elif choice == "Pixelate":
            self.create_pixelate_slider()
        elif choice == "Sharpen":
            self.to_sharpen()
        
    # Функция для сохранение изменений
    def save_difference(self):
        self.current_layer["copy"] = self.current_layer["image"]
        self.display_image(self.current_layer["image"])
        self.window.negative_flag = False
        self.window.sepia_flag = False
        self.window.grayscale_flag = False
        if self.window.brightness_frame:
            self.window.brightness_frame.destroy()
        if self.window.scale_frame:
            self.window.scale_frame.destroy()
        if self.window.pixel_frame:
            self.window.pixel_frame.destroy()
        if self.window.contrast_frame:
            self.window.contrast_frame.destroy()
        if self.window.rotation_frame:
            self.window.rotation_frame.destroy()
        self.window.scale_frame = None
        self.window.scale_flag = False
        self.window.pixel_frame = None
        self.window.pixel_flag = False
        self.window.contrast_frame = None
        self.window.contrast_flag = False
        self.window.rotation_frame = None
        self.window.rotation_flag = False
        self.window.brightness_frame = None
        self.window.brightness_flag = False
        self.current_layer["bright_flag"] = False
        self.current_layer["bright_frame"] = False
        self.current_layer["scale_flag"] = False
        self.current_layer["scale_frame"] = False
        self.current_layer["pixel_frame"] = False
        self.current_layer["pixel_flag"] = False
        self.current_layer["contrast_frame"] = False
        self.current_layer["contrast_flag"] = False
        self.current_layer["rotation_frame"] = False
        self.current_layer["rotation_flag"] = False
        
        
        self.current_layer["br_flag_saved"] = True

    # Функция удаления временного файла
    def delete_file(self, file_path):
        os.remove(file_path)

# Функции связанные с Haskell
    # Функция Negative
    def invert_colors(self):
        """Инвертирует цвета изображения, используя Haskell (Negative.hs)."""
        if self.mult_process_on:
            # Обработка множества изображений
            for i in range(len(self.mult_images)):
                self.mult_images[i] = self.func_helper._apply_invert_filter(self.mult_images[i], i)
            
            # Отображаем первое изображение
            self.display_mult_image(self.mult_images[self.current_image_index])
        else:
            if self.current_layer and self.current_layer["image"]:
                image_path = "HaskProj/temp/inputPath/input_invert.png"
                output_path = "HaskProj/temp/outputPath/output_invert.png"
                # Создаем временную папку, если её нет
                os.makedirs("HaskProj", exist_ok=True)
                # Сохраняем текущее изображение для обработки
                self.current_layer["copy"].save(image_path)
                if self.window.negative_flag:
                    # Возвращаем оригинал изображения
                    self.current_layer["image"] = self.current_layer["copy"]
                    self.window.negative_flag = False
                else:
                    try:
                        # Вызываем Haskell-программу Negative.hs с помощью runhaskell
                        subprocess.run([
                            "./backend_Erbol/Function/ImageProcessing/Negative/negative",
                            image_path,
                            output_path
                        ], check=True)
                        
                        # Загружаем обработанное изображение
                        with Image.open(output_path) as img:
                            filtered_image = img.copy()  # Копируем изображение в память
                        
                        # Удаляем временный файл
                        self.delete_file(output_path)
                        self.delete_file(image_path)

                        # Обновляем изображение в слое
                        self.current_layer["image"] = filtered_image
                        self.window.edited_image = filtered_image
                        self.window.negative_flag = True
                    except subprocess.CalledProcessError as e:
                        print(f"Ошибка при вызове Haskell-фильтра: {e}")
                # Обновляем отображение
                self.display_image(self.current_layer["image"])

    # Функция Ч\Б
    def to_black_white(self):
        """Переключает изображение между цветным и черно-белым, используя Haskell (Grayscale.hs)."""
        if self.mult_process_on:
            # Обработка множества изображений
            for i in range(len(self.mult_images)):
                self.mult_images[i] = self.func_helper._apply_grayscale_filter(self.mult_images[i], i)
            
            # Отображаем первое изображение
            self.display_mult_image(self.mult_images[self.current_image_index])
        else:
            if self.current_layer and self.current_layer["image"]:
                image_path = "HaskProj/temp/inputPath/input_bw.png"
                output_path = "HaskProj/temp/outputPath/output_bw.png"
                os.makedirs("HaskProj", exist_ok=True)
                self.current_layer["copy"].save(image_path)
                if self.window.grayscale_flag:
                    self.current_layer["image"] = self.current_layer["copy"]
                    self.window.grayscale_flag = False
                else:
                    try:
                        subprocess.run([
                            "./backend_Erbol/Function/ImageProcessing/Grayscale/grayscale",
                            image_path,
                            output_path
                        ], check=True)

                        with Image.open(output_path) as img:
                            filtered_image = img.copy()  # Копируем изображение в память
                        
                        # Удаляем временный файл
                        self.delete_file(output_path)
                        self.delete_file(image_path)

                        # Обновляем изображение в слое
                        self.current_layer["image"] = filtered_image
                        self.window.edited_image = filtered_image
                        self.window.grayscale_flag = True
                    except subprocess.CalledProcessError as e:
                        print(f"Ошибка при вызове Haskell-фильтра (Grayscale): {e}")
                self.display_image(self.current_layer["image"])

    # Функция Solarize
    def to_solarize(self):
        """Переключает изображение на сепию, используя Haskell (Solarize.hs)."""
        if self.mult_process_on:
            # Обработка множества изображений
            for i in range(len(self.mult_images)):
                self.mult_images[i] = self.func_helper._apply_solarize_filter(self.mult_images[i], i)
            
            # Отображаем первое изображение
            self.display_mult_image(self.mult_images[self.current_image_index])
        else:
            if self.current_layer and self.current_layer["image"]:
                image_path = "HaskProj/temp/inputPath/input_solarize.png"
                output_path = "HaskProj/temp/outputPath/output_solarize.png"
                os.makedirs("HaskProj", exist_ok=True)
                self.current_layer["copy"].save(image_path)
                if self.window.solarize_flag:
                    self.current_layer["image"] = self.current_layer["copy"]
                    self.window.solarize_flag = False
                    
                else:
                    try:
                        subprocess.run([
                            "./backend_Erbol/Function/ImageProcessing/Solarize/solarize",
                            image_path,
                            output_path
                        ], check=True)
                        with Image.open(output_path) as img:
                            filtered_image = img.copy()  # Копируем изображение в память
                        
                        # Удаляем временный файл
                        self.delete_file(output_path)
                        self.delete_file(image_path)

                        # Обновляем изображение в слое
                        self.current_layer["image"] = filtered_image
                        self.window.edited_image = filtered_image
                        self.window.solarize_flag = True
                    except subprocess.CalledProcessError as e:
                        print(f"Ошибка при вызове Haskell-фильтра (Solarize): {e}")
                self.display_image(self.current_layer["image"])

    # Функция Sepia
    def to_sepia(self):
        """Переключает изображение на сепию, используя Haskell (Solarize.hs)."""
        if self.mult_process_on:
            # Обработка множества изображений
            for i in range(len(self.mult_images)):
                self.mult_images[i] = self.func_helper._apply_sepia_filter(self.mult_images[i], i)
            
            # Отображаем первое изображение
            self.display_mult_image(self.mult_images[self.current_image_index])
        else:
            if self.current_layer and self.current_layer["image"]:
                image_path = "HaskProj/temp/inputPath/input_sepia.png"
                output_path = "HaskProj/temp/outputPath/output_sepia.png"
                os.makedirs("HaskProj", exist_ok=True)
                self.current_layer["copy"].save(image_path)
                if self.window.sepia_flag:
                    self.current_layer["image"] = self.current_layer["copy"]
                    self.window.sepia_flag = False
                    
                else:
                    try:
                        subprocess.run([
                            "./backend_Erbol/Function/ImageProcessing/Sepia/sepia",
                            image_path,
                            output_path
                        ], check=True)
                        with Image.open(output_path) as img:
                            filtered_image = img.copy()  # Копируем изображение в память
                        
                        # Удаляем временный файл
                        self.delete_file(output_path)
                        self.delete_file(image_path)

                        # Обновляем изображение в слое
                        self.current_layer["image"] = filtered_image
                        self.window.edited_image = filtered_image
                        self.window.sepia_flag = True
                    except subprocess.CalledProcessError as e:
                        print(f"Ошибка при вызове Haskell-фильтра (Solarize): {e}")
                self.display_image(self.current_layer["image"])

    # Функция изменения яркости
    def adjust_brightness(self, value, starter):
        """Регулирует яркость, отправляя коэффициент яркости в Haskell."""
        if self.mult_process_on:
            # Обработка множества изображений
            factor = float(value)
            for i in range(len(self.mult_images)):
                self.mult_images[i] = self.func_helper._apply_brightness_filter(self.mult_images[i], i, factor)
            
            # Отображаем первое изображение
            self.display_mult_image(self.mult_images[self.current_image_index])
        else:
            if self.current_layer and self.current_layer["image"]:
                factor = float(value)
                if abs(factor - self.current_layer["brightness_value"]) < 0.01:
                    return  # Изменение слишком мало для обработки
                self.current_layer["brightness_value"] = factor

                image_path = "HaskProj/temp/inputPath/input_brightness.png"
                output_path = "HaskProj/temp/outputPath/output_brightness.png"
                os.makedirs("HaskProj", exist_ok=True)
                self.current_layer["copy"].save(image_path)

                try:
                    subprocess.run([
                        "./backend_Erbol/Function/ImageEffects/Brightness/brightness",
                        image_path,
                        output_path,
                        str(factor)
                    ], check=True)

                    with Image.open(output_path) as img:
                        filtered_image = img.copy()

                    # Удаляем временные файлы
                    self.delete_file(output_path)
                    self.delete_file(image_path)

                    # Обновляем изображение в слое
                    self.current_layer["image"] = filtered_image
                    self.window.edited_image = filtered_image
                    if starter == 1:
                        self.current_layer["br_flag_saved"] = True
                    else:
                        self.current_layer["br_flag_saved"] = False
                    self.display_image(filtered_image)
                except subprocess.CalledProcessError as e:
                    print(f"Ошибка при вызове Haskell-фильтра (Brightness): {e}")
            else:
                if self.window.brightness_frame:
                    self.window.brightness_frame.destroy()
                    self.window.brightness_frame = None
                    self.current_layer["bright_frame"] = False
                    self.current_layer["bright_flag"] = False

    # Функция созднания ползунка для яркости
    def create_brightness_slider(self):
        """Создаёт окно с ползунком для яркости и применяет фильтр после окончания движения."""
        def on_slider_release(event):
            try:
                value = float(brightness_slider.get())
                if 0.0 <= value <= 2.0:  # Проверяем допустимый диапазон
                    self.adjust_brightness(value, 0)
                else:
                    print("Ошибка: Значение яркости вне допустимого диапазона.")
            except ValueError:
                print("Ошибка: Неверное значение яркости.")
        
        if self.mult_process_on:
            try: # Создаем новый фрейм для ползунка яркости
                self.window.brightness_frame = ctk.CTkFrame(self.window.right_panel, height=300, width=300)
                self.window.brightness_frame.pack(pady=10)

                # Создаем ползунок яркости
                brightness_slider = ctk.CTkSlider(
                    master=self.window.brightness_frame,
                    from_=0.0,
                    to=1.0,
                    command=lambda value: None  # Не применять фильтр во время перемещения
                )
                brightness_slider.pack(pady=10)
                
                brightness_slider.set(1.0)
                # Применять фильтр только после отпускания бегунка
                brightness_slider.bind("<ButtonRelease-1>", on_slider_release)
                
            except Exception as e:
                print(f"Ошибка при создании ползунка яркости: {e}")



        if self.current_layer["bright_flag"]:
            # Если окно ползунка уже открыто, закрываем его
            if self.current_layer["bright_frame"]:
                self.window.brightness_frame.destroy()
                self.window.brightness_frame = None
                self.current_layer["bright_flag"] = False
                self.current_layer["bright_frame"] = False
        else:
            try:
                # Создаем новый фрейм для ползунка яркости
                self.window.brightness_frame = ctk.CTkFrame(self.window.right_panel, height=300, width=300)
                self.window.brightness_frame.pack(pady=10)

                # Создаем ползунок яркости
                brightness_slider = ctk.CTkSlider(
                    master=self.window.brightness_frame,
                    from_=0.0,
                    to=1.0,
                    command=lambda value: None  # Не применять фильтр во время перемещения
                )
                brightness_slider.pack(pady=10)
                if (self.current_layer["br_flag_saved"] == True):
                    brightness_slider.set(1.0)
                else:
                    brightness_slider.set(self.current_layer["brightness_value"])  # Устанавливаем значение по умолчанию

                # Применять фильтр только после отпускания бегунка
                brightness_slider.bind("<ButtonRelease-1>", on_slider_release)

                # Переключаем флаг состояния окна ползунка
                self.current_layer["bright_flag"] = True
                self.current_layer["bright_frame"] = True

            except Exception as e:
                print(f"Ошибка при создании ползунка яркости: {e}")

    def adjust_scale(self, value):
        """Регулирует масштаб изображения, отправляя коэффициент масштабирования в Haskell."""
        if self.mult_process_on:
            # Обработка множества изображений
            scale_factor = float(value)
            for i in range(len(self.mult_images)):
                self.mult_images[i] = self.func_helper._apply_scale_filter(self.mult_images[i], i, scale_factor)
            
            # Отображаем первое изображение
            self.display_mult_image(self.mult_images[self.current_image_index])
        else:
            # Обработка одного изображения
            if self.current_layer and self.current_layer["image"]:
                scale_factor = float(value)
                if abs(scale_factor - self.current_layer["scale_value"]) < 0.01:
                    return  # Изменение слишком мало для обработки
                self.current_layer["scale_value"] = scale_factor

                # Применяем фильтр
                self.current_layer["image"] = self.func_helper._apply_scale_filter(self.current_layer["copy"], 0, scale_factor)
                
                # Обновляем отображение
                self.display_image(self.current_layer["image"])
            else:
                # Закрываем фрейм масштабирования, если он открыт
                if self.window.scale_frame:
                    self.window.scale_frame.destroy()
                    self.window.scale_frame = None
                    self.current_layer["scale_frame"] = False
                    self.current_layer["scale_flag"] = False

    def create_scale_slider(self):
        """Создаёт окно с ползунком для масштабирования и применяет фильтр после окончания движения."""
        def on_slider_release(event):
            try:
                value = float(scale_slider.get())
                if 0.1 <= value <= 4.0:  # Проверяем допустимый диапазон
                    self.adjust_scale(value)
                else:
                    print("Ошибка: Значение масштаба вне допустимого диапазона.")
            except ValueError:
                print("Ошибка: Неверное значение масштаба.")

        if self.mult_process_on:
            try:
                # Создаем новый фрейм для ползунка масштабирования
                self.window.scale_frame = ctk.CTkFrame(self.window.right_panel, height=300, width=300)
                self.window.scale_frame.pack(pady=10)

                # Создаем ползунок масштабирования
                scale_slider = ctk.CTkSlider(
                    master=self.window.scale_frame,
                    from_=0.1,
                    to=4.0,
                    number_of_steps=40,
                    command=lambda value: None  # Не применять фильтр во время перемещения
                )
                scale_slider.pack(pady=10)
                scale_slider.set(1.0)  # Устанавливаем значение по умолчанию

                # Применять фильтр только после отпускания бегунка
                scale_slider.bind("<ButtonRelease-1>", on_slider_release)

                # Текстовая метка
                ctk.CTkLabel(
                    self.window.scale_frame,
                    text="Масштаб (0.1x - 4.0x)"
                ).pack(pady=2)

            except Exception as e:
                print(f"Ошибка при создании ползунка масштабирования: {e}")

        if self.current_layer["scale_flag"]:
            # Если окно ползунка уже открыто, закрываем его
            if self.current_layer["scale_frame"]:
                self.window.scale_frame.destroy()
                self.window.scale_frame = None
                self.current_layer["scale_flag"] = False
                self.current_layer["scale_frame"] = False
        else:
            try:
                # Создаем новый фрейм для ползунка масштабирования
                self.window.scale_frame = ctk.CTkFrame(self.window.right_panel, height=300, width=300)
                self.window.scale_frame.pack(pady=10)

                # Создаем ползунок масштабирования
                scale_slider = ctk.CTkSlider(
                    master=self.window.scale_frame,
                    from_=0.1,
                    to=4.0,
                    number_of_steps=40,
                    command=lambda value: None  # Не применять фильтр во время перемещения
                )
                scale_slider.pack(pady=10)
                if self.current_layer["scale_value"]:
                    scale_slider.set(self.current_layer["scale_value"])  # Устанавливаем значение по умолчанию
                else:
                    scale_slider.set(1.0)

                # Применять фильтр только после отпускания бегунка
                scale_slider.bind("<ButtonRelease-1>", on_slider_release)

                # Текстовая метка
                ctk.CTkLabel(
                    self.window.scale_frame,
                    text="Масштаб (0.1x - 4.0x)"
                ).pack(pady=2)

                # Переключаем флаг состояния окна ползунка
                self.current_layer["scale_flag"] = True
                self.current_layer["scale_frame"] = True

            except Exception as e:
                print(f"Ошибка при создании ползунка масштабирования: {e}")
                
    def adjust_pixelate(self, value):
        """Регулирует мозаику, отправляя коэффициент в Haskell."""
        if self.mult_process_on:
            # Обработка множества изображений
            factor = round(float(value))
            for i in range(len(self.mult_images)):
                self.mult_images[i] = self.func_helper._apply_pixelate_filter(self.mult_images[i], i, factor)
            
            # Отображаем первое изображение
            self.display_mult_image(self.mult_images[self.current_image_index])
        else:
            # Обработка одного изображения
            if self.current_layer and self.current_layer["image"]:
                try:
                    # Округляем значение до целого числа
                    factor = round(float(value))
                    
                    # Проверяем допустимость значения
                    if factor < 1:
                        raise ValueError("Размер блока должен быть ≥ 1")
                        
                    # Проверяем минимальное изменение
                    if abs(factor - self.current_layer.get("pixel_value", 0)) < 1:
                        return
                        
                    self.current_layer["pixel_value"] = factor

                    # Применяем фильтр
                    self.current_layer["image"] = self.func_helper._apply_pixelate_filter(self.current_layer["copy"], 0, factor)
                    
                    # Обновляем отображение
                    self.display_image(self.current_layer["image"])
                except Exception as e:
                    print(f"Ошибка в adjust_pixelate: {str(e)}")
            else:
                # Логика закрытия окна ползунка
                if getattr(self.window, "pixel_frame", None):
                    self.window.pixel_frame.destroy()
                    self.window.pixel_frame = None
                    self.current_layer["pixel_flag"] = False
                    self.current_layer["pixel_frame"] = False

    def create_pixelate_slider(self):
        """Создаёт окно с ползунком для мозаики."""
        def on_slider_release(event):
            try:
                # Округляем значение до целого
                value = round(float(pixel_slider.get()))
                if 1 <= value <= 15:
                    self.adjust_pixelate(value)
                else:
                    print("Значение должно быть между 1 и 15")
            except ValueError:
                print("Неверное значение")

        if self.mult_process_on:
            try:
                # Создаем новый фрейм для ползунка мозаики
                self.window.pixel_frame = ctk.CTkFrame(
                    self.window.right_panel,
                    height=200,
                    width=300
                )
                self.window.pixel_frame.pack(pady=10)

                # Создаем ползунок мозаики
                pixel_slider = ctk.CTkSlider(
                    master=self.window.pixel_frame,
                    from_=1,
                    to=15,
                    number_of_steps=15  # Только целые числа
                )
                pixel_slider.pack(pady=10)
                pixel_slider.set(1)  # Устанавливаем значение по умолчанию

                # Текстовая метка
                ctk.CTkLabel(
                    self.window.pixel_frame,
                    text="Коэффициент (1x - 15x)"
                ).pack(pady=2)

                # Применять фильтр только после отпускания бегунка
                pixel_slider.bind("<ButtonRelease-1>", on_slider_release)

            except Exception as e:
                print(f"Ошибка при создании ползунка мозаики: {e}")

        if self.current_layer.get("pixel_flag", False):
            # Если окно ползунка уже открыто, закрываем его
            if getattr(self.window, "pixel_frame", None):
                self.window.pixel_frame.destroy()
                self.window.pixel_frame = None
                self.current_layer["pixel_flag"] = False
        else:
            try:
                # Создаем новый фрейм для ползунка мозаики
                self.window.pixel_frame = ctk.CTkFrame(
                    self.window.right_panel,
                    height=200,
                    width=300
                )
                self.window.pixel_frame.pack(pady=10)

                # Создаем ползунок мозаики
                pixel_slider = ctk.CTkSlider(
                    master=self.window.pixel_frame,
                    from_=1,
                    to=15,
                    number_of_steps=15  # Только целые числа
                )
                pixel_slider.pack(pady=10)
                if self.current_layer.get("pixel_value"):
                    pixel_slider.set(self.current_layer["pixel_value"])  # Устанавливаем значение по умолчанию
                else:
                    pixel_slider.set(1)

                # Текстовая метка
                ctk.CTkLabel(
                    self.window.pixel_frame,
                    text="Коэффициент (1x - 15x)"
                ).pack(pady=2)

                # Применять фильтр только после отпускания бегунка
                pixel_slider.bind("<ButtonRelease-1>", on_slider_release)

                # Переключаем флаг состояния окна ползунка
                self.current_layer["pixel_flag"] = True
                self.current_layer["pixel_frame"] = True

            except Exception as e:
                print(f"Ошибка при создании ползунка мозаики: {e}")

    def create_contrast_slider(self):
        """Создаёт окно с ползунком для контрастности."""
        def on_slider_release(event):
            try:
                # Округляем значение до двух знаков после запятой
                value = round(float(contrast_slider.get()), 2)
                if 0.0 <= value <= 2.0:
                    self.adjust_contrast(value)
                else:
                    print("Значение должно быть между 0.0 и 2.0")
            except ValueError:
                print("Неверное значение")

        if self.mult_process_on:
            try:
                # Создаем новый фрейм для ползунка контрастности
                self.window.contrast_frame = ctk.CTkFrame(
                    self.window.right_panel,
                    height=200,
                    width=300
                )
                self.window.contrast_frame.pack(pady=10)

                # Создаем ползунок контрастности
                contrast_slider = ctk.CTkSlider(
                    master=self.window.contrast_frame,
                    from_=0.0,
                    to=2.0,
                    number_of_steps=20  # Шаги с шагом 0.1
                )
                contrast_slider.pack(pady=10)
                contrast_slider.set(1.0)  # Устанавливаем значение по умолчанию

                # Текстовая метка
                ctk.CTkLabel(
                    self.window.contrast_frame,
                    text="Контрастность (0.0x - 2.0x)"
                ).pack(pady=2)

                # Применять фильтр только после отпускания бегунка
                contrast_slider.bind("<ButtonRelease-1>", on_slider_release)

            except Exception as e:
                print(f"Ошибка при создании ползунка контрастности: {e}")

        if self.current_layer.get("contrast_flag", False):
            # Если окно ползунка уже открыто, закрываем его
            if getattr(self.window, "contrast_frame", None):
                self.window.contrast_frame.destroy()
                self.window.contrast_frame = None
                self.current_layer["contrast_flag"] = False
        else:
            try:
                # Создаем новый фрейм для ползунка контрастности
                self.window.contrast_frame = ctk.CTkFrame(
                    self.window.right_panel,
                    height=200,
                    width=300
                )
                self.window.contrast_frame.pack(pady=10)

                # Создаем ползунок контрастности
                contrast_slider = ctk.CTkSlider(
                    master=self.window.contrast_frame,
                    from_=0.0,
                    to=2.0,
                    number_of_steps=20  # Шаги с шагом 0.1
                )
                contrast_slider.pack(pady=10)
                if self.current_layer.get("contrast_value"):
                    contrast_slider.set(self.current_layer["contrast_value"])  # Устанавливаем значение по умолчанию
                else:
                    contrast_slider.set(1.0)

                # Текстовая метка
                ctk.CTkLabel(
                    self.window.contrast_frame,
                    text="Контрастность (0.0x - 2.0x)"
                ).pack(pady=2)

                # Применять фильтр только после отпускания бегунка
                contrast_slider.bind("<ButtonRelease-1>", on_slider_release)

                # Переключаем флаг состояния окна ползунка
                self.current_layer["contrast_flag"] = True
                self.current_layer["contrast_frame"] = True

            except Exception as e:
                print(f"Ошибка при создании ползунка контрастности: {e}")
    
    def adjust_contrast(self, value):
        """Регулирует контрастность, отправляя коэффициент в Haskell."""
        if self.mult_process_on:
            # Обработка множества изображений
            factor = round(float(value), 2)
            for i in range(len(self.mult_images)):
                self.mult_images[i] = self.func_helper._apply_contrast_filter(self.mult_images[i], i, factor)
            
            # Отображаем первое изображение
            self.display_mult_image(self.mult_images[self.current_image_index])
        else:
            # Обработка одного изображения
            if self.current_layer and self.current_layer["image"]:
                try:
                    # Округляем значение до двух знаков после запятой
                    factor = round(float(value), 2)
                    
                    # Проверяем минимальное изменение
                    if abs(factor - self.current_layer.get("contrast_value", 1.0)) < 0.01:
                        return
                        
                    self.current_layer["contrast_value"] = factor

                    # Применяем фильтр
                    self.current_layer["image"] = self.func_helper._apply_contrast_filter(self.current_layer["copy"], 0, factor)
                    
                    # Обновляем отображение
                    self.display_image(self.current_layer["image"])
                except Exception as e:
                    print(f"Ошибка в adjust_contrast: {str(e)}")
            else:
                # Логика закрытия окна ползунка
                if getattr(self.window, "contrast_frame", None):
                    self.window.contrast_frame.destroy()
                    self.window.contrast_frame = None
                self.current_layer["contrast_flag"] = False
                self.current_layer["contrast_frame"] = False

    def adjust_rotation(self, value):
        """Регулирует угол поворота изображения, отправляя коэффициент в Haskell."""
        if self.mult_process_on:
            # Обработка множества изображений
            factor = round(float(value), 2)
            for i in range(len(self.mult_images)):
                self.mult_images[i] = self.func_helper._apply_rotation_filter(self.mult_images[i], i, factor)
            
            # Отображаем первое изображение
            self.display_mult_image(self.mult_images[self.current_image_index])
        else:
            # Обработка одного изображения
            if self.current_layer and self.current_layer["image"]:
                try:
                    # Округляем значение до двух знаков после запятой
                    factor = round(float(value), 2)
                    
                    # Проверяем минимальное изменение
                    if abs(factor - self.current_layer.get("rotation_value", 0.0)) < 0.01:
                        return
                        
                    self.current_layer["rotation_value"] = factor

                    # Применяем фильтр
                    self.current_layer["image"] = self.func_helper._apply_rotation_filter(self.current_layer["copy"], 0, factor)
                    
                    # Обновляем отображение
                    self.display_image(self.current_layer["image"])
                except Exception as e:
                    print(f"Ошибка в adjust_rotation: {str(e)}")
            else:
                # Логика закрытия окна ползунка
                if getattr(self.window, "rotation_frame", None):
                    self.window.rotation_frame.destroy()
                    self.window.rotation_frame = None
                self.current_layer["rotation_flag"] = False
                self.current_layer["rotation_frame"] = False

        
    def create_rotation_slider(self):
        """Создаёт окно с ползунком для поворота."""
        def on_slider_release(event):
            try:
                # Округляем значение до двух знаков после запятой
                value = round(float(rotation_slider.get()), 2)
                if -180 <= value <= 180:
                    self.adjust_rotation(value)
                else:
                    print("Значение должно быть между -180 и 180")
            except ValueError:
                print("Неверное значение")

        if self.mult_process_on:
            try:
                # Создаем новый фрейм для ползунка поворота
                self.window.rotation_frame = ctk.CTkFrame(
                    self.window.right_panel,
                    height=200,
                    width=300
                )
                self.window.rotation_frame.pack(pady=10)

                # Создаем ползунок поворота
                rotation_slider = ctk.CTkSlider(
                    master=self.window.rotation_frame,
                    from_=-180,
                    to=180,
                    number_of_steps=360  # Шаги с шагом 1
                )
                rotation_slider.pack(pady=10)
                rotation_slider.set(0.0)  # Устанавливаем значение по умолчанию

                # Текстовая метка
                ctk.CTkLabel(
                    self.window.rotation_frame,
                    text="Поворот (-180° - +180°)"
                ).pack(pady=2)

                # Применять фильтр только после отпускания бегунка
                rotation_slider.bind("<ButtonRelease-1>", on_slider_release)

            except Exception as e:
                print(f"Ошибка при создании ползунка поворота: {e}")

        if self.current_layer.get("rotation_flag", False):
            # Если окно ползунка уже открыто, закрываем его
            if getattr(self.window, "rotation_frame", None):
                self.window.rotation_frame.destroy()
                self.window.rotation_frame = None
                self.current_layer["rotation_flag"] = False
        else:
            try:
                # Создаем новый фрейм для ползунка поворота
                self.window.rotation_frame = ctk.CTkFrame(
                    self.window.right_panel,
                    height=200,
                    width=300
                )
                self.window.rotation_frame.pack(pady=10)

                # Создаем ползунок поворота
                rotation_slider = ctk.CTkSlider(
                    master=self.window.rotation_frame,
                    from_=-180,
                    to=180,
                    number_of_steps=360  # Шаги с шагом 1
                )
                rotation_slider.pack(pady=10)
                if self.current_layer.get("rotation_value"):
                    rotation_slider.set(self.current_layer["rotation_value"])  # Устанавливаем значение по умолчанию
                else:
                    rotation_slider.set(0.0)

                # Текстовая метка
                ctk.CTkLabel(
                    self.window.rotation_frame,
                    text="Поворот (-180° - +180°)"
                ).pack(pady=2)

                # Применять фильтр только после отпускания бегунка
                rotation_slider.bind("<ButtonRelease-1>", on_slider_release)

                # Переключаем флаг состояния окна ползунка
                self.current_layer["rotation_flag"] = True
                self.current_layer["rotation_frame"] = True

            except Exception as e:
                print(f"Ошибка при создании ползунка поворота: {e}")

    def hor_flip(self):
        """Инвертирует цвета изображения, используя Haskell (Negative.hs)."""
        if self.mult_process_on:
            # Обработка множества изображений
            for i in range(len(self.mult_images)):
                self.mult_images[i] = self.func_helper._apply_hor_flip_filter(self.mult_images[i], i)
            
            # Отображаем первое изображение
            self.display_mult_image(self.mult_images[self.current_image_index])
        else:
            # Обработка одног
            if self.current_layer and self.current_layer["image"]:
                image_path = "HaskProj/temp/inputPath/input_horizFlip.png"
                output_path = "HaskProj/temp/outputPath/output_horizFlip.png"
                # Создаем временную папку, если её нет
                os.makedirs("HaskProj", exist_ok=True)
                # Сохраняем текущее изображение для обработки
                self.current_layer["copy"].save(image_path)
                if self.window.horiz_flag:
                    # Возвращаем оригинал изображения
                    self.current_layer["image"] = self.current_layer["copy"]
                    self.window.horiz_flag = False
                else:
                    try:
                        # Вызываем Haskell-программу Negative.hs с помощью runhaskell
                        subprocess.run([
                            "./backend_Erbol/Function/ImageTransform/HorizontalFlip/horizontalFlip",
                            image_path,
                            output_path
                        ], check=True)
                        
                        # Загружаем обработанное изображение
                        with Image.open(output_path) as img:
                            filtered_image = img.copy()  # Копируем изображение в память
                        
                        # Удаляем временный файл
                        self.delete_file(output_path)
                        self.delete_file(image_path)

                        # Обновляем изображение в слое
                        self.current_layer["image"] = filtered_image
                        self.window.edited_image = filtered_image
                        self.window.horiz_flag = True
                    except subprocess.CalledProcessError as e:
                        print(f"Ошибка при вызове Haskell-фильтра: {e}")
                # Обновляем отображение
                self.display_image(self.current_layer["image"])


    def vert_flip(self):
        """Инвертирует цвета изображения, используя Haskell (Negative.hs)."""
        if self.mult_process_on:
            # Обработка множества изображений
            for i in range(len(self.mult_images)):
                self.mult_images[i] = self.func_helper._apply_vert_flip_filter(self.mult_images[i], i)
            
            # Отображаем первое изображение
            self.display_mult_image(self.mult_images[self.current_image_index])
        else:
            if self.current_layer and self.current_layer["image"]:
                # Создаем временные пути
                input_dir = "HaskProj/temp/inputPath"
                output_dir = "HaskProj/temp/outputPath"
                image_path = os.path.join(input_dir, "input_vertFlip.png")
                output_path = os.path.join(output_dir, "output_vertFlip.png")

                # Создаем временные папки, если их нет
                os.makedirs(input_dir, exist_ok=True)
                os.makedirs(output_dir, exist_ok=True)

                # Сохраняем текущее изображение для обработки
                self.current_layer["copy"].save(image_path)

                if self.window.vert_flag:
                    # Возвращаем оригинал изображения
                    self.current_layer["image"] = self.current_layer["copy"]
                    self.window.vert_flag = False
                else:
                    try:
                        # Вызываем Haskell-программу VerticalFlip.hs
                        subprocess.run([
                            "./backend_Erbol/Function/ImageTransform/VerticalFlip/verticalFlip",
                            image_path,
                            output_path
                        ], check=True)

                        # Загружаем обработанное изображение
                        with Image.open(output_path) as img:
                            filtered_image = img.copy()  # Копируем изображение в память

                        # Удаляем временные файлы
                        self.delete_file(output_path)
                        self.delete_file(image_path)

                        # Обновляем изображение в слое
                        self.current_layer["image"] = filtered_image
                        self.window.edited_image = filtered_image
                        self.window.vert_flag = True
                    except subprocess.CalledProcessError as e:
                        print(f"Ошибка при вызове Haskell-фильтра: {e}")
                    except Exception as e:
                        print(f"Ошибка: {e}")

                # Обновляем отображение
                self.display_image(self.current_layer["image"])
    
    def to_sharpen(self):
        """Применяет эффект повышения резкости, используя Haskell (Sharpen.hs)."""
        if self.mult_process_on:
            # Обработка множества изображений
            for i in range(len(self.mult_images)):
                self.mult_images[i] = self.func_helper._apply_sharpen_filter(self.mult_images[i], i)
            
            # Отображаем первое изображение
            self.display_mult_image(self.mult_images[self.current_image_index])
        else:
            if self.current_layer and self.current_layer["image"]:
                # Создаем временные пути
                input_dir = "HaskProj/temp/inputPath"
                output_dir = "HaskProj/temp/outputPath"
                image_path = os.path.join(input_dir, "input_sharpen.png")
                output_path = os.path.join(output_dir, "output_sharpen.png")

                # Создаем временные папки, если их нет
                os.makedirs(input_dir, exist_ok=True)
                os.makedirs(output_dir, exist_ok=True)

                # Сохраняем текущее изображение для обработки
                self.current_layer["copy"].save(image_path)

                if self.window.sharpen_flag:
                    # Возвращаем оригинал изображения
                    self.current_layer["image"] = self.current_layer["copy"]
                    self.window.sharpen_flag = False
                else:
                    try:
                        # Вызываем Haskell-программу Sharpen.hs
                        subprocess.run([
                            "./backend_Erbol/Function/ImageEffects/Sharpen/sharpen",
                            image_path,
                            output_path
                        ], check=True)

                        # Загружаем обработанное изображение
                        with Image.open(output_path) as img:
                            filtered_image = img.copy()  # Копируем изображение в память

                        # Удаляем временные файлы
                        self.delete_file(output_path)
                        self.delete_file(image_path)

                        # Обновляем изображение в слое
                        self.current_layer["image"] = filtered_image
                        self.window.edited_image = filtered_image
                        self.window.sharpen_flag = True
                    except subprocess.CalledProcessError as e:
                        print(f"Ошибка при вызове Haskell-фильтра (sharpen): {e}")
                    except FileNotFoundError as e:
                        print(f"Файл не найден: {e}")
                    except Exception as e:
                        print(f"Неизвестная ошибка: {e}")

                # Обновляем отображение
                self.display_image(self.current_layer["image"])


    # Множественная обработка фото
    def open_child_window(self):

        answer = messagebox.askyesno("Warning!", "When applying filters with this option, you must know in advance which filter to apply, because this option does not have an undo function! Do you want to continue?")
        """Открывает дочернее окно для обработки изображений."""
        if answer:
            # Сбрасываем флаги и уничтожаем фреймы
            self.window.negative_flag = False
            self.window.sepia_flag = False
            self.window.grayscale_flag = False

            if self.window.brightness_frame:
                self.window.brightness_frame.destroy()
            if self.window.scale_frame:
                self.window.scale_frame.destroy()
            if self.window.pixel_frame:
                self.window.pixel_frame.destroy()
            if self.window.contrast_frame:
                self.window.contrast_frame.destroy()
            if self.window.rotation_frame:
                self.window.rotation_frame.destroy()

            self.window.scale_frame = None
            self.window.scale_flag = False
            self.window.pixel_frame = None
            self.window.pixel_flag = False
            self.window.contrast_frame = None
            self.window.contrast_flag = False
            self.window.rotation_frame = None
            self.window.rotation_flag = False
            self.window.brightness_frame = None
            self.window.brightness_flag = False

            # Очищаем Canvas
            self.window.canvas.delete("all")
            self.mult_process_on = True

            self.button_down_frame = ctk.CTkFrame(self.window.canvas, fg_color="white")
            self.button_down_frame.pack(side="bottom", pady=10)
            
            # Проверяем, были ли кнопки уже созданы
            if not hasattr(self, "mult_load") or self.mult_load.winfo_exists() == 0:
                # Если кнопка не существует, создаём её
                self.mult_load = ctk.CTkButton(
                    self.button_down_frame,
                    text="📂 Open",
                    command=self.load_images
                )
                self.mult_load.grid(row=0, column=0, padx=5)

            if not hasattr(self, "mult_close") or self.mult_close.winfo_exists() == 0:
                # Если кнопка не существует, создаём её
                self.mult_close = ctk.CTkButton(
                    self.button_down_frame,
                    text="🚪 Exit",
                    command=self.on_close
                )
                self.mult_close.grid(row=0, column=1, padx=5)

            if not hasattr(self, "mult_delete") or self.mult_delete.winfo_exists() == 0:
                # Если кнопка не существует, создаём её
                self.mult_delete = ctk.CTkButton(
                    self.button_down_frame,
                    text="🗑 Delete all",
                    command=self.delete_all
                )
                self.mult_delete.grid(row=0, column=2, padx=5)

            # Устанавливаем флаг, что кнопки созданы
            self.buttons_created = True
            self.mult_process_on = True


    def load_images(self):
        """Загружает изображения из проводника."""
        files = filedialog.askopenfilenames(
            title="Выберите изображения",
            filetypes=[("Image Files", "*.png *.jpg *.jpeg *.bmp")]
        )
        if files:
            self.mult_images = []  # Инициализируем список для хранения изображений
            input_dir = "HaskProj/temp/multiplProcess/input"
            os.makedirs(input_dir, exist_ok=True)  # Создаем временную папку, если её нет

            for index, file in enumerate(files):
                image = Image.open(file)
                new_image = {
                    "image": image.copy() if image else None,
                    "brightness_value": 1.0,  # Значение по умолчанию
                    "copy": image.copy() if image else None,  # Храним оригинал слоя
                    "scale_value": 1.0,
                    "bright_flag": False,
                    "scale_flag": False,
                    "scale_frame": False,
                    "bright_frame": False,
                    "br_flag_saved": True,
                    "pixel_value": 1,
                    "pixel_flag": False,
                    "pixel_frame": False,
                    "pixel_flag_saved": True,
                    "contrast_value": 1,
                    "contrast_flag": False,
                    "contrast_frame": False,
                    "rotation_value": 0,
                    "rotation_frame": False,
                    "rotation_flag": False,
                    "position_x": 0,
                    "position_y": 0            
                }
                self.mult_images.append(new_image)  # Добавляем изображение в список

                # Сохраняем изображение во временную папку
                image_path = os.path.join(input_dir, f"input_{index + 1}.png")
                image.save(image_path)
                print(f"Изображение сохранено: {image_path}")

            print(f"Загружено {len(self.mult_images)} изображений.")
            if self.mult_images:
                self.display_mult_image(self.mult_images[0])  # Отображаем первое изображение
        else:
            print("Изображения не выбраны.")

        if len(self.mult_images) > 1:
            # Создаем фрейм для кнопок
            self.button_frame = ctk.CTkFrame(self.window.canvas, fg_color="white")
            self.button_frame.pack(side="top", pady=10)

            # Кнопка "Назад"
            self.prev_button = ctk.CTkButton(
                self.button_frame,
                text="👈🏻",
                command=self.prev_image,
                width=50,  # Ширина кнопки
                height=25  # Высота кнопки (равна ширине для квадратной формы)
            )
            self.prev_button.grid(row=0, column=0, padx=5)  # Размещаем в первой колонке

            # Кнопка "Вперёд"
            self.next_button = ctk.CTkButton(
                self.button_frame,
                text="👉🏻",
                command=self.next_image,
                width=50,  # Ширина кнопки
                height=25  # Высота кнопки (равна ширине для квадратной формы)
            )
            self.next_button.grid(row=0, column=1, padx=5)  # Размещаем во второй колонке
        self.current_image_index = 0
        
                    
    def on_close(self):
        """Закрывает дочернее окно и очищает ресурсы."""
        answer = messagebox.askokcancel("Warning", "Do you want to end?")
        if answer:
            self.mult_load.destroy()
            self.mult_close.destroy()
            self.mult_delete.destroy()
            self.next_button.destroy()
            self.prev_button.destroy()
            self.button_frame.destroy()
            self.button_down_frame.destroy()
            self.mult_images = []  # Очищаем список изображений
            
            self.window.canvas.delete("all")
            self.buttons_created = False
            self.mult_process_on = False

            # Сбрасываем флаги и уничтожаем фреймы
            self.window.negative_flag = False
            self.window.sepia_flag = False
            self.window.grayscale_flag = False

            if self.window.brightness_frame:
                self.window.brightness_frame.destroy()
            if self.window.scale_frame:
                self.window.scale_frame.destroy()
            if self.window.pixel_frame:
                self.window.pixel_frame.destroy()
            if self.window.contrast_frame:
                self.window.contrast_frame.destroy()
            if self.window.rotation_frame:
                self.window.rotation_frame.destroy()

            self.window.scale_frame = None
            self.window.scale_flag = False
            self.window.pixel_frame = None
            self.window.pixel_flag = False
            self.window.contrast_frame = None
            self.window.contrast_flag = False
            self.window.rotation_frame = None
            self.window.rotation_flag = False
            self.window.brightness_frame = None
            self.window.brightness_flag = False

            messagebox.showinfo("Info", "All files are in 'temp/multiplProcess/output'")

    def delete_all(self):
        """Закрывает дочернее окно и очищает ресурсы."""
        answer = messagebox.askokcancel("Warning", "Do you want to delete all?")
        if answer:
            self.next_button.destroy()
            self.prev_button.destroy()
            self.button_frame.destroy()
            self.mult_images = []  # Очищаем список изображений

            #Очищаем папки от временных файлов
            folderinput_path = "HaskProj/temp/multiplProcess/input"
            for file_name in os.listdir(folderinput_path):
                file_path = os.path.join(folderinput_path, file_name)
                self.delete_file(file_path)

            folderoutput_path = "HaskProj/temp/multiplProcess/output"
            for file_name in os.listdir(folderoutput_path):
                file_path = os.path.join(folderoutput_path, file_name)
                self.delete_file(file_path)

            foldermovein_path = "HaskProj/temp/move/input"
            for file_name in os.listdir(foldermovein_path):
                file_path = os.path.join(foldermovein_path, file_name)
                self.delete_file(file_path)

            foldermoveout_path = "HaskProj/temp/move/output"
            for file_name in os.listdir(foldermoveout_path):
                file_path = os.path.join(foldermoveout_path, file_name)
                self.delete_file(file_path)
            
            self.window.canvas.delete("all")

            # Сбрасываем флаги и уничтожаем фреймы
            self.window.negative_flag = False
            self.window.sepia_flag = False
            self.window.grayscale_flag = False

            if self.window.brightness_frame:
                self.window.brightness_frame.destroy()
            if self.window.scale_frame:
                self.window.scale_frame.destroy()
            if self.window.pixel_frame:
                self.window.pixel_frame.destroy()
            if self.window.contrast_frame:
                self.window.contrast_frame.destroy()
            if self.window.rotation_frame:
                self.window.rotation_frame.destroy()

            self.window.scale_frame = None
            self.window.scale_flag = False
            self.window.pixel_frame = None
            self.window.pixel_flag = False
            self.window.contrast_frame = None
            self.window.contrast_flag = False
            self.window.rotation_frame = None
            self.window.rotation_flag = False
            self.window.brightness_frame = None
            self.window.brightness_flag = False



    def display_mult_image(self, image):
        """Отображает изображение на Canvas."""
        if not image:
            print("Ошибка: Изображение не загружено.")
            return
        
        new_width = int(image.width * 0.6)  # Уменьшаем ширину в 2 раза
        new_height = int(image.height * 0.6)  # Уменьшаем высоту в 2 раза

        # Изменяем размер изображения
        resized_image = image.resize((new_width, new_height), Image.Resampling.LANCZOS)

        # Преобразуем изображение в формат, подходящий для Tkinter
        photo = ImageTk.PhotoImage(resized_image)

        # Очищаем Canvas и отображаем новое изображение
        self.window.canvas.delete("all")
        self.window.canvas.create_image(600, 400, image=photo, anchor="center")  # Центрируем изображение
        self.window.canvas.image = photo  # Сохраняем ссылку на изображение

    
    def next_image(self):
        """Переключает на следующее изображение."""
        if not self.mult_images:
            print("Ошибка: Нет загруженных изображений.")
            return

        self.current_image_index = (self.current_image_index + 1) % len(self.mult_images)
        self.display_mult_image(self.mult_images[self.current_image_index])

    def prev_image(self):
        """Переключает на предыдущее изображение."""
        if not self.mult_images:
            print("Ошибка: Нет загруженных изображений.")
            return

        self.current_image_index = (self.current_image_index - 1) % len(self.mult_images)
        self.display_mult_image(self.mult_images[self.current_image_index])





    def x_move(self, x_offset):
        """Перемещает изображение по горизонтали (X)"""
        if self.mult_process_on:
            for i in range(len(self.mult_images)):
                self.mult_images[i] = self.func_helper._apply_X_filter(self.mult_images[i], i, x_offset)
            self.display_mult_image(self.mult_images[self.current_image_index])
        else:
            if self.current_layer and self.current_layer["image"]:
                try:
                    # Накопительное смещение
                    self.current_layer["position_x"] = self.current_layer.get("position_x", 0) + x_offset

                    # Применяем фильтр перемещения
                    self.current_layer["image"] = self.func_helper._apply_X_filter(
                        self.current_layer["copy"], 0, self.current_layer["position_x"]
                    )

                    # Проверяем и создаем директории
                    os.makedirs("HaskProj/temp/move/input", exist_ok=True)
                    os.makedirs("HaskProj/temp/move/output", exist_ok=True)

                    image_path = f"HaskProj/temp/move/input/picture_moveX_0.png"
                    output_path = f"HaskProj/temp/move/output/picture_moveX_0.png"

                    # Проверяем существование файла перед удалением
                    if os.path.exists(image_path):
                        self.delete_file(image_path)
                    else:
                        print(f"Файл {image_path} не найден, пропускаем удаление")

                    if os.path.exists(output_path):
                        self.delete_file(output_path)
                    else:
                        print(f"Файл {output_path} не найден, пропускаем удаление")

                    # Обновляем отображение
                    self.display_image(self.current_layer["image"])
                except Exception as e:
                    print(f"Ошибка в x_move: {str(e)}")


    def y_move(self, y_offset):
        """Перемещает изображение по горизонтали (X)"""
        if self.mult_process_on:
            for i in range(len(self.mult_images)):
                self.mult_images[i] = self.func_helper._apply_Y_filter(self.mult_images[i], i, y_offset)
            self.display_mult_image(self.mult_images[self.current_image_index])
        else:
            if self.current_layer and self.current_layer["image"]:
                try:
                    # Накопительное смещение
                    self.current_layer["position_y"] = self.current_layer.get("position_y", 0) + y_offset

                    # Применяем фильтр перемещения
                    self.current_layer["image"] = self.func_helper._apply_Y_filter(
                        self.current_layer["copy"], 0, self.current_layer["position_y"]
                    )

                    # Проверяем и создаем директории
                    os.makedirs("HaskProj/temp/move/input", exist_ok=True)
                    os.makedirs("HaskProj/temp/move/output", exist_ok=True)

                    image_path = f"HaskProj/temp/move/input/picture_moveY_0.png"
                    output_path = f"HaskProj/temp/move/output/picture_moveY_0.png"

                    # Проверяем существование файла перед удалением
                    if os.path.exists(image_path):
                        self.delete_file(image_path)
                    else:
                        print(f"Файл {image_path} не найден, пропускаем удаление")

                    if os.path.exists(output_path):
                        self.delete_file(output_path)
                    else:
                        print(f"Файл {output_path} не найден, пропускаем удаление")

                    # Обновляем отображение
                    self.display_image(self.current_layer["image"])
                except Exception as e:
                    print(f"Ошибка в y_move: {str(e)}")