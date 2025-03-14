import customtkinter as ctk
from tkinter import filedialog, messagebox
from PIL import Image, ImageTk, ImageEnhance, ImageOps
import subprocess
import os

class ImageProcces_and_TopMenu:
    
    def __init__(self, window):
        """Принимаем ссылку на `Window`, чтобы работать с GUI."""
        self.window = window
        self.layers = []
        self.images = []
        self.current_layer = None  

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
            "rotation_flag": False
        }

        new_layer["button"] = ctk.CTkButton(
            self.window.layers_list, text=new_layer["name"], command=lambda l=new_layer: self.select_layer(l)
        )
        new_layer["button"].pack(fill="x", pady=2, padx=5)

        self.layers.append(new_layer)
        self.select_layer(new_layer)  # Делаем новый слой активным
    
    def open_child_window(self):
        # Создаем дочернее окно
        child_window = ctk.CTkToplevel(self.window.root)
        child_window.title("SCRIPTS")
        child_window.geometry("1000x600")

        child_window.grab_set()

        # Добавляем обработчик закрытия окна
        def on_close():
            child_window.grab_release()  # Снимаем блокировку
            child_window.destroy()

        # Левая панель для кнопок эффектов
        left_frame = ctk.CTkFrame(child_window)
        left_frame.pack(side="left", padx=10, pady=5, fill="y")

        left_word = ctk.CTkLabel(left_frame, text="Image Effects")
        left_word.pack(side="top")

        # Список эффектов и соответствующих функций
        effects = [
            ("Brightness", self.create_brightness_slider),
            ("Pixelate", self.create_pixelate_slider),
            ("Sharpen", self.to_sharpen),
            ("Contrast", self.create_contrast_slider),
            ("Grayscale", self.to_black_white),
            ("Negative", self.invert_colors),
            ("Sepia", self.to_sepia),
            ("Solarize", self.to_solarize),
            ("Horizontal Flip", self.hor_flip),
            ("Vertical Flip", self.vert_flip),
            ("Rotation", self.create_rotation_slider),
            ("Scale", self.create_scale_slider)
        ]

        # Добавляем кнопки для выбора эффектов
        for name, func in effects:
            ctk.CTkButton(
                left_frame,
                text=name,
                command=lambda f=func: self.preview_and_apply(f, child_window)
            ).pack(pady=2)

        # Кнопка загрузки изображений
        ctk.CTkButton(
            left_frame,
            text="📂 Open",
            command=lambda: self.load_images(child_window)
        ).pack(side="bottom", pady=2)

        ctk.CTkButton(left_frame, text="🚪 Exit", command=on_close).pack(side="bottom", pady=2)


    def load_images(self, child_window):
        """Загружает изображения из проводника."""
        files = filedialog.askopenfilenames(
            title="Выберите изображения",
            filetypes=[("Image Files", "*.png *.jpg *.jpeg *.bmp")]
        )
        if files:
            self.images = [Image.open(file) for file in files]
            print(f"Загружено {len(self.images)} изображений.")
            self.display_child(child_window, self.images[0])
        else:
            print("Изображения не выбраны.")

    def preview_and_apply(self, effect_func, child_window):
        """Показывает предварительный просмотр и применяет эффект."""
        if not hasattr(self, "images") or not self.images:
            print("Ошибка: Нет загруженных изображений.")
            return

        # Применяем эффект к первому изображению для предпросмотра
        preview_image = effect_func(self.images[0])

        # Отображаем предварительный просмотр
        self.display_child(child_window, preview_image)

        # Кнопка для применения эффекта ко всем изображениям
        apply_button = ctk.CTkButton(
            child_window,
            text="Apply to All",
            command=lambda: self.apply_effect_to_all(effect_func)
        )
        apply_button.pack(pady=10)

    def display_child(self, child_window, image):
        """Отображает изображение на дочернем окне."""
        if not image:
            print("Ошибка: Изображение не загружено.")
            return

        # Преобразуем изображение в формат, подходящий для отображения в Tkinter
        photo = ImageTk.PhotoImage(image)

        # Создаем Canvas на дочернем окне, если он еще не создан
        if not hasattr(child_window, "canvas"):
            child_window.canvas = ctk.CTkCanvas(child_window, width=1000, height=600)
            child_window.canvas.pack()

        # Очищаем Canvas и отображаем новое изображение
        child_window.canvas.delete("all")
        child_window.canvas.create_image(300, 300, image=photo, anchor="center")  # Центрируем изображение
        child_window.canvas.image = photo  # Сохраняем ссылку на изображение
        
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

    # Функция отображения изображения на canvas
    def display_image(self, image):
        """Отображает изображение на canvas"""
        if image:
            photo = ImageTk.PhotoImage(image)
            self.window.canvas.delete("all")
            self.window.canvas.create_image(500, 400, image=photo, anchor="center")
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

    def top_menu2(self):
        options = ["Scale", "Drawing"]
        self.menu_combobox3 = ctk.CTkComboBox(self.window.top_frame, values=options, command=self.menu_action2)
        self.menu_combobox3.pack(side="left", padx=2)
        self.menu_combobox3.set("Function")

    def menu_action2(self, choice):
        """Обрабатывает выбор из выпадающего списка."""
        if choice == "Scale":
            self.create_scale_slider()
        elif choice == "Drawing":
            messagebox.showinfo("О нас", "Разработчик: ...")

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
        if self.current_layer and self.current_layer["image"]:
            image_path = "temp/inputPath/input_invert.png"
            output_path = "temp/outputPath/output_invert.png"
            # Создаем временную папку, если её нет
            os.makedirs("temp", exist_ok=True)
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
        if self.current_layer and self.current_layer["image"]:
            image_path = "temp/inputPath/input_bw.png"
            output_path = "temp/outputPath/output_bw.png"
            os.makedirs("temp", exist_ok=True)
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
        if self.current_layer and self.current_layer["image"]:
            image_path = "temp/inputPath/input_solarize.png"
            output_path = "temp/outputPath/output_solarize.png"
            os.makedirs("temp", exist_ok=True)
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
        if self.current_layer and self.current_layer["image"]:
            image_path = "temp/inputPath/input_sepia.png"
            output_path = "temp/outputPath/output_sepia.png"
            os.makedirs("temp", exist_ok=True)
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
        if self.current_layer and self.current_layer["image"]:
            factor = float(value)
            if abs(factor - self.current_layer["brightness_value"]) < 0.01:
                return  # Изменение слишком мало для обработки
            self.current_layer["brightness_value"] = factor

            image_path = "temp/inputPath/input_brightness.png"
            output_path = "temp/outputPath/output_brightness.png"
            os.makedirs("temp", exist_ok=True)
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


    def create_scale_slider(self):
        """Основной метод для создания ползунка масштабирования"""
        # Проверяем наличие атрибута scale_flag
        if not hasattr(self.window, 'scale_flag'):
            self.current_layer["scale_flag"] = False

        if self.current_layer["scale_flag"]:
            self.window.scale_frame.destroy()
            self.current_layer["scale_flag"] = False
            self.current_layer["scale_frame"] = False
        else:
            self._create_scale_slider_frame()

    def _create_scale_slider_frame(self):
        """Создает и настраивает графические компоненты ползунка"""
        try:
            # Создаем контейнер
            self.window.scale_frame = ctk.CTkFrame(
                self.window.right_panel,
                height=100,
                fg_color=("gray85", "gray25")
            )
            self.window.scale_frame.pack(pady=5, fill='x')

            # Ползунок масштабирования
            self.scale_slider = ctk.CTkSlider(
                master=self.window.scale_frame,
                from_=0.1,
                to=4.0,
                number_of_steps=40,
                command=lambda value: None
            )
            if self.current_layer and "scale_value" in self.current_layer:
                self.scale_slider.set(self.current_layer["scale_value"])
            else:
                self.scale_slider.set(self.current_layer["scale_value"])  # Устанавливаем значение по умолчанию
                
            self.scale_slider.pack(pady=5, padx=10, fill='x')

            # Привязываем обработчик
            self.scale_slider.bind("<ButtonRelease-1>", self._on_scale_slider_release)

            # Текстовая метка
            ctk.CTkLabel(
                self.window.scale_frame,
                text="Масштаб (0.1x - 4.0x)"
            ).pack(pady=2)

            self.current_layer["scale_flag"] = True

        except Exception as e:
            messagebox.showerror("Ошибка", f"Не удалось создать ползунок: {e}")

    def _on_scale_slider_release(self, event):
        """Обработчик изменения масштаба"""
        if self.current_layer and self.current_layer["image"]:
            # Определяем пути к временным файлам
            scale_input_path = "temp/inputPath/input_Scale.png"
            scale_output_path = "temp/outputPath/output_Scale.png"
            
            # Создаем необходимые директории
            os.makedirs("temp", exist_ok=True)
            
            self.current_layer["copy"].save(scale_input_path)

            try:
                # Выполняем масштабирование
                scale_factor = self.scale_slider.get()
                subprocess.run([
                    "./backend_Erbol/Function/ImageTransform/Scale/scale",
                    scale_input_path,
                    scale_output_path,
                    str(scale_factor)
                ], check=True) 
                self.current_layer["scale_value"] = scale_factor
                # Обновляем изображение
                with Image.open(scale_output_path) as scaled_img:
                    scaled_img = scaled_img.copy()
                    self.current_layer["image"] = scaled_img
                    self.window.edited_image = scaled_img
                    self.display_image(scaled_img)

                    self.delete_file(scale_input_path)
                    self.delete_file(scale_output_path)
            except subprocess.CalledProcessError as e:
                print(f"Ошибка при вызове Haskell-фильтра (Brightness): {e}")
        else:
            if self.window.scale_frame:
                self.window.scale_frame.destroy()
                self.window.scale_frame = None
                self.current_layer["scale_flag"] = False
                self.current_layer["scale_frame"] = False
            
    # Функция для наложения мозаики
    def adjust_pixelate(self, value):
        """Регулирует мозаику, отправляя коэффициент в Haskell."""
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

                # Создаем необходимые директории
                input_dir = "temp/inputPath"
                output_dir = "temp/outputPath"
                os.makedirs(input_dir, exist_ok=True)
                os.makedirs(output_dir, exist_ok=True)

                image_path = os.path.join(input_dir, "input_pixelate.png")
                output_path = os.path.join(output_dir, "output_pixelate.png")

                # Сохраняем копию изображения
                self.current_layer["copy"].save(image_path)

                # Выполняем Haskell-скрипт
                try:
                    result = subprocess.run([
                        "./backend_Erbol/Function/ImageEffects/Pixelate/pixelate",
                        image_path,
                        output_path,
                        str(factor)  # Теперь передаем целое число
                    ], capture_output=True, text=True, check=True, timeout=10)
            
                except subprocess.TimeoutExpired:
                    raise RuntimeError("Haskell-процесс превысил время ожидания")
                except subprocess.CalledProcessError as e:
                    error_msg = f"Ошибка выполнения Haskell: {e}\nstdout: {e.stdout}\nstderr: {e.stderr}"
                    raise RuntimeError(error_msg)

                # Проверяем существование выходного файла
                if not os.path.exists(output_path):
                    raise FileNotFoundError(f"Файл {output_path} не был создан!")

                # Открываем и обрабатываем результат
                with Image.open(output_path) as img:
                    filtered_image = img.copy()

                # Обновляем данные слоя
                self.current_layer["image"] = filtered_image
                self.window.edited_image = filtered_image
                self.display_image(filtered_image)

            except Exception as e:
                print(f"Ошибка в adjust_pixelate: {str(e)}")
            finally:
                # Удаляем временные файлы в любом случае
                for path in [image_path, output_path]:
                    try:
                        if os.path.exists(path):
                            os.remove(path)
                    except Exception as e:
                        print(f"Ошибка при удалении {path}: {e}")
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
                    print("Значение должно быть между 1 и 10")
            except ValueError:
                print("Неверное значение")

        if self.current_layer.get("pixel_flag", False):
            if getattr(self.window, "pixel_frame", None):
                self.window.pixel_frame.destroy()
                self.window.pixel_frame = None
                self.current_layer["pixel_flag"] = False
        else:
            try:
                self.window.pixel_frame = ctk.CTkFrame(
                    self.window.right_panel,
                    height=200,
                    width=300
                )
                self.window.pixel_frame.pack(pady=10)

                initial_value = round(self.current_layer.get("pixel_value", 1))
                pixel_slider = ctk.CTkSlider(
                    master=self.window.pixel_frame,
                    from_=1,
                    to=15,
                    number_of_steps = 15  # Теперь только целые числа
                )

                # Текстовая метка
                ctk.CTkLabel(
                    self.window.pixel_frame,
                    text="Коэффицент (1x - 15x)"
                ).pack(pady=2)

                pixel_slider.pack(pady=10)
                pixel_slider.set(initial_value)
                pixel_slider.bind("<ButtonRelease-1>", on_slider_release)

                self.current_layer["pixel_flag"] = True
                self.current_layer["pixel_frame"] = True

            except Exception as e:
                print(f"Ошибка при создании ползунка: {e}")


    def adjust_contrast(self, value):
        if self.current_layer and self.current_layer["image"]:
            try:
                # Округляем значение до двух знаков после запятой
                factor = round(float(value), 2)
                
                # Проверяем минимальное изменение
                if abs(factor - self.current_layer.get("contrast_value", 1.0)) < 0.01:
                    return
                    
                self.current_layer["contrast_value"] = factor

                # Создаем необходимые директории
                image_path = "temp/inputPath/input_contrast.png"
                output_path = "temp/outputPath/output_contrast.png"
                os.makedirs(os.path.dirname(image_path), exist_ok=True)
                os.makedirs(os.path.dirname(output_path), exist_ok=True)

                # Сохраняем копию изображения
                self.current_layer["copy"].save(image_path)

                # Выполняем Haskell-скрипт
                try:
                    result = subprocess.run([
                        "./backend_Erbol/Function/ImageProcessing/Contrast/contrast",
                        image_path,
                        output_path,
                        str(factor)
                    ], capture_output=True, text=True, check=True, timeout=10)
                    
                except subprocess.TimeoutExpired:
                    raise RuntimeError("Haskell-процесс превысил время ожидания")
                except subprocess.CalledProcessError as e:
                    error_msg = f"Ошибка выполнения Haskell: {e}\nstdout: {e.stdout}\nstderr: {e.stderr}"
                    raise RuntimeError(error_msg)

                # Проверяем существование выходного файла
                if not os.path.exists(output_path):
                    raise FileNotFoundError(f"Файл {output_path} не был создан!")

                # Открываем и обрабатываем результат
                with Image.open(output_path) as img:
                    filtered_image = img.copy()

                # Обновляем данные слоя
                self.current_layer["image"] = filtered_image
                self.window.edited_image = filtered_image
                self.display_image(filtered_image)

            except Exception as e:
                print(f"Ошибка в adjust_contrast: {str(e)}")
            finally:
                # Удаляем временные файлы в любом случае
                for path in [image_path, output_path]:
                    try:
                        if os.path.exists(path):
                            os.remove(path)
                    except Exception as e:
                        print(f"Ошибка при удалении {path}: {e}")
        else:
            # Логика закрытия окна ползунка
            if getattr(self.window, "contrast_frame", None):
                self.window.contrast_frame.destroy()
                self.window.contrast_frame = None
            self.current_layer["contrast_flag"] = False
            self.current_layer["contrast_frame"] = False

    def create_contrast_slider(self):
        """Создаёт окно с ползунком."""
        def on_slider_release(event):
            try:
                # Округляем значение до целого
                value = float(contrast_slider.get())
                if 0 <= value <= 2:
                    self.adjust_contrast(value)
                else:
                    print("Значение должно быть между 1 и 2")
            except ValueError:
                print("Неверное значение")

        if self.current_layer.get("contrast_flag", False):
            if getattr(self.window, "contrast_frame", None):
                self.window.contrast_frame.destroy()
                self.window.contrast_frame = None
                self.current_layer["contrast_flag"] = False
        else:
            try:
                self.window.contrast_frame = ctk.CTkFrame(
                    self.window.right_panel,
                    height=200,
                    width=300
                )
                self.window.contrast_frame.pack(pady=10)

                initial_value = self.current_layer.get("contrast_value", 1)
                contrast_slider = ctk.CTkSlider(
                    master=self.window.contrast_frame,
                    from_=0,
                    to=2,
                    number_of_steps = 20 
                )

                # Текстовая метка
                ctk.CTkLabel(
                    self.window.contrast_frame,
                    text="Коэффицент (0.1x - 2.0x)"
                ).pack(pady=2)

                contrast_slider.pack(pady=10)
                contrast_slider.set(initial_value)
                contrast_slider.bind("<ButtonRelease-1>", on_slider_release)

                self.current_layer["contrast_flag"] = True
                self.current_layer["contrast_frame"] = True

            except Exception as e:
                print(f"Ошибка при создании ползунка: {e}")


    def adjust_rotation(self, value):
        if self.current_layer and self.current_layer["image"]:
            try:
                # Округляем значение до двух знаков после запятой
                factor = round(float(value), 2)
                
                # Проверяем минимальное изменение
                if abs(factor - self.current_layer.get("rotation_value", 1.0)) < 0.01:
                    return
                    
                self.current_layer["rotation_value"] = factor

                # Создаем необходимые директории
                image_path = "temp/inputPath/input_rotation.png"
                output_path = "temp/outputPath/output_rotation.png"
                os.makedirs(os.path.dirname(image_path), exist_ok=True)
                os.makedirs(os.path.dirname(output_path), exist_ok=True)

                # Сохраняем копию изображения
                self.current_layer["copy"].save(image_path)

                # Выполняем Haskell-скрипт
                try:
                    result = subprocess.run([
                        "./backend_Erbol/Function/ImageTransform/Rotation/rotation",
                        image_path,
                        output_path,
                        str(factor)
                    ], capture_output=True, text=True, check=True, timeout=10)
                    
                except subprocess.TimeoutExpired:
                    raise RuntimeError("Haskell-процесс превысил время ожидания")
                except subprocess.CalledProcessError as e:
                    error_msg = f"Ошибка выполнения Haskell: {e}\nstdout: {e.stdout}\nstderr: {e.stderr}"
                    raise RuntimeError(error_msg)

                # Проверяем существование выходного файла
                if not os.path.exists(output_path):
                    raise FileNotFoundError(f"Файл {output_path} не был создан!")

                # Открываем и обрабатываем результат
                with Image.open(output_path) as img:
                    filtered_image = img.copy()

                # Обновляем данные слоя
                self.current_layer["image"] = filtered_image
                self.window.edited_image = filtered_image
                self.display_image(filtered_image)

            except Exception as e:
                print(f"Ошибка в adjust_rotation: {str(e)}")
            finally:
                # Удаляем временные файлы в любом случае
                for path in [image_path, output_path]:
                    try:
                        if os.path.exists(path):
                            os.remove(path)
                    except Exception as e:
                        print(f"Ошибка при удалении {path}: {e}")
        else:
            # Логика закрытия окна ползунка
            if getattr(self.window, "rotation_frame", None):
                self.window.rotation_frame.destroy()
                self.window.rotation_frame = None
            self.current_layer["rotation_flag"] = False
            self.current_layer["rotation_frame"] = False

    def create_rotation_slider(self):
        """Создаёт окно с ползунком."""
        def on_slider_release(event):
            try:
                # Округляем значение до целого
                value = float(rotation_slider.get())
                if -180 <= value <= 180:
                    self.adjust_rotation(value)
                else:
                    print("Значение должно быть между -180 и 180")
            except ValueError:
                print("Неверное значение")

        if self.current_layer.get("rotation_flag", False):
            if getattr(self.window, "rotation_frame", None):
                self.window.rotation_frame.destroy()
                self.window.rotation_frame = None
                self.current_layer["rotation_flag"] = False
        else:
            try:
                self.window.rotation_frame = ctk.CTkFrame(
                    self.window.right_panel,
                    height=200,
                    width=300
                )
                self.window.rotation_frame.pack(pady=10)

                initial_value = self.current_layer.get("rotation_value", 1)
                rotation_slider = ctk.CTkSlider(
                    master=self.window.rotation_frame,
                    from_=-180,
                    to=180,
                    number_of_steps = 1080 
                )

                # Текстовая метка
                ctk.CTkLabel(
                    self.window.rotation_frame,
                    text="Угол (-180° - +180°)"
                ).pack(pady=2)

                rotation_slider.pack(pady=10)
                rotation_slider.set(initial_value)
                rotation_slider.bind("<ButtonRelease-1>", on_slider_release)

                self.current_layer["rotation_flag"] = True
                self.current_layer["rotation_frame"] = True

            except Exception as e:
                print(f"Ошибка при создании ползунка: {e}")


    def hor_flip(self):
        """Инвертирует цвета изображения, используя Haskell (Negative.hs)."""
        if self.current_layer and self.current_layer["image"]:
            image_path = "temp/inputPath/input_horizFlip.png"
            output_path = "temp/outputPath/output_horizFlip.png"
            # Создаем временную папку, если её нет
            os.makedirs("temp", exist_ok=True)
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
        if self.current_layer and self.current_layer["image"]:
            # Создаем временные пути
            input_dir = "temp/inputPath"
            output_dir = "temp/outputPath"
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
        if self.current_layer and self.current_layer["image"]:
            # Создаем временные пути
            input_dir = "temp/inputPath"
            output_dir = "temp/outputPath"
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
