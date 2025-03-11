import customtkinter as ctk
from function_main import ImageProcces_and_TopMenu
from tkinter import filedialog, messagebox
from PIL import Image, ImageTk, ImageEnhance, ImageOps

class Window:
    def __init__(self, main_icon="resourses/icon.ico"):
        ctk.set_appearance_mode("dark")
        ctk.set_default_color_theme("dark-blue")

        self.root = ctk.CTk()
        self.root.title("MiniPhotoshop")
        self.root.geometry("1200x700")
        self.toollip = None

        # Теперь передаём self в ImageProcces_and_TopMenu
        self.func = ImageProcces_and_TopMenu(self)

        # ========== Центральная панель ==========
        self.main_frame = ctk.CTkFrame(self.root, fg_color="#424242", width=900, height=700)
        self.main_frame.pack(side="left", fill="both", expand=True)

        self.top_frame = ctk.CTkFrame(self.main_frame, fg_color="#424242", width=900, height=40)
        self.top_frame.pack(fill="both")

        self.canvas = ctk.CTkCanvas(self.main_frame, bg="#282828")
        self.canvas.pack(fill="both", expand=True)

        # ========== Правая панель ==========
        self.right_panel = ctk.CTkFrame(self.root, fg_color="#535353", width=300, height=700)
        self.right_panel.pack(side="right", fill="y")

        self.tools_frame = ctk.CTkFrame(self.right_panel, fg_color="#535353", width=300, height=600)
        self.tools_frame.pack(fill="both", padx=5, pady=5)

        # ======= Слои ==========
        self.layers_frame = ctk.CTkFrame(self.right_panel, width=300, height=300)
        self.layers_frame.pack(side = "bottom", fill="both", padx=5, pady=5)

        # Место для кнопки сохранения изменений
        self.but_save = ctk.CTkFrame(self.right_panel, width=300, height=300)
        self.but_save.pack(side = "bottom", fill="both", padx=10, pady=5)

        self.btn_save = ctk.CTkButton(self.but_save, text="💾 Cохранить изменения", command=self.func.save_difference)
        self.btn_save.pack(fill = "x", padx=5, pady=5)

        # Слои        
        self.layers_label = ctk.CTkLabel(self.layers_frame, text="🖼 Слои", font=("Arial", 12, "bold"))
        self.layers_label.pack(pady=5)

        # Заменяем Listbox на ScrollableFrame
        self.layers_list = ctk.CTkScrollableFrame(self.layers_frame, height=300)
        self.layers_list.pack(fill="both", expand=True, padx=5, pady=5)

        # 🔹 Кнопка для добавления нового слоя
        self.add_layer_btn = ctk.CTkButton(self.layers_frame, text="➕ Добавить слой", command=self.func.add_layer)
        self.add_layer_btn.pack(pady=5)

        # 🔹 Список слоёв
        self.layers = []  # Хранит данные всех слоёв (изображение, кнопка)
        self.current_layer = None
        
        # Разделение панели инструментов
        

        # 🖼 Переменные для изображений
        self.image = None
        self.edited_image = None
        self.image_path = None

        # Флаги для фильтров
        self.negative_flag = False
        self.sepia_flag = False
        self.grayscale_flag = False
        self.brightness_flag = False
        self.brightness_value = 1
        self.brightness_frame = None
        self.scale_flag = False
        self.scale_frame = None
        self.solarize_flag = False

        # Добавляем виджеты и меню из `func`
        self.func.right_panel_widgets()
        self.func.top_menu1()
        self.func.top_menu2()
        self.func.top_menu3()

    def run(self):
        self.root.mainloop()

