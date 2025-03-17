import customtkinter as ctk
from function_main import ImageProcces_and_TopMenu  # Импорт из общего модуля

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

        self.canvas = ctk.CTkCanvas(self.main_frame, bg="white")
        self.canvas.pack(fill="both", expand=True)

        # ========== Правая панель ==========
        self.right_panel = ctk.CTkFrame(self.root, fg_color="#535353", width=300, height=700)
        self.right_panel.pack(side="right", fill="y")

        self.tools_frame = ctk.CTkFrame(self.right_panel, fg_color="#535353", width=300, height=600)
        self.tools_frame.pack(fill="both", padx=5, pady=5)

        # ======= Слои ==========
        self.layers_frame = ctk.CTkFrame(self.right_panel, width=300, height=300)
        self.layers_frame.pack(side = "bottom", fill="both", padx=5, pady=5)

        self.remove_button = ctk.CTkButton(self.layers_frame, text="Delete Layer", command=self.func.remove_layer)
        self.remove_button.pack(side="bottom", pady=10)

        # Место для кнопки сохранения изменений
        self.but_save = ctk.CTkFrame(self.right_panel, width=300, height=300)
        self.but_save.pack(side = "bottom", fill="both", padx=10, pady=5)

        self.btn_save = ctk.CTkButton(self.but_save, text="💾 Save changes", command=self.func.save_difference)
        self.btn_save.pack(side="bottom", fill = "x", padx=5, pady=5)

        # Слои        
        self.layers_label = ctk.CTkLabel(self.layers_frame, text="🖼 Layers")
        self.layers_label.pack(pady=5)

        # Заменяем Listbox на ScrollableFrame
        self.layers_list = ctk.CTkScrollableFrame(self.layers_frame, height=200)
        self.layers_list.pack(fill="both", expand=True, padx=5, pady=5)

        self.mult_work = ctk.CTkButton(self.but_save, text="Scripts", command=self.func.open_child_window)
        self.mult_work.pack(fill = "x", padx=5, pady=5)

        # 🔹 Список слоёв
        self.layers = []  # Хранит данные всех слоёв (изображение, кнопка)
        self.current_layer = None
        
        # Кнопки движения фото в осям
        self.y_move = ctk.CTkFrame(self.canvas, fg_color="light gray", width=30)
        self.y_move.pack(side="right", fill="y")

        self.x_move = ctk.CTkFrame(self.canvas, fg_color="light gray", height=30)
        self.x_move.pack(side="bottom", fill="x")

        self.btn_left_x_move = ctk.CTkButton(
                self.x_move, 
                fg_color="#535353", 
                text="⬅️", 
                command= lambda: self.func.x_move(-15),
                width=20,
                height=20
            )
        self.btn_left_x_move.pack(side="left")

        self.btn_right_x_move = ctk.CTkButton(
                self.x_move, 
                fg_color="#535353", 
                text="➡️", 
                command= lambda: self.func.x_move(15),
                width=20,
                height=20
            )
        self.btn_right_x_move.pack(side="right")

        self.btn_top_y_move = ctk.CTkButton(
                self.y_move, 
                fg_color="#535353", 
                text="⬆️", 
                command= lambda: self.func.y_move(-15),
                width=20,
                height=20
            )
        self.btn_top_y_move.pack(side="top")

        self.btn_bottom_y_move = ctk.CTkButton(
                self.y_move, 
                fg_color="#535353", 
                text="⬇️", 
                command= lambda: self.func.y_move(15),
                width=20,
                height=20
            )
        self.btn_bottom_y_move.pack(side="bottom")

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
        self.contrast_flag = False
        self.contrast_frame = False
        self.pixel_flag = False
        self.pixel_frame = False
        self.rotation_flag = False
        self.rotation_frame = False
        self.horiz_flag = False
        self.vert_flag = False
        self.sharpen_flag = False
        self.noise_flag = False
        self.noise_frame = False

        # Добавляем виджеты и меню из `func`
        self.func.top_menu1()
        self.func.top_menu3()
        self.func.color_corrections()
        self.func.geometry_transform()
        self.func.image_effects()

    def run(self):
        self.root.mainloop()

