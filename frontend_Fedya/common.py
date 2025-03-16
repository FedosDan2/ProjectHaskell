import os

class ImageProcces_and_TopMenu:
    def __init__(self, window):
        self.window = window
        self.layers = []
        self.images = []
        self.current_layer = None
        self.buttons_created = False
        self.mult_process_on = False

    def delete_file(self, file_path):
        """Удаляет файл, если он существует."""
        if os.path.exists(file_path):
            os.remove(file_path)