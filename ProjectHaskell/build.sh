#!/bin/bash

echo "🚀 Запуск сборки проекта..."

if ! command -v stack &> /dev/null; then
    echo "❌ stack (Haskell) не установлен. Установите stack и повторите попытку."
    exit 1
fi


echo "📂 Проверка и создание временных директорий завершена."

# Сборка backend-а (Haskell)
echo "🔧 Сборка Haskell проекта..."
cd backend_Erbol || exit
stack build || { echo "❌ Ошибка сборки Haskell проекта"; exit 1; }
cd ..

# Установка зависимостей для Python (фронтенд)
echo "🐍 Проверка и установка зависимостей Python..."
cd frontend_Fedya || exit
python install -r requirements.txt
python install customtkinter
cd ..

echo "Создание папок хранения данных..."
mkdir -p HaskProj
mkdir -p HaskProj/temp
mkdir -p HaskProj/temp/inputPath
mkdir -p HaskProj/temp/outputPath
mkdir -p HaskProj/temp/multiplProcess/input
mkdir -p HaskProj/temp/multiplProcess/output
mkdir -p HaskProj/temp/move/input
mkdir -p HaskProj/temp/move/output
echo "Создание папок успешно законченно"

echo "✅ Сборка завершена!"
