#!/bin/bash

echo "🚀 Запуск сборки проекта..."

# Проверка наличия необходимых инструментов
if ! command -v pip &> /dev/null; then
    echo "❌ pip не установлен. Установите pip и повторите попытку."
    exit 1
fi

if ! command -v stack &> /dev/null; then
    echo "❌ stack (Haskell) не установлен. Установите stack и повторите попытку."
    exit 1
fi

# Проверка и создание временных директорий
mkdir -p temp/move/input temp/move/output
mkdir -p temp/multiplProcess/input temp/multiplProcess/output
mkdir -p temp/inputPath temp/outputPath

echo "📂 Проверка и создание временных директорий завершена."

# Сборка backend-а (Haskell)
echo "🔧 Сборка Haskell проекта..."
cd backend_Erbol || exit
stack build || { echo "❌ Ошибка сборки Haskell проекта"; exit 1; }
cd ..

# Установка зависимостей для Python (фронтенд)
echo "🐍 Проверка и установка зависимостей Python..."
cd frontend_Fedya || exit
pip install -r requirements.txt || { echo "❌ Ошибка установки зависимостей Python"; exit 1; }
cd ..

echo "✅ Сборка завершена!"