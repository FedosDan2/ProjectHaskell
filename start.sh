#!/bin/bash

echo "🚀 Запуск проекта..."

# Запуск backend-а (Haskell)
echo "🔧 Запуск backend (Haskell)..."
cd backend_Erbol || exit
stack exec HaskProj &  # Запуск в фоне
BACKEND_PID=$!  # Сохраняем PID процесса
cd ..

# Запуск frontend-а (Python)
echo "🐍 Запуск frontend (Python)..."
cd frontend_Fedya || exit
python starter.py &  # Запуск в фоне
FRONTEND_PID=$!  # Сохраняем PID процесса
cd ..

echo "✅ Проект запущен!"

# Ожидание завершения процессов (опционально)
wait $BACKEND_PID
wait $FRONTEND_PID