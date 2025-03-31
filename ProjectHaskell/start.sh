#!/bin/bash

echo "🚀 Запуск проекта..."

# Запуск frontend-а (Python)
echo "🐍 Запуск frontend (Python)..."
cd frontend_Fedya || exit
python main.py  # Запуск в фоне
FRONTEND_PID=$!  # Сохраняем PID процесса

# Ожидание завершения frontend-процесса
wait $FRONTEND_PID
