# 📊 GPS Data Dashboard

Este dashboard en **R Shiny** permite visualizar, filtrar y analizar datos GPS deportivos de forma interactiva. Fue diseñado para facilitar el análisis de sesiones individuales o acumuladas por jugador, posición, match day y tipo de tarea, con múltiples gráficos personalizables.

---

## 🚀 Características principales

- ✅ **Carga flexible de datos** (`.csv`, `.xlsx`, `.json`)
- ✅ Mapeo dinámico de columnas clave
- ✅ Filtros personalizados por jugador, posición, fecha, match day, tarea y duración
- ✅ Filtros independientes por cada gráfico
- ✅ Visualización de métricas en el tiempo
- ✅ Gráficos tipo boxplot por Match Day y por Tarea
- ✅ Análisis individual por sesión (por fecha)
- ✅ Gráfico de Z-score por jugador (cálculo dinámico)
- ✅ Interfaz limpia, responsive y estética (usando `shinythemes` y `bslib`)
- ✅ Exportable a shinyapps.io o servidor propio

---

## 📁 Estructura esperada del archivo de datos

El archivo cargado debe contener al menos algunas de las siguientes columnas:

- **Player** (obligatorio): Nombre o ID del jugador
- **Date** (obligatorio): Fecha de la sesión (formato flexible)
- **Metric columns**: Columnas numéricas con métricas (HSR, total distance, etc.)
- **Opcional**:
  - `Position`: Puesto del jugador
  - `Match Day`: Día relativo al partido (MD-1, MD+1, etc.)
  - `Task`: Tipo de tarea o drill
  - `Duration`: Duración directa (en minutos)
  - `Start time / End time`: Si no hay duración directa

> ⚠️ **IMPORTANTE:** Si bien algunas columnas son opcionales, para aprovechar al máximo las funcionalidades del dashboard, se recomienda que el archivo contenga al menos: jugador, fecha, duración (directa o derivada), y varias métricas numéricas.

---

## 🧪 Visualizaciones disponibles

- 📊 **Promedio de métricas por fecha y jugador**
- 📦 **Boxplot por Match Day**
- 📦 **Boxplot por tipo de Tarea**
- 📈 **Z-score por jugador con suavizado Loess**
- 🧪 **Análisis puntual de sesión con métricas múltiples**

---

## ▶️ Cómo ejecutar la app

1. Cloná el repositorio:
`bash`
git clone https://github.com/tu_usuario/gps-dashboard.gi

2.	Instalá los paquetes requeridos en R:

     install.packages(c("shiny", "readr", "readxl", "jsonlite", "DT", "ggplot2", 
                   "plotly", "dplyr", "lubridate", "hms", "bslib", 
                   "shinythemes", "rsconnect", "slider", "shinyWidgets", "tidyr"))
       
3.	Ejecutá la app localmente desde app.R o desde consola: 

    shiny::runApp()
    
4.	Para desplegar en shinyapps.io:

    rsconnect::deployApp()

👨‍💻 Autor

Leandro Carbone – @licleandrocarbone
Performance Specialist - Sports Scientist
⸻

📎 Licencia

MIT License. Usalo libremente y adaptalo a tus necesidades ⚙️