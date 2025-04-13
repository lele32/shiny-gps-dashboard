# 📊 GPS Data Dashboard

Este dashboard en **R Shiny** permite visualizar, filtrar y analizar datos GPS deportivos de forma interactiva. Fue diseñado para facilitar el análisis de sesiones individuales o acumuladas por jugador, posición, match day y tipo de tarea, con múltiples gráficos personalizables.

---

## 📂 Funcionalidades actuales

- **Carga de archivos**: permite cargar bases en formato `.csv`, `.xlsx` o `.json`.
- **Mapeo flexible de columnas**: podés seleccionar qué columnas corresponden a:
  - Jugador
  - Puesto
  - Fecha
  - Match Day
  - Tarea
  - Duración (columna directa o calculada)
  - Métricas (permite múltiples)
- **Filtros dinámicos** por:
  - Jugador
  - Puesto
  - Match Day
  - Tarea
  - Rango de fechas
  - Duración de sesión
  - Valores de cada métrica

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

## 📊 Visualizaciones disponibles

1. **📅 Métrica en el tiempo**
   - Promedio por jugador y fecha
   - Gráfico de barras interactivas (`plotly`)
   - Un gráfico por cada métrica seleccionada

2. **📦 Distribución por Match Day**
   - Boxplots por MD para cada métrica
   - Visualiza la variabilidad entre jugadores

3. **🧪 Distribución por Tarea**
   - Boxplots por tipo de tarea
   - Útil para diferenciar cargas en entrenamientos y partidos

4. **📈 Z-score por Fecha**
   - Z-score por jugador con media y SD global (sin incluir el valor actual)
   - Smoothed trend (`loess`), áreas sombreadas ±1.5 Z

5. **📋 Análisis de sesión**
   - Filtros para seleccionar una sesión puntual
   - Gráfico de barras ordenadas por jugador con media y ±1 / ±2 SD
   - Filtros independientes por cada métrica

6. **📉 Z-score competitivo**
   - Compara el partido actual con los 3–5 partidos previos
   - Cálculo por jugador, incluye tabla resumen con media, SD y Z-score

---

## ⚙️ Próximas funcionalidades (en desarrollo)

- Exportación automática de informes por sesión y jugador
- Guardado persistente de base de datos
- Nuevas visualizaciones:
  - ACWR (Acute-Chronic Work Ratio)
  - KPI dashboards
  - Relaciones entre métricas
- Parámetros de referencia personalizados

---

## 🧠 Requisitos

- R (versión reciente)
- Paquetes: `shiny`, `ggplot2`, `plotly`, `dplyr`, `lubridate`, `DT`, `readxl`, `readr`, `jsonlite`

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