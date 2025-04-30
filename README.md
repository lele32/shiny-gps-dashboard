# 📊 GPS Data Dashboard

Este dashboard en **R Shiny** permite visualizar, filtrar y analizar datos GPS deportivos de forma interactiva. Fue diseñado para facilitar el análisis de sesiones individuales o acumuladas por jugador, posición, match day y tipo de tarea, con múltiples gráficos personalizables y estética inspirada en Visual Studio Code.

---

## ⚙️ Funcionalidades actuales

- **Carga acumulativa de datos**:
  - Carga múltiples archivos `.csv`, `.xlsx`, `.json` de forma progresiva.
  - Soporte para pegar links de **Google Sheets** como fuente directa.
  - Detección automática del delimitador y encabezado.

- **Base de datos persistente en memoria**:
  - Se acumulan las sesiones cargadas sin sobrescribir datos anteriores.
  - Prevención automática de duplicados por `Jugador + Fecha`.

- **Botón de reset**:
  - Limpia la base de datos y resetea todos los inputs de carga.

- **Mapeo flexible de columnas**: podés seleccionar qué columnas corresponden a:
  - Jugador
  - Puesto
  - Fecha
  - Match Day
  - Tarea
  - Duración (columna directa o calculada desde hora de inicio y fin)
  - Métricas (permite múltiples métricas numéricas)

- **Filtros dinámicos por gráfico**:
  - Jugador
  - Puesto
  - Match Day
  - Tarea
  - Fecha (rango)
  - Duración de sesión (rango)
  - Rango de valores por métrica

- **Interfaz visual optimizada**:
  - Estética minimalista y funcional (paleta LIFT: rojo, gris, negro).
  - Tipografías personalizadas (Righteous, Inter).
  - Tabs con íconos estilo Visual Studio Code.
  - `selectInput` y `selectizeInput` totalmente estilizados.

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

> ⚠️ **IMPORTANTE:** Para aprovechar al máximo las funcionalidades, se recomienda incluir: jugador, fecha, duración (directa o derivada), y varias métricas numéricas.

---

## 📊 Visualizaciones disponibles

1. **📅 Métrica en el tiempo**
   - Promedio por jugador y fecha
   - Gráfico de barras (`plotly`)
   - Un gráfico independiente por métrica seleccionada

2. **📦 Distribución por Match Day**
   - Boxplots por MD para cada métrica
   - Compara variabilidad entre jugadores

3. **🧪 Distribución por Tarea**
   - Boxplots por tipo de tarea o drill

4. **📈 Z-score por Fecha**
   - Cálculo de Z-score con ventana móvil por jugador (sin incluir el valor actual)
   - Líneas suavizadas, escalado automático, comparaciones internas

5. **🧪 Análisis de sesión puntual**
   - Selección de sesión por fecha, jugador, tarea o MD
   - Gráfico ordenado por jugador con media ±1/±2 SD

6. **📊 Competitive Analysis**
   - Z-score por jugador en partido actual vs 3–5 partidos anteriores
   - Tabla resumen con media histórica, valor actual y Z-score

7. **📉 ACWR (Acute-Chronic Work Ratio)**
   - Cálculo exponencial con parámetros ajustables (agudo y crónico)
   - Visualización con zonas de riesgo y líneas suavizadas
   - Código de colores automático:
     - 🔴 ACWR > 1.5
     - 🟢 0.8 ≤ ACWR ≤ 1.5
     - 🔴 ACWR < 0.8

---

## 🔜 Próximas funcionalidades (en desarrollo)

- 🧾 **Exportación de reportes PDF**
  - Por jugador, sesión o período

- 📈 **Reporte Match vs Semana**
  - Ratio entre cargas de partido y media semanal

- 💡 **Panel de KPIs**
  - Indicadores clave por sesión o jugador (Player Load, HSR, etc.)

- 🎯 **Tarjetas informativas por tab**
  - Uso de `valueBox` para mostrar indicadores clave al ingresar a cada visualización

- 🧩 **Exploración de nuevos widgets**
  - Alternativas como `echarts4r`, `highcharter`, `shinyWidgets`, etc.

---

## 🧠 Requisitos

- **Lenguaje**: R (versión reciente)
- **Paquetes requeridos**:

r
install.packages(c("shiny", "readr", "readxl", "jsonlite", "DT", "ggplot2", 
                   "plotly", "dplyr", "lubridate", "hms", "bslib", 
                   "shinythemes", "rsconnect", "slider", "shinyWidgets", "tidyr"))

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