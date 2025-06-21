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
  - Estética oscura y minimalista, inspirada en Visual Studio Code y glassmorphism.
	-	Paleta LIFT personalizada:
 	  -	Fondo: negro profundo (#0E1117)
	  -	Texto: blanco (#ffffff)
	  -	Primario: azul eléctrico (#00FFFF)
	  -	Secundario: violeta (#7F00FF)
	  -	Detalles en rojo (#fd002b) y verde menta
	  -	Glass boxes: fondos semitranslúcidos con desenfoque y bordes suaves.
	-	Tipografías:
	  -	Heading: Space Grotesk (Google Fonts)
	  -	Body: Inter (Google Fonts)
	  -	Títulos especiales: Righteous (en headers principales)
	-	Tabs con íconos:
	  -	Navegación con iconografía SVG (Bootstrap Icons, FontAwesome) y títulos grandes.
	-	Filtros y selectores modernos:
	  -	Inputs, sliders y selects estilizados a medida, con labels coloridos, bordes y backgrounds adaptados.
	  -	Efecto glass en paneles laterales y cajas de filtro.
	-	Gráficos y visuales:
	  -	Gráficos plotly en modo oscuro, sin bordes, títulos llamativos y leyenda custom.
	  -	Facet grids limpios, con separación clara por métrica y nombres destacados.
	-	UX refinada:
	  -	Feedback visual para estados vacíos, advertencias y tooltips informativos.
	  -	Responsive para diferentes resoluciones.
	  -	Animaciones y transiciones suaves en la interacción..

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
     
8. **⚖️ Análisis de Microciclo: Ratio Partido vs Semana**  
     - 	Calcula el ratio de carga entre partidos y entrenamientos acumulados, para cada jugador y cada métrica seleccionada.
	   - 	Permite seleccionar la métrica, la ventana móvil de partidos (rolling), los días de entrenamiento a comparar, y todos los filtros habituales (jugador, puesto, tarea, duración, etc.).
	   - 	Umbrales de ratio personalizables por métrica: podés ajustar desde el propio gráfico los valores de corte para definir si el ratio es bajo, normal o alto en cada métrica, de forma totalmente independiente.
	   - 	Los colores se adaptan en tiempo real según estos umbrales:
	      -	🔴 Alto (ej: Ratio > 1.5)
	      -	🟢 Bajo (ej: Ratio < 0.8)
	      -	⚪ Normal (entre umbrales)
	   - 	Visualización interactiva tipo facet grid por métrica, con estética oscura minimalista y sin leyenda visible para máxima limpieza visual.
	   - 	Use case: identificar rápidamente quiénes tuvieron cargas inusuales comparando su partido más reciente (o promedio de partidos recientes) vs el acumulado de días seleccionados de entrenamiento.

---

## 🔜 Próximas funcionalidades (en desarrollo)

- 🧾 **Exportación de reportes PDF**
  - Por jugador, sesión o período

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