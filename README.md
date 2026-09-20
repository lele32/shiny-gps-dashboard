# 📊 GPS Data Dashboard

This R Shiny dashboard allows for interactive visualization, filtering, and analysis of sports GPS data. It is designed to facilitate the analysis of individual or accumulated sessions by player, position, match day, and task type, with multiple customizable plots and a modern dark aesthetic inspired by Visual Studio Code.

---

## ⚙️ Current Features

- **Cumulative data upload**:
  - Upload multiple`.csv`, `.xlsx`, `.json` files progressively.
  - Support for pasting **Google Sheets** links or IDs, loaded explicitly with a button.
  - Automatic detection of delimiter, metadata rows, and decimal comma in CSV exports.
  - Provider/source metadata retained for Catapult, WIMU, Stats Sports, and unknown exports.

- **Persistent in-memory database:**:
  -	Uploaded sessions are accumulated without overwriting previous data.
  - Only exact duplicate rows are removed; different tasks/sessions on the same day are preserved.

- **Reset button**:
  - Clears the database and resets all data upload inputs.

- **Flexible column mapping**: you can select which columns correspond to:
  •	Player
	•	Position
	•	Date
	•	Match Day
	•	Task
	•	Duration (direct column or calculated from start and end time)
	•	Metrics (multiple numeric metrics allowed)

The importer also accepts numeric-looking text columns, including values with a decimal comma.

- **Dynamic per-plot filters**:
  •	Player
	•	Position
	•	Match Day
	•	Task
	•	Date (range)
	•	Session duration (range)
	•	Value range per metric

- **Optimized visual interface:**:
  	•	Dark, minimalist aesthetic inspired by Visual Studio Code and glassmorphism.
	•	Custom LIFT color palette:
	•	Background: deep black (#0E1117)
	•	Text: white (#ffffff)
	•	Primary: electric blue (#00FFFF)
	•	Secondary: violet (#7F00FF)
	•	Accents in red (#fd002b) and mint green
	•	Glass boxes: semi-transparent backgrounds with blur and soft borders
	•	Typography:
	•	Heading: Space Grotesk (Google Fonts)
	•	Body: Inter (Google Fonts)
	•	Special headers: Righteous (for main titles)
	•	Tabs with icons:
	•	Navigation using SVG iconography (Bootstrap Icons, FontAwesome) and large titles.
	•	Modern filters and selectors:
	•	Inputs, sliders, and selects with custom styling, colorful labels, borders, and adapted backgrounds.
	•	Glass effect in side panels and filter boxes.
	•	Charts and visuals:
	•	Plotly plots in dark mode, no borders, bold titles, and custom legend.
	•	Clean facet grids, with clear separation per metric and prominent names.
	•	Refined UX:
	•	Visual feedback for empty states, warnings, and informative tooltips.
	•	Responsive for different resolutions.
	•	Smooth animations and transitions during interaction.
	
---

## 📁  Expected data file structure

The uploaded file should contain at least some of the following columns:

- **Player** (required): Player name or ID
- **Date** (required): Session date (flexible format)
- **Metric columns**: Numeric columns with metrics (HSR, total distance, etc.)
- **Optional**:
  - `Position`: Player position
  - `Match Day`: Relative day to the match (MD-1, MD+1, etc.)
  - `Task`: Task or drill type
  - `Duration`: Direct duration (in minutes)
  - `Start time / End time`: If no direct duration is provided

> ⚠️ **IMPORTANTE:** To maximize all features, it is recommended to include: player, date, duration (direct or derived), and several numeric metrics..

---

 **🟦 Value Boxes and KPI Chips **

Each major visualization tab features a row of value boxes (“KPIs”) above the plot, showing quick stats per selected metric.
All value boxes include horizontal scroll when more than three metrics are selected, for clean presentation.
	•	What you see in a value box:
	•	Metric Name (centered, prominent color).
	•	Mini-KPI grid: Number of players, best/worst player and value, change vs previous session, or stat summary (mean, IQR, etc).
	•	Chips (“badges”) indicating status, such as:
	•	🔴 Red: High risk or abnormal value (e.g. Z-score > 1.5, ACWR > 1.5).
	•	🟢 Green: Low or safe value (e.g. Z-score < -1.5, ACWR < 0.8).
	•	⚪️ Grey chip: Shown when no player meets the high/low criterion (shows 0 and a dash icon).

Chips are always visible (grey with 0 if there are no cases), ensuring a consistent interface and fast at-a-glance review.

Example value box logic:
	•	ACWR:
	•	Red chip: ACWR > 1.5
	•	Green chip: ACWR < 0.8
	•	Grey chip: No players in that category
	•	Z-score (Competitive & Over Time):
	•	Red chip: Z ≥ 1.5
	•	Green chip: Z ≤ -1.5
	•	Grey chip: No players in that category

All chips are interactive: Click to see a modal listing affected players.


## 📊 Available visualizations

1. **📅 Metric over Time**
  •	Mean by player and date
	•	Bar chart (plotly)
	•	One independent chart per selected metric

2. **📦Distribution by Match Day**
   •	Boxplots by MD for each metric
	 •	Compare variability among players, position and task

3. **🧪 Distribution by Task**
	 •	Boxplots by task or drill type

4. **📈 Z-score by Date**
	•	Z-score calculation with configurable rolling window per player (excluding the current value)
	 •	Smoothed lines, automatic scaling, internal comparisons

5. **🧪  Single Session Analysis**
   •	Select session by date, player, task, or MD
	 •	Sorted bar chart by player with mean ±1/±2 SD

6. **📊 Competitive Analysis**
   •	Z-score by player in current match vs 3–5 previous matches
   •	Summary table with historical mean, current value, and Z-score

7. **📉 ACWR (Acute-Chronic Work Ratio)**
  •	Exponential calculation with adjustable parameters (acute and chronic)
	•	Visualization with risk zones and smoothed lines
	•	Automatic color code:
	  •	🔴 ACWR > 1.5
	  •	🟢 0.8 ≤ ACWR ≤ 1.5
	  •	🔴 ACWR < 0.8
     
8. **⚖️ Microcycle Analysis: Match vs Week Ratio**  
	•	Calculates the load ratio between matches and accumulated training for each player and selected metric.
	•	Canonical ratio: **Match rolling average / Training cumulative load**.
	•	Allows selection of metric, match rolling window, training days to compare, and all usual filters (player, position, task, duration, etc.).
	•	Customizable ratio thresholds per metric: adjust threshold values directly on the plot to define if the ratio is low, normal, or high for each metric, independently.
	•	Colors adapt in real time according to these thresholds:
	  •	🔴 High (e.g., Ratio > 1.5)
	  •	🟢 Low (e.g., Ratio < 0.8)
	  •	⚪ Normal (between thresholds)
	•	Interactive facet grid visualization per metric, with minimalist dark aesthetic and no visible legend for maximum visual cleanliness.
	•	Use case: quickly identify who had unusual loads comparing their most recent match (or average of recent matches) vs the sum of selected training days.

8. **🟦 Quadrant Plot**  
  •	Visualizes each player in a 2D plane defined by two selected metrics (X/Y).
	•	The chart is divided into quadrants according to the median of each metric:
	  •	High-High: both metrics above median
	  •	Low-Low: both metrics below median
	  •	High-Low and Low-High: mixed cases
	•	Each quadrant has a specific color (red, violet, green, cyan).
	•	Interactive tooltip with player name, X value, Y value, quadrant, and all relevant details.
	•	Filters for player, position, task, duration, match day, and multiple session selection.
	•	Sliders to filter value ranges for each metric.
	•	Glassmorphism dark aesthetic, with animation and custom tooltips.
	•	Useful to visualize the load or performance profile of each player according to different metrics.

---

## 🔜 Upcoming features (in development)

- 🧾 **PDF Report Export**
	•	By player, session, or period

- 🧩 **Exploration of new widgets**
  - Alternatives like `echarts4r`, `highcharter`, `shinyWidgets`, etc.

---

## 🧠 Requirements

- **Lenguage**: R (Latest Version)
- **Required Packages**:

r
install.packages(c("shiny", "readr", "readxl", "jsonlite", "DT", "ggplot2", 
                   "plotly", "dplyr", "lubridate", "hms", "bslib", 
                   "shinythemes", "rsconnect", "slider", "shinyWidgets", "tidyr"))

---

## ▶️ How to run the app

1. Clone the repository:
`bash`
git clone https://github.com/tu_usuario/gps-dashboard.gi

2.	Install the required R packages:

     install.packages(c("shiny", "readr", "readxl", "jsonlite", "DT", "ggplot2", 
                   "plotly", "dplyr", "lubridate", "hms", "bslib", 
                   "shinythemes", "rsconnect", "slider", "shinyWidgets", "tidyr"))
       
3.	Run the app locally from app.R or from console: 

    shiny::runApp()

5. Run the helper tests:

    Rscript -e "testthat::test_dir('tests/testthat', reporter = 'progress')"
    
4.	To deploy on shinyapps.io:

    rsconnect::deployApp()

👨‍💻 Author

Leandro Carbone – @licleandrocarbone
Performance Specialist - Sports Scientist
⸻

📎 License

MIT License. Use and adapt freely for your needs ⚙️
