# GPS Dashboard - Shiny App

Este proyecto es una aplicación interactiva construida en R con Shiny, diseñada para visualizar, filtrar y analizar datos GPS deportivos. La aplicación permite cargar archivos CSV, Excel o JSON, asignar columnas clave, aplicar filtros dinámicos, y generar visualizaciones como gráficos de barras, boxplots y Z-score.

---

## Requisitos

- R >= 4.2.0
- Paquetes:
  - `shiny`, `plotly`, `ggplot2`, `readr`, `readxl`, `jsonlite`, `dplyr`, `lubridate`, `hms`, `bslib`, `shinythemes`, `DT`, `slider`, `rsconnect`

Instalalos con:

```r
install.packages(c(
  "shiny", "plotly", "ggplot2", "readr", "readxl", "jsonlite", "dplyr",
  "lubridate", "hms", "bslib", "shinythemes", "DT", "slider", "rsconnect"
))
```

---

## Uso local

1. Cloná este repositorio o descargalo.
2. Abrí el archivo `app.R` en RStudio.
3. Ejecutá la app con:

```r
shiny::runApp()
```

---

## Deploy en ShinyApps.io

### 1. Instalá y configurá `rsconnect`

```r
install.packages("rsconnect")

library(rsconnect)

# Configurá tus credenciales (una sola vez)
rsconnect::setAccountInfo(
  name = "tu_usuario_shinyapps",
  token = "TU_TOKEN",
  secret = "TU_SECRET"
)
```

> Podés encontrar tu `name`, `token` y `secret` desde [tu cuenta en shinyapps.io](https://www.shinyapps.io/admin/#/tokens).

---

### 2. Deploy de la app

Si estás trabajando desde RStudio o tu consola R, corré:

```r
rsconnect::deployApp("ruta/a/la/carpeta_de_tu_app")
```

Por ejemplo, si tu app está en `~/gps-dashboard`, usá:

```r
rsconnect::deployApp("~/gps-dashboard")
```

O si estás ubicado dentro del proyecto:

```r
rsconnect::deployApp()
```

---

### 3. Listo! 🎉

Recibirás la URL donde estará publicada tu app.

---

## Autor

Leandro Carbone  
[@comunidadlift](https://comunidadlift.com)

---

## Licencia

MIT License. Libre de usar, modificar y distribuir.

