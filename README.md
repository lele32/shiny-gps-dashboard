# GPS LIFT

Workspace local de Comunidad LIFT para importar exports de Catapult, WIMU y Stats Sports, mapear su contexto y convertirlos en decisiones operativas para sports science.

## Qué cambió en la arquitectura

La app ya no mantiene dos copias monolíticas de la lógica. El runtime está separado en módulos:

- `R/gps_helpers.R`: lectura de archivos, normalización, fechas, duración, proveedor y candidatos de métricas.
- `R/gps_semantics.R`: contexto canónico, filtros, agregaciones, tendencias, z-score, EWMA/ACWR, microciclo y perfiles.
- `R/lift_ui.R`: workspace de una sola vista activa, panel lateral persistente, navegación móvil y estados vacíos.
- `R/lift_server.R`: importación, mapeo, reactivos, tablas y gráficos.
- `www/lift-theme.css`: sistema visual LIFT en negro, rojo, blanco y gris industrial.
- `app.R`: entrypoint único. `beta.R` es un alias compatible del mismo runtime.

Los datos permanecen en memoria durante la sesión. Se eliminan únicamente filas completamente idénticas al acumular fuentes y se conserva el origen de cada export.

## Flujo de trabajo

1. **Importar**: CSV, XLSX, JSON o una hoja de Google Sheets exportable como CSV.
2. **Mapear**: jugador, fecha, tipo de sesión, match day, tarea, posición y duración directa o derivada entre hora de inicio y fin. El botón **Aplicar mapeo y abrir Overview** confirma el contrato que usan todas las vistas.
3. **Overview**: volumen visible, jugadores, fechas, unidad, agregación y señales descriptivas para revisar.
4. **Análisis**:
   - **Tendencia**: evolución diaria por tipo de sesión.
   - **Match day**: distribución, mediana y rango intercuartílico.
   - **Tareas**: carga por drill o estímulo.
   - **Sesión**: informe operativo por jugador para una fecha.
   - **Readiness**: desvío respecto de historia previa y relación aguda/crónica; no es diagnóstico.
   - **Microciclo**: partido promedio frente a entrenamiento acumulado previo.
   - **Perfil**: relación entre dos métricas dividida por medianas de la sesión.
5. **Datos**: trazabilidad de la base activa y descarga CSV de lo visible.

## Unidades y agregación

Las unidades se infieren desde el nombre de la métrica: metros, segundos, minutos, conteos y km/h. Las métricas de volumen se suman por jugador y fecha; las métricas de intensidad se promedian. Si el nombre no permite inferir una unidad, se utiliza una media descriptiva y la unidad queda marcada como `unknown`.

Los exports tabulares con columnas como `Player`, `Date`, `Session`, `Match Day`, `Selection`, `Start hour` y `Final Hour` se detectan automáticamente. Las horas y columnas de calendario no se ofrecen como métricas de trabajo por defecto; el selector prioriza distancia, distancia explosiva, HIBD, HMLD y otras señales de carga.

El z-score usa únicamente sesiones anteriores del jugador. ACWR utiliza EWMA con medias vidas configurables y fechas reales —los días sin registro no se tratan como sesiones consecutivas— y muestra bandas descriptivas. Ambas lecturas requieren contexto de disponibilidad, contenido y conversación del staff.

## Requisitos

R reciente con estos paquetes:

```r
install.packages(c(
  "shiny", "readr", "readxl", "jsonlite", "DT", "ggplot2", "plotly",
  "dplyr", "lubridate", "slider", "bslib", "base64enc", "testthat"
))
```

## Ejecutar localmente

Desde la raíz del proyecto:

```r
shiny::runApp()
```

O desde shell:

```bash
Rscript -e 'shiny::runApp(".", launch.browser = TRUE)'
```

Si RStudio conserva una instancia anterior, detené la sesión y ejecutá **Run App** nuevamente para que cargue el nuevo `app.R`, CSS y módulos.

## Verificación

```bash
Rscript -e 'testthat::test_dir("tests/testthat", reporter = "progress")'
```

Las pruebas cubren parsing, delimitadores, números con coma decimal, detección de proveedor, deduplicación, contexto canónico, agregaciones, z-score, ACWR, microciclo y perfiles.

## Estado de deploy

El deploy está cancelado. Esta refactorización se valida en local; no se ejecuta ningún `rsconnect::deployApp()` desde `app.R` ni desde `beta.R`.

## Licencia

MIT License. Leandro Carbone — Performance Specialist / Sports Scientist.
