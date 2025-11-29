# Ev1-reproducibilidad
Evaluación Sumativa N°1 – Reproducibilidad y comunicación de resultados

# Ev1 – Informe reproducible sobre consumo de alcohol

Este repositorio contiene el material de la Evaluación 1 del diplomado, cuyo objetivo es elaborar un informe **reproducible y replicable**, en este caso, sobre patrones de consumo de alcohol en población adulta, usando R y Quarto.

## Contenidos del repositorio

- `Ev1_NGD.qmd`: archivo Quarto con el código, las tablas y los gráficos del informe.
- `Ev1_NGD.html`: informe final renderizado en formato HTML.
- `senda_final.rds`: base de datos utilizada en el análisis.

## Notas sobre datos y replicabilidad

- Todas las recodificaciones de variables (edad, macrozonas, categorías de consumo) están documentadas en el archivo `.qmd`.
- El informe fija una semilla (`set.seed()`) para garantizar que cualquier procedimiento aleatorio produzca los mismos resultados al ser replicado.
