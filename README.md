# TFM2024

## Archivos de python
- El archivo "TFM Scraping.ipynb" recoge todos los datos de los barcos publicados en el portal de venta, con la excepción de la variable "price".
- El archivo "TFM Scraping2.ipynb" recoge el nombre de los barcos y la variable "price".

En el Trabajo de Fin de Máster se utilizan los datos que fueron recopilados mediante la ejecución de ambos scripts durante el día 24 de enero del 2024. Además, todas las observaciones y variables se guardaron respectivamente en los archivos "Barcos_bruto.csv" y "Barcos_precios.csv".

## Archivos de R
- El archivo "Preprocesado dataset.R" carga los archivos "Barcos_bruto.csv" y "Barcos_precios.csv" para corregir errores de codificación, renombrar las variables, eliminar variables duplicadas, convertir los valores de las variables "price" y "charter" a euros de acuerdo con los tipos de cambio medios de enero de 2024 y, por encima de todo, unificar los 2 conjuntos de datos en uno llamado "Barcos.csv".
- El archivo "Exploracion y Modificacion dataset.R" desarrolla las 3 primeras fases de la metodología SEMMA (aunque no se hace muestreo por contar con 695 observaciones), lo cual incluye un proceso de imputación de ausentes, tratamiento de outliers, recategorización de variables cualitativas y normalización por rango de variables numéricas. El resultado de este proceso se guarda en el archivo "Barcos2.csv".
- El archivo "Modelizacion y Evaluacion dataset.R" carga el dataset de los barcos con todas las modificaciones hechas y crea 9 modelos de machine learning distintos para valorar su rendimiento y capacidad predictiva en validación cruzada repetida. Más adelante se prueban modelos ensamblados mediante "stacking" y se busca obtener una interpretación de las relaciones entre las variables predictoras y la variable objetivo en los modelos de regresión lineal, árbol de decisión y random forest.

## Otras observaciones
Una parte de la exploración (tanto analítica como gráfica) se ha llevado a cabo mediante SAS Enterprise Miner 14.1, pero estos resultados no han podido ser compartidos en este repositorio, si bien es cierto que tanto los resultados como las capturas de pantalla explicativas del proceso se hallan presentes en el TFM.
