# Predicción de accidente cerebrovascular con Random Forest

Modelo de clasificación en **R** que predice la probabilidad de que un paciente sufra un accidente cerebrovascular (*stroke*) a partir de variables clínicas y demográficas.

El dataset está **muy desbalanceado**: solo 249 de 5,110 registros (4.87 %) son casos positivos. Por eso el pipeline balancea las clases antes de entrenar y reporta ROC/AUC además de la exactitud.

Trabajo desarrollado para el curso de **Minería de Datos** de la Licenciatura en Ingeniería de Software (Universidad Autónoma de Zacatecas).

---

## El dataset

`stroke.csv` — 5,110 registros de pacientes con 11 atributos predictivos:

| Variable | Tipo | Descripción |
|---|---|---|
| `gender` | Categórica | Male / Female / Other |
| `age` | Numérica | Edad del paciente |
| `hypertension` | Binaria | Diagnóstico de hipertensión |
| `heart_disease` | Binaria | Cardiopatía previa |
| `ever_married` | Binaria | Ha estado casado(a) |
| `work_type` | Categórica | Tipo de empleo |
| `Residence_type` | Categórica | Urbana / Rural |
| `avg_glucose_level` | Numérica | Nivel promedio de glucosa |
| `bmi` | Numérica | Índice de masa corporal (**con valores faltantes**) |
| `smoking_status` | Categórica | Hábito de tabaquismo |
| `stroke` | **Objetivo** | 1 = sufrió un ACV, 0 = no |

---

## Metodología

El pipeline en `stroke_classification.R` sigue estas etapas:

1. **Análisis de valores faltantes** — inspección con `naniar` y visualización del patrón de ausencia.
2. **Imputación** — la variable `bmi` se imputa por la media de la columna (`impute_mean_if`), redondeada a un decimal.
3. **Codificación de variables categóricas** — conversión de niveles a índices numéricos mediante `match()` sobre vectores de niveles explícitos.
4. **Selección de características** — algoritmo **Boruta**, un wrapper sobre Random Forest que contrasta la importancia de cada variable real contra variables "sombra" aleatorias, confirmando o rechazando su relevancia estadística.
5. **Balanceo de clases** — sobremuestreo de la clase minoritaria con `ovun.sample()` del paquete **ROSE** (`method = "over"`), duplicando el conjunto hasta igualar las clases antes de entrenar.
6. **Entrenamiento** — **Random Forest** a través de `caret` (`method = "rf"`), con partición 80/20 mediante `createDataPartition`.
7. **Evaluación** — matriz de confusión con sensibilidad y especificidad (`caret`), y **curva ROC / AUC** con `pROC`, calculadas tanto sobre entrenamiento como sobre prueba.

---

## Estructura del repositorio

```
.
├── stroke_classification.R   # Pipeline completo: EDA, imputación, Boruta, ROSE, Random Forest, ROC
├── stroke.csv                # Dataset de 5,110 pacientes
└── README.md
```

---

## Cómo ejecutarlo

Requiere **R 4.x**. Instalar las dependencias:

```r
install.packages(c(
  "naniar", "caret", "magrittr", "pROC",
  "dplyr", "ggplot2", "Boruta", "randomForest", "ROSE"
))
```

Ejecutar el script desde el directorio del repositorio:

```r
source("stroke_classification.R")
```

> El script original fija la ruta de trabajo con `setwd()`. Al clonar el repositorio, comenta esa línea o ajústala a tu propio directorio; `stroke.csv` se lee de forma relativa.

---

## Stack

`R` · `randomForest` · `caret` · `Boruta` · `ROSE` · `pROC` · `naniar` · `ggplot2` · `dplyr`

---

## Autor

**Adalberto Cerrillo Vázquez** — Ingeniería de Software, Universidad Autónoma de Zacatecas.
