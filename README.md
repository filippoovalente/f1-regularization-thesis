# Regularisation and Variable Selection in Linear Models: An F1 Qualifying Telemetry Case Study

## 🏎️ Project Description

This repository contains the code and data behind a bachelor's thesis on **regularisation and variable selection in linear models**, applied to a real-world Formula 1 qualifying telemetry dataset.

The goal is to compare classical Ordinary Least Squares (OLS) against penalised regression methods — **Ridge, LASSO, Relaxed LASSO and Elastic Net** — when predicting a driver's qualifying lap time from aggregated telemetry, and to study what each method offers in terms of accuracy, stability and parsimony.

### Research Question

**Do penalised regression methods improve predictive performance over OLS on a hierarchically structured telemetry dataset — and at what cost in complexity?**

The short answer that emerges from the analysis: on this dataset all methods cluster around the same predictive accuracy, so the value of penalisation lies in **stability and parsimony** rather than raw accuracy gains.

---

## 🎯 Objectives

1. **Data extraction**: Build a multi-season telemetry dataset directly from the FastF1 / Jolpica APIs via the `f1dataR` package
2. **Exploratory analysis**: Inspect distributions, correlations and collinearity among the telemetric predictors
3. **Model comparison**: Fit OLS, Ridge, LASSO, Relaxed LASSO and Elastic Net under a consistent cross-validation protocol
4. **Validation design**: Use a nested cross-validation pipeline with folds stratified by driver to respect the hierarchical structure of the data
5. **Interpretation**: Compare models on RMSE, on the train–validation gap, and on the number of active predictors

---

## 📁 Dataset

### Source

The dataset is built from **Formula 1 qualifying sessions across the 2019, 2020 and 2021 seasons**, extracted through:

- **`f1dataR`** — R interface to F1 timing and telemetry data
- **FastF1** — the underlying Python library (accessed via `reticulate`)
- **Jolpica API** — successor to the (now retired) Ergast API, with a local fallback list of tracks and drivers in case the API is offline

Telemetry is aggregated to **one row per driver per qualifying session**, giving ~1,183 observations.

### Target variable

- `final_time`: the qualifying lap time (in seconds) — the quantity being predicted

### Predictors

Aggregated telemetry per lap, including:

- **Speed**: `speed_mean`, `speed_max`, `speed_min`, `speed_std`
- **Engine**: `rpm_mean`, `rpm_max`, `rpm_std`
- **Throttle**: `throttle_mean`, `throttle_std`, `throttle_full_ratio`
- **Braking**: `brake_ratio`
- **Gears**: `gear_mean`, `gear_std`
- **Identifiers used as factors**: `name` (driver) and `season`

### Variables excluded from modelling

A few columns are dropped before fitting because they are identifiers, redundant or structurally collinear:

- `session` (constant — only qualifying)
- `round` (track identifier, not a performance signal)
- `distance_total` (track length proxy)
- `speed_range` (collinear with the other speed variables — its inclusion caused VIF aliasing)

### Data cleaning

- **Row 853** is removed: an anomalous observation with `speed_range = 0` and all telemetry channels at zero (a corrupted/empty extraction)

---

## 🔍 Methodology

### Validation design

- **75/25 train–test split** (`set.seed(100)` for reproducibility)
- **Nested cross-validation**: a **10-fold external CV stratified by driver** to estimate generalisation, with a **5-fold internal CV** for selecting the penalty parameter λ
- Stratification by driver matters because observations from the same driver are correlated — a plain random split would leak that structure

### Models compared

| Model | glmnet `alpha` | Penalty |
|-------|----------------|---------|
| **OLS** | — | none (baseline `lm`) |
| **Ridge** | 0 | L2 |
| **LASSO** | 1 | L1 |
| **Relaxed LASSO** | 1 (`relax = TRUE`) | L1 with de-biasing |
| **Elastic Net** | 0.5 | L1 + L2 |

For the penalised models, λ is chosen with the **one-standard-error rule** (`lambda.1se`) as well as `lambda.min`, and the trade-off between the two is examined.

### Evaluation

- **RMSE** on training and validation folds
- **Train–validation gap** as a measure of overfitting / stability
- **Number of active (non-zero) coefficients** as a measure of parsimony

---

## 📈 Main Findings

- All models cluster at roughly the **same predictive accuracy** (~37% of variance explained), so penalisation does **not** beat OLS on raw accuracy here
- Penalised methods instead deliver **more stable** estimates and, for LASSO-type methods, a **more parsimonious** model
- The **Ridge `1se`** specification is the weakest performer in this setting
- The dataset's **hierarchical structure** (repeated measurements per driver) and **intra-driver correlation** are key limitations, pointing towards mixed models or group LASSO as natural extensions

---

## 🛠️ Tools and Technologies

### Language

- **R** (primary), with **Python** under the hood for telemetry extraction via `reticulate` + FastF1

### Libraries

```r
# Data extraction
library(f1dataR)
library(reticulate)

# Data manipulation & visualisation
library(tidyverse)
library(ggcorrplot)
library(patchwork)
library(scales)

# Modelling & diagnostics
library(glmnet)
library(car)
```

---

## 📂 File Structure

```
├── README.md                  # This file
├── f1_data.R                  # Telemetry extraction & aggregation (multi-season)
├── Script_Esplorativa.R       # Exploratory data analysis
├── Script_Modelli.R           # Model comparison: OLS, Ridge, LASSO, Relaxed LASSO, Elastic Net
└── DATI_COMPLETI.csv          # Aggregated telemetry dataset (~1,183 obs, 20 variables)
```

---

## 🚀 How to Reproduce

### 1. Clone the repository

```bash
git clone https://github.com/<your-username>/<repo-name>.git
cd <repo-name>
```

### 2. Install the dependencies

```r
packages <- c("f1dataR", "reticulate", "tidyverse",
              "ggcorrplot", "patchwork", "scales", "glmnet", "car")

install.packages(packages)
```

`f1dataR` relies on the Python **FastF1** library. The first time, run `f1dataR::setup_fastf1()` to install it.

### 3. Reproduce the analysis

```r
# Option A — rebuild the dataset from the APIs (slow, needs internet):
#   run f1_data.R   (creates the aggregated telemetry table)

# Option B — start from the provided dataset (recommended):
data <- read.csv("DATI_COMPLETI.csv", header = TRUE)

# Then run:
#   Script_Esplorativa.R   (EDA)
#   Script_Modelli.R       (model comparison)
```

> **Note**: `Script_Modelli.R` currently reads the file as `"DATI COMPLETI.csv"` (with a space), while the file in this repo is `DATI_COMPLETI.csv` (with an underscore). Either rename the file or update the `read.csv()` line so the two match.

---

## 📚 References

1. James, G., Witten, D., Hastie, T. & Tibshirani, R. (2021). *An Introduction to Statistical Learning: with Applications in R*. Springer, 2nd ed.
2. Hastie, T., Tibshirani, R. & Friedman, J. (2009). *The Elements of Statistical Learning*. Springer, 2nd ed.
3. Tibshirani, R. (1996). Regression Shrinkage and Selection via the Lasso. *Journal of the Royal Statistical Society B*, 58(1), 267–288.
4. Meinshausen, N. (2007). Relaxed Lasso. *Computational Statistics & Data Analysis*, 52(1), 374–393.
5. Zou, H. & Hastie, T. (2005). Regularization and Variable Selection via the Elastic Net. *Journal of the Royal Statistical Society B*, 67(2), 301–320.
6. Friedman, J., Hastie, T. & Tibshirani, R. (2010). Regularization Paths for Generalized Linear Models via Coordinate Descent. *Journal of Statistical Software*, 33(1), 1–22. (glmnet)

---

## 📋 License

This project is released under the **MIT** license. See the `LICENSE` file for details.

---

## 👤 Author

- Filippo Valente

Bachelor's thesis in Scienze Statistiche ed Economiche — Università degli Studi di Milano-Bicocca

---

**Data**: F1 qualifying telemetry, 2019–2021 seasons (FastF1 / Jolpica via f1dataR)
