---
layout: post
title: "Complex samples in R and Python: survey and svy side by side"
subtitle: "With the ICT Households 2025 microdata"
excerpt: "The same complex sample analysis run with the `survey` package (R) and the `svy` library (Python), to check whether estimates and standard errors match and, when they don't, why."
date: 2026-04-24 09:25:00 -0300
categories: [statistics]
tags: [survey, R, Python]
# image: /assets/images/your-banner-here.jpg # Uncomment to add a cover image
---

# Complex samples in R and Python: survey and svy side by side

In R, complex sample survey analysis usually goes through the [`survey`](https://cran.r-project.org/web/packages/survey/index.html) package; in Python, one option is the [`svy`](https://svylab.com/learn/notes/posts/design-based-survey-analysis-in-python/) library. This post runs the same analysis in both tools to check whether point estimates and standard errors match. More specifically, it shows where the difference in standard errors comes from and how to line the two results up.

All replication materials are in this [GitHub repository](https://github.com/thiagomeireles/survey_svy_tic_dom25).

**Data:** the microdata used in the analysis are available here: [ICT Households 2025 (SAV)](https://cetic.br/media/microdados/983/tic_domicilios_2025_domicilios_base_de_microdados_v1.0.sav).

## 1. The data
The analysis uses microdata from [**ICT Households 2025**](https://cetic.br/en/pesquisa/domicilios/), a survey conducted every year since 2005 to measure ICT access and use in Brazilian households, following ITU and UNESCO methodological standards.

Two indicators go into the comparison:
- **A1: Households with computer**, disaggregated by **REGION** (Sudeste, Nordeste, Sul, Norte, Centro-Oeste).
- **A4: Households with internet access**, disaggregated by **AREA** (Urban/Rural).

**Labels and codes:**
- **AREA**: 1 = Urban, 2 = Rural
- **REGION**: 1 = Southeast, 2 = Northeast, 3 = South, 4 = North, 5 = Center-West
- **A1/A4**: 1 = Yes, 0 = No

## 2. Implementation in R (survey)
```r
# Setup and Loading
pacman::p_load(survey, haven, here, janitor)
path_data <- here::here("02_dados", "tic_domicilios_2025_domicilios_base_de_microdados_v1.0.sav")

households_r <- haven::read_sav(path_data) |> 
  janitor::clean_names() |> 
  dplyr::mutate(dplyr::across(dplyr::any_of(c("a4", "area")), as.integer))

# Survey Design
design_households_r <- survey::svrepdesign(
  weights = ~peso, 
  repweights = "rep[0-9]+", 
  type = "bootstrap", 
  scale = 0.0065365334145709, # Version 1: Canty-Davison
  # scale = 0.0050251256,    # Version 2: Rao-Wu
  data = households_r, 
  combined.weights = TRUE
)

# Estimation for indicator A4
estimates_r <- survey::svyby(
  formula = ~as.factor(a4), 
  by = ~area, 
  design = design_households_r, 
  FUN = survey::svymean
)
```

## 3. Implementation in Python (svy)
```python
import svy
import polars as pl
import os
import math

# Loading and Processing
path_sav = os.path.join("02_dados", "tic_domicilios_2025_domicilios_base_de_microdados_v1.0.sav")
households_py = svy.io.read_sav(path_sav).with_columns([
    pl.col("A4").cast(pl.Int32),
    pl.col("AREA").cast(pl.Int32)
])

scale = 0.0065365334145709  # CETIC's published scale
R = 200  # Number of replicates

# Survey Design
rep_weights_config = svy.RepWeights(method="bootstrap", prefix="REP", n_reps=R)
design_py = svy.Design(wgt="PESO", rep_wgts=rep_weights_config)
sample_py = svy.Sample(data=households_py, design=design_py)

# Estimation
estimates_py = sample_py.estimation.prop("A4", by="AREA", method="replication")

# Adjust SE to match R
estimates_py_df = estimates_py.to_polars().with_columns(
    se_r=pl.col("se") * math.sqrt(scale * R)
)
```

## 4. Indicator A4: households with internet access, by area
The tables compare the 2025 estimates of household internet access computed in Python (`svy`) with the two bootstrap scales tested in R (`survey`).

### 4.1 Comparison: Version 1 (Canty-Davison) vs Python
Here R uses the **Canty-Davison** method.

| Area | Response (A4) | Est. R | Est. Python | SE R (v1) | SE Python | SE Diff |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **Urban** | **Yes (1)** | **0.869906** | **0.869906** | 0.004602 | 0.004602 | 0.000000 |
| Urban | No (0) | 0.128482 | 0.128482 | 0.004512 | 0.004512 | 0.000000 |
| Urban | Don't Know (97) | 0.001452 | 0.001452 | 0.000409 | 0.000409 | 0.000000 |
| **Rural** | **Yes (1)** | **0.809164** | **0.809164** | 0.008636 | 0.008636 | 0.000000 |
| Rural | No (0) | 0.188962 | 0.188962 | 0.008584 | 0.008584 | 0.000000 |
| Rural | Don't Know (97) | 0.001851 | 0.001851 | 0.000881 | 0.000881 | 0.000000 |

### 4.2 Comparison: Version 2 (Rao-Wu) vs Python
Here R uses the **Rao-Wu** method.

| Area | Response (A4) | Est. R | Est. Python | SE R (v2) | SE Python | SE Diff |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **Urban** | **Yes (1)** | **0.869906** | **0.869906** | 0.004035 | 0.004035 | 0.000000 |
| Urban | No (0) | 0.128482 | 0.128482 | 0.003956 | 0.003956 | 0.000000 |
| Urban | Don't Know (97) | 0.001452 | 0.001452 | 0.000359 | 0.000359 | 0.000000 |
| **Rural** | **Yes (1)** | **0.809164** | **0.809164** | 0.007572 | 0.007572 | 0.000000 |
| Rural | No (0) | 0.188962 | 0.188962 | 0.007526 | 0.007526 | 0.000000 |
| Rural | Don't Know (97) | 0.001851 | 0.001851 | 0.000772 | 0.000772 | 0.000000 |

## 5. Indicator A1: households with a computer, by region
The same comparison applies to **indicator A1**, now disaggregated by **region**. 

**A note on methodology.** The official **ICT Households** results, published by Cetic.br, estimate variance with the **Canty-Davison** bootstrap. The `svy` library, on the other hand, uses the **Rao-Wu** method. For that reason, both versions were computed in R and entered manually in the tables below, which makes it possible to validate the Python implementation against the Rao-Wu standard.

#### Comparison A1: Version 1 (Canty-Davison) vs Python (by Region)
Here R follows the official methodology (Canty-Davison).

| Region | Response (A1) | Est. R | Est. Python | SE R (v1) | SE Python | SE Diff |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **Southeast** | **Yes (1)** | **0.376789** | **0.376789** | 0.014972 | 0.014972 | 0.000000 |
| **Northeast** | **Yes (1)** | **0.217300** | **0.217300** | 0.007479 | 0.007479 | 0.000000 |
| **South** | **Yes (1)** | **0.370627** | **0.370627** | 0.014156 | 0.014156 | 0.000000 |
| **North** | **Yes (1)** | **0.253914** | **0.253914** | 0.013751 | 0.013751 | 0.000000 |
| **Center-West** | **Yes (1)** | **0.278927** | **0.278927** | 0.011757 | 0.011757 | 0.000000 |

#### Comparison A1: Version 2 (Rao-Wu) vs Python (by Region)
Here R uses the **Rao-Wu** scale, the same internal logic as the `svy` library.

| Region | Response (A1) | Est. R | Est. Python | SE R (v2) | SE Python | SE Diff |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **Southeast** | **Yes (1)** | **0.376789** | **0.376789** | 0.013127 | 0.013127 | 0.000000 |
| **Northeast** | **Yes (1)** | **0.217300** | **0.217300** | 0.006558 | 0.006558 | 0.000000 |
| **South** | **Yes (1)** | **0.370627** | **0.370627** | 0.012412 | 0.012412 | 0.000000 |
| **North** | **Yes (1)** | **0.253914** | **0.253914** | 0.012057 | 0.012057 | 0.000000 |
| **Center-West** | **Yes (1)** | **0.278927** | **0.278927** | 0.010308 | 0.010308 | 0.000000 |

### Standard errors (SE) and scale adjustments
The two bootstrap scales tested come from the **Canty-Davison** and **Rao-Wu** methods, and each tool starts from one of them:
- **ICT Households (official)**: uses the **Canty-Davison** bootstrap, with a producer-specific scale of 0.0065365334145709.
- **Python (`svy` library)**: uses the **Rao-Wu** bootstrap internally (scale = 1/R).
- **R (`survey` package)**: defaults to Canty-Davison (scale = 1/(R-1)), but the `scale` parameter lets you switch.

The gap in standard errors comes entirely from the bootstrap scale convention. There are two ways to line them up:
1. To reproduce svy's SEs in R, pass `scale = 1/R` to `svrepdesign` (where `R` is the number of replicates).
2. To reproduce R's SEs (with any scale `c`) from svy's output, multiply svy's SEs by `math.sqrt(c * R)`.

Since both versions were computed in R and entered manually in the tables, the comparison starts from the same base. Point estimates are identical across all tools; the Python standard errors, however, follow the Rao-Wu implementation.

## 6. What can we say?
Point estimates from R and Python are identical, and standard errors also match once the bootstrap scale is handled explicitly. Moving a complex sample pipeline from R to Python with `svy`, then, doesn't change the results, and it opens the door to using Polars for data handling.

The comparison, however, covers only proportions for two ICT Households 2025 indicators. Totals, ratios and regression models (the equivalent of `svyglm`) still need to be tested before treating the two tools as interchangeable.
