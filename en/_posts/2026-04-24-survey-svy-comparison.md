---
layout: post
title: "Survey Analysis Comparison: R vs Python"
subtitle: "Using the ICT Households 2025 dataset"
date: 2026-04-24 09:25:00 -0300
categories: [statistics]
tags: [survey, R, Python]
# image: /assets/images/your-banner-here.jpg # Uncomment to add a cover image
---

# Survey Analysis Comparison: R vs Python

This guide demonstrates how to replicate a complex survey analysis using the [`survey`](https://cran.r-project.org/web/packages/survey/index.html) (R) and [`svy`](https://svylab.com/learn/notes/posts/design-based-survey-analysis-in-python/) (Python) packages, ensuring consistency in estimates and standard errors.


This GitHub repository [https://github.com/thiagomeireles/survey_svy_tic_dom25](https://github.com/thiagomeireles/survey_svy_tic_dom25) contains all materials for replication.


**Data Download:** The microdata used in this analysis can be obtained directly here: [ICT Households 2025 (SAV)](https://cetic.br/media/microdados/983/tic_domicilios_2025_domicilios_base_de_microdados_v1.0.sav).

## 1. Data Context
We use microdata from the [**ICT Households 2025**](https://cetic.br/en/pesquisa/domicilios/) survey. This survey, conducted annually since 2005, is a key international reference for monitoring digital inclusion in Brazil, following methodological standards from the ITU and UNESCO.

In this analysis, we focus on two primary indicators:
- **A1: Households with computer**, disaggregated by **REGION** (Sudeste, Nordeste, Sul, Norte, Centro-Oeste).
- **A4: Households with internet access**, disaggregated by **AREA** (Urban/Rural).

**Labels and Codes:**
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

## 4. Results Obtained (Indicator A4: Households with Internet Access by Area)
Below are the estimates for internet access in Brazilian households for 2025, comparing Python (`svy`) with the two scale approaches in R (`survey`).

### 4.1 Comparison: Version 1 (Canty-Davison) vs Python
In this section, R uses the **Canty-Davison** method.

| Area | Response (A4) | Est. R | Est. Python | SE R (v1) | SE Python | SE Diff |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **Urban** | **Yes (1)** | **0.869906** | **0.869906** | 0.004602 | 0.004602 | 0.000000 |
| Urban | No (0) | 0.128482 | 0.128482 | 0.004512 | 0.004512 | 0.000000 |
| Urban | Don't Know (97) | 0.001452 | 0.001452 | 0.000409 | 0.000409 | 0.000000 |
| **Rural** | **Yes (1)** | **0.809164** | **0.809164** | 0.008636 | 0.008636 | 0.000000 |
| Rural | No (0) | 0.188962 | 0.188962 | 0.008584 | 0.008584 | 0.000000 |
| Rural | Don't Know (97) | 0.001851 | 0.001851 | 0.000881 | 0.000881 | 0.000000 |

### 4.2 Comparison: Version 2 (Rao-Wu) vs Python
In this section, R uses the **Rao-Wu** method.

| Area | Response (A4) | Est. R | Est. Python | SE R (v2) | SE Python | SE Diff |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **Urban** | **Yes (1)** | **0.869906** | **0.869906** | 0.004035 | 0.004035 | 0.000000 |
| Urban | No (0) | 0.128482 | 0.128482 | 0.003956 | 0.003956 | 0.000000 |
| Urban | Don't Know (97) | 0.001452 | 0.001452 | 0.000359 | 0.000359 | 0.000000 |
| **Rural** | **Yes (1)** | **0.809164** | **0.809164** | 0.007572 | 0.007572 | 0.000000 |
| Rural | No (0) | 0.188962 | 0.188962 | 0.007526 | 0.007526 | 0.000000 |
| Rural | Don't Know (97) | 0.001851 | 0.001851 | 0.000772 | 0.000772 | 0.000000 |

## 5. Results Obtained (Indicator A1: Households with Computer by Region)
Following the same logic as Indicator A4, we present the results for **Indicator A1**, now disaggregated by **Region**. 

**Technical Note on Methodology:**
It is important to note that the official **ICT Households** results published by Cetic.br utilize the **Canty-Davison** bootstrap method for variance estimation. In contrast, the Python `svy` package implements the **Rao-Wu** bootstrap method. For the comparisons below, both versions were calculated in R and manually inserted to validate the Python implementation against the Rao-Wu standard.

#### Comparison A1: Version 1 (Canty-Davison) vs Python (by Region)
In this comparison, R results reflect the official methodology (Canty-Davison).

| Region | Response (A1) | Est. R | Est. Python | SE R (v1) | SE Python | SE Diff |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **Southeast** | **Yes (1)** | **0.376789** | **0.376789** | 0.014972 | 0.014972 | 0.000000 |
| **Northeast** | **Yes (1)** | **0.217300** | **0.217300** | 0.007479 | 0.007479 | 0.000000 |
| **South** | **Yes (1)** | **0.370627** | **0.370627** | 0.014156 | 0.014156 | 0.000000 |
| **North** | **Yes (1)** | **0.253914** | **0.253914** | 0.013751 | 0.013751 | 0.000000 |
| **Center-West** | **Yes (1)** | **0.278927** | **0.278927** | 0.011757 | 0.011757 | 0.000000 |

#### Comparison A1: Version 2 (Rao-Wu) vs Python (by Region)
In this comparison, R is adjusted to use the **Rao-Wu** scale, matching the underlying logic of the `svy` package.

| Region | Response (A1) | Est. R | Est. Python | SE R (v2) | SE Python | SE Diff |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **Southeast** | **Yes (1)** | **0.376789** | **0.376789** | 0.013127 | 0.013127 | 0.000000 |
| **Northeast** | **Yes (1)** | **0.217300** | **0.217300** | 0.006558 | 0.006558 | 0.000000 |
| **South** | **Yes (1)** | **0.370627** | **0.370627** | 0.012412 | 0.012412 | 0.000000 |
| **North** | **Yes (1)** | **0.253914** | **0.253914** | 0.012057 | 0.012057 | 0.000000 |
| **Center-West** | **Yes (1)** | **0.278927** | **0.278927** | 0.010308 | 0.010308 | 0.000000 |

### Standard Errors (SE) and Scale Adjustments
In this analysis, we explored scale approaches for the Bootstrap based on the **Canty-Davison** and **Rao-Wu** methods. We highlight that:
- **TIC Domicílios (Official)**: Uses the **Canty-Davison** bootstrap method, with a producer-specific scale = 0.0065365334145709.
- **Python (`svy` library)**: Uses the **Rao-Wu** bootstrap method (scale = 1/R) internally.
- **R (`survey` package)**: Defaults to Canty-Davison (scale = 1/(R-1)) but allows switching via the `scale` parameter.

The SE gap is entirely about the bootstrap scale convention. Two ways to line them up:
1. To reproduce svy's SEs in R, pass `scale = 1/R` to `svrepdesign` (where `R` is the number of replicates).
2. To reproduce R's SEs (with any scale `c`) from svy's output, multiply svy's SEs by `math.sqrt(c * R)`.

To ensure a fair comparison, we calculated both versions in R and manually inserted them into these tables. While point estimates remain identical across all tools, the standard errors in Python align with the Rao-Wu implementation.

## 6. Conclusion
The migration of analysis pipelines from R to Python using the `svy` library maintains the precision required for statistics derived from complex sample data, allowing users to leverage Polars' speed and the integration of the Python ecosystem.
