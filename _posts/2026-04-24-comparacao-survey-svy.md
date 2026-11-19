---
layout: post
title: "Amostras complexas em R e Python: survey e svy lado a lado"
subtitle: "Com os microdados da TIC Domicílios 2025"
excerpt: "A mesma análise de amostra complexa feita com o pacote `survey` (R) e com a biblioteca `svy` (Python), para conferir se estimativas e erros padrão batem e, quando não batem, por quê."
date: 2026-04-24 09:25:00 -0300
categories: [estatistica]
tags: [survey, R, Python]
# image: /assets/images/seu-banner-aqui.jpg # Descomente para adicionar uma imagem de capa
---

# Amostras complexas em R e Python: survey e svy lado a lado

Em R, a análise de pesquisas com amostra complexa costuma passar pelo pacote [`survey`](https://cran.r-project.org/web/packages/survey/index.html); em Python, uma das opções é a biblioteca [`svy`](https://svylab.com/learn/notes/posts/design-based-survey-analysis-in-python/). Este texto replica a mesma análise nas duas ferramentas para verificar se as estimativas pontuais e os erros padrão coincidem. De forma mais específica, mostra de onde vem a diferença nos erros padrão e como alinhar os dois resultados.

Todo o material para replicação está neste [repositório no GitHub](https://github.com/thiagomeireles/survey_svy_tic_dom25).

**Dados:** os microdados usados na análise estão disponíveis aqui: [TIC Domicílios 2025 (SAV)](https://cetic.br/media/microdados/983/tic_domicilios_2025_domicilios_base_de_microdados_v1.0.sav).

## 1. Os dados
A análise usa os microdados da [**TIC Domicílios 2025**](https://cetic.br/pesquisa/domicilios/), pesquisa feita anualmente desde 2005 para medir o acesso e o uso de TIC nos domicílios brasileiros, com padrões metodológicos da UIT e da UNESCO.

Dois indicadores entram na comparação:
- **A1: Domicílios com computador**, desagregado por **REGIÃO** (Sudeste, Nordeste, Sul, Norte, Centro-Oeste).
- **A4: Domicílios com acesso à internet**, desagregado por **AREA** (Urbana/Rural).

**Rótulos e códigos:**
- **AREA**: 1 = Urbana, 2 = Rural
- **REGIÃO**: 1 = Sudeste, 2 = Nordeste, 3 = Sul, 4 = Norte, 5 = Centro-Oeste
- **A1/A4**: 1 = Sim, 0 = Não

## 2. Implementação em R (survey)
```r
# Setup e Leitura
pacman::p_load(survey, haven, here, janitor)
path_dados <- here::here("02_dados", "tic_domicilios_2025_domicilios_base_de_microdados_v1.0.sav")

dados_dom <- haven::read_sav(path_dados) |> 
  janitor::clean_names() |> 
  dplyr::mutate(dplyr::across(dplyr::any_of(c("a4", "area")), as.integer))

# Desenho Amostral
design_dom <- survey::svrepdesign(
  weights = ~peso, 
  repweights = "rep[0-9]+", 
  scale = 0.0065365334145709, # Versão 1: Canty-Davison
  # scale = 0.0050251256,    # Versão 2: Rao-Wu
  data = dados_dom, 
  combined.weights = TRUE
)

# Estimativa para o indicador A4
estimativa_r <- survey::svyby(
  formula = ~as.factor(a4), 
  by = ~area, 
  design = design_dom, 
  FUN = survey::svymean
)
```

## 3. Implementação em Python (svy)
```python
import svy
import polars as pl
import os
import math

# Leitura e Tratamento
path_sav = os.path.join("02_dados", "cetic", "tic_domicilios_2025_domicilios_base_de_microdados_v1.0.sav")
dados_dom = svy.io.read_sav(path_sav).with_columns([
    pl.col("A4").cast(pl.Int32),
    pl.col("AREA").cast(pl.Int32)
])

scale = 0.0065365334145709  # Escala do produtor (CETIC)
R = 200  # Número de réplicas

# Desenho Amostral
rep_weights_config = svy.RepWeights(method="bootstrap", prefix="REP", n_reps=R)
smp_design = svy.Design(wgt="PESO", rep_wgts=rep_weights_config)
sample = svy.Sample(data=dados_dom, design=smp_design)

# Estimativa
estimativa_py = sample.estimation.prop("A4", by="AREA", method="replication")

# Ajuste do SE para corresponder ao R
estimativa_py_df = estimativa_py.to_polars().with_columns(
    se_r=pl.col("se") * math.sqrt(scale * R)
)
```

## 4. Indicador A4: domicílios com acesso à internet, por área
As tabelas comparam as estimativas de acesso à internet nos domicílios brasileiros em 2025 feitas no Python (`svy`) com as duas escalas de bootstrap testadas no R (`survey`).

### 4.1 Comparação: Versão 1 (Canty-Davison) vs Python
Aqui o R usa o método de **Canty-Davison**.

| Área | Resposta (A4) | Est. R | Est. Python | SE R (v1) | SE Python | Dif. SE |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **Urbana** | **Sim (1)** | **0.869906** | **0.869906** | 0.004602 | 0.004602 | 0.000000 |
| Urbana | Não (0) | 0.128482 | 0.128482 | 0.004512 | 0.004512 | 0.000000 |
| Urbana | Não Sabe (97) | 0.001452 | 0.001452 | 0.000409 | 0.000409 | 0.000000 |
| **Rural** | **Sim (1)** | **0.809164** | **0.809164** | 0.008636 | 0.008636 | 0.000000 |
| Rural | Não (0) | 0.188962 | 0.188962 | 0.008584 | 0.008584 | 0.000000 |
| Rural | Não Sabe (97) | 0.001851 | 0.001851 | 0.000881 | 0.000881 | 0.000000 |

### 4.2 Comparação: Versão 2 (Rao-Wu) vs Python
Aqui o R usa o método de **Rao-Wu**.

| Área | Resposta (A4) | Est. R | Est. Python | SE R (v2) | SE Python | Dif. SE |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **Urbana** | **Sim (1)** | **0.869906** | **0.869906** | 0.004035 | 0.004035 | 0.000000 |
| Urbana | Não (0) | 0.128482 | 0.128482 | 0.003956 | 0.003956 | 0.000000 |
| Urbana | Não Sabe (97) | 0.001452 | 0.001452 | 0.000359 | 0.000359 | 0.000000 |
| **Rural** | **Sim (1)** | **0.809164** | **0.809164** | 0.007572 | 0.007572 | 0.000000 |
| Rural | Não (0) | 0.188962 | 0.188962 | 0.007526 | 0.007526 | 0.000000 |
| Rural | Não Sabe (97) | 0.001851 | 0.001851 | 0.000772 | 0.000772 | 0.000000 |

## 5. Indicador A1: domicílios com computador, por região
A mesma comparação vale para o **indicador A1**, agora desagregado por **região**.

**Nota sobre a metodologia.** Os resultados oficiais da **TIC Domicílios**, publicados pelo Cetic.br, calculam a variância com o bootstrap de **Canty-Davison**. A biblioteca `svy`, por outro lado, usa o método de **Rao-Wu**. Por isso, as duas versões foram calculadas no R e inseridas manualmente nas tabelas abaixo, o que permite validar a implementação do Python contra o padrão Rao-Wu.

#### Comparação A1: Versão 1 (Canty-Davison) vs Python (por Região)
Aqui o R segue a metodologia oficial (Canty-Davison).

| Região | Resposta (A1) | Est. R | Est. Python | SE R (v1) | SE Python | Dif. SE |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **Sudeste** | **Sim (1)** | **0.376789** | **0.376789** | 0.014972 | 0.014972 | 0.000000 |
| **Nordeste** | **Sim (1)** | **0.217300** | **0.217300** | 0.007479 | 0.007479 | 0.000000 |
| **Sul** | **Sim (1)** | **0.370627** | **0.370627** | 0.014156 | 0.014156 | 0.000000 |
| **Norte** | **Sim (1)** | **0.253914** | **0.253914** | 0.013751 | 0.013751 | 0.000000 |
| **Centro-Oeste** | **Sim (1)** | **0.278927** | **0.278927** | 0.011757 | 0.011757 | 0.000000 |

#### Comparação A1: Versão 2 (Rao-Wu) vs Python (por Região)
Aqui o R usa a escala de **Rao-Wu**, a mesma lógica interna da biblioteca `svy`.

| Região | Resposta (A1) | Est. R | Est. Python | SE R (v2) | SE Python | Dif. SE |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **Sudeste** | **Sim (1)** | **0.376789** | **0.376789** | 0.013127 | 0.013127 | 0.000000 |
| **Nordeste** | **Sim (1)** | **0.217300** | **0.217300** | 0.006558 | 0.006558 | 0.000000 |
| **Sul** | **Sim (1)** | **0.370627** | **0.370627** | 0.012412 | 0.012412 | 0.000000 |
| **Norte** | **Sim (1)** | **0.253914** | **0.253914** | 0.012057 | 0.012057 | 0.000000 |
| **Centro-Oeste** | **Sim (1)** | **0.278927** | **0.278927** | 0.010308 | 0.010308 | 0.000000 |

### Erros padrão (SE) e ajustes de escala
As duas escalas testadas para o bootstrap vêm dos métodos de **Canty-Davison** e de **Rao-Wu**, e cada ferramenta parte de uma delas:
- **TIC Domicílios (oficial)**: usa o bootstrap de **Canty-Davison**, com uma escala própria do produtor, igual a 0.0065365334145709.
- **Python (biblioteca `svy`)**: usa internamente o bootstrap de **Rao-Wu** (escala = 1/R).
- **R (pacote `survey`)**: o padrão é Canty-Davison (escala = 1/(R-1)), mas o parâmetro `scale` permite trocar.

A diferença nos erros padrão vem inteiramente da convenção de escala do bootstrap. Há duas formas de alinhá-los:
1. Para reproduzir os SEs do `svy` no R, passe `scale = 1/R` para `svrepdesign` (onde `R` é o número de réplicas).
2. Para reproduzir os SEs do R (com qualquer escala `c`) a partir da saída do `svy`, multiplique os SEs do `svy` por `math.sqrt(c * R)`.

Como as duas versões foram calculadas no R e inseridas manualmente nas tabelas, a comparação parte da mesma base. As estimativas pontuais são idênticas em todas as ferramentas; os erros padrão do Python, no entanto, seguem a implementação de Rao-Wu.

## 6. O que é possível dizer?
As estimativas pontuais do R e do Python são idênticas, e os erros padrão também coincidem quando a escala do bootstrap é tratada de forma explícita. Dessa forma, levar um pipeline de amostras complexas do R para o Python com a `svy` não muda os resultados e ainda permite usar o Polars no tratamento dos dados.

A comparação, no entanto, cobre apenas proporções de dois indicadores da TIC Domicílios 2025. Totais, razões e modelos de regressão (o equivalente ao `svyglm`) ainda precisam ser testados antes de tratar as duas ferramentas como intercambiáveis.
