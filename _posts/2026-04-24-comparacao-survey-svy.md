---
layout: post
title: "Comparação de Análise de Pesquisa: R vs Python"
subtitle: "Usando o dataset TIC Domicílios 2025"
date: 2026-04-24 09:25:00 -0300
categories: [estatistica]
tags: [survey, R, Python]
# image: /assets/images/seu-banner-aqui.jpg # Descomente para adicionar uma imagem de capa
---

Este guia demonstra como replicar uma análise de amostra complexa utilizando os pacotes [`survey`](https://cran.r-project.org/web/packages/survey/index.html) (R) e [`svy`](https://svylab.com/learn/notes/posts/design-based-survey-analysis-in-python/) (Python), garantindo a consistência das estimativas e erros padrão.


Este repositório no GitHub [https://github.com/thiagomeireles/survey_svy_tic_dom25](https://github.com/thiagomeireles/survey_svy_tic_dom25) contém todo o material para replicação.


**Download dos Dados:** Os microdados utilizados nesta análise podem ser obtidos diretamente aqui: [TIC Domicílios 2025 (SAV)](https://cetic.br/media/microdados/983/tic_domicilios_2025_domicilios_base_de_microdados_v1.0.sav).

## 1. Contexto dos Dados
Utilizamos os microdados da pesquisa [**TIC Domicílios 2025**](https://cetic.br/pesquisa/domicilios/). Esta pesquisa, realizada anualmente desde 2005, é uma referência internacional para o monitoramento da inclusão digital no Brasil, seguindo padrões metodológicos da UIT e da UNESCO.

Nesta análise, focamos em dois indicadores principais:
- **A1: Domicílios com computador**, desagregado por **REGIÃO** (Sudeste, Nordeste, Sul, Norte, Centro-Oeste).
- **A4: Domicílios com acesso à internet**, desagregado por **AREA** (Urbana/Rural).

**Rótulos e Códigos:**
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

## 4. Resultados Obtidos (Indicador A4: Domicílios com Acesso à Internet por Área)
Abaixo apresentamos as estimativas de acesso à internet nos domicílios brasileiros em 2025, comparando o Python (`svy`) com as duas abordagens de escala do R (`survey`).

### 4.1 Comparação: Versão 1 (Canty-Davison) vs Python
Nesta seção, o R utiliza o método de **Canty-Davison**.

| Área | Resposta (A4) | Est. R | Est. Python | SE R (v1) | SE Python | Dif. SE |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **Urbana** | **Sim (1)** | **0.869906** | **0.869906** | 0.004602 | 0.004602 | 0.000000 |
| Urbana | Não (0) | 0.128482 | 0.128482 | 0.004512 | 0.004512 | 0.000000 |
| Urbana | Não Sabe (97) | 0.001452 | 0.001452 | 0.000409 | 0.000409 | 0.000000 |
| **Rural** | **Sim (1)** | **0.809164** | **0.809164** | 0.008636 | 0.008636 | 0.000000 |
| Rural | Não (0) | 0.188962 | 0.188962 | 0.008584 | 0.008584 | 0.000000 |
| Rural | Não Sabe (97) | 0.001851 | 0.001851 | 0.000881 | 0.000881 | 0.000000 |

### 4.2 Comparação: Versão 2 (Rao-Wu) vs Python
Nesta seção, o R utiliza o método de **Rao-Wu**.

| Área | Resposta (A4) | Est. R | Est. Python | SE R (v2) | SE Python | Dif. SE |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **Urbana** | **Sim (1)** | **0.869906** | **0.869906** | 0.004035 | 0.004035 | 0.000000 |
| Urbana | Não (0) | 0.128482 | 0.128482 | 0.003956 | 0.003956 | 0.000000 |
| Urbana | Não Sabe (97) | 0.001452 | 0.001452 | 0.000359 | 0.000359 | 0.000000 |
| **Rural** | **Sim (1)** | **0.809164** | **0.809164** | 0.007572 | 0.007572 | 0.000000 |
| Rural | Não (0) | 0.188962 | 0.188962 | 0.007526 | 0.007526 | 0.000000 |
| Rural | Não Sabe (97) | 0.001851 | 0.001851 | 0.000772 | 0.000772 | 0.000000 |

## 5. Resultados Obtidos (Indicador A1: Domicílios com Computador por Região)
Seguindo a mesma lógica do Indicador A4, apresentamos os resultados para o **Indicador A1**, agora desagregado por **Região**.

**Nota Técnica sobre Metodologia:**
É importante destacar que os resultados oficiais da **TIC Domicílios** publicados pelo Cetic.br utilizam o método de bootstrap **Canty-Davison** para o cálculo de variância. Em contrapartida, o pacote Python `svy` utiliza o método **Rao-Wu**. Para as comparações abaixo, ambas as versões foram calculadas em R e inseridas manualmente para validar a implementação do Python frente ao padrão Rao-Wu.

#### Comparação A1: Versão 1 (Canty-Davison) vs Python (por Região)
Nesta comparação, os resultados do R refletem a metodologia oficial (Canty-Davison).

| Região | Resposta (A1) | Est. R | Est. Python | SE R (v1) | SE Python | Dif. SE |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **Sudeste** | **Sim (1)** | **0.376789** | **0.376789** | 0.014972 | 0.014972 | 0.000000 |
| **Nordeste** | **Sim (1)** | **0.217300** | **0.217300** | 0.007479 | 0.007479 | 0.000000 |
| **Sul** | **Sim (1)** | **0.370627** | **0.370627** | 0.014156 | 0.014156 | 0.000000 |
| **Norte** | **Sim (1)** | **0.253914** | **0.253914** | 0.013751 | 0.013751 | 0.000000 |
| **Centro-Oeste** | **Sim (1)** | **0.278927** | **0.278927** | 0.011757 | 0.011757 | 0.000000 |

#### Comparação A1: Versão 2 (Rao-Wu) vs Python (por Região)
Nesta comparação, o R é ajustado para usar a escala **Rao-Wu**, igualando a lógica interna da biblioteca `svy`.

| Região | Resposta (A1) | Est. R | Est. Python | SE R (v2) | SE Python | Dif. SE |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **Sudeste** | **Sim (1)** | **0.376789** | **0.376789** | 0.013127 | 0.013127 | 0.000000 |
| **Nordeste** | **Sim (1)** | **0.217300** | **0.217300** | 0.006558 | 0.006558 | 0.000000 |
| **Sul** | **Sim (1)** | **0.370627** | **0.370627** | 0.012412 | 0.012412 | 0.000000 |
| **Norte** | **Sim (1)** | **0.253914** | **0.253914** | 0.012057 | 0.012057 | 0.000000 |
| **Centro-Oeste** | **Sim (1)** | **0.278927** | **0.278927** | 0.010308 | 0.010308 | 0.000000 |

### Erros Padrão (SE) e Ajustes de Escala
Nesta análise, exploramos abordagens de escala para o Bootstrap baseadas nos métodos de **Canty-Davison** e **Rao-Wu**. Destacamos que:
- **TIC Domicílios (Oficial)**: Utiliza o método bootstrap **Canty-Davison**, com uma escala específica do produtor = 0.0065365334145709.
- **Python (biblioteca `svy`)**: Utiliza o método bootstrap **Rao-Wu** (escala = 1/R) internamente.
- **R (pacote `survey`)**: O padrão é Canty-Davison (escala = 1/(R-1)), mas permite alternar via parâmetro `scale`.

A diferença nos Erros Padrão (SE) se deve inteiramente à convenção de escala do bootstrap. Duas formas de alinhá-los:
1. Para reproduzir os SEs do `svy` no R, passe `scale = 1/R` para `svrepdesign` (onde `R` é o número de réplicas).
2. Para reproduzir os SEs do R (com qualquer escala `c`) a partir da saída do `svy`, multiplique os SEs do `svy` por `math.sqrt(c * R)`.

Para garantir uma comparação justa, calculamos ambas as versões no R e as inserimos manualmente nestas tabelas. Embora as estimativas pontuais sejam idênticas em todas as ferramentas, os erros padrão no Python alinham-se com a implementação Rao-Wu.

## 6. Conclusão
A migração de pipelines de análise de R para Python utilizando a biblioteca `svy` mantém a precisão necessária para estatísticas oriundas de dados amostrais complexos, permitindo aproveitar a velocidade do Polars e a integração do ecossistema Python.
