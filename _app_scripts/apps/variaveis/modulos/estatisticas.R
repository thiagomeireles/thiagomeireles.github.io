# =============================================================================
# Script:  estatisticas.R (dados simulados e estatísticas do tema Tipos de variáveis)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-18
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/apps/variaveis/app.R)
# Saída:
#   - objetos em memória: tipos_variavel, exemplos_*, formatos_ordinal e funções
#     de resumo usadas pelos módulos mod_*.R
# =============================================================================

# Textos em pt e en via tr() (comum/utilitarios.R)

# Descrição dos quatro tipos, usada na visão geral e no topo de cada aba
tipos_variavel <- list(
  nominal = list(
    nome = tr("Qualitativa nominal", "Nominal categorical"),
    grupo = tr("Qualitativa (categórica)", "Categorical (qualitative)"),
    definicao = tr(
      "Categorias sem ordem natural. Dá para dizer se duas observações são iguais ou diferentes, e só.",
      "Categories with no natural order. You can tell whether two observations are equal or different, and nothing more."
    ),
    exemplos = tr("região, tipo sanguíneo, estado civil, CEP", "region, blood type, marital status, ZIP code"),
    operacoes = "= ≠",
    estatisticas = tr("frequências, proporções e moda", "frequencies, proportions and mode"),
    grafico = tr("barras, em qualquer ordem", "bars, in any order")
  ),
  ordinal = list(
    nome = tr("Qualitativa ordinal", "Ordinal categorical"),
    grupo = tr("Qualitativa (categórica)", "Categorical (qualitative)"),
    definicao = tr(
      "Categorias com ordem natural, mas sem distância definida entre elas.",
      "Categories with a natural order, but no defined distance between them."
    ),
    exemplos = tr(
      "escolaridade, escala de satisfação, classificação de risco",
      "education level, satisfaction scale, triage category"
    ),
    operacoes = "= ≠ < >",
    estatisticas = tr(
      "frequências acumuladas, moda, mediana e quartis",
      "cumulative frequencies, mode, median and quartiles"
    ),
    grafico = tr("barras na ordem das categorias", "bars in category order")
  ),
  discreta = list(
    nome = tr("Quantitativa discreta", "Discrete quantitative"),
    grupo = tr("Quantitativa (numérica)", "Quantitative (numeric)"),
    definicao = tr(
      "Números cujos valores possíveis são separados entre si, em geral vindos de contagens (0, 1, 2…).",
      "Numbers whose possible values are separate from each other, usually counts (0, 1, 2…)."
    ),
    exemplos = tr("número de filhos, de moradores, de gols", "number of children, household members, goals"),
    operacoes = "= ≠ < > + −",
    estatisticas = tr(
      "média, mediana, moda, desvio padrão e quartis",
      "mean, median, mode, standard deviation and quartiles"
    ),
    grafico = tr("barras por valor", "one bar per value")
  ),
  continua = list(
    nome = tr("Quantitativa contínua", "Continuous quantitative"),
    grupo = tr("Quantitativa (numérica)", "Quantitative (numeric)"),
    definicao = tr(
      "Números que vêm de medições e podem assumir qualquer valor num intervalo.",
      "Numbers that come from measuring and can take any value within a range."
    ),
    exemplos = tr("altura, renda, tempo de espera, temperatura", "height, income, waiting time, temperature"),
    operacoes = "= ≠ < > + −",
    estatisticas = tr(
      "média, mediana, desvio padrão, quartis e assimetria",
      "mean, median, standard deviation, quartiles and skewness"
    ),
    grafico = tr("histograma e boxplot", "histogram and boxplot")
  )
)

exemplos_nominal <- list(
  regiao = list(
    rotulo = tr("Região de residência", "Region of residence (Brazil)"),
    categorias = tr(
      c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul"),
      c("North", "Northeast", "Central-West", "Southeast", "South")
    ),
    prob = c(0.085, 0.269, 0.080, 0.418, 0.147),
    nota = tr("proporções próximas às do Censo 2022", "proportions close to Brazil's 2022 Census")
  ),
  transporte = list(
    rotulo = tr("Principal meio de transporte para o trabalho", "Main means of transport to work"),
    categorias = tr(
      c("Ônibus", "Carro", "A pé", "Moto", "Bicicleta", "Metrô ou trem"),
      c("Bus", "Car", "On foot", "Motorcycle", "Bicycle", "Subway or train")
    ),
    prob = c(0.30, 0.28, 0.17, 0.13, 0.07, 0.05),
    nota = tr("proporções ilustrativas", "illustrative proportions")
  ),
  sangue = list(
    rotulo = tr("Tipo sanguíneo", "Blood type"),
    categorias = c("O+", "A+", "B+", "AB+", "O-", "A-", "B-", "AB-"),
    prob = c(0.36, 0.34, 0.08, 0.025, 0.09, 0.08, 0.02, 0.005),
    nota = tr("proporções aproximadas", "approximate proportions")
  )
)

exemplos_ordinal <- list(
  satisfacao = list(
    rotulo = tr("Satisfação com o atendimento", "Satisfaction with customer service"),
    categorias = tr(
      c("Muito insatisfeito", "Insatisfeito", "Neutro", "Satisfeito", "Muito satisfeito"),
      c("Very dissatisfied", "Dissatisfied", "Neutral", "Satisfied", "Very satisfied")
    )
  ),
  escolaridade = list(
    rotulo = tr("Escolaridade", "Education level"),
    categorias = tr(
      c("Sem instrução", "Fundamental", "Médio", "Superior", "Pós-graduação"),
      c("No schooling", "Elementary/middle school", "High school", "College", "Graduate degree")
    )
  ),
  risco = list(
    rotulo = tr("Classificação de risco no pronto-socorro", "Emergency room triage category"),
    categorias = tr(
      c("Não urgente", "Pouco urgente", "Urgente", "Muito urgente", "Emergência"),
      c("Non-urgent", "Less urgent", "Urgent", "Very urgent", "Emergency")
    )
  )
)

# Pesos das categorias (1 a k) para cada formato de distribuição
formatos_ordinal <- list(
  centro     = list(rotulo = tr("Concentrada no meio", "Concentrated in the middle"), pesos = \(k) stats::dnorm(seq_len(k), (k + 1) / 2, k / 5)),
  inicio     = list(rotulo = tr("Concentrada nas primeiras categorias", "Concentrated in the first categories"), pesos = \(k) rev(seq_len(k))^2),
  fim        = list(rotulo = tr("Concentrada nas últimas categorias", "Concentrated in the last categories"), pesos = \(k) seq_len(k)^2),
  polarizada = list(rotulo = tr("Polarizada (extremos)", "Polarized (extremes)"), pesos = \(k) {
    d <- abs(seq_len(k) - (k + 1) / 2)
    ifelse(d == max(d), 3.5, 1)
  }),
  uniforme   = list(rotulo = tr("Uniforme", "Uniform"), pesos = \(k) rep(1, k))
)

exemplos_discreta <- list(
  filhos = list(
    rotulo = tr("Número de filhos", "Number of children"), unidade = tr("filhos", "children"), extremo = 12,
    gerar = \(n) sample(0:6, n, replace = TRUE, prob = c(0.28, 0.24, 0.25, 0.13, 0.06, 0.03, 0.01))
  ),
  moradores = list(
    rotulo = tr("Moradores no domicílio", "Household members"), unidade = tr("moradores", "people"), extremo = 15,
    gerar = \(n) 1 + stats::rpois(n, 1.9)
  ),
  gols = list(
    rotulo = tr("Gols em uma partida de futebol", "Goals in a soccer match"), unidade = tr("gols", "goals"), extremo = 10,
    gerar = \(n) stats::rpois(n, 2.5)
  )
)

exemplos_continua <- list(
  altura = list(
    rotulo = tr("Altura de adultos", "Adult height"), unidade = "cm", digitos = 1, extremo = 215,
    gerar = \(n) stats::rnorm(n, 168, 9)
  ),
  renda = list(
    rotulo = tr("Renda mensal", "Monthly income"), unidade = "R$", digitos = 0, extremo = 80000,
    gerar = \(n) stats::rlnorm(n, log(2500), 0.8)
  ),
  espera = list(
    rotulo = tr("Tempo de espera na fila", "Waiting time in line"), unidade = "min", digitos = 1, extremo = 150,
    gerar = \(n) stats::rexp(n, 1 / 12)
  )
)

opcoes_exemplo <- function(exemplos) {
  stats::setNames(names(exemplos), purrr::map_chr(exemplos, "rotulo"))
}

# Valor com unidade: "R$ 2.500" ou "168,3 cm"
fmt_valor <- function(x, unidade, digitos = 1, quadrado = FALSE) {
  un <- if (quadrado) paste0(unidade, "²") else unidade
  if (identical(unidade, "R$")) paste(un, fmt_num(x, digitos)) else paste(fmt_num(x, digitos), un)
}

gerar_categorica <- function(categorias, prob, n) {
  factor(sample(categorias, n, replace = TRUE, prob = prob), levels = categorias)
}

tabela_frequencias <- function(x) {
  contagem <- table(x)
  data.frame(
    categoria = names(contagem),
    n         = as.vector(contagem),
    prop      = as.vector(contagem) / length(x),
    acum      = cumsum(as.vector(contagem)) / length(x)
  )
}

# Liga itens com "e"/"and": empates de moda, mínimo e máximo
juntar <- function(x) paste(x, collapse = tr(" e ", " and "))

# Todas as categorias empatadas na maior frequência
moda_tabela <- function(freq) {
  juntar(freq$categoria[freq$n == max(freq$n)])
}

# Quantil de uma ordinal: a categoria onde a frequência acumulada atinge p
# (type = 1 devolve sempre um valor observado, sem interpolar)
quantil_ordinal <- function(x, p) {
  levels(x)[stats::quantile(as.integer(x), p, type = 1, names = FALSE)]
}

assimetria <- function(x) {
  d <- x - mean(x)
  mean(d^3) / mean(d^2)^1.5
}

# Corte de |g| < 0,5 para "aproximadamente simétrica": regra prática, não vem
# de Morettin e Bussab nem de Agresti e Finlay
leitura_assimetria <- function(g) {
  if (abs(g) < 0.5) return(tr("aproximadamente simétrica", "approximately symmetric"))
  if (g > 0) {
    tr("assimétrica à direita (cauda longa nos valores altos)", "right-skewed (long tail of high values)")
  } else {
    tr("assimétrica à esquerda (cauda longa nos valores baixos)", "left-skewed (long tail of low values)")
  }
}

moda_valores <- function(x) {
  contagem <- table(x)
  as.numeric(names(contagem)[contagem == max(contagem)])
}

# Variância e desvio padrão com divisor n − 1 (stats::var), como em Agresti e
# Finlay; Morettin e Bussab usam n na descritiva. Quartis pelo método padrão do
# R (type = 7), que difere pouco dos métodos dos dois livros com n >= 10.
resumo_quantitativo <- function(x) {
  q <- stats::quantile(x, c(0.25, 0.75), names = FALSE)
  list(
    n = length(x), media = mean(x), mediana = stats::median(x),
    minimo = min(x), maximo = max(x), amplitude = max(x) - min(x),
    q1 = q[1], q3 = q[2], iiq = q[2] - q[1],
    variancia = stats::var(x), dp = stats::sd(x),
    cv = stats::sd(x) / mean(x), assimetria = assimetria(x)
  )
}

# Tabelas de frequência: cabeçalhos comuns às abas
nomes_freq <- function(primeira) c(primeira, "n", "%", tr("% acumulada", "cumulative %"))
nomes_estatisticas <- function() tr(c("Estatística", "Valor", "Como ler"), c("Statistic", "Value", "How to read it"))

# Topo de cada aba: nome do tipo, definição e operações válidas
cabecalho_tipo <- function(tipo) {
  t <- tipos_variavel[[tipo]]
  shiny::div(
    class = "mb-2",
    shiny::tags$h4(class = "mb-1", t$nome, shiny::tags$small(class = "text-muted ms-2", t$grupo)),
    shiny::p(
      class = "mb-0", t$definicao, " ",
      shiny::span(class = "badge text-bg-primary ms-1", paste(tr("Operações válidas:", "Valid operations:"), t$operacoes))
    )
  )
}

# Lista de alertas: o que não usar (categóricas) ou cuidados (quantitativas).
# `itens` é um vetor nomeado: o nome vai em negrito, o valor é o texto.
cartao_cuidados <- function(titulo, itens) {
  bslib::card(
    bslib::card_header(titulo),
    bslib::card_body(
      shiny::tags$ul(
        class = "mb-0",
        purrr::imap(itens, \(texto, nome) shiny::tags$li(shiny::strong(paste0(nome, ": ")), texto))
      )
    )
  )
}

botao_sortear <- function(id) {
  shiny::actionButton(id, tr("Sortear nova amostra", "Draw a new sample"), class = "btn-primary", icon = shiny::icon("dice"))
}

slider_amostra <- function(id, min, max, value) {
  shiny::sliderInput(id, tr("Tamanho da amostra", "Sample size"), min = min, max = max, value = value, step = 10, sep = sep_milhar())
}

seletor_exemplo <- function(id, exemplos) {
  shiny::selectInput(id, tr("Variável", "Variable"), choices = opcoes_exemplo(exemplos))
}

# Com o extremo incluído, mostra a mesma estatística calculada sem ele
descricao_sem_extremo <- function(sem, unidade, digitos, incluir) {
  if (!incluir) return("")
  paste(tr("Sem o valor extremo:", "Without the extreme value:"), fmt_valor(sem, unidade, digitos))
}

# Entrada do valor extremo, mostrada quando a caixa está marcada
entrada_extremo <- function(session, valor, step = NA) {
  shiny::numericInput(session$ns("valor_extremo"), tr("Valor extremo", "Extreme value"), value = valor, min = 0, step = step)
}

caixa_extremo <- function(id) {
  shiny::checkboxInput(id, tr("Incluir um valor extremo", "Add an extreme value"), value = FALSE)
}

# Tabela de estatísticas de uma quantitativa: estatística, valor e como ler
tabela_estatisticas_quant <- function(r, unidade, digitos, extras = NULL) {
  v <- \(x, quadrado = FALSE) fmt_valor(x, unidade, digitos, quadrado)
  linhas <- data.frame(
    estatistica = tr(
      c(
        "Tamanho da amostra (n)", "Média", "Mediana", "Mínimo e máximo", "Amplitude",
        "1º e 3º quartis", "Distância interquartil (d_q)", "Variância (S²)", "Desvio padrão (S)", "Coeficiente de variação"
      ),
      c(
        "Sample size (n)", "Mean", "Median", "Minimum and maximum", "Range",
        "1st and 3rd quartiles", "Interquartile range (IQR)", "Variance (s²)", "Standard deviation (s)", "Coefficient of variation"
      )
    ),
    valor = c(
      fmt_num(r$n, 0), v(r$media), v(r$mediana), juntar(c(v(r$minimo), v(r$maximo))), v(r$amplitude),
      juntar(c(v(r$q1), v(r$q3))), v(r$iiq), v(r$variancia, quadrado = TRUE), v(r$dp), fmt_pct(r$cv)
    ),
    leitura = tr(
      c(
        "Número de observações",
        "Soma dos valores dividida por n; sensível a extremos",
        "Valor do meio dos dados ordenados: pelo menos metade das observações está nele ou abaixo, e pelo menos metade nele ou acima",
        "Menor e maior valor observados",
        "Máximo menos mínimo; depende só dos dois extremos",
        "Cerca de 25% das observações estão em Q1 ou abaixo, e cerca de 75% em Q3 ou abaixo",
        "Q3 − Q1: largura dos 50% centrais; resiste a extremos",
        "Soma dos desvios ao quadrado em relação à média, dividida por n − 1; em unidade ao quadrado",
        "Raiz da variância: distância típica até a média, na unidade original",
        "Desvio padrão como % da média; compara a variabilidade de variáveis em unidades diferentes"
      ),
      c(
        "Number of observations",
        "Sum of the values divided by n; sensitive to extremes",
        "Middle value of the sorted data: at least half of the observations are at or below it, and at least half at or above it",
        "Smallest and largest observed values",
        "Maximum minus minimum; depends only on the two extremes",
        "About 25% of the observations are at or below Q1, and about 75% at or below Q3",
        "Q3 − Q1: width of the middle 50%; resistant to extremes",
        "Sum of squared deviations from the mean divided by n − 1, in squared units",
        "Square root of the variance: typical distance from the mean, in the original unit",
        "Standard deviation as a % of the mean; compares variability across variables measured in different units"
      )
    )
  )
  tabela <- rbind(linhas, extras)
  names(tabela) <- nomes_estatisticas()
  tabela
}

# Linha extra da tabela de estatísticas (mesmas colunas de tabela_estatisticas_quant)
linha_estatistica <- function(estatistica, valor, leitura) {
  data.frame(estatistica = estatistica, valor = valor, leitura = leitura)
}
