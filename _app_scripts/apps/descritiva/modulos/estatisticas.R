# =============================================================================
# Script:  estatisticas.R (dados simulados e estatísticas do tema Descritiva)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-18
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/apps/descritiva/app.R)
# Saída:
#   - objetos em memória: tipos_variavel, exemplos_*, formatos_ordinal e funções
#     de resumo usadas pelos módulos mod_*.R
# =============================================================================

# Descrição dos quatro tipos, usada na visão geral e no topo de cada aba
tipos_variavel <- list(
  nominal = list(
    nome = "Categórica nominal", grupo = "Categórica (qualitativa)",
    definicao = "Categorias sem ordem natural. Dá para dizer se duas observações são iguais ou diferentes, e só.",
    exemplos = "região, tipo sanguíneo, estado civil, CEP",
    operacoes = "= ≠",
    estatisticas = "frequências, proporções e moda",
    grafico = "barras, em qualquer ordem"
  ),
  ordinal = list(
    nome = "Categórica ordinal", grupo = "Categórica (qualitativa)",
    definicao = "Categorias com ordem natural, mas sem distância definida entre elas.",
    exemplos = "escolaridade, escala de satisfação, classificação de risco",
    operacoes = "= ≠ < >",
    estatisticas = "frequências acumuladas, moda, mediana e quartis",
    grafico = "barras na ordem das categorias"
  ),
  discreta = list(
    nome = "Quantitativa discreta", grupo = "Quantitativa (numérica)",
    definicao = "Números que vêm de contagens: valores inteiros, separados entre si.",
    exemplos = "número de filhos, de moradores, de gols",
    operacoes = "= ≠ < > + −",
    estatisticas = "média, mediana, moda, desvio padrão e quartis",
    grafico = "barras por valor"
  ),
  continua = list(
    nome = "Quantitativa contínua", grupo = "Quantitativa (numérica)",
    definicao = "Números que vêm de medições e podem assumir qualquer valor num intervalo.",
    exemplos = "altura, renda, tempo de espera, temperatura",
    operacoes = "= ≠ < > + −",
    estatisticas = "média, mediana, desvio padrão, quartis e assimetria",
    grafico = "histograma e boxplot"
  )
)

exemplos_nominal <- list(
  regiao = list(
    rotulo = "Região de residência",
    categorias = c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul"),
    prob = c(0.085, 0.269, 0.080, 0.418, 0.147),
    nota = "proporções próximas às do Censo 2022"
  ),
  transporte = list(
    rotulo = "Principal meio de transporte para o trabalho",
    categorias = c("Ônibus", "Carro", "A pé", "Moto", "Bicicleta", "Metrô ou trem"),
    prob = c(0.30, 0.28, 0.17, 0.13, 0.07, 0.05),
    nota = "proporções ilustrativas"
  ),
  sangue = list(
    rotulo = "Tipo sanguíneo",
    categorias = c("O+", "A+", "B+", "AB+", "O-", "A-", "B-", "AB-"),
    prob = c(0.36, 0.34, 0.08, 0.025, 0.09, 0.08, 0.02, 0.005),
    nota = "proporções aproximadas"
  )
)

exemplos_ordinal <- list(
  satisfacao = list(
    rotulo = "Satisfação com o atendimento",
    categorias = c("Muito insatisfeito", "Insatisfeito", "Neutro", "Satisfeito", "Muito satisfeito")
  ),
  escolaridade = list(
    rotulo = "Escolaridade",
    categorias = c("Sem instrução", "Fundamental", "Médio", "Superior", "Pós-graduação")
  ),
  risco = list(
    rotulo = "Classificação de risco no pronto-socorro",
    categorias = c("Não urgente", "Pouco urgente", "Urgente", "Muito urgente", "Emergência")
  )
)

# Pesos das categorias (1 a k) para cada formato de distribuição
formatos_ordinal <- list(
  centro     = list(rotulo = "Concentrada no meio", pesos = \(k) stats::dnorm(seq_len(k), (k + 1) / 2, k / 5)),
  inicio     = list(rotulo = "Concentrada nas primeiras categorias", pesos = \(k) rev(seq_len(k))^2),
  fim        = list(rotulo = "Concentrada nas últimas categorias", pesos = \(k) seq_len(k)^2),
  polarizada = list(rotulo = "Polarizada (extremos)", pesos = \(k) {
    d <- abs(seq_len(k) - (k + 1) / 2)
    ifelse(d == max(d), 3.5, 1)
  }),
  uniforme   = list(rotulo = "Uniforme", pesos = \(k) rep(1, k))
)

exemplos_discreta <- list(
  filhos = list(
    rotulo = "Número de filhos", unidade = "filhos", extremo = 12,
    gerar = \(n) sample(0:6, n, replace = TRUE, prob = c(0.28, 0.24, 0.25, 0.13, 0.06, 0.03, 0.01))
  ),
  moradores = list(
    rotulo = "Moradores no domicílio", unidade = "moradores", extremo = 15,
    gerar = \(n) 1 + stats::rpois(n, 1.9)
  ),
  gols = list(
    rotulo = "Gols em uma partida de futebol", unidade = "gols", extremo = 10,
    gerar = \(n) stats::rpois(n, 2.5)
  )
)

exemplos_continua <- list(
  altura = list(
    rotulo = "Altura de adultos", unidade = "cm", digitos = 1, extremo = 215,
    gerar = \(n) stats::rnorm(n, 168, 9)
  ),
  renda = list(
    rotulo = "Renda mensal", unidade = "R$", digitos = 0, extremo = 80000,
    gerar = \(n) stats::rlnorm(n, log(2500), 0.8)
  ),
  espera = list(
    rotulo = "Tempo de espera na fila", unidade = "min", digitos = 1, extremo = 150,
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

# Todas as categorias empatadas na maior frequência
moda_tabela <- function(freq) {
  paste(freq$categoria[freq$n == max(freq$n)], collapse = " e ")
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

leitura_assimetria <- function(g) {
  if (abs(g) < 0.5) return("aproximadamente simétrica")
  if (g > 0) "assimétrica à direita (cauda longa nos valores altos)" else "assimétrica à esquerda (cauda longa nos valores baixos)"
}

moda_valores <- function(x) {
  contagem <- table(x)
  as.numeric(names(contagem)[contagem == max(contagem)])
}

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

# Topo de cada aba: nome do tipo, definição e operações válidas
cabecalho_tipo <- function(tipo) {
  t <- tipos_variavel[[tipo]]
  shiny::div(
    class = "mb-2",
    shiny::tags$h4(class = "mb-1", t$nome, shiny::tags$small(class = "text-muted ms-2", t$grupo)),
    shiny::p(
      class = "mb-0", t$definicao, " ",
      shiny::span(class = "badge text-bg-primary ms-1", paste("Operações válidas:", t$operacoes))
    )
  )
}

# Lista de alertas: o que não usar (categóricas) ou cuidados (quantitativas)
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
  shiny::actionButton(id, "Sortear nova amostra", class = "btn-primary", icon = shiny::icon("dice"))
}
