# =============================================================================
# Script:  dados.R (conjuntos de dados e funções puras do tópico Medidas de
#          posição e dispersão)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-25
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/apps/posicao-dispersao/app.R antes
#     dos módulos)
# Saída:
#   - objetos em memória: salarios_mb, ex35_mb, quiz1_af, quiz2_af,
#     exemplos_dados, modelos_robustez e funções puras de quantis, variância,
#     cinco números, simetria, escore z e M-estimador de Huber
# =============================================================================

# Salários da Companhia MB (Morettin e Bussab, Tabela 2.1, n = 36, em salários mínimos)
salarios_mb <- c(
  4.00, 4.56, 5.25, 5.73, 6.26, 6.66, 6.86, 7.39, 7.59, 7.44, 8.12, 8.46,
  8.74, 8.95, 9.13, 9.35, 9.77, 9.80, 10.53, 10.76, 11.06, 11.59, 12.00, 12.79,
  13.23, 13.60, 13.85, 14.69, 14.71, 15.99, 16.22, 16.61, 17.26, 18.75, 19.40, 23.30
)

# Exemplo 3.5 de Morettin e Bussab (n = 9): permite conferir no olho a diferença
# entre pᵢ = (i − 0,5)/n (q1 = 4,50; q3 = 11,25) e stats::fivenum() (5,00 e 11,00)
ex35_mb <- c(15, 5, 3, 8, 10, 2, 7, 11, 12)

# Exemplo 3.7 de Agresti e Finlay (duas turmas com mesma média 5 e mesma amplitude 10,
# mas dispersões distintas: s = 3,35 contra s = 5,14)
quiz1_af <- c(0, 4, 4, 5, 7, 10)
quiz2_af <- c(0, 0, 1, 9, 10, 10)

exemplos_dados <- list(
  salarios = list(
    rotulo = tr("Salários da Cia. MB (n = 36, Morettin e Bussab)", "Salaries at MB Co. (n = 36, Morettin and Bussab)"),
    unidade = tr("salários mínimos", "minimum wages"), un_curta = tr("s.m.", "m.w."),
    fixo = TRUE, digitos = 2, extremo = 55,
    fonte = tr("Morettin e Bussab, Tabela 2.1", "Morettin and Bussab, Table 2.1"),
    gerar = function(n) salarios_mb
  ),
  ex35 = list(
    rotulo = tr("Amostra pequena (n = 9, Exemplo 3.5 de Morettin e Bussab)", "Small sample (n = 9, Morettin and Bussab Ex. 3.5)"),
    unidade = tr("unidades", "units"), un_curta = tr("un.", "un."),
    fixo = TRUE, digitos = 2, extremo = 40,
    fonte = tr("Morettin e Bussab, Exemplo 3.5", "Morettin and Bussab, Example 3.5"),
    gerar = function(n) ex35_mb
  ),
  quiz_meio = list(
    rotulo = tr("Notas concentradas no meio (n = 6, Agresti e Finlay Ex. 3.7a)", "Scores clustered in the middle (n = 6, Agresti and Finlay Ex. 3.7a)"),
    unidade = tr("pontos", "points"), un_curta = tr("pts", "pts"),
    fixo = TRUE, digitos = 2, extremo = 25,
    fonte = tr("Agresti e Finlay, Exemplo 3.7 (amostra 1)", "Agresti and Finlay, Example 3.7 (sample 1)"),
    gerar = function(n) quiz1_af
  ),
  quiz_pontas = list(
    rotulo = tr("Notas polarizadas nos extremos (n = 6, Agresti e Finlay Ex. 3.7b)", "Scores polarized at the extremes (n = 6, Agresti and Finlay Ex. 3.7b)"),
    unidade = tr("pontos", "points"), un_curta = tr("pts", "pts"),
    fixo = TRUE, digitos = 2, extremo = 25,
    fonte = tr("Agresti e Finlay, Exemplo 3.7 (amostra 2)", "Agresti and Finlay, Example 3.7 (sample 2)"),
    gerar = function(n) quiz2_af
  ),
  altura = list(
    rotulo = tr("Altura de adultos (simulada, em sino)", "Adult height (simulated, bell-shaped)"),
    unidade = "cm", un_curta = "cm",
    fixo = FALSE, digitos = 1, extremo = 225,
    fonte = tr("dados simulados: N(170, 9²)", "simulated data: N(170, 9²)"),
    gerar = function(n) round(stats::rnorm(n, mean = 170, sd = 9), 1)
  ),
  renda = list(
    rotulo = tr("Renda mensal (simulada, assimétrica à direita)", "Monthly income (simulated, right-skewed)"),
    unidade = tr("R$ mil", "R$ thousand"), un_curta = tr("R$ mil", "R$ k"),
    fixo = FALSE, digitos = 2, extremo = 35,
    fonte = tr("dados simulados: log-normal", "simulated data: log-normal"),
    gerar = function(n) round(stats::rlnorm(n, meanlog = log(2.5), sdlog = 0.65), 2)
  )
)

# Variância e desvio padrão conforme o divisor ("n" = Bussab cap. 3; "n1" = Agresti / inferência)
var_div <- function(x, divisor = "n1") {
  if (identical(divisor, "n")) mean((x - mean(x))^2) else stats::var(x)
}

dp_div <- function(x, divisor = "n1") sqrt(var_div(x, divisor))

# Desvio médio absoluto em relação à média (Morettin e Bussab, eq. 3.4) e DAM em relação à mediana
dm_media <- function(x) mean(abs(x - mean(x)))
dam_mediana <- function(x) stats::median(abs(x - stats::median(x)))

# Quartis segundo cada convenção:
# - "bm": Morettin e Bussab (pᵢ = (i − 0,5)/n, interpolação linear -> type = 5)
# - "af": Agresti e Finlay (medianas das metades inferior e superior; com n ímpar, exclui a mediana central,
#         enquanto stats::fivenum() de Tukey a inclui nas duas metades)
# - "fivenum": dobradiças de Tukey (stats::fivenum)
# - "r7": padrão do R (pᵢ = (i − 1)/(n − 1) -> type = 7)
quartis_convencao <- function(x, metodo = "bm") {
  s <- sort(x)
  n <- length(s)
  if (identical(metodo, "bm")) {
    return(stats::quantile(s, probs = c(0.25, 0.50, 0.75), type = 5, names = FALSE))
  }
  if (identical(metodo, "af")) {
    md <- stats::median(s)
    if (n %% 2 == 0) {
      metade <- n %/% 2
      return(c(stats::median(s[seq_len(metade)]), md, stats::median(s[(metade + 1):n])))
    }
    if (n == 1) return(rep(s[1], 3))
    meio <- (n + 1) %/% 2
    return(c(stats::median(s[seq_len(meio - 1)]), md, stats::median(s[(meio + 1):n])))
  }
  if (identical(metodo, "fivenum")) {
    fn <- stats::fivenum(s)
    return(fn[c(2, 3, 4)])
  }
  stats::quantile(s, probs = c(0.25, 0.50, 0.75), type = 7, names = FALSE)
}

# Quantil de ordem p arbitrária (bm = type 5; r7 = type 7; tipo1 = inversa generalizada F⁻¹)
quantil_ordem <- function(x, p, metodo = "bm") {
  tipo <- switch(metodo, bm = 5, tipo1 = 1, 7)
  stats::quantile(x, probs = p, type = tipo, names = FALSE)
}

# Esquema dos cinco números, cercas e pontos exteriores / valores atípicos na convenção escolhida
resumo_boxplot <- function(x, metodo = "bm") {
  qs <- quartis_convencao(x, metodo)
  q1 <- qs[1]
  md <- qs[2]
  q3 <- qs[3]
  dq <- q3 - q1
  li_teorico <- q1 - 1.5 * dq
  ls_teorico <- q3 + 1.5 * dq
  dentro <- x[x >= li_teorico & x <= ls_teorico]
  bigode_inf <- if (length(dentro) > 0) min(dentro) else q1
  bigode_sup <- if (length(dentro) > 0) max(dentro) else q3
  exteriores <- x[x < li_teorico | x > ls_teorico]

  list(
    n = length(x), min = min(x), q1 = q1, md = md, q3 = q3, max = max(x),
    dq = dq, cerca_inf = li_teorico, cerca_sup = ls_teorico,
    bigode_inf = bigode_inf, bigode_sup = bigode_sup,
    exteriores = exteriores,
    dist_inf = md - q1, dist_sup = q3 - md
  )
}

# Pares (uᵢ, vᵢ) do gráfico de simetria (Morettin e Bussab, seção 3.5)
pares_simetria <- function(x) {
  s <- sort(x)
  n <- length(s)
  md <- stats::median(s)
  m <- n %/% 2
  idx <- seq_len(m)
  data.frame(
    ordem = idx,
    u = md - s[idx],
    v = s[n + 1 - idx] - md
  )
}

# M-estimador de locação de Huber via mínimos quadrados reponderados (Casella e Berger, seção 10.2.3)
huber_locacao <- function(x, k = 1.5, max_iter = 30, tol = 1e-6) {
  escala <- stats::mad(x, constant = 1.4826)
  theta <- stats::median(x)
  if (!is.finite(escala) || escala < 1e-12) return(theta)

  iter <- 0L
  repeat {
    iter <- iter + 1L
    u <- (x - theta) / escala
    pesos <- ifelse(abs(u) <= k, 1, k / abs(u))
    novo <- sum(pesos * x) / sum(pesos)
    if (abs(novo - theta) < tol || iter >= max_iter) return(novo)
    theta <- novo
  }
}

# Populações simétricas em torno de μ = 0 para comparar média, mediana, média aparada e Huber
modelos_robustez <- list(
  normal = list(
    rotulo = tr("Normal padrão N(0, 1) — caudas leves", "Standard normal N(0, 1) — light tails"),
    mu = 0,
    are_texto = tr(
      "ARE(mediana, média) = 2/π ≈ 0,637: sem valores extremos, a média tem variância 36% menor.",
      "ARE(median, mean) = 2/π ≈ 0.637: without outliers, the mean has 36% smaller variance."
    ),
    gerar = function(n, delta = 0) stats::rnorm(n)
  ),
  contaminada = list(
    rotulo = tr("Normal contaminada: (1 − δ) N(0, 1) + δ N(0, 10²)", "Contaminated normal: (1 − δ) N(0, 1) + δ N(0, 10²)"),
    mu = 0,
    usa_delta = TRUE,
    are_texto = tr(
      "Bastam poucos pontos de variância 100 para inflar Var(x̄), enquanto mediana, média aparada e Huber mal se movem.",
      "Just a few points with variance 100 inflate Var(ȳ), whereas the median, trimmed mean and Huber barely move."
    ),
    gerar = function(n, delta = 0.05) {
      contam <- stats::runif(n) < delta
      ifelse(contam, stats::rnorm(n, 0, 10), stats::rnorm(n, 0, 1))
    }
  ),
  laplace = list(
    rotulo = tr("Dupla exponencial / Laplace (0, 1) — pico agudo e cauda pesada", "Double exponential / Laplace (0, 1) — sharp peak and heavy tails"),
    mu = 0,
    are_texto = tr(
      "ARE(mediana, média) = 2,00: na dupla exponencial a mediana é o estimador de máxima verossimilhança e tem metade da variância assintótica da média.",
      "ARE(median, mean) = 2.00: under the double exponential, the median is the MLE and has half the asymptotic variance of the mean."
    ),
    gerar = function(n, delta = 0) {
      sinal <- ifelse(stats::runif(n) < 0.5, -1, 1)
      sinal * stats::rexp(n, rate = 1)
    }
  ),
  t3 = list(
    rotulo = tr("t de Student com 3 gl — variância finita (3), curtose infinita", "Student's t with 3 df — finite variance (3), infinite kurtosis"),
    mu = 0,
    are_texto = tr(
      "ARE(mediana, média) = 16 / π² ≈ 1,62: caudas pesadas penalizam a soma de quadrados da média.",
      "ARE(median, mean) = 16 / π² ≈ 1.62: heavy tails penalize the squared-loss average."
    ),
    gerar = function(n, delta = 0) stats::rt(n, df = 3)
  )
)

# Perda associada a cada medida de posição: quadrática, absoluta, check (quantil τ) e Huber
perda_posicao <- function(u, tipo = "quadratica", tau = 0.5, k = 1.5) {
  switch(
    tipo,
    quadratica = 0.5 * u^2,
    absoluta   = abs(u),
    check      = u * (tau - ifelse(u < 0, 1, 0)),
    huber      = ifelse(abs(u) <= k, 0.5 * u^2, k * (abs(u) - 0.5 * k))
  )
}

# Formatação com unidade curta ou longa
fmt_un <- function(valor, un, digitos = 2, quadrado = FALSE) {
  u <- if (quadrado) {
    if (grepl("^R\\$", un)) paste0("(", un, ")²") else paste0(un, "²")
  } else {
    un
  }
  if (grepl("^R\\$", un) && !quadrado) {
    paste(u, fmt_num(valor, digitos))
  } else {
    paste(fmt_num(valor, digitos), u)
  }
}

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
