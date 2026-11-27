# =============================================================================
# Script:  dados.R (funções de cálculo e simulação do app Distribuições normal e t)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/apps/probabilidade/app.R)
# Saída:
#   - funções de densidade, área, rótulos e simulação usadas pelos módulos
# =============================================================================

dens_dist <- function(x, dist, gl, mu = 0, sigma = 1) {
  if (dist == "normal_geral") return(stats::dnorm(x, mean = mu, sd = sigma))
  if (dist == "z") stats::dnorm(x) else stats::dt(x, df = gl)
}

acum_dist <- function(q, dist, gl, mu = 0, sigma = 1) {
  if (dist == "normal_geral") return(stats::pnorm(q, mean = mu, sd = sigma))
  if (dist == "z") stats::pnorm(q) else stats::pt(q, df = gl)
}

quant_dist <- function(p, dist, gl, mu = 0, sigma = 1) {
  if (dist == "normal_geral") return(stats::qnorm(p, mean = mu, sd = sigma))
  if (dist == "z") stats::qnorm(p) else stats::qt(p, df = gl)
}

# Letra da variável aleatória em cada distribuição (Z, X ou T)
letra_dist <- function(dist) switch(dist, z = "Z", normal_geral = "X", t = "T")

# Intervalos (lista de pares inf/sup) que formam a área pedida
intervalos_area <- function(tipo, a, b, centro = 0) {
  afast <- abs(a - centro)
  switch(
    tipo,
    inferior = list(c(-Inf, a)),
    superior = list(c(a, Inf)),
    entre    = list(sort(c(a, b))),
    bicaudal = list(c(-Inf, centro - afast), c(centro + afast, Inf))
  )
}

prob_area <- function(intervalos, dist, gl, mu = 0, sigma = 1) {
  intervalos |>
    purrr::map_dbl(\(l) acum_dist(l[2], dist, gl, mu, sigma) - acum_dist(l[1], dist, gl, mu, sigma)) |>
    sum()
}

dentro_area <- function(x, intervalos) {
  intervalos |>
    purrr::map(\(l) x >= l[1] & x <= l[2]) |>
    purrr::reduce(`|`)
}

rotulo_prob <- function(tipo, a, b, centro = 0, letra = "X") {
  afast <- abs(a - centro)
  switch(
    tipo,
    inferior = sprintf("P(%s ≤ %s)", letra, fmt_num(a, 2)),
    superior = sprintf("P(%s ≥ %s)", letra, fmt_num(a, 2)),
    entre    = sprintf("P(%s ≤ %s ≤ %s)", fmt_num(min(a, b), 2), letra, fmt_num(max(a, b), 2)),
    bicaudal = if (centro == 0) {
      sprintf("P(|%s| ≥ %s)", letra, fmt_num(afast, 2))
    } else {
      sprintf("P(|%s − μ| ≥ %s)", letra, fmt_num(afast, 2))
    }
  )
}

# Intervalo [inf; sup] no padrão do idioma (ponto e vírgula em pt, vírgula em en)
fmt_intervalo <- function(inf, sup, digitos = 2) {
  sprintf(tr("[%s; %s]", "[%s, %s]"), fmt_num(inf, digitos), fmt_num(sup, digitos))
}

# E e Var da distribuição escolhida, com os casos sem momentos da t
texto_momentos <- function(dist, gl, mu = 0, sigma = 1) {
  if (dist != "t") {
    return(sprintf("E(%s) = %s | Var(%s) = %s", letra_dist(dist), fmt_num(mu, 2), letra_dist(dist), fmt_num(sigma^2, 2)))
  }
  if (gl == 1) return(tr("gl = 1 (Cauchy): sem E(T) nem Var(T)", "df = 1 (Cauchy): no E(T) or Var(T)"))
  if (gl == 2) return("E(T) = 0 | Var(T) = ∞")
  sprintf("E(T) = 0 | Var(T) = %s", fmt_num(gl / (gl - 2), 3))
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

simular_pivote_t <- function(n, k_rep, pop = "normal") {
  if (identical(pop, "exponencial")) {
    # Exponencial(1), com μ = 1 e σ = 1: assimétrica, X̄ e S² deixam de ser independentes
    m <- matrix(stats::rexp(n * k_rep, rate = 1), nrow = k_rep)
    mu <- 1
    sigma <- 1
  } else {
    m <- matrix(stats::rnorm(n * k_rep, mean = 0, sd = 1), nrow = k_rep)
    mu <- 0
    sigma <- 1
  }
  medias <- rowMeans(m)
  s2 <- rowSums((m - medias)^2) / (n - 1)
  s <- sqrt(s2)
  z <- (medias - mu) / (sigma / sqrt(n))
  escala_s <- s / sigma
  t_stat <- z / escala_s
  list(z = z, s_sobre_sigma = escala_s, u = (n - 1) * s2 / sigma^2, t = t_stat)
}

# Histograma de T em [−lim; lim] com densidade relativa a todas as amostras,
# para as barras não ficarem infladas quando há valores fora do recorte
histograma_recortado <- function(valores, lim = 6, classes = 48) {
  quebras <- seq(-lim, lim, length.out = classes + 1)
  largura <- quebras[2] - quebras[1]
  dentro <- valores[abs(valores) <= lim]
  contagens <- graphics::hist(dentro, breaks = quebras, plot = FALSE)$counts
  data.frame(
    inf = utils::head(quebras, -1),
    sup = quebras[-1],
    densidade = contagens / (length(valores) * largura)
  )
}
