# =============================================================================
# Script:  dados.R (cálculos puros de Variáveis aleatórias discretas)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Parâmetros fornecidos pelos módulos de interface
# Saída:
#   - Funções puras para momentos, transformação linear, binomial, Poisson,
#     hipergeométrica e geométrica (com e sem deslocamento do R)
# =============================================================================

momentos_discreta <- function(x, probs) {
  probs <- probs / sum(probs)
  ex <- sum(x * probs)
  ex2 <- sum((x^2) * probs)
  vx <- max(0, ex2 - ex^2)
  list(
    x = x,
    prob = probs,
    fda = cumsum(probs),
    media = ex,
    variancia = vx,
    dp = sqrt(vx)
  )
}

transformar_linear <- function(mom, a, b) {
  list(
    a = a,
    b = b,
    media_y = a * mom$media + b,
    variancia_y = (a^2) * mom$variancia,
    dp_y = abs(a) * mom$dp
  )
}

dist_binomial <- function(n, p, k_inf, k_sup) {
  k <- 0:n
  pmf <- stats::dbinom(k, size = n, prob = p)
  cdf <- stats::pbinom(k, size = n, prob = p)
  a <- min(k_inf, k_sup)
  b <- max(k_inf, k_sup)
  prob_interv <- sum(pmf[k >= a & k <= b])
  list(
    df = data.frame(k = k, pmf = pmf, cdf = cdf, destaque = k >= a & k <= b, stringsAsFactors = FALSE),
    media = n * p,
    variancia = n * p * (1 - p),
    dp = sqrt(n * p * (1 - p)),
    prob_interv = prob_interv,
    a = a,
    b = b
  )
}

dist_poisson_vs_binomial <- function(n, p) {
  lambda <- n * p
  k_max <- max(15L, min(n, ceiling(lambda + 5 * sqrt(max(lambda, 1)))))
  k <- 0:k_max
  p_bin <- stats::dbinom(k, size = n, prob = p)
  p_pois <- stats::dpois(k, lambda = lambda)
  list(
    df = data.frame(k = k, binomial = p_bin, poisson = p_pois, stringsAsFactors = FALSE),
    lambda = lambda,
    erro_max = max(abs(p_bin - p_pois)),
    var_bin = n * p * (1 - p),
    var_pois = lambda
  )
}

dist_hiper_vs_bin <- function(pop_n, r_suc, n_amostra) {
  k_min <- max(0L, n_amostra - (pop_n - r_suc))
  k_max <- min(n_amostra, r_suc)
  k <- 0:n_amostra
  p_hiper <- stats::dhyper(k, m = r_suc, n = pop_n - r_suc, k = n_amostra)
  p_prop <- r_suc / pop_n
  p_bin <- stats::dbinom(k, size = n_amostra, prob = p_prop)
  fator_fpc <- if (pop_n > 1L) (pop_n - n_amostra) / (pop_n - 1L) else 0
  list(
    df = data.frame(k = k, hiper = p_hiper, binomial = p_bin, stringsAsFactors = FALSE),
    k_min = k_min,
    k_max = k_max,
    media = n_amostra * p_prop,
    var_hiper = n_amostra * p_prop * (1 - p_prop) * fator_fpc,
    var_bin = n_amostra * p_prop * (1 - p_prop),
    fator_fpc = fator_fpc
  )
}

dist_geometrica_comp <- function(p, s_espera = 3L, t_extra = 2L) {
  k_max <- max(12L, min(30L, ceiling(6 / p)))
  # Convenção de ensaios até o 1º sucesso (x = 1, 2, ...)
  x_ensaios <- 1:k_max
  pmf_ensaios <- stats::dgeom(x_ensaios - 1L, prob = p)
  # Convenção do R: fracassos antes do 1º sucesso (y = 0, 1, ...)
  y_frac <- 0:(k_max - 1L)
  pmf_frac <- stats::dgeom(y_frac, prob = p)

  p_maior_t <- (1 - p)^t_extra
  p_maior_s_t_dado_s <- ((1 - p)^(s_espera + t_extra)) / ((1 - p)^s_espera)

  list(
    df = data.frame(
      valor = x_ensaios,
      pmf_ensaios = pmf_ensaios,
      y_frac = y_frac,
      pmf_frac = pmf_frac,
      stringsAsFactors = FALSE
    ),
    media_ensaios = 1 / p,
    media_frac = (1 - p) / p,
    variancia = (1 - p) / (p^2),
    p_maior_t = p_maior_t,
    p_cond = p_maior_s_t_dado_s
  )
}
