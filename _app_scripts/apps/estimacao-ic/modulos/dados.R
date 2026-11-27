# =============================================================================
# Script:  dados.R (cálculos puros de Estimação e intervalos de confiança)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Parâmetros fornecidos pelos módulos de interface
# Saída:
#   - Funções puras para propriedades de estimadores, cobertura de ICs para
#     média e proporção (Wald vs. Wilson), MLE e intervalo credível bayesiano
# =============================================================================

simular_propriedades <- function(n = 20L, sigma = 5, n_rep = 500L) {
  mu <- 50
  mat <- matrix(stats::rnorm(n * n_rep, mean = mu, sd = sigma), nrow = n_rep)
  medias <- rowMeans(mat)
  medianas <- purrr::map_dbl(seq_len(n_rep), \(i) stats::median(mat[i, ]))

  # Variância amostral: n - 1 vs. n
  ss <- rowSums((mat - medias)^2)
  s2_n1 <- ss / (n - 1L)
  s2_n <- ss / n
  sigma2 <- sigma^2

  resumo <- function(est, alvo) {
    vies <- mean(est) - alvo
    vr <- stats::var(est)
    c(vies = vies, var = vr, eqm = vr + vies^2)
  }

  list(
    mu = mu, sigma2 = sigma2,
    medias = medias, medianas = medianas,
    res_media = resumo(medias, mu),
    res_mediana = resumo(medianas, mu),
    res_s2_n1 = resumo(s2_n1, sigma2),
    res_s2_n = resumo(s2_n, sigma2)
  )
}

simular_cobertura_media <- function(n = 20L, gama = 0.95, sigma = 5, usar_t = TRUE, n_ics = 100L) {
  mu <- 50
  mat <- matrix(stats::rnorm(n * n_ics, mean = mu, sd = sigma), nrow = n_ics)
  xbar <- rowMeans(mat)
  crit <- if (usar_t) stats::qt((1 + gama) / 2, df = n - 1L) else stats::qnorm((1 + gama) / 2)
  ep <- if (usar_t) {
    sqrt(rowSums((mat - xbar)^2) / (n - 1L)) / sqrt(n)
  } else {
    rep(sigma / sqrt(n), n_ics)
  }
  inf <- xbar - crit * ep
  sup <- xbar + crit * ep
  cobre <- (inf <= mu) & (sup >= mu)
  data.frame(
    id = seq_len(n_ics), xbar = xbar, inf = inf, sup = sup, cobre = cobre,
    stringsAsFactors = FALSE
  )
}

ic_wald_vec <- function(phat, n, z) {
  ep <- sqrt(phat * (1 - phat) / n)
  cbind(inf = phat - z * ep, sup = phat + z * ep)
}

ic_wilson_vec <- function(phat, n, z) {
  z2 <- z^2
  denom <- 1 + z2 / n
  centro <- (phat + z2 / (2 * n)) / denom
  margem <- (z * sqrt(phat * (1 - phat) / n + z2 / (4 * n^2))) / denom
  cbind(inf = centro - margem, sup = centro + margem)
}

simular_cobertura_prop <- function(n = 25L, p_verd = 0.10, gama = 0.95, n_ics = 400L) {
  z <- stats::qnorm((1 + gama) / 2)
  y <- stats::rbinom(n_ics, size = n, prob = p_verd)
  phat <- y / n
  w1 <- ic_wald_vec(phat, n, z)
  w2 <- ic_wilson_vec(phat, n, z)
  cob_wald <- mean(w1[, "inf"] <= p_verd & w1[, "sup"] >= p_verd)
  cob_wilson <- mean(w2[, "inf"] <= p_verd & w2[, "sup"] >= p_verd)
  larg_wald <- mean(w1[, "sup"] - w1[, "inf"])
  larg_wilson <- mean(w2[, "sup"] - w2[, "inf"])

  list(
    cob_wald = cob_wald, cob_wilson = cob_wilson,
    larg_wald = larg_wald, larg_wilson = larg_wilson
  )
}

comparar_mle_bayes <- function(n = 30L, y = 9L, alpha0 = 1, beta0 = 1, gama = 0.95) {
  p_grid <- seq(0.002, 0.998, length.out = 400L)
  lik <- stats::dbinom(y, size = n, prob = p_grid)
  lik_norm <- lik / max(lik)
  priori <- stats::dbeta(p_grid, alpha0, beta0)
  post <- stats::dbeta(p_grid, alpha0 + y, beta0 + n - y)

  mle <- y / n
  z <- stats::qnorm((1 + gama) / 2)
  ic_w <- as.numeric(ic_wald_vec(mle, n, z))
  ic_bayes <- stats::qbeta(c((1 - gama) / 2, (1 + gama) / 2), alpha0 + y, beta0 + n - y)
  media_post <- (alpha0 + y) / (alpha0 + beta0 + n)

  list(
    df = data.frame(p = p_grid, lik_norm = lik_norm, priori = priori, post = post, stringsAsFactors = FALSE),
    mle = mle, media_post = media_post,
    ic_wald = ic_w, ic_bayes = ic_bayes
  )
}
