# =============================================================================
# Script:  dados.R (cálculos puros de Testes de hipóteses)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Parâmetros fornecidos pelos módulos de interface
# Saída:
#   - Funções puras para testes Z/t/proporção, região crítica, valor-p,
#     erros tipo I/II, função poder e simulação de múltiplos testes
# =============================================================================

calcular_teste_hipotese <- function(tipo = c("media_z", "media_t", "prop"), est_obs, h0_val, disp, n, alt = c("bilateral", "maior", "menor"), alpha = 0.05) {
  tipo <- match.arg(tipo)
  alt <- match.arg(alt)

  ep <- if (tipo == "prop") sqrt(h0_val * (1 - h0_val) / n) else disp / sqrt(n)
  stat <- (est_obs - h0_val) / ep
  gl <- n - 1L

  p_acum <- if (tipo == "media_t") stats::pt(stat, df = gl) else stats::pnorm(stat)
  valor_p <- switch(
    alt,
    bilateral = 2 * min(p_acum, 1 - p_acum),
    maior = 1 - p_acum,
    menor = p_acum
  )

  q_fn <- if (tipo == "media_t") \(pr) stats::qt(pr, df = gl) else stats::qnorm
  crit <- switch(
    alt,
    bilateral = c(q_fn(alpha / 2), q_fn(1 - alpha / 2)),
    maior = c(NA_real_, q_fn(1 - alpha)),
    menor = c(q_fn(alpha), NA_real_)
  )

  list(
    tipo = tipo, alt = alt, ep = ep, stat = stat, gl = gl,
    valor_p = valor_p, crit = crit, rejeita = valor_p <= alpha
  )
}

calcular_erros_poder <- function(mu0 = 50, mu1 = 53, sigma = 6, n = 25L, alpha = 0.05, alt = c("maior", "bilateral")) {
  alt <- match.arg(alt)
  ep <- sigma / sqrt(n)

  if (alt == "maior") {
    xc_sup <- mu0 + stats::qnorm(1 - alpha) * ep
    xc_inf <- -Inf
    beta <- stats::pnorm((xc_sup - mu1) / ep)
  } else {
    z_c <- stats::qnorm(1 - alpha / 2)
    xc_sup <- mu0 + z_c * ep
    xc_inf <- mu0 - z_c * ep
    beta <- stats::pnorm((xc_sup - mu1) / ep) - stats::pnorm((xc_inf - mu1) / ep)
  }

  list(
    ep = ep, xc_inf = xc_inf, xc_sup = xc_sup,
    alpha = alpha, beta = beta, poder = 1 - beta
  )
}

curva_poder_n <- function(mu0 = 50, sigma = 6, n = 25L, alpha = 0.05, alt = c("maior", "bilateral")) {
  alt <- match.arg(alt)
  ns <- c(max(5L, round(n / 2)), n, n * 2L)
  mus <- seq(mu0 - 3 * sigma / sqrt(min(ns)), mu0 + 3.5 * sigma / sqrt(min(ns)), length.out = 120L)

  dfs <- purrr::map(ns, \(nk) {
    ep <- sigma / sqrt(nk)
    pod <- if (alt == "maior") {
      xc <- mu0 + stats::qnorm(1 - alpha) * ep
      1 - stats::pnorm((xc - mus) / ep)
    } else {
      zc <- stats::qnorm(1 - alpha / 2)
      1 - (stats::pnorm((mu0 + zc * ep - mus) / ep) - stats::pnorm((mu0 - zc * ep - mus) / ep))
    }
    data.frame(mu = mus, poder = pod, n_rot = paste0("n = ", nk), stringsAsFactors = FALSE)
  })
  do.call(rbind, dfs)
}

simular_multiplos_testes <- function(m = 20L, alpha = 0.05) {
  ps <- stats::runif(m)
  falso_pos <- sum(ps <= alpha)
  falso_pos_bonf <- sum(ps <= (alpha / m))
  fwer_teo <- 1 - (1 - alpha)^m
  list(
    ps = ps, falso_pos = falso_pos, falso_pos_bonf = falso_pos_bonf,
    alpha_bonf = alpha / m, fwer_teo = fwer_teo
  )
}
