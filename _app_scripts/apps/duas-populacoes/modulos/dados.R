# =============================================================================
# Script:  dados.R (cálculos puros de Comparação de duas populações)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Parâmetros fornecidos pelos módulos de interface
# Saída:
#   - Funções puras para testes t (pooled/Welch), F, Wilcoxon/Mann-Whitney,
#     t pareado, duas proporções e teste de permutação
# =============================================================================

simular_indep <- function(n1 = 25L, n2 = 25L, diff_mu = 3, sd1 = 5, sd2 = 5) {
  x <- stats::rnorm(n1, mean = 50 + diff_mu, sd = sd1)
  y <- stats::rnorm(n2, mean = 50, sd = sd2)
  f_res <- stats::var.test(x, y)
  t_eq <- stats::t.test(x, y, var.equal = TRUE)
  t_we <- stats::t.test(x, y, var.equal = FALSE)
  w_res <- stats::wilcox.test(x, y, exact = FALSE)
  list(x = x, y = y, f_res = f_res, t_eq = t_eq, t_we = t_we, w_res = w_res)
}

simular_pareado_dados <- function(n = 25L, diff_mu = 2.5, corr_pares = 0.80) {
  z1 <- stats::rnorm(n)
  z2 <- stats::rnorm(n)
  y <- 50 + 6 * z1
  x <- 50 + diff_mu + 6 * (corr_pares * z1 + sqrt(max(0, 1 - corr_pares^2)) * z2)
  t_par <- stats::t.test(x, y, paired = TRUE)
  t_ind <- stats::t.test(x, y, paired = FALSE)
  list(x = x, y = y, d = x - y, t_par = t_par, t_ind = t_ind)
}

calcular_duas_prop <- function(k1 = 38L, n1 = 80L, k2 = 26L, n2 = 80L, gama = 0.95) {
  p1 <- k1 / n1
  p2 <- k2 / n2
  p_comb <- (k1 + k2) / (n1 + n2)
  ep_h0 <- sqrt(p_comb * (1 - p_comb) * (1 / n1 + 1 / n2))
  z <- if (ep_h0 > 0) (p1 - p2) / ep_h0 else 0
  valor_p <- 2 * stats::pnorm(-abs(z))

  ep_ic <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
  z_crit <- stats::qnorm((1 + gama) / 2)
  ic <- (p1 - p2) + c(-1, 1) * z_crit * ep_ic

  list(p1 = p1, p2 = p2, p_comb = p_comb, z = z, valor_p = valor_p, ic = ic)
}

simular_permutacao_dados <- function(n1 = 20L, n2 = 20L, assimetrico = FALSE, n_perm = 500L) {
  if (assimetrico) {
    x <- stats::rexp(n1, rate = 1 / 12) + 4
    y <- stats::rexp(n2, rate = 1 / 8)
  } else {
    x <- stats::rnorm(n1, mean = 53, sd = 5)
    y <- stats::rnorm(n2, mean = 50, sd = 5)
  }
  diff_obs <- mean(x) - mean(y)
  combinado <- c(x, y)
  diffs_perm <- purrr::map_dbl(seq_len(n_perm), \(i) {
    perm <- sample(combinado)
    mean(perm[seq_len(n1)]) - mean(perm[(n1 + 1L):(n1 + n2)])
  })
  p_perm <- mean(abs(diffs_perm) >= abs(diff_obs))
  t_res <- stats::t.test(x, y)
  w_res <- stats::wilcox.test(x, y, exact = FALSE)
  list(
    x = x, y = y, diff_obs = diff_obs, diffs_perm = diffs_perm,
    p_perm = p_perm, p_t = t_res$p.value, p_wilcox = w_res$p.value
  )
}
