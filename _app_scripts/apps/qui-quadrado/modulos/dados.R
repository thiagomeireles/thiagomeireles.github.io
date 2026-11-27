# =============================================================================
# Script:  dados.R (cálculos puros de Qui-quadrado de aderência e independência)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Parâmetros fornecidos pelos módulos de interface
# Saída:
#   - Funções puras para aderência, tabelas de contingência, K-S, teste de
#     correlação (ρ = 0) e comparação Qui-quadrado vs. Fisher
# =============================================================================

calcular_aderencia <- function(o_vec, p0_vec, n_par_est = 0L) {
  p0_norm <- p0_vec / sum(p0_vec)
  n_tot <- sum(o_vec)
  e_vec <- n_tot * p0_norm
  contrib <- ((o_vec - e_vec)^2) / e_vec
  chi2 <- sum(contrib)
  gl <- max(1L, length(o_vec) - 1L - n_par_est)
  valor_p <- 1 - stats::pchisq(chi2, df = gl)

  list(
    o = o_vec, e = e_vec, contrib = contrib,
    chi2 = chi2, gl = gl, valor_p = valor_p,
    min_e = min(e_vec)
  )
}

calcular_contingencia <- function(mat, linhas, colunas) {
  res <- suppressWarnings(stats::chisq.test(mat, correct = FALSE))
  n_tot <- sum(mat)
  min_dim <- min(nrow(mat) - 1L, ncol(mat) - 1L)
  cramer_v <- sqrt(unname(res$statistic) / (n_tot * min_dim))

  grade <- expand.grid(i = seq_len(nrow(mat)), j = seq_len(ncol(mat)))
  grade$linha <- linhas[grade$i]
  grade$coluna <- colunas[grade$j]
  grade$obs <- as.numeric(mat)
  grade$esp <- as.numeric(res$expected)
  grade$contrib <- ((grade$obs - grade$esp)^2) / grade$esp

  list(
    chi2 = unname(res$statistic),
    gl = unname(res$parameter),
    valor_p = res$p.value,
    cramer_v = cramer_v,
    min_esp = min(res$expected),
    grade = grade
  )
}

simular_ks_dados <- function(n = 40L, dist_real = c("normal", "assim", "unif")) {
  dist_real <- match.arg(dist_real)
  x <- switch(
    dist_real,
    normal = stats::rnorm(n, mean = 0, sd = 1),
    assim = (stats::rexp(n, rate = 1) - 1),
    unif = stats::runif(n, min = -sqrt(3), max = sqrt(3))
  )
  ks <- stats::ks.test(x, "pnorm", mean = 0, sd = 1)
  xs_ord <- sort(x)
  fe <- seq_len(n) / n
  f0 <- stats::pnorm(xs_ord)
  diffs <- abs(fe - f0)
  idx_max <- which.max(diffs)

  list(
    df = data.frame(x = xs_ord, fe = fe, f0 = f0, stringsAsFactors = FALSE),
    d_stat = unname(ks$statistic),
    valor_p = ks$p.value,
    x_max = xs_ord[idx_max],
    fe_max = fe[idx_max],
    f0_max = f0[idx_max]
  )
}

simular_corr_teste <- function(n = 30L, rho_real = 0.35) {
  z1 <- stats::rnorm(n)
  z2 <- stats::rnorm(n)
  x <- z1
  y <- rho_real * z1 + sqrt(max(0, 1 - rho_real^2)) * z2
  ct <- stats::cor.test(x, y)
  list(
    r = unname(ct$estimate),
    t_stat = unname(ct$statistic),
    gl = unname(ct$parameter),
    valor_p = ct$p.value
  )
}

comparar_fisher_qui2 <- function(a, b, c_val, d) {
  mat <- matrix(c(a, c_val, b, d), nrow = 2, byrow = TRUE)
  q_sem <- suppressWarnings(stats::chisq.test(mat, correct = FALSE))
  q_yates <- suppressWarnings(stats::chisq.test(mat, correct = TRUE))
  fis <- stats::fisher.test(mat)
  list(
    mat = mat,
    min_esp = min(q_sem$expected),
    p_qui2 = q_sem$p.value,
    p_yates = q_yates$p.value,
    p_fisher = fis$p.value,
    chi2 = unname(q_sem$statistic)
  )
}
