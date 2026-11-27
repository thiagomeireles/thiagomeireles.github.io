# =============================================================================
# Script:  dados.R (cálculos puros de Análise de variância - ANOVA)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Parâmetros fornecidos pelos módulos de interface
# Saída:
#   - Funções puras para simulação de grupos, tabela ANOVA, Bartlett,
#     comparações múltiplas (sem ajuste, Bonferroni, Tukey) e partial pooling
# =============================================================================

simular_anova <- function(k_grupos = 3L, n_grupo = 15L, sep_medias = 3, sd_dentro = 4) {
  medias_reais <- 50 + (seq_len(k_grupos) - mean(seq_len(k_grupos))) * sep_medias
  grupos <- factor(rep(paste0("G", seq_len(k_grupos)), each = n_grupo))
  y <- unlist(purrr::map(medias_reais, \(m) stats::rnorm(n_grupo, mean = m, sd = sd_dentro)))
  df <- data.frame(grupo = grupos, y = y, stringsAsFactors = FALSE)

  aj <- stats::aov(y ~ grupo, data = df)
  tab <- summary(aj)[[1]]
  sq_ent <- tab["grupo", "Sum Sq"]
  sq_den <- tab["Residuals", "Sum Sq"]
  sq_tot <- sq_ent + sq_den
  gl_ent <- tab["grupo", "Df"]
  gl_den <- tab["Residuals", "Df"]
  qm_ent <- tab["grupo", "Mean Sq"]
  qm_den <- tab["Residuals", "Mean Sq"]
  f_val <- tab["grupo", "F value"]
  p_val <- tab["grupo", "Pr(>F)"]
  r2 <- sq_ent / sq_tot

  bart <- stats::bartlett.test(y ~ grupo, data = df)

  list(
    df = df, aj = aj,
    sq_ent = sq_ent, sq_den = sq_den, sq_tot = sq_tot,
    gl_ent = gl_ent, gl_den = gl_den,
    qm_ent = qm_ent, qm_den = qm_den,
    f_val = f_val, p_val = p_val, r2 = r2,
    bart_p = bart$p.value, bart_k2 = unname(bart$statistic),
    residuos = stats::residuals(aj), ajustados = stats::fitted(aj)
  )
}

comparacoes_pares <- function(an) {
  pw_sem <- stats::pairwise.t.test(an$df$y, an$df$grupo, p.adjust.method = "none", pool.sd = TRUE)
  pw_bonf <- stats::pairwise.t.test(an$df$y, an$df$grupo, p.adjust.method = "bonferroni", pool.sd = TRUE)
  tuk <- stats::TukeyHSD(an$aj)$grupo

  pares <- rownames(tuk)
  extrair_pw <- function(mat) {
    purrr::map_dbl(pares, \(par) {
      partes <- strsplit(par, "-", fixed = TRUE)[[1]]
      g1 <- partes[1]; g2 <- partes[2]
      if (g1 %in% rownames(mat) && g2 %in% colnames(mat) && !is.na(mat[g1, g2])) {
        mat[g1, g2]
      } else {
        mat[g2, g1]
      }
    })
  }

  data.frame(
    par = pares,
    diff = unname(tuk[, "diff"]),
    p_sem = extrair_pw(pw_sem$p.value),
    p_bonf = extrair_pw(pw_bonf$p.value),
    p_tukey = unname(tuk[, "p adj"]),
    stringsAsFactors = FALSE
  )
}

simular_partial_pooling <- function(tau = 4, sigma_y = 6) {
  # 5 grupos com tamanhos desiguais (de 4 a 40 observações)
  ns <- c(4L, 6L, 12L, 25L, 40L)
  grupos <- paste0("G", seq_len(5L))
  medias_reais <- c(62, 42, 54, 48, 51)
  y_list <- purrr::map2(ns, medias_reais, \(nk, mk) stats::rnorm(nk, mean = mk, sd = sigma_y))
  y_bars <- purrr::map_dbl(y_list, mean)
  mu_geral <- mean(unlist(y_list))

  prec_dados <- ns / (sigma_y^2)
  prec_prior <- 1 / (tau^2)
  pooling_parcial <- (prec_dados * y_bars + prec_prior * mu_geral) / (prec_dados + prec_prior)

  data.frame(
    grupo = factor(grupos, levels = grupos),
    n = ns,
    sem_pooling = y_bars,
    parcial = pooling_parcial,
    geral = rep(mu_geral, 5L),
    stringsAsFactors = FALSE
  )
}
