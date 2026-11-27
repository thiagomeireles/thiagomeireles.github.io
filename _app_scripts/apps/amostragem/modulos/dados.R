# =============================================================================
# Script:  dados.R (cálculos puros de Amostragem)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Parâmetros fornecidos pelos módulos de interface
# Saída:
#   - Funções puras para AAS com/sem reposição, fator de correção finita,
#     desenhos amostrais (AAS, estratificada, conglomerados), tamanho de
#     amostra e coleta não ignorável
# =============================================================================

criar_populacao_finita <- function(pop_n = 100L) {
  # População determinística assimétrica de tamanho N
  i <- seq_len(pop_n)
  y <- 30 + 25 * (i / pop_n)^1.5 + 8 * sin(i / 3)
  mu <- mean(y)
  sigma2_pop <- mean((y - mu)^2)
  list(y = y, pop_n = pop_n, mu = mu, sigma2_pop = sigma2_pop)
}

simular_aas_fpc <- function(pop_n = 100L, n_amostra = 20L, n_rep = 500L) {
  pop <- criar_populacao_finita(pop_n)
  medias_com <- purrr::map_dbl(seq_len(n_rep), \(i) mean(sample(pop$y, n_amostra, replace = TRUE)))
  medias_sem <- purrr::map_dbl(seq_len(n_rep), \(i) mean(sample(pop$y, n_amostra, replace = FALSE)))

  fpc <- if (pop_n > 1L) (pop_n - n_amostra) / (pop_n - 1L) else 0
  var_teo_com <- pop$sigma2_pop / n_amostra
  var_teo_sem <- var_teo_com * fpc

  list(
    pop = pop,
    medias_com = medias_com,
    medias_sem = medias_sem,
    fpc = fpc,
    var_teo_com = var_teo_com,
    var_teo_sem = var_teo_sem,
    var_emp_com = stats::var(medias_com),
    var_emp_sem = stats::var(medias_sem)
  )
}

criar_pop_desenhos <- function() {
  # 600 unidades em 3 estratos (200 cada) e 20 conglomerados (30 cada)
  # Conglomerados ficam dentro dos estratos, mas com variância intraconglomerado
  n_tot <- 600L
  estrato <- rep(c("E1", "E2", "E3"), each = 200L)
  congl <- rep(seq_len(20L), each = 30L)
  medias_est <- c(E1 = 25, E2 = 50, E3 = 75)
  efeito_congl <- rep(seq(-12, 12, length.out = 20L), each = 30L)
  idx <- seq_len(n_tot)
  y <- unname(medias_est[estrato]) + efeito_congl + 4 * cos(idx * 0.7)
  data.frame(id = idx, estrato = estrato, congl = congl, y = y, stringsAsFactors = FALSE)
}

simular_desenhos <- function(pop, n_amostra = 60L, n_rep = 300L) {
  n_tot <- nrow(pop)
  estratos <- c("E1", "E2", "E3")
  n_por_est <- rep(floor(n_amostra / 3L), 3L)
  resto <- n_amostra - sum(n_por_est)
  if (resto > 0L) n_por_est[seq_len(resto)] <- n_por_est[seq_len(resto)] + 1L

  m_congl <- max(2L, round(n_amostra / 30L))

  medias_aas <- purrr::map_dbl(seq_len(n_rep), \(i) mean(pop$y[sample.int(n_tot, n_amostra, replace = FALSE)]))

  medias_est <- purrr::map_dbl(seq_len(n_rep), \(i) {
    vals <- purrr::map2(estratos, n_por_est, \(est, nk) {
      idx_e <- which(pop$estrato == est)
      pop$y[sample(idx_e, nk, replace = FALSE)]
    })
    mean(unlist(vals))
  })

  medias_congl <- purrr::map_dbl(seq_len(n_rep), \(i) {
    c_esc <- sample.int(20L, m_congl, replace = FALSE)
    mean(pop$y[pop$congl %in% c_esc])
  })

  list(
    mu = mean(pop$y),
    aas = medias_aas,
    estratificada = medias_est,
    conglomerados = medias_congl,
    m_congl = m_congl
  )
}

calcular_n_amostra <- function(tipo = c("media", "proporcao"), gama = 0.95, eps = 0.5, sigma = 4, p_esp = 0.5, pop_n = Inf) {
  tipo <- match.arg(tipo)
  z <- stats::qnorm((1 + gama) / 2)
  n0 <- if (tipo == "media") {
    (sigma^2) * (z^2) / (eps^2)
  } else {
    (z^2) * p_esp * (1 - p_esp) / (eps^2)
  }
  n_inf <- ceiling(n0)
  n_fin <- if (is.finite(pop_n) && pop_n > 1) ceiling(n0 / (1 + (n0 - 1) / pop_n)) else n_inf
  list(z = z, n0 = n0, n_inf = n_inf, n_fin = n_fin)
}

simular_nao_ignoravel <- function(n_amostra = 300L, vies_nr = 0.70, n_rep = 300L) {
  # População de 2.000 indivíduos com renda/valor y ~ N(50, 12^2)
  # Probabilidade de resposta cai quando y > 55 (mecanismo não ignorável)
  pop_y <- 50 + 12 * stats::qnorm((seq_len(2000L) - 0.5) / 2000L)
  mu <- mean(pop_y)
  prob_resp <- ifelse(pop_y > 55, 1 - vies_nr, 0.95)

  medias_aas <- purrr::map_dbl(seq_len(n_rep), \(i) mean(sample(pop_y, n_amostra, replace = FALSE)))
  medias_nr <- purrr::map_dbl(seq_len(n_rep), \(i) {
    idx <- sample.int(2000L, n_amostra, replace = FALSE)
    resp <- stats::rbinom(n_amostra, size = 1L, prob = prob_resp[idx]) == 1L
    if (sum(resp) == 0L) mean(pop_y[idx]) else mean(pop_y[idx[resp]])
  })

  list(mu = mu, medias_aas = medias_aas, medias_nr = medias_nr)
}
