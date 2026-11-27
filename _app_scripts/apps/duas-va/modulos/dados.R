# =============================================================================
# Script:  dados.R (cálculos puros de Duas variáveis aleatórias)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Parâmetros fornecidos pelos módulos de interface
# Saída:
#   - Funções puras para tabela conjunta, marginais, condicionais,
#     independência, soma Z = X + Y, combinações lineares, leis totais e
#     normal bidimensional
# =============================================================================

presets_conjunta <- function(caso = c("dependente", "independente", "cov_nula_dep")) {
  caso <- match.arg(caso)
  if (caso == "dependente") {
    xs <- c(0, 1, 2)
    ys <- c(0, 1, 2)
    mat <- matrix(
      c(0.20, 0.08, 0.02,
        0.07, 0.22, 0.08,
        0.03, 0.09, 0.21),
      nrow = 3, byrow = TRUE
    )
  } else if (caso == "independente") {
    xs <- c(0, 1, 2)
    ys <- c(0, 1, 2)
    px <- c(0.25, 0.50, 0.25)
    py <- c(0.30, 0.40, 0.30)
    mat <- outer(px, py)
  } else {
    xs <- c(-1, 0, 1)
    ys <- c(0, 1)
    mat <- matrix(
      c(0, 1 / 3,
        1 / 3, 0,
        0, 1 / 3),
      nrow = 3, byrow = TRUE
    )
  }
  list(xs = xs, ys = ys, mat = mat / sum(mat))
}

analisar_conjunta <- function(mat, xs, ys) {
  px <- rowSums(mat)
  py <- colSums(mat)
  prod_indep <- outer(px, py)
  desvio_max <- max(abs(mat - prod_indep))

  ex <- sum(xs * px)
  ey <- sum(ys * py)
  vx <- max(0, sum((xs^2) * px) - ex^2)
  vy <- max(0, sum((ys^2) * py) - ey^2)

  grade <- expand.grid(i = seq_along(xs), j = seq_along(ys))
  grade$x <- xs[grade$i]
  grade$y <- ys[grade$j]
  grade$p <- as.numeric(mat)
  grade$p_indep <- as.numeric(prod_indep)

  exy <- sum(grade$x * grade$y * grade$p)
  cov_xy <- exy - ex * ey
  rho <- if (vx > 0 && vy > 0) cov_xy / sqrt(vx * vy) else NA_real_

  # Momentos condicionais de X dado Y = y_j
  cond_y <- purrr::map(seq_along(ys), \(j) {
    pj <- py[j]
    p_x_dado_y <- if (pj > 0) mat[, j] / pj else rep(NA_real_, length(xs))
    ex_y <- sum(xs * p_x_dado_y)
    vx_y <- max(0, sum((xs^2) * p_x_dado_y) - ex_y^2)
    list(y = ys[j], py = pj, p_x_dado_y = p_x_dado_y, media = ex_y, var = vx_y)
  })

  list(
    xs = xs, ys = ys, mat = mat, px = px, py = py,
    prod_indep = prod_indep, desvio_max = desvio_max,
    independentes = desvio_max < 1e-6,
    ex = ex, ey = ey, vx = vx, vy = vy,
    cov_xy = cov_xy, rho = rho,
    grade = grade, cond_y = cond_y
  )
}

distribuicao_soma <- function(grade) {
  zs <- sort(unique(grade$x + grade$y))
  probs <- purrr::map_dbl(zs, \(z) sum(grade$p[grade$x + grade$y == z]))
  data.frame(z = zs, prob = probs, stringsAsFactors = FALSE)
}

combinacao_linear_vars <- function(vx, vy, cov_xy, ex, ey, a, b) {
  media <- a * ex + b * ey
  parte_ind <- (a^2) * vx + (b^2) * vy
  termo_cov <- 2 * a * b * cov_xy
  var_total <- max(0, parte_ind + termo_cov)
  list(
    media = media,
    parte_ind = parte_ind,
    termo_cov = termo_cov,
    var_total = var_total,
    dp_total = sqrt(var_total)
  )
}

leis_totais <- function(an) {
  pys <- purrr::map_dbl(an$cond_y, \(c) c$py)
  medias <- purrr::map_dbl(an$cond_y, \(c) c$media)
  vars <- purrr::map_dbl(an$cond_y, \(c) c$var)

  e_da_cond <- sum(pys * medias)
  e_da_var_cond <- sum(pys * vars)
  var_da_media_cond <- max(0, sum(pys * (medias^2)) - e_da_cond^2)

  list(
    ex = an$ex,
    e_da_cond = e_da_cond,
    vx = an$vx,
    e_da_var_cond = e_da_var_cond,
    var_da_media_cond = var_da_media_cond,
    soma_vars = e_da_var_cond + var_da_media_cond
  )
}

simular_normal_bivariada <- function(n = 400L, rho = 0.7) {
  z1 <- stats::rnorm(n)
  z2 <- stats::rnorm(n)
  x <- z1
  y <- rho * z1 + sqrt(max(0, 1 - rho^2)) * z2
  data.frame(x = x, y = y, stringsAsFactors = FALSE)
}
