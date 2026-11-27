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

# Conjuntas de partida. As quatro primeiras são do cap. 8 de Morettin e Bussab;
# a matriz tem X nas linhas e Y nas colunas, em frações do livro.
presets_conjunta <- function(caso = c("meninos", "trocas", "cov_nula", "meninas", "parabola")) {
  caso <- match.arg(caso)
  switch(caso,
    meninos = list(
      xs = 0:3, ys = 0:1, den = 8,
      mat = matrix(c(1, 0,
                     2, 1,
                     1, 2,
                     0, 1), nrow = 4, byrow = TRUE)
    ),
    trocas = list(
      xs = 0:2, ys = 0:1, den = 8,
      mat = matrix(c(1, 1,
                     2, 2,
                     1, 1), nrow = 3, byrow = TRUE)
    ),
    cov_nula = list(
      xs = 0:2, ys = 1:3, den = 20,
      mat = matrix(c(3, 1, 4,
                     3, 1, 1,
                     2, 2, 3), nrow = 3, byrow = TRUE)
    ),
    meninas = list(
      xs = 0:3, ys = 0:3, den = 8,
      mat = matrix(c(0, 0, 0, 1,
                     0, 0, 3, 0,
                     0, 3, 0, 0,
                     1, 0, 0, 0), nrow = 4, byrow = TRUE)
    ),
    parabola = list(
      xs = -1:1, ys = 0:1, den = 3,
      mat = matrix(c(0, 1,
                     1, 0,
                     0, 1), nrow = 3, byrow = TRUE)
    )
  ) |>
    (\(pr) {
      pr$mat <- pr$mat / pr$den
      pr
    })()
}

rotulos_presets <- function() {
  stats::setNames(
    c("meninos", "trocas", "cov_nula", "meninas", "parabola"),
    c(
      tr("Meninos × primeiro filho (Ex. 8.1)", "Boys × first child (Ex. 8.1)"),
      tr("Trocas de sexo × primeiro filho (Ex. 8.3)", "Sex changes × first child (Ex. 8.3)"),
      tr("Covariância nula, mas dependentes (Ex. 8.7)", "Zero covariance, yet dependent (Ex. 8.7)"),
      tr("Meninos × meninas (Ex. 8.10)", "Boys × girls (Ex. 8.10)"),
      tr("Y = X², com X em {−1, 0, 1}", "Y = X², with X in {−1, 0, 1}")
    )
  )
}

# O que X e Y significam em cada conjunta de partida
descricao_preset <- function(caso) {
  switch(caso,
    meninos = tr(
      "Famílias com três crianças, todas as composições igualmente prováveis. X = número de meninos; Y = 1 se o primeiro filho é menino e 0 se é menina (Morettin e Bussab, Ex. 8.1, Tabela 8.5).",
      "Families with three children, all compositions equally likely. X = number of boys; Y = 1 if the first child is a boy and 0 if a girl (Morettin and Bussab, Ex. 8.1, Table 8.5)."
    ),
    trocas = tr(
      "Mesmas famílias. X = número de vezes em que o sexo muda de um nascimento para o seguinte (Z no livro); Y = 1 se o primeiro filho é menino (Morettin e Bussab, Ex. 8.3, Tabela 8.8).",
      "Same families. X = number of times the sex changes from one birth to the next (Z in the book); Y = 1 if the first child is a boy (Morettin and Bussab, Ex. 8.3, Table 8.8)."
    ),
    cov_nula = tr(
      "Tabela sem história por trás, montada para dar E(XY) = E(X)E(Y) sem independência (Morettin e Bussab, Ex. 8.7, Tabela 8.12).",
      "A table with no story behind it, built so that E(XY) = E(X)E(Y) without independence (Morettin and Bussab, Ex. 8.7, Table 8.12)."
    ),
    meninas = tr(
      "Famílias com três crianças. X = número de meninos; Y = número de meninas (W no livro). Como X + Y = 3, a relação é linear e exata (Morettin e Bussab, Ex. 8.10, Tabela 8.13).",
      "Families with three children. X = number of boys; Y = number of girls (W in the book). Since X + Y = 3, the relation is exactly linear (Morettin and Bussab, Ex. 8.10, Table 8.13)."
    ),
    tr(
      "X vale −1, 0 ou 1 com probabilidade 1/3 cada, e Y = X². Y depende totalmente de X, mas a covariância é zero, porque E(X) = E(X³) = 0 (Wooldridge, apêndice B, propriedade COV.1).",
      "X takes −1, 0 or 1 with probability 1/3 each, and Y = X². Y depends entirely on X, yet the covariance is zero because E(X) = E(X³) = 0 (Wooldridge, appendix B, property COV.1)."
    )
  )
}

# Zera resíduos de ponto flutuante (evita "−0,0000" nas tabelas)
limpar_zero <- function(x, tol = 1e-12) {
  ifelse(is.finite(x) & abs(x) < tol, 0, x)
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
  cov_xy <- limpar_zero(exy - ex * ey)
  rho <- if (vx > 0 && vy > 0) limpar_zero(cov_xy / sqrt(vx * vy)) else NA_real_

  # Distribuição de X dado Y = y_j; só existe quando P(Y = y_j) > 0
  cond_y <- purrr::map(seq_along(ys), \(j) {
    pj <- py[j]
    if (pj <= 1e-12) {
      return(list(y = ys[j], py = pj, p_x_dado_y = rep(NA_real_, length(xs)), media = NA_real_, var = NA_real_))
    }
    p_x_dado_y <- mat[, j] / pj
    ex_y <- sum(xs * p_x_dado_y)
    vx_y <- max(0, sum((xs^2) * p_x_dado_y) - ex_y^2)
    list(y = ys[j], py = pj, p_x_dado_y = p_x_dado_y, media = ex_y, var = vx_y)
  })

  list(
    xs = xs, ys = ys, mat = mat, px = px, py = py,
    prod_indep = prod_indep, desvio_max = limpar_zero(desvio_max),
    independentes = desvio_max < 1e-9,
    ex = ex, ey = ey, vx = vx, vy = vy, exy = exy,
    cov_xy = cov_xy, rho = rho,
    grade = grade, cond_y = cond_y
  )
}

# Tabela de dupla entrada com as marginais, no formato da Tabela 8.5
tabela_dupla_entrada <- function(an, digitos = 3) {
  corpo <- t(an$mat)
  df <- data.frame(
    rotulo = c(paste0("Y = ", an$ys), "p_X(x)"),
    rbind(corpo, an$px) |> (\(m) matrix(fmt_num(m, digitos), nrow = nrow(m)))(),
    c(fmt_num(an$py, digitos), fmt_num(1, digitos)),
    stringsAsFactors = FALSE
  )
  names(df) <- c("", paste0("X = ", an$xs), "p_Y(y)")
  df
}

distribuicao_soma <- function(grade) {
  zs <- sort(unique(grade$x + grade$y))
  probs <- purrr::map_dbl(zs, \(z) sum(grade$p[grade$x + grade$y == z]))
  df <- data.frame(z = zs, prob = probs, stringsAsFactors = FALSE)
  df[df$prob > 1e-12, , drop = FALSE]
}

momentos_soma <- function(an) {
  df_z <- distribuicao_soma(an$grade)
  ez <- sum(df_z$z * df_z$prob)
  vz <- limpar_zero(max(0, sum(df_z$z^2 * df_z$prob) - ez^2))
  list(
    ez = ez,
    soma_medias = an$ex + an$ey,
    vz = vz,
    soma_vars = an$vx + an$vy,
    termo_cov = 2 * an$cov_xy
  )
}

combinacao_linear_vars <- function(vx, vy, cov_xy, ex, ey, a, b) {
  media <- a * ex + b * ey
  parte_ind <- (a^2) * vx + (b^2) * vy
  termo_cov <- limpar_zero(2 * a * b * cov_xy)
  var_total <- limpar_zero(max(0, parte_ind + termo_cov))
  list(
    media = media,
    parte_ind = parte_ind,
    termo_cov = termo_cov,
    var_total = var_total,
    dp_total = sqrt(var_total)
  )
}

# Coeficiente sem casas decimais desnecessárias: 1, −0,5, 2
fmt_coef <- function(x) {
  txt <- if (isTRUE(all.equal(x, round(x)))) sprintf("%d", as.integer(round(x))) else fmt_num(x, 1)
  sub("^-", "−", txt)
}

# Combinação aX + bY escrita como no papel: X − Y, 2X + 0,5Y, −Y, 0
fmt_combinacao <- function(a, b) {
  termo <- function(coef, var) {
    if (coef == 0) return(NULL)
    modulo <- if (abs(coef) == 1) "" else sub("^−", "", fmt_coef(abs(coef)))
    list(sinal = if (coef < 0) "−" else "+", texto = paste0(modulo, var))
  }
  termos <- purrr::compact(list(termo(a, "X"), termo(b, "Y")))
  if (length(termos) == 0) return("0")
  primeiro <- paste0(if (termos[[1]]$sinal == "−") "−" else "", termos[[1]]$texto)
  resto <- purrr::map_chr(termos[-1], \(t) paste0(" ", t$sinal, " ", t$texto))
  paste0(primeiro, paste(resto, collapse = ""))
}

leis_totais <- function(an) {
  validos <- purrr::keep(an$cond_y, \(c) c$py > 1e-12)
  pys <- purrr::map_dbl(validos, \(c) c$py)
  medias <- purrr::map_dbl(validos, \(c) c$media)
  vars <- purrr::map_dbl(validos, \(c) c$var)

  e_da_cond <- sum(pys * medias)
  e_da_var_cond <- sum(pys * vars)
  var_da_media_cond <- limpar_zero(max(0, sum(pys * (medias^2)) - e_da_cond^2))

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

# Linha discreta que avisa qual conjunta da aba 1 está em uso
nota_conjunta_em_uso <- function(an) {
  shiny::p(
    class = "small text-muted mb-2",
    sprintf(tr("Conjunta em uso (escolhida na aba 1): %s.", "Joint distribution in use (chosen in tab 1): %s."), an$rotulo)
  )
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
