# =============================================================================
# Script:  dados.R (cálculos puros de Regressão linear simples)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Parâmetros fornecidos pelos módulos de interface
# Saída:
#   - Funções puras para MQO, comparação com reta manual, faixas de confiança
#     e predição, diagnósticos de resíduos e erros-padrão robustos (White HC1)
# =============================================================================

simular_regressao <- function(n = 40L, beta0 = 10, beta1 = 2.0, sigma_e = 4, cenario = c("linear", "quadratico", "heteroced")) {
  cenario <- match.arg(cenario)
  x <- stats::runif(n, min = 1, max = 10)
  e <- if (cenario == "heteroced") {
    stats::rnorm(n, mean = 0, sd = 0.6 * sigma_e * (0.3 + x / 3))
  } else {
    stats::rnorm(n, mean = 0, sd = sigma_e)
  }
  y <- if (cenario == "quadratico") {
    beta0 + beta1 * x + 0.55 * (x - 5.5)^2 + e
  } else {
    beta0 + beta1 * x + e
  }

  df <- data.frame(x = x, y = y, stringsAsFactors = FALSE)
  fit <- stats::lm(y ~ x, data = df)
  sm <- summary(fit)
  an <- stats::anova(fit)
  ci <- stats::confint(fit)

  res <- stats::residuals(fit)
  xc <- x - mean(x)
  sxx <- sum(xc^2)
  # White HC0 e HC1 (com correção n/(n - 2))
  var_hc0 <- sum((xc^2) * (res^2)) / (sxx^2)
  var_hc1 <- (n / (n - 2L)) * var_hc0

  list(
    df = df, fit = fit, sm = sm, an = an, ci = ci,
    alpha_hat = unname(stats::coef(fit)[1]),
    beta_hat = unname(stats::coef(fit)[2]),
    r2 = sm$r.squared,
    s_e = sm$sigma,
    sq_reg = an["x", "Sum Sq"],
    sq_res = an["Residuals", "Sum Sq"],
    ep_ols = sm$coefficients["x", "Std. Error"],
    ep_hc1 = sqrt(var_hc1),
    residuos = res,
    ajustados = stats::fitted(fit)
  )
}

calcular_faixas_reg <- function(reg, z_pred = 6) {
  x_seq <- seq(min(reg$df$x), max(reg$df$x), length.out = 80L)
  nd <- data.frame(x = x_seq)
  ic <- stats::predict(reg$fit, newdata = nd, interval = "confidence")
  ip <- stats::predict(reg$fit, newdata = nd, interval = "prediction")

  nd_z <- data.frame(x = z_pred)
  ic_z <- stats::predict(reg$fit, newdata = nd_z, interval = "confidence")
  ip_z <- stats::predict(reg$fit, newdata = nd_z, interval = "prediction")

  list(
    faixas = data.frame(
      x = x_seq, fit = ic[, "fit"],
      ic_inf = ic[, "lwr"], ic_sup = ic[, "upr"],
      ip_inf = ip[, "lwr"], ip_sup = ip[, "upr"],
      stringsAsFactors = FALSE
    ),
    z_pred = z_pred,
    y_hat_z = ic_z[1, "fit"],
    ic_z = c(ic_z[1, "lwr"], ic_z[1, "upr"]),
    ip_z = c(ip_z[1, "lwr"], ip_z[1, "upr"])
  )
}
