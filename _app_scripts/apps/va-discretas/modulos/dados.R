# =============================================================================
# Script:  dados.R (cálculos puros de Variáveis aleatórias discretas)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Parâmetros fornecidos pelos módulos de interface
# Saída:
#   - Exemplos dos livros, funções puras para momentos, quantis, transformação
#     linear, binomial, Poisson, processo de Poisson, hipergeométrica e
#     geométrica (com e sem deslocamento do R) e cartão de cuidados
# =============================================================================

# Distribuições de partida da aba 1, todas tiradas dos livros
exemplos_fp <- function() {
  list(
    gss = list(
      x = 0:5,
      p = c(0.01, 0.03, 0.60, 0.23, 0.12, 0.01),
      rotulo = tr("Nº ideal de filhos (Agresti e Finlay, Tab. 4.1)", "Ideal number of children (Agresti and Finlay, Table 4.1)"),
      descricao = tr(
        "X = número ideal de filhos para uma família, segundo um adulto sorteado no General Social Survey (EUA).",
        "X = ideal number of children for a family, according to a randomly chosen adult in the General Social Survey (US)."
      )
    ),
    lucro = list(
      x = c(-5, 5, 10, 15),
      p = c(0.19, 0.02, 0.23, 0.56),
      rotulo = tr("Lucro por montagem (Morettin e Bussab, Ex. 6.1)", "Profit per assembly (Morettin and Bussab, Ex. 6.1)"),
      descricao = tr(
        "X = lucro, em reais, por conjunto montado com um cilindro e uma esfera de fábricas diferentes (Tabela 6.3).",
        "X = profit, in reais, per unit assembled from a cylinder and a sphere made by different factories (Table 6.3)."
      )
    ),
    dado = list(
      x = 1:6,
      p = rep(1 / 6, 6),
      rotulo = tr("Uniforme discreta: dado (Morettin e Bussab, Ex. 6.10)", "Discrete uniform: die (Morettin and Bussab, Ex. 6.10)"),
      descricao = tr(
        "X = pontos na face de cima de um dado equilibrado. Cada valor tem probabilidade 1/6.",
        "X = points on the top face of a fair die. Each value has probability 1/6."
      )
    ),
    bernoulli = list(
      x = 0:1,
      p = c(5 / 6, 1 / 6),
      rotulo = tr("Bernoulli: face 5 no dado (Morettin e Bussab, Ex. 6.11)", "Bernoulli: face 5 on a die (Morettin and Bussab, Ex. 6.11)"),
      descricao = tr(
        "X = 1 se sai a face 5 e X = 0 se sai outra face. É um ensaio de Bernoulli com p = 1/6.",
        "X = 1 if face 5 comes up and X = 0 otherwise. It is a Bernoulli trial with p = 1/6."
      )
    )
  )
}

# Agrupa valores repetidos (ex.: Y = 0·X + b) e ordena
agrupar_fp <- function(x, probs) {
  x <- round(x, 10)
  valores <- sort(unique(x))
  list(x = valores, prob = purrr::map_dbl(valores, \(v) sum(probs[x == v])))
}

# Quantil Q(p) de Morettin e Bussab (6.26): P(X ≤ Q) ≥ p e P(X ≥ Q) ≥ 1 − p.
# Devolve o intervalo [inf, sup] de valores que satisfazem a definição.
quantil_discreto <- function(x, probs, p = 0.5, tol = 1e-9) {
  fda <- cumsum(probs)
  sobrevive <- rev(cumsum(rev(probs)))
  ok <- fda >= p - tol & sobrevive >= 1 - p - tol
  inf <- x[which(ok)[1]]
  sup <- x[utils::tail(which(ok), 1)]
  list(inf = inf, sup = sup, unico = isTRUE(abs(sup - inf) < tol))
}

momentos_discreta <- function(x, probs) {
  agr <- agrupar_fp(x, probs / sum(probs))
  x <- agr$x
  probs <- agr$prob
  ex <- sum(x * probs)
  ex2 <- sum((x^2) * probs)
  vx <- max(0, ex2 - ex^2)
  list(
    x = x,
    prob = probs,
    fda = cumsum(probs),
    media = ex,
    ex2 = ex2,
    variancia = vx,
    dp = sqrt(vx),
    mediana = quantil_discreto(x, probs, 0.5)
  )
}

# Y = aX + b: momentos pelas fórmulas (6.4)-(6.5) e distribuição de Y calculada
# valor a valor, para o leitor conferir que as duas contas batem
transformar_linear <- function(mom, a, b) {
  list(
    a = a,
    b = b,
    media_y = a * mom$media + b,
    variancia_y = (a^2) * mom$variancia,
    dp_y = abs(a) * mom$dp,
    dist_y = momentos_discreta(a * mom$x + b, mom$prob)
  )
}

# Exemplos dos livros para as abas de binomial, hipergeométrica e processo
exemplos_binomial <- function() {
  list(
    moeda = list(n = 3, p = 0.5, k = c(2, 2), rotulo = tr("Duas caras em 3 lançamentos (Ex. 6.12)", "Two heads in 3 tosses (Ex. 6.12)")),
    pecas = list(n = 10, p = 0.1, k = c(10, 10), rotulo = tr("10 peças defeituosas em 10 (Ex. 6.13)", "10 defective parts out of 10 (Ex. 6.13)")),
    b17 = list(n = 20, p = 0.9, k = c(17, 17), rotulo = tr("b(17; 20; 0,9) (Ex. 6.14)", "b(17; 20, 0.9) (Ex. 6.14)")),
    bernoulli = list(n = 1, p = 0.5, k = c(1, 1), rotulo = tr("Bernoulli: n = 1", "Bernoulli: n = 1"))
  )
}

exemplos_hiper <- function() {
  list(
    mb = list(N = 100, r = 10, n = 5, rotulo = tr("Lote de 100 peças (Morettin e Bussab, Ex. 6.15)", "Lot of 100 parts (Morettin and Bussab, Ex. 6.15)")),
    cb = list(N = 25, r = 6, n = 10, rotulo = tr("Inspeção de lote (Casella e Berger, Ex. 3.2.1)", "Acceptance sampling (Casella and Berger, Ex. 3.2.1)"))
  )
}

# Probabilidades muito pequenas em notação científica (ex.: 1e-10 do Ex. 6.13)
fmt_prob <- function(x, digitos = 4) {
  if (x > 0 && x < 10^(-digitos)) {
    expoente <- floor(log10(x))
    sobrescrito <- chartr("-0123456789", "⁻⁰¹²³⁴⁵⁶⁷⁸⁹", as.character(expoente))
    sprintf("%s × 10%s", fmt_num(x / 10^expoente, 2), sobrescrito)
  } else {
    fmt_num(x, digitos)
  }
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
    var_pois = lambda,
    regra_ok = lambda <= 7
  )
}

# Processo de Poisson (Morettin e Bussab, seção 6.7): N_t ~ Po(λt)
processo_poisson <- function(taxa, t, k) {
  media <- taxa * t
  k_max <- max(10L, k, ceiling(media + 4 * sqrt(media)))
  valores <- 0:k_max
  list(
    df = data.frame(k = valores, prob = stats::dpois(valores, media), destaque = valores <= k),
    media = media,
    prob_ate_k = stats::ppois(k, media),
    prob_zero = stats::dpois(0, media)
  )
}

dist_hiper_vs_bin <- function(pop_n, r_suc, n_amostra) {
  k <- 0:n_amostra
  p_hiper <- stats::dhyper(k, m = r_suc, n = pop_n - r_suc, k = n_amostra)
  p_prop <- r_suc / pop_n
  p_bin <- stats::dbinom(k, size = n_amostra, prob = p_prop)
  fator_fpc <- if (pop_n > 1L) (pop_n - n_amostra) / (pop_n - 1L) else 0
  list(
    df = data.frame(k = k, hiper = p_hiper, binomial = p_bin, stringsAsFactors = FALSE),
    media = n_amostra * p_prop,
    var_hiper = n_amostra * p_prop * (1 - p_prop) * fator_fpc,
    var_bin = n_amostra * p_prop * (1 - p_prop),
    fator_fpc = fator_fpc,
    p0_hiper = p_hiper[1],
    p0_bin = p_bin[1]
  )
}

# Geométrica em x = 1, 2, ... (ensaios até o 1º sucesso; Casella e Berger, 3.2.9).
# A distribuição condicional de X − s dado X > s sai da própria FP, sem usar a
# identidade algébrica, para o leitor ver que ela coincide com a original.
dist_geometrica_comp <- function(p, s_espera = 3L, t_extra = 2L) {
  k_max <- max(12L, min(30L, ceiling(6 / p)))
  suporte <- 1:5000
  pmf_longa <- stats::dgeom(suporte - 1L, prob = p)
  p_maior_s <- sum(pmf_longa[suporte > s_espera])

  x <- 1:k_max
  pmf_cond <- pmf_longa[s_espera + x] / p_maior_s

  list(
    df = data.frame(
      x = x,
      pmf = pmf_longa[x],
      pmf_cond = pmf_cond,
      stringsAsFactors = FALSE
    ),
    media_ensaios = 1 / p,
    media_frac = (1 - p) / p,
    variancia = (1 - p) / (p^2),
    p_maior_t = sum(pmf_longa[suporte > t_extra]),
    p_cond = sum(pmf_longa[suporte > s_espera + t_extra]) / p_maior_s
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
