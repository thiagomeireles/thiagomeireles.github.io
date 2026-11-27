# =============================================================================
# Script:  dados.R (cálculos puros de Probabilidade condicional e Bayes)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Parâmetros fornecidos pelos módulos de interface
# Saída:
#   - Funções puras para probabilidade condicional, independência, árvores,
#     Teorema de Bayes, chances a posteriori, contraexemplos, Três Prisioneiros,
#     atualização sequencial (hemofilia) e correção ortográfica ('radom')
# =============================================================================

# Regra do produto e inversão condicional a partir de P(B), P(A|B) e P(A|B^c)
calcular_condicional <- function(p_b, p_a_dado_b, p_a_dado_bc) {
  p_bc <- 1 - p_b
  p_a_inter_b <- p_b * p_a_dado_b
  p_a_inter_bc <- p_bc * p_a_dado_bc
  p_a <- p_a_inter_b + p_a_inter_bc

  p_b_dado_a <- if (p_a > 0) p_a_inter_b / p_a else NA_real_
  p_bc_dado_a <- if (p_a > 0) p_a_inter_bc / p_a else NA_real_

  celulas <- data.frame(
    grupo_b = c("B", "B", "Bc", "Bc"),
    grupo_a = c("A", "Ac", "A", "Ac"),
    xmin = c(0, 0, p_b, p_b),
    xmax = c(p_b, p_b, 1, 1),
    ymin = c(0, p_a_dado_b, 0, p_a_dado_bc),
    ymax = c(p_a_dado_b, 1, p_a_dado_bc, 1),
    prob = c(p_a_inter_b, p_b * (1 - p_a_dado_b), p_a_inter_bc, p_bc * (1 - p_a_dado_bc)),
    stringsAsFactors = FALSE
  )

  list(
    p_b = p_b,
    p_bc = p_bc,
    p_a_dado_b = p_a_dado_b,
    p_a_dado_bc = p_a_dado_bc,
    p_a_inter_b = p_a_inter_b,
    p_a_inter_bc = p_a_inter_bc,
    p_a = p_a,
    p_b_dado_a = p_b_dado_a,
    p_bc_dado_a = p_bc_dado_a,
    celulas = celulas
  )
}

# Passo do slider da interseção: P(A) e P(B) andam de 0,05 em 0,05, então
# P(A)P(B) é sempre múltiplo de 0,0025 e a independência exata fica alcançável
passo_intersecao <- 0.0025
arredondar_passo <- function(x, passo = passo_intersecao) round(round(x / passo) * passo, 4)

# Verificação de independência vs. eventos mutuamente exclusivos
verificar_independencia <- function(p_a, p_b, p_a_inter_b) {
  min_inter <- max(0, p_a + p_b - 1)
  max_inter <- min(p_a, p_b)
  p_inter <- min(max(p_a_inter_b, min_inter), max_inter)
  prod_indep <- p_a * p_b
  p_a_dado_b <- if (p_b > 0) p_inter / p_b else NA_real_
  p_b_dado_a <- if (p_a > 0) p_inter / p_a else NA_real_

  list(
    p_a = p_a,
    p_b = p_b,
    p_a_inter_b = p_inter,
    min_inter = min_inter,
    max_inter = max_inter,
    prod_indep = prod_indep,
    diferenca = p_inter - prod_indep,
    independentes = abs(p_inter - prod_indep) < 1e-6,
    disjuntos = p_inter < 1e-6,
    disjuncao_possivel = min_inter < 1e-6,
    p_a_dado_b = p_a_dado_b,
    p_b_dado_a = p_b_dado_a
  )
}

# Árvore de 2 etapas para a urna de Morettin e Bussab (Exs. 5.10 e 5.11):
# bolas brancas (B) e vermelhas (V), duas extrações com ou sem reposição.
# As frações ficam como no livro, sem simplificar (2/4, e não 1/2).
calcular_arvore_urna <- function(n_branca, n_verm, com_reposicao = TRUE) {
  total <- n_branca + n_verm
  den2 <- if (com_reposicao) total else total - 1
  num1 <- c(n_branca, n_branca, n_verm, n_verm)
  num2 <- if (com_reposicao) {
    c(n_branca, n_verm, n_branca, n_verm)
  } else {
    c(max(0, n_branca - 1), n_verm, n_branca, max(0, n_verm - 1))
  }

  caminhos <- data.frame(
    caminho = c("BB", "BV", "VB", "VV"),
    frac1 = paste0(num1, "/", total),
    frac2 = paste0(num2, "/", den2),
    etapa1 = num1 / total,
    etapa2_cond = num2 / den2,
    stringsAsFactors = FALSE
  )
  caminhos$prob <- caminhos$etapa1 * caminhos$etapa2_cond

  list(
    caminhos = caminhos,
    p_b1 = n_branca / total,
    p_b2 = caminhos$prob[1] + caminhos$prob[3],
    p_mesma_cor = caminhos$prob[1] + caminhos$prob[4]
  )
}

# Posições dos nós e dos galhos para desenhar a árvore com ggplot2
posicoes_arvore <- function(arv) {
  cam <- arv$caminhos
  y1 <- c(0.75, 0.25)
  y2 <- c(0.875, 0.625, 0.375, 0.125)
  galhos <- data.frame(
    x = c(0, 0, 1, 1, 1, 1),
    y = c(0.5, 0.5, y1[1], y1[1], y1[2], y1[2]),
    xend = c(1, 1, 2, 2, 2, 2),
    yend = c(y1, y2),
    rotulo = c(cam$frac1[1], cam$frac1[3], cam$frac2),
    stringsAsFactors = FALSE
  )
  galhos$xm <- (galhos$x + galhos$xend) / 2
  galhos$ym <- (galhos$y + galhos$yend) / 2
  nos <- data.frame(
    x = c(1, 1, 2, 2, 2, 2),
    y = c(y1, y2),
    cor = c("B", "V", "B", "V", "B", "V"),
    stringsAsFactors = FALSE
  )
  folhas <- data.frame(
    x = 2, y = y2,
    caminho = cam$caminho, prob = cam$prob, frac1 = cam$frac1, frac2 = cam$frac2,
    stringsAsFactors = FALSE
  )
  list(galhos = galhos, nos = nos, folhas = folhas)
}

simular_urna <- function(n_branca, n_verm, com_reposicao = TRUE, n_rep = 500) {
  urna <- c(rep("B", n_branca), rep("V", n_verm))
  sorteios <- purrr::map_chr(seq_len(n_rep), \(i) {
    par <- sample(urna, size = 2, replace = com_reposicao)
    paste0(par[1], par[2])
  })
  tabela <- table(factor(sorteios, levels = c("BB", "BV", "VB", "VV")))
  as.numeric(tabela) / n_rep
}

# Teorema de Bayes para partição discreta C_1, ..., C_k e chances a posteriori
# (Gelman et al., eq. 1.5)
calcular_bayes_particao <- function(nomes, priori, verossim) {
  priori_norm <- priori / sum(priori)
  conjunta <- priori_norm * verossim
  prob_total <- sum(conjunta)
  posteriori <- if (prob_total > 0) conjunta / prob_total else rep(NA_real_, length(priori))

  tabela <- data.frame(
    hipotese = nomes,
    priori = priori_norm,
    verossimilhanca = verossim,
    conjunta = conjunta,
    posteriori = posteriori,
    stringsAsFactors = FALSE
  )

  odds <- NULL
  if (length(nomes) == 2 && priori_norm[2] > 0 && verossim[2] > 0 && !is.na(posteriori[2]) && posteriori[2] > 0) {
    odds <- list(
      odds_priori = priori_norm[1] / priori_norm[2],
      razao_verossim = verossim[1] / verossim[2],
      odds_posteriori = posteriori[1] / posteriori[2]
    )
  }

  list(tabela = tabela, prob_total = prob_total, odds = odds)
}

# Frequências naturais: contagens inteiras das quatro células em n casos.
# Usa maiores restos, para o arredondamento não criar nem sumir casos.
contagens_naturais <- function(tabela, n = 1000L) {
  probs <- c(
    tabela$conjunta[1],
    tabela$conjunta[2],
    tabela$priori[1] - tabela$conjunta[1],
    tabela$priori[2] - tabela$conjunta[2]
  )
  bruto <- pmax(probs, 0) * n
  base <- floor(bruto + 1e-9)
  faltam <- n - sum(base)
  extra <- order(bruto - base, decreasing = TRUE)[seq_len(faltam)]
  base[extra] <- base[extra] + 1
  stats::setNames(as.integer(base), c("C1_A", "C2_A", "C1_nA", "C2_nA"))
}

# Fração irredutível k/n, para mostrar as probabilidades exatas
mdc <- function(a, b) if (b == 0) a else mdc(b, a %% b)
fmt_frac <- function(k, n) {
  if (k == 0) return("0")
  d <- mdc(k, n)
  paste0(k / d, "/", n / d)
}

# Independência dois a dois vs. mútua com os exemplos de Casella e Berger,
# seção 1.3: Ex. 1.3.10 (dois dados), Ex. 1.3.11 (letras) e Ex. 1.3.13 (três
# moedas). Os eventos são vetores lógicos sobre resultados equiprováveis, e as
# contagens inteiras dão a comparação exata, sem tolerância de ponto flutuante.
contraexemplo_independencia <- function(caso = c("ex1310", "ex1311", "ex1313")) {
  caso <- match.arg(caso)
  if (caso == "ex1310") {
    dados <- expand.grid(d1 = 1:6, d2 = 1:6)
    soma <- dados$d1 + dados$d2
    eventos <- list(
      A = dados$d1 == dados$d2,
      B = soma >= 7 & soma <= 10,
      C = soma %in% c(2, 7, 8)
    )
  } else if (caso == "ex1311") {
    omega <- c("aaa", "bbb", "ccc", "abc", "acb", "bac", "bca", "cab", "cba")
    eventos <- purrr::map(1:3, \(i) substr(omega, i, i) == "a") |>
      stats::setNames(c("A", "B", "C"))
  } else {
    moedas <- expand.grid(m1 = c("H", "T"), m2 = c("H", "T"), m3 = c("H", "T"), stringsAsFactors = FALSE)
    eventos <- purrr::map(1:3, \(i) moedas[[i]] == "H") |>
      stats::setNames(c("A", "B", "C"))
  }

  n <- length(eventos$A)
  k <- purrr::map_int(eventos, sum)
  k_ab <- sum(eventos$A & eventos$B)
  k_ac <- sum(eventos$A & eventos$C)
  k_bc <- sum(eventos$B & eventos$C)
  k_abc <- sum(eventos$A & eventos$B & eventos$C)

  # P(X ∩ Y) = P(X)P(Y)  <=>  k_xy * n = k_x * k_y
  par_ok <- c(
    ab = k_ab * n == k[["A"]] * k[["B"]],
    ac = k_ac * n == k[["A"]] * k[["C"]],
    bc = k_bc * n == k[["B"]] * k[["C"]]
  )
  triplo_ok <- k_abc * n^2 == k[["A"]] * k[["B"]] * k[["C"]]

  list(
    caso = caso,
    n = n,
    k = k,
    k_ab = k_ab, k_ac = k_ac, k_bc = k_bc, k_abc = k_abc,
    par_ok = par_ok,
    dois_a_dois = all(par_ok),
    produto_triplo = triplo_ok,
    mutua = all(par_ok) && triplo_ok
  )
}

# Problema dos Três Prisioneiros (Casella e Berger, Ex. 1.3.4)
simular_tres_prisioneiros <- function(n_sim = 1000) {
  perdoado <- sample(c("A", "B", "C"), size = n_sim, replace = TRUE)
  guarda <- purrr::map_chr(perdoado, \(p) {
    if (p == "A") {
      sample(c("B", "C"), size = 1)
    } else if (p == "B") {
      "C"
    } else {
      "B"
    }
  })
  idx_w_b <- guarda == "B"
  n_w_b <- sum(idx_w_b)
  prop_a_dado_w <- if (n_w_b > 0) mean(perdoado[idx_w_b] == "A") else NA_real_
  prop_c_dado_w <- if (n_w_b > 0) mean(perdoado[idx_w_b] == "C") else NA_real_

  list(
    n_sim = n_sim,
    n_w_b = n_w_b,
    prop_a_dado_w = prop_a_dado_w,
    prop_c_dado_w = prop_c_dado_w
  )
}

# Atualização sequencial de hemofilia (Gelman et al., seção 1.4) e preditiva a
# posteriori do próximo filho (eq. 1.4, seção 1.3)
atualizar_hemofilia_seq <- function(priori_inicial = 0.5, filhos = c(0L, 0L, 0L)) {
  passo <- function(priori_atual, filho) {
    lik_port <- 0.5
    lik_nao <- if (filho == 1L) 0.0 else 1.0
    num <- lik_port * priori_atual
    den <- num + lik_nao * (1 - priori_atual)
    if (den > 0) num / den else NA_real_
  }

  probs <- purrr::accumulate(filhos, passo, .init = priori_inicial)
  # Preditiva a posteriori para o próximo filho ser afetado: 0.5 * P(theta = 1 | y)
  pred_proximo <- 0.5 * probs

  data.frame(
    etapa = seq_along(probs) - 1L,
    evento = c(-1L, filhos),
    prob_portadora = probs,
    pred_afetado = pred_proximo,
    stringsAsFactors = FALSE
  )
}

# Correção ortográfica bayesiana 'radom' (Gelman et al., seção 1.4, pp. 9-11).
# A priori é renormalizada entre as três candidatas; a posteriori não muda com isso.
calcular_radom <- function(mult_contexto_random = 1) {
  palavras <- c("random", "radon", "radom")
  priori_base <- c(7.60e-5, 6.05e-6, 3.12e-7)
  priori_adj <- c(priori_base[1] * mult_contexto_random, priori_base[2], priori_base[3])
  priori_norm <- priori_adj / sum(priori_adj)
  verossim <- c(0.00193, 0.000143, 0.975)
  nao_norm <- priori_norm * verossim
  posteriori <- nao_norm / sum(nao_norm)

  data.frame(
    palavra = palavras,
    priori = priori_norm,
    verossimilhanca = verossim,
    posteriori = posteriori,
    stringsAsFactors = FALSE
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
