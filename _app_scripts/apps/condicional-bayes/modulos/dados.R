# =============================================================================
# Script:  dados.R (cálculos puros de Probabilidade condicional e Bayes)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-26
#
# Entrada:
#   - Parâmetros fornecidos pelos módulos de interface
# Saída:
#   - Funções puras para probabilidade condicional, independência, árvores,
#     Teorema de Bayes, razão de chances, contraexemplos, Três Prisioneiros,
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
    p_a_dado_b = p_a_dado_b,
    p_b_dado_a = p_b_dado_a
  )
}

# Árvore de 2 etapas para urna com bolas azuis (A) e vermelhas (V)
calcular_arvore_urna <- function(n_azul, n_verm, com_reposicao = TRUE) {
  total <- n_azul + n_verm
  p_a1 <- n_azul / total
  p_v1 <- n_verm / total

  if (com_reposicao) {
    p_a2_dado_a1 <- p_a1
    p_v2_dado_a1 <- p_v1
    p_a2_dado_v1 <- p_a1
    p_v2_dado_v1 <- p_v1
  } else {
    denom2 <- total - 1
    p_a2_dado_a1 <- max(0, n_azul - 1) / denom2
    p_v2_dado_a1 <- n_verm / denom2
    p_a2_dado_v1 <- n_azul / denom2
    p_v2_dado_v1 <- max(0, n_verm - 1) / denom2
  }

  caminhos <- data.frame(
    caminho = c("AA", "AV", "VA", "VV"),
    etapa1 = c(p_a1, p_a1, p_v1, p_v1),
    etapa2_cond = c(p_a2_dado_a1, p_v2_dado_a1, p_a2_dado_v1, p_v2_dado_v1),
    prob = c(
      p_a1 * p_a2_dado_a1,
      p_a1 * p_v2_dado_a1,
      p_v1 * p_a2_dado_v1,
      p_v1 * p_v2_dado_v1
    ),
    stringsAsFactors = FALSE
  )

  list(
    caminhos = caminhos,
    p_a2 = caminhos$prob[1] + caminhos$prob[3],
    p_mesma_cor = caminhos$prob[1] + caminhos$prob[4]
  )
}

simular_urna <- function(n_azul, n_verm, com_reposicao = TRUE, n_rep = 500) {
  urna <- c(rep("A", n_azul), rep("V", n_verm))
  sorteios <- purrr::map_chr(seq_len(n_rep), \(i) {
    par <- sample(urna, size = 2, replace = com_reposicao)
    paste0(par[1], par[2])
  })
  tabela <- table(factor(sorteios, levels = c("AA", "AV", "VA", "VV")))
  as.numeric(tabela) / n_rep
}

# Teorema de Bayes para partição discreta C_1, ..., C_k e razão de chances (Gelman Eq. 1.5)
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

# Contraexemplos de independência dois a dois vs. independência mútua (Casella & Berger 1.3)
contraexemplo_independencia <- function(caso = c("ex1311", "ex1310", "mutua")) {
  caso <- match.arg(caso)
  if (caso == "ex1311") {
    # 4 resultados equiprováveis {1, 2, 3, 4}: A={1,2}, B={1,3}, C={1,4}
    omega <- 1:4
    a <- c(1, 2)
    b <- c(1, 3)
    c_ev <- c(1, 4)
  } else if (caso == "ex1310") {
    # 8 resultados equiprováveis {1..8}: A={1,2,3,4}, B={1,5,6,7}, C={1,2,3,8}
    omega <- 1:8
    a <- c(1, 2, 3, 4)
    b <- c(1, 5, 6, 7)
    c_ev <- c(1, 2, 3, 8)
  } else {
    # 3 moedas honestas em {1..8}: independência mútua completa
    omega <- 1:8
    a <- c(1, 2, 3, 4)
    b <- c(1, 2, 5, 6)
    c_ev <- c(1, 3, 5, 7)
  }

  n <- length(omega)
  p_a <- length(a) / n
  p_b <- length(b) / n
  p_c <- length(c_ev) / n
  p_ab <- length(intersect(a, b)) / n
  p_ac <- length(intersect(a, c_ev)) / n
  p_bc <- length(intersect(b, c_ev)) / n
  p_abc <- length(Reduce(intersect, list(a, b, c_ev))) / n

  dois_a_dois <- abs(p_ab - p_a * p_b) < 1e-9 &&
    abs(p_ac - p_a * p_c) < 1e-9 &&
    abs(p_bc - p_b * p_c) < 1e-9
  produto_triplo <- abs(p_abc - p_a * p_b * p_c) < 1e-9

  list(
    caso = caso,
    omega = omega,
    a = a,
    b = b,
    c = c_ev,
    p_a = p_a,
    p_b = p_b,
    p_c = p_c,
    p_ab = p_ab,
    p_ac = p_ac,
    p_bc = p_bc,
    p_abc = p_abc,
    dois_a_dois = dois_a_dois,
    produto_triplo = produto_triplo,
    mutua = dois_a_dois && produto_triplo
  )
}

# Problema dos Três Prisioneiros (Casella & Berger Ex. 1.3.4)
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

# Atualização sequencial de hemofilia e preditiva a posteriori (Gelman et al., Seção 1.4)
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

# Correção ortográfica bayesiana 'radom' (Gelman et al., Seção 1.4, pp. 9-11)
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
