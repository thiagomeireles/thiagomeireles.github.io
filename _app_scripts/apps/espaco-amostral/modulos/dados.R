# =============================================================================
# Script:  dados.R (espaços amostrais, funções de conjuntos, contagem e cotas
#          do tópico Espaço amostral, eventos e axiomas)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-26
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/apps/espaco-amostral/app.R antes
#     dos módulos)
# Saída:
#   - objetos e funções puras: experimentos_omega, cenarios_eventos,
#     exemplos_contagem, maos_poquer, colecoes_sigma3, cartao_cuidados(),
#     formatar_inteiro(), simular_trajetorias_freq(), operar_eventos(),
#     contar_quatro_casos(), distribuicao_com_reposicao_3(),
#     distribuicao_medias_cb1220(), cotas_inclusao_exclusao()
# =============================================================================

# Formatação de inteiros grandes sem notação científica (fiel ao idioma do app)
formatar_inteiro <- function(x) {
  purrr::map_chr(x, \(v) {
    if (!is.finite(v)) return("∞")
    if (abs(v) < 1e15 && abs(v - round(v)) < 1e-6) {
      formatC(round(v), format = "f", digits = 0, big.mark = sep_milhar(), decimal.mark = tr(",", "."))
    } else {
      expo <- floor(log10(abs(v)))
      mant <- v / (10^expo)
      paste0(fmt_num(mant, 3), " × 10^", expo)
    }
  })
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

# Catálogo de experimentos da Aba 1 (Morettin e Bussab 5.1; Agresti e Finlay 4.1; Casella e Berger 1.1–1.2)
experimentos_omega <- list(
  dado = list(
    rotulo = tr("1 dado de 6 faces (Ω finito: simetria × dado viciado)", "1 six-sided die (finite Ω: symmetry vs. loaded die)"),
    omega_desc = "Ω = {1, 2, 3, 4, 5, 6}",
    cardinalidade = "6",
    tipo_espaco = tr("Finito (enumerável)", "Finite (countable)"),
    permite_vicio = TRUE,
    fonte = tr("Morettin e Bussab, seção 5.1; Casella e Berger, Ex. 1.2.5", "Morettin and Bussab, section 5.1; Casella and Berger, Ex. 1.2.5"),
    eventos = list(
      par = list(
        rotulo = tr("Face par: A = {2, 4, 6}", "Even face: A = {2, 4, 6}"),
        idx = c(2L, 4L, 6L)
      ),
      maior4 = list(
        rotulo = tr("Maior que 4: A = {5, 6}", "Greater than 4: A = {5, 6}"),
        idx = c(5L, 6L)
      ),
      primo = list(
        rotulo = tr("Face prima: A = {2, 3, 5}", "Prime face: A = {2, 3, 5}"),
        idx = c(2L, 3L, 5L)
      ),
      seis = list(
        rotulo = tr("Sair 6: A = {6}", "Roll a 6: A = {6}"),
        idx = 6L
      )
    ),
    distribuicao = function(param = 1 / 6) {
      p6 <- min(max(param, 0), 1)
      restante <- (1 - p6) / 5
      probs <- c(rep(restante, 5L), p6)
      data.frame(
        ponto = as.character(1:6),
        prob = probs,
        stringsAsFactors = FALSE
      )
    }
  ),
  dois_dados = list(
    rotulo = tr("Soma de 2 dados honestos (36 pares equiprováveis × 11 somas)", "Sum of 2 fair dice (36 equally likely pairs vs. 11 sums)"),
    omega_desc = tr("Ω = {(1,1), (1,2), …, (6,6)} → soma ∈ {2, …, 12}", "Ω = {(1,1), (1,2), …, (6,6)} → sum ∈ {2, …, 12}"),
    cardinalidade = tr("36 pares (11 somas)", "36 pairs (11 sums)"),
    tipo_espaco = tr("Finito: pares equiprováveis, somas não", "Finite: pairs equally likely, sums not"),
    permite_vicio = FALSE,
    fonte = tr("Morettin e Bussab, Exemplo 5.4 (seção 5.1)", "Morettin and Bussab, Example 5.4 (section 5.1)"),
    eventos = list(
      soma7 = list(
        rotulo = tr("Soma igual a 7: 6 dos 36 pares", "Sum equal to 7: 6 of 36 pairs"),
        idx = 6L
      ),
      soma_alta = list(
        rotulo = tr("Soma ≥ 10: {10, 11, 12} (6 pares)", "Sum ≥ 10: {10, 11, 12} (6 pairs)"),
        idx = c(9L, 10L, 11L)
      ),
      extremos = list(
        rotulo = tr("Soma extrema 2 ou 12 (2 pares)", "Extreme sum 2 or 12 (2 pairs)"),
        idx = c(1L, 11L)
      ),
      soma_par = list(
        rotulo = tr("Soma par: {2, 4, 6, 8, 10, 12} (18 pares)", "Even sum: {2, 4, 6, 8, 10, 12} (18 pairs)"),
        idx = c(1L, 3L, 5L, 7L, 9L, 11L)
      )
    ),
    distribuicao = function(param = NULL) {
      somas <- 2:12
      contagem <- c(1, 2, 3, 4, 5, 6, 5, 4, 3, 2, 1)
      data.frame(
        ponto = as.character(somas),
        prob = contagem / 36,
        stringsAsFactors = FALSE
      )
    }
  ),
  tres_moedas = list(
    rotulo = tr("3 lançamentos de moeda (8 sequências ordenadas)", "3 coin tosses (8 ordered sequences)"),
    omega_desc = tr("Ω = {CCC, CCK, CKC, KCC, CKK, KCK, KKC, KKK}", "S = {HHH, HHT, HTH, THH, HTT, THT, TTH, TTT}"),
    cardinalidade = "8",
    tipo_espaco = tr("Finito (2³ = 8 sequências)", "Finite (2³ = 8 sequences)"),
    permite_vicio = TRUE,
    fonte = tr("Casella e Berger, Exemplo 1.3.13; Morettin e Bussab, seção 5.1", "Casella and Berger, Example 1.3.13; Morettin and Bussab, section 5.1"),
    eventos = list(
      duas_caras = list(
        rotulo = tr("Exatamente 2 caras (3 sequências)", "Exactly 2 heads (3 sequences)"),
        idx = c(2L, 3L, 4L)
      ),
      pelo_menos_uma = list(
        rotulo = tr("Pelo menos 1 cara (7 sequências)", "At least 1 head (7 sequences)"),
        idx = 1:7
      ),
      primeira_cara = list(
        rotulo = tr("Cara no 1º lançamento (4 sequências)", "Head on 1st toss (4 sequences)"),
        idx = c(1L, 2L, 3L, 5L)
      ),
      todas_iguais = list(
        rotulo = tr("Três faces iguais (2 sequências)", "All three match (2 sequences)"),
        idx = c(1L, 8L)
      )
    ),
    distribuicao = function(param = 0.5) {
      p <- min(max(param, 0), 1)
      q <- 1 - p
      seqs <- tr(
        c("CCC", "CCK", "CKC", "KCC", "CKK", "KCK", "KKC", "KKK"),
        c("HHH", "HHT", "HTH", "THH", "HTT", "THT", "TTH", "TTT")
      )
      n_caras <- c(3, 2, 2, 2, 1, 1, 1, 0)
      data.frame(
        ponto = seqs,
        prob = (p^n_caras) * (q^(3 - n_caras)),
        stringsAsFactors = FALSE
      )
    }
  ),
  eleicao_af = list(
    rotulo = tr("Eleição na Califórnia 2006 (Agresti e Finlay, seção 4.1)", "2006 California election (Agresti and Finlay, section 4.1)"),
    omega_desc = tr("Ω = {Schwarzenegger, Outro candidato} (N = 6.921.442 votos)", "S = {Schwarzenegger, Other candidate} (N = 6,921,442 votes)"),
    cardinalidade = "2",
    tipo_espaco = tr("Amostragem de população finita grande", "Sampling from a large finite population"),
    permite_vicio = FALSE,
    fonte = tr("Agresti e Finlay, seção 4.1 e Exemplo 4.11 (p = 0,559)", "Agresti and Finlay, section 4.1 and Example 4.11 (p = 0.559)"),
    eventos = list(
      republicano = list(
        rotulo = tr("Eleitor sorteado votou em Schwarzenegger (p = 0,559)", "Sampled voter chose Schwarzenegger (p = 0.559)"),
        idx = 1L
      ),
      outro = list(
        rotulo = tr("Não votou nele: Aᶜ (1 − p = 0,441)", "Did not vote for him: not A (1 − p = 0.441)"),
        idx = 2L
      )
    ),
    distribuicao = function(param = NULL) {
      data.frame(
        ponto = tr(c("Schwarzenegger", "Outro"), c("Schwarzenegger", "Other")),
        prob = c(0.559, 0.441),
        stringsAsFactors = FALSE
      )
    }
  )
)

# Simulação de trajetórias de frequência relativa n_A / n (sem laços for nem apply)
simular_trajetorias_freq <- function(prob_evento, n_lanc, n_traj = 6L) {
  ns <- seq_len(n_lanc)
  lista <- purrr::map(seq_len(n_traj), \(id_traj) {
    ocorrencias <- stats::rbinom(n_lanc, size = 1L, prob = prob_evento)
    data.frame(
      trajetoria = factor(id_traj),
      passo = ns,
      freq_rel = cumsum(ocorrencias) / ns
    )
  })
  do.call(rbind, lista)
}

# Cenários de conjuntos para a Aba 2 (operações, De Morgan, regra da soma e axiomas)
cenarios_eventos <- list(
  dodecaedro = list(
    rotulo = tr("Urna numerada de 1 a 12 (grade 4 × 3)", "Numbered urn 1 to 12 (4 × 3 grid)"),
    pontos = as.character(1:12),
    ncol = 4L,
    pesos_desiguais = c(3, 3, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1) / 20,
    opcoes_a = list(
      pares = list(rotulo = tr("Pares: {2, 4, 6, 8, 10, 12}", "Evens: {2, 4, 6, 8, 10, 12}"), elem = as.character(c(2, 4, 6, 8, 10, 12))),
      ate4 = list(rotulo = tr("Baixos (≤ 4): {1, 2, 3, 4}", "Low (≤ 4): {1, 2, 3, 4}"), elem = as.character(1:4)),
      mult3 = list(rotulo = tr("Múltiplos de 3: {3, 6, 9, 12}", "Multiples of 3: {3, 6, 9, 12}"), elem = as.character(c(3, 6, 9, 12))),
      primos = list(rotulo = tr("Primos: {2, 3, 5, 7, 11}", "Primes: {2, 3, 5, 7, 11}"), elem = as.character(c(2, 3, 5, 7, 11)))
    ),
    opcoes_b = list(
      mult3 = list(rotulo = tr("Múltiplos de 3: {3, 6, 9, 12}", "Multiples of 3: {3, 6, 9, 12}"), elem = as.character(c(3, 6, 9, 12))),
      impares = list(rotulo = tr("Ímpares: {1, 3, 5, 7, 9, 11} (disjunto dos pares)", "Odds: {1, 3, 5, 7, 9, 11} (disjoint from evens)"), elem = as.character(c(1, 3, 5, 7, 9, 11))),
      altos = list(rotulo = tr("Altos (≥ 9): {9, 10, 11, 12}", "High (≥ 9): {9, 10, 11, 12}"), elem = as.character(9:12)),
      ate6 = list(rotulo = tr("Metade inferior (≤ 6): {1, …, 6} (contém ≤ 4)", "Lower half (≤ 6): {1, …, 6} (contains ≤ 4)"), elem = as.character(1:6))
    )
  ),
  naipes_cb = list(
    rotulo = tr("Naipes do baralho (Casella e Berger, Ex. 1.1.3)", "Card suits (Casella and Berger, Ex. 1.1.3)"),
    pontos = tr(c("♣ Paus", "♦ Ouros", "♥ Copas", "♠ Espadas"), c("♣ Clubs", "♦ Diamonds", "♥ Hearts", "♠ Spades")),
    ncol = 4L,
    pesos_desiguais = c(0.15, 0.35, 0.30, 0.20),
    opcoes_a = list(
      livro_a = list(rotulo = tr("Ex. 1.1.3: A = {♣, ♦}", "Ex. 1.1.3: A = {♣, ♦}"), elem = tr(c("♣ Paus", "♦ Ouros"), c("♣ Clubs", "♦ Diamonds"))),
      vermelhos = list(rotulo = tr("Naipes vermelhos: {♦, ♥}", "Red suits: {♦, ♥}"), elem = tr(c("♦ Ouros", "♥ Copas"), c("♦ Diamonds", "♥ Hearts"))),
      apenas_espadas = list(rotulo = tr("Evento simples: {♠}", "Simple event: {♠}"), elem = tr("♠ Espadas", "♠ Spades"))
    ),
    opcoes_b = list(
      livro_b = list(rotulo = tr("Ex. 1.1.3: B = {♦, ♥, ♠}", "Ex. 1.1.3: B = {♦, ♥, ♠}"), elem = tr(c("♦ Ouros", "♥ Copas", "♠ Espadas"), c("♦ Diamonds", "♥ Hearts", "♠ Spades"))),
      pretos = list(rotulo = tr("Naipes pretos: {♣, ♠} (disjunto dos vermelhos)", "Black suits: {♣, ♠} (disjoint from red)"), elem = tr(c("♣ Paus", "♠ Espadas"), c("♣ Clubs", "♠ Spades"))),
      copas_ouros = list(rotulo = tr("Vermelhos: {♦, ♥}", "Red suits: {♦, ♥}"), elem = tr(c("♦ Ouros", "♥ Copas"), c("♦ Diamonds", "♥ Hearts")))
    )
  )
)

# Operações de conjuntos e probabilidades sobre um espaço amostral finito
operar_eventos <- function(pontos, probs, elem_a, elem_b, operacao = "uniao") {
  em_a <- pontos %in% elem_a
  em_b <- pontos %in% elem_b
  destaque <- switch(
    operacao,
    a          = em_a,
    b          = em_b,
    comp_a     = !em_a,
    uniao      = em_a | em_b,
    intersecao = em_a & em_b,
    b_menos_a  = em_b & (!em_a),
    demorgan1  = !(em_a | em_b),
    demorgan2  = !(em_a & em_b),
    vazio      = rep(FALSE, length(pontos))
  )

  p_a <- sum(probs[em_a])
  p_b <- sum(probs[em_b])
  p_inter <- sum(probs[em_a & em_b])
  p_uniao <- sum(probs[em_a | em_b])
  p_destaque <- sum(probs[destaque])

  list(
    em_a = em_a,
    em_b = em_b,
    destaque = destaque,
    elementos_destaque = pontos[destaque],
    disjuntos = !any(em_a & em_b),
    a_sub_b = all(!em_a | em_b),
    p_a = p_a,
    p_b = p_b,
    p_comp_a = sum(probs[!em_a]),
    p_b_menos_a = sum(probs[em_b & !em_a]),
    p_inter = p_inter,
    p_uniao = p_uniao,
    p_destaque = p_destaque
  )
}

# Tabela 1.2.1 de Casella e Berger: contagem nos quatro esquemas (ordenado/não × sem/com reposição)
contar_quatro_casos <- function(n, r) {
  n <- as.integer(n)
  r <- as.integer(r)

  ord_sem <- if (r > n) {
    0
  } else if (r == 0L) {
    1
  } else {
    vals <- seq(from = n - r + 1L, to = n, by = 1L)
    if (sum(log10(vals)) < 15) prod(as.numeric(vals)) else exp(lgamma(n + 1) - lgamma(n - r + 1))
  }

  ord_com <- if (r * log10(max(n, 1L)) < 15) as.numeric(n)^r else exp(r * log(n))

  nao_ord_sem <- if (r > n) {
    0
  } else if (lchoose(n, r) / log(10) < 15) {
    choose(n, r)
  } else {
    exp(lchoose(n, r))
  }

  nao_ord_com <- if (lchoose(n + r - 1L, r) / log(10) < 15) {
    choose(n + r - 1L, r)
  } else {
    exp(lchoose(n + r - 1L, r))
  }

  c(
    ordenado_sem = ord_sem,
    ordenado_com = ord_com,
    nao_ordenado_sem = nao_ord_sem,
    nao_ordenado_com = nao_ord_com
  )
}

exemplos_contagem <- list(
  loteria_cb = list(
    rotulo = tr("Loteria 6 de 44 (Casella e Berger, Exs. 1.2.12 e 1.2.15)", "6-from-44 Lottery (Casella and Berger, Exs. 1.2.12 and 1.2.15)"),
    n = 44L, r = 6L,
    nota = tr(
      paste(
        "No caso não ordenado com reposição, C(49, 6) = 13.983.816 supera o palpite intuitivo 44⁶ / 6! = 10.078.213,7",
        "(que sequer é inteiro), porque sequências com números repetidos têm menos de 6! permutações distintas."
      ),
      paste(
        "In the unordered-with-replacement cell, C(49, 6) = 13,983,816 exceeds the naive guess 44⁶ / 6! = 10,078,213.7",
        "(not even an integer), because draws with repeated numbers have fewer than 6! distinct permutations."
      )
    )
  ),
  poquer_cb = list(
    rotulo = tr("Mão de pôquer: 5 cartas de 52 (Casella e Berger, Ex. 1.2.18)", "Poker hand: 5 cards from 52 (Casella and Berger, Ex. 1.2.18)"),
    n = 52L, r = 5L,
    nota = tr(
      "Sem reposição, a chance de qualquer evento que não depende da ordem das cartas é a mesma usando 311.875.200 mãos ordenadas ou C(52, 5) = 2.598.960 mãos não ordenadas: o fator 5! = 120 cancela no numerador e no denominador.",
      "Without replacement, any order-free event gets the exact same probability from the 311,875,200 ordered hands or from C(52, 5) = 2,598,960 unordered hands: the factor 5! = 120 cancels out in numerator and denominator."
    )
  ),
  urnas_cb = list(
    rotulo = tr("2 sorteios de {1, 2, 3} (Casella e Berger, Ex. 1.2.19)", "2 draws from {1, 2, 3} (Casella and Berger, Ex. 1.2.19)"),
    n = 3L, r = 2L,
    nota = tr(
      "Com reposição, há 3² = 9 pares ordenados equiprováveis (1/9 cada) e C(4, 2) = 6 conjuntos não ordenados. Mas {1, 3} reúne (1, 3) e (3, 1), tendo probabilidade 2/9, enquanto {1, 1} tem 1/9.",
      "With replacement, there are 3² = 9 equally likely ordered pairs (1/9 each) and C(4, 2) = 6 unordered sets. Yet {1, 3} collects both (1, 3) and (3, 1) with probability 2/9, whereas {1, 1} has 1/9."
    )
  ),
  medias_cb = list(
    rotulo = tr("Amostra de 4 valores em {2, 4, 9, 12} (Casella e Berger, Ex. 1.2.20)", "Sample of 4 from {2, 4, 9, 12} (Casella and Berger, Ex. 1.2.20)"),
    n = 4L, r = 4L,
    nota = tr(
      "Com reposição, há 4⁴ = 256 amostras ordenadas equiprováveis e C(7, 4) = 35 amostras não ordenadas não equiprováveis. Por exemplo, {2, 4, 4, 9} tem 4!/2! = 12 ordenações, logo P = 12/256, e não 1/35.",
      "With replacement, there are 4⁴ = 256 equally likely ordered samples and C(7, 4) = 35 non-equally-likely unordered samples. For instance, {2, 4, 4, 9} has 4!/2! = 12 orderings, so P = 12/256 rather than 1/35."
    )
  ),
  livre = list(
    rotulo = tr("Personalizado (escolha n e r abaixo)", "Custom (choose n and r below)"),
    n = 10L, r = 3L,
    nota = tr(
      "Compare a razão entre ordenado e não ordenado: sem reposição ela vale exatamente r!; com reposição ela é menor que r! sempre que r ≥ 2.",
      "Compare the ordered-to-unordered ratio: without replacement it equals r! exactly; with replacement it is strictly smaller than r! whenever r ≥ 2."
    )
  )
)

# Exemplo 1.2.18 de Casella e Berger: mãos clássicas em C(52, 5) = 2.598.960
maos_poquer <- function() {
  total <- choose(52, 5)
  contagens <- c(
    48,
    13 * 48,
    13 * choose(4, 3) * 12 * choose(4, 2),
    choose(13, 2) * choose(4, 2) * choose(4, 2) * 44,
    13 * choose(4, 2) * choose(12, 3) * (4^3)
  )
  data.frame(
    mao = tr(
      c("Quatro ases (Ex. 1.2.18)", "Quadra (qualquer valor, Ex. 1.2.18)", "Full house (trinca + par)", "Dois pares", "Exatamente um par (eq. 1.2.11)"),
      c("Four aces (Ex. 1.2.18)", "Four of a kind (Ex. 1.2.18)", "Full house (three + pair)", "Two pairs", "Exactly one pair (eq. 1.2.11)")
    ),
    decomposicao = c(
      "1 × C(4, 4) × 48",
      "13 × C(4, 4) × 48",
      "13 × C(4, 3) × 12 × C(4, 2)",
      "C(13, 2) × C(4, 2)² × 44",
      "13 × C(4, 2) × C(12, 3) × 4³"
    ),
    favoraveis = formatar_inteiro(contagens),
    prob = purrr::map_chr(contagens / total, \(p) fmt_num(p, 6)),
    stringsAsFactors = FALSE
  )
}

# Exemplo 1.2.19: r = 2 de {1, 2, 3} com reposição (não ordenados verdadeiros vs. ingênuo 1/6)
distribuicao_com_reposicao_3 <- function() {
  data.frame(
    conjunto = c("{1, 1}", "{2, 2}", "{3, 3}", "{1, 2}", "{1, 3}", "{2, 3}"),
    ordenados = c("(1,1)", "(2,2)", "(3,3)", "(1,2), (2,1)", "(1,3), (3,1)", "(2,3), (3,2)"),
    n_ord = c(1L, 1L, 1L, 2L, 2L, 2L),
    prob_real = c(1, 1, 1, 2, 2, 2) / 9,
    prob_ingenua = rep(1 / 6, 6L),
    stringsAsFactors = FALSE
  )
}

# Exemplo 1.2.20: distribuição exata das médias de r = 4 com reposição em {2, 4, 9, 12}
distribuicao_medias_cb1220 <- function() {
  vals <- c(2, 4, 9, 12)
  grade <- expand.grid(a = vals, b = vals, c = vals, d = vals)
  medias <- (grade$a + grade$b + grade$c + grade$d) / 4
  tab <- table(medias)
   data.frame(
    media = as.numeric(names(tab)),
    contagem_ordenada = as.integer(tab),
    prob_real = as.numeric(tab) / 256,
    stringsAsFactors = FALSE
  )
}

# Aba 4 ("Para ir além"): verificação explícita de σ-álgebras sobre S = {1, 2, 3}
colecoes_sigma3 <- list(
  partes = list(
    rotulo = tr("Conjunto das partes 2^S (todos os 2³ = 8 subconjuntos)", "Power set 2^S (all 2³ = 8 subsets)"),
    conjuntos = list(integer(0), 1L, 2L, 3L, c(1L, 2L), c(1L, 3L), c(2L, 3L), 1:3),
    comentario = tr(
      "Em qualquer espaço finito ou enumerável, podemos medir todos os subconjuntos sem contradição (Ex. 1.2.2).",
      "On any finite or countable sample space, every subset can be assigned a probability without contradiction (Ex. 1.2.2)."
    )
  ),
  gerada_1 = list(
    rotulo = tr("Menor σ-álgebra contendo {1}: {∅, {1}, {2, 3}, S}", "Smallest σ-algebra containing {1}: {∅, {1}, {2, 3}, S}"),
    conjuntos = list(integer(0), 1L, c(2L, 3L), 1:3),
    comentario = tr(
      "Representa informação parcial: sabemos apenas se o resultado foi 1 ou não, sem distinguir 2 de 3.",
      "Represents partial information: we only observe whether outcome 1 occurred, without telling 2 and 3 apart."
    )
  ),
  trivial = list(
    rotulo = tr("σ-álgebra trivial: {∅, S}", "Trivial σ-algebra: {∅, S}"),
    conjuntos = list(integer(0), 1:3),
    comentario = tr(
      "É a menor σ-álgebra possível em qualquer espaço amostral: só os eventos impossível (P = 0) e certo (P = 1) são medidos.",
      "The smallest possible σ-algebra on any sample space: only the impossible (P = 0) and sure (P = 1) events are measured."
    )
  ),
  sem_comp = list(
    rotulo = tr("Coleção inválida I (falta complemento): {∅, {1}, S}", "Invalid collection I (missing complement): {∅, {1}, S}"),
    conjuntos = list(integer(0), 1L, 1:3),
    comentario = tr(
      "Se medimos P({1}), o axioma do complemento exige que {1}ᶜ = {2, 3} também pertença à coleção para que P({2, 3}) = 1 − P({1}) exista.",
      "Once {1} has a probability, closure under complements requires {1}ᶜ = {2, 3} to belong to the collection so P({2, 3}) = 1 − P({1}) is defined."
    )
  ),
  sem_uniao = list(
    rotulo = tr("Coleção inválida II (falta união): {∅, {1}, {2}, {2, 3}, {1, 3}, S}", "Invalid collection II (missing union): {∅, {1}, {2}, {2, 3}, {1, 3}, S}"),
    conjuntos = list(integer(0), 1L, 2L, c(2L, 3L), c(1L, 3L), 1:3),
    comentario = tr(
      "É fechada no complemento, mas falha na união: {1} e {2} estão na coleção, porém {1} ∪ {2} = {1, 2} (e seu complemento {3}) ficaram de fora.",
      "Closed under complements, yet fails under unions: {1} and {2} are included, whereas {1} ∪ {2} = {1, 2} (and its complement {3}) are missing."
    )
  )
)

formatar_subconjunto <- function(v) {
  if (length(v) == 0L) return("∅")
  if (identical(sort(v), 1:3)) return("S = {1, 2, 3}")
  paste0("{", paste(sort(v), collapse = ", "), "}")
}

verificar_sigma_algebra <- function(conjuntos, universo = 1:3) {
  chaves <- purrr::map_chr(conjuntos, \(x) paste(sort(x), collapse = ","))
  tem_vazio <- "" %in% chaves

  faltam_comp <- purrr::keep(conjuntos, \(x) {
    comp_chave <- paste(sort(setdiff(universo, x)), collapse = ",")
    !(comp_chave %in% chaves)
  })

  pares <- expand.grid(i = seq_along(conjuntos), j = seq_along(conjuntos))
  unioes_faltando <- purrr::keep(seq_len(nrow(pares)), \(idx) {
    u <- sort(union(conjuntos[[pares$i[idx]]], conjuntos[[pares$j[idx]]]))
    !(paste(u, collapse = ",") %in% chaves)
  })

  list(
    tem_vazio = tem_vazio,
    fechada_comp = length(faltam_comp) == 0L,
    fechada_uniao = length(unioes_faltando) == 0L,
    valida = tem_vazio && length(faltam_comp) == 0L && length(unioes_faltando) == 0L,
    exemplo_falha_comp = if (length(faltam_comp) > 0L) {
      x <- faltam_comp[[1]]
      paste0(formatar_subconjunto(x), "ᶜ = ", formatar_subconjunto(setdiff(universo, x)))
    } else "",
    exemplo_falha_uniao = if (length(unioes_faltando) > 0L) {
      idx <- unioes_faltando[1]
      a <- conjuntos[[pares$i[idx]]]
      b <- conjuntos[[pares$j[idx]]]
      paste0(formatar_subconjunto(a), " ∪ ", formatar_subconjunto(b), " = ", formatar_subconjunto(union(a, b)))
    } else ""
  )
}

# Cotas de Boole, Bonferroni e Inclusão-Exclusão para k eventos simétricos com P(A_i) = p
# Regimes: "independente", "disjunto" (quando k*p <= 1; caso contrário empacotamento mínimo), "comonotonico" (A_1 = ... = A_k)
cotas_inclusao_exclusao <- function(k, p, regime = "independente") {
  k <- as.integer(k)
  p <- min(max(p, 0), 1)
  ordens <- seq_len(k)

  if (identical(regime, "independente")) {
    termos_p <- purrr::map_dbl(ordens, \(m) choose(k, m) * (p^m))
    p_uniao_real <- 1 - (1 - p)^k
    p_inter_real <- p^k
  } else if (identical(regime, "comonotonico")) {
    termos_p <- purrr::map_dbl(ordens, \(m) choose(k, m) * p)
    p_uniao_real <- p
    p_inter_real <- p
  } else {
    # Disjuntos quando k*p <= 1; se k*p > 1, eventos com sobreposição mínima no círculo [0, 1)
    # Simulação determinística sobre grade fina de 2.000 pontos equiprováveis para qualquer k e p!
    grade_n <- 2000L
    u <- (seq_len(grade_n) - 0.5) / grade_n
    largura <- p
    indicadores <- purrr::map(seq_len(k), \(i) {
      inicio <- ((i - 1) / k) %% 1
      fim <- inicio + largura
      if (fim <= 1) (u >= inicio & u < fim) else (u >= inicio | u < (fim - 1))
    })
    mat <- do.call(cbind, indicadores)
    somas_linha <- rowSums(mat)
    # Em cada ponto s com c eventos ativos, ele contribui C(c, m) para Σ_{i1<...<im} 1(s ∈ A_i1 ∩ ... ∩ A_im)
    termos_p <- purrr::map_dbl(ordens, \(m) mean(choose(somas_linha, m)))
    p_uniao_real <- mean(somas_linha >= 1L)
    p_inter_real <- mean(somas_linha == k)
  }

  sinais <- ifelse(ordens %% 2L == 1L, 1, -1)
  somas_parciais <- cumsum(sinais * termos_p)

  list(
    k = k,
    p = p,
    ordens = ordens,
    termos_p = termos_p,
    somas_parciais = somas_parciais,
    tipo_cota = ifelse(ordens %% 2L == 1L, "superior", "inferior"),
    p_uniao_real = p_uniao_real,
    p_boole = min(1, k * p),
    cota_boole_bruta = k * p,
    p_inter_real = p_inter_real,
    cota_bonferroni_bruta = k * p - (k - 1),
    cota_bonferroni_util = max(0, k * p - (k - 1))
  )
}
