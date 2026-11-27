# =============================================================================
# Script:  dados.R (dados e funções comuns do tópico Distribuições de frequência)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/apps/distribuicoes-frequencia/app.R,
#     antes dos módulos)
# Saída:
#   - objetos em memória: salarios_mb, exemplos_categoricos, exemplos_continuos,
#     modelos_densidade, integrar(), tabela_frequencias(),
#     tabela_classes_densidade(), quebras_largura(), fmt_intervalo()
# =============================================================================

# Companhia MB, seção de orçamentos (Morettin e Bussab, Estatística Básica,
# Tabela 2.1): salários em salários mínimos dos 36 empregados. Conferido:
# média 11,12, mediana 10,165 e classes de amplitude 4 a partir de 4 com
# 10, 12, 8, 5 e 1 empregados.
salarios_mb <- c(
  4.00, 4.56, 5.25, 5.73, 6.26, 6.66, 6.86, 7.39, 7.59, 7.44, 8.12, 8.46,
  8.74, 8.95, 9.13, 9.35, 9.77, 9.80, 10.53, 10.76, 11.06, 11.59, 12.00, 12.79,
  13.23, 13.60, 13.85, 14.69, 14.71, 15.99, 16.22, 16.61, 17.26, 18.75, 19.40, 23.30
)

# Variáveis categóricas e discretas: categorias na ordem natural e contagens.
# As três primeiras vêm da mesma Tabela 2.1 (número de filhos só para os 20
# casados, média 1,65); a última é simulada com semente fixa.
# Contagens fixas, sorteadas uma vez com set.seed(2026) e probabilidades 0,31;
# 0,24; 0,17; 0,12; 0,05; 0,07; 0,04. Gravadas aqui para o app não fixar a
# semente global, o que repetiria as amostras de toda sessão nova.
contagens_transporte <- c(156, 121, 91, 57, 22, 37, 16)

exemplos_categoricos <- list(
  instrucao = list(
    rotulo = tr("Grau de instrução (Cia. MB, n = 36)", "Education level (MB Co., n = 36)"),
    tipo = "ordinal",
    categorias = tr(c("Fundamental", "Médio", "Superior"), c("Primary", "Secondary", "College")),
    contagens = c(12, 18, 6),
    fonte = tr("Morettin e Bussab, Tabela 2.1", "Morettin and Bussab, Table 2.1")
  ),
  regiao = list(
    rotulo = tr("Região de procedência (Cia. MB, n = 36)", "Region of origin (MB Co., n = 36)"),
    tipo = "nominal",
    categorias = tr(c("Capital", "Interior", "Outra"), c("State capital", "Interior", "Other")),
    contagens = c(11, 12, 13),
    fonte = tr("Morettin e Bussab, Tabela 2.1", "Morettin and Bussab, Table 2.1")
  ),
  filhos = list(
    rotulo = tr("Número de filhos dos casados (Cia. MB, n = 20)", "Number of children, married employees (MB Co., n = 20)"),
    tipo = "discreta",
    categorias = c("0", "1", "2", "3", "4", "5"),
    contagens = c(4, 5, 7, 3, 0, 1),
    fonte = tr("Morettin e Bussab, Tabela 2.1", "Morettin and Bussab, Table 2.1")
  ),
  transporte = list(
    rotulo = tr("Meio de transporte para o trabalho (simulado, n = 500)", "Means of transport to work (simulated, n = 500)"),
    tipo = "nominal",
    categorias = tr(
      c("Ônibus", "Carro", "A pé", "Metrô ou trem", "Bicicleta", "Moto", "Aplicativo"),
      c("Bus", "Car", "On foot", "Subway or train", "Bicycle", "Motorcycle", "Ride-hailing app")
    ),
    contagens = contagens_transporte,
    fonte = tr("dados simulados", "simulated data")
  )
)

# Variáveis contínuas do histograma e do ramo-e-folhas. `gerar(n)` devolve uma
# amostra; o exemplo da Cia. MB ignora n (são sempre os 36 salários).
exemplos_continuos <- list(
  salarios = list(
    rotulo = tr("Salários da Cia. MB (n = 36, Morettin e Bussab)", "Salaries at MB Co. (n = 36, Morettin and Bussab)"),
    unidade = tr("salários mínimos", "minimum wages"),
    fixo = TRUE, digitos = 2, largura = 4, origem = 4,
    gerar = function(n) salarios_mb
  ),
  renda = list(
    rotulo = tr("Renda mensal (simulada, assimétrica)", "Monthly income (simulated, skewed)"),
    unidade = tr("R$ mil", "R$ thousand"),
    fixo = FALSE, digitos = 1, largura = 1, origem = 0,
    gerar = function(n) round(stats::rlnorm(n, meanlog = log(2.4), sdlog = 0.6), 2)
  ),
  altura = list(
    rotulo = tr("Altura de adultos (simulada, em sino)", "Adult height (simulated, bell-shaped)"),
    unidade = "cm",
    fixo = FALSE, digitos = 1, largura = 5, origem = 140,
    gerar = function(n) round(stats::rnorm(n, 170, 9), 1)
  ),
  espera = list(
    rotulo = tr("Tempo de espera no posto (simulado, bimodal)", "Waiting time at the clinic (simulated, bimodal)"),
    unidade = "min",
    fixo = FALSE, digitos = 1, largura = 5, origem = 0,
    gerar = function(n) {
      grupo <- stats::runif(n) < 0.55
      round(pmax(0.5, ifelse(grupo, stats::rnorm(n, 15, 5), stats::rnorm(n, 50, 9))), 1)
    }
  )
)

# Modelos com densidade conhecida, para as abas de estimação e de estatísticas
# de ordem: gerador, densidade, cdf, um intervalo que cobre quase toda a massa
# (suporte, usado nas grades) e o suporte verdadeiro (limites, ±Inf se ilimitado).
modelos_densidade <- list(
  normal = list(
    rotulo = tr("Normal (0, 1)", "Normal (0, 1)"),
    gerar = function(n) stats::rnorm(n),
    f = function(x) stats::dnorm(x),
    F = function(x) stats::pnorm(x),
    suporte = c(-4, 4), limites = c(-Inf, Inf)
  ),
  assimetrica = list(
    rotulo = tr("Assimétrica: gama (forma 2, escala 1)", "Skewed: gamma (shape 2, scale 1)"),
    gerar = function(n) stats::rgamma(n, shape = 2, scale = 1),
    f = function(x) stats::dgamma(x, shape = 2, scale = 1),
    F = function(x) stats::pgamma(x, shape = 2, scale = 1),
    suporte = c(0, 10), limites = c(0, Inf)
  ),
  bimodal = list(
    rotulo = tr("Bimodal: mistura 0,5 N(−2; 0,5²) + 0,5 N(2; 0,5²)", "Bimodal: mixture 0.5 N(−2, 0.5²) + 0.5 N(2, 0.5²)"),
    gerar = function(n) {
      lado <- ifelse(stats::runif(n) < 0.5, -2, 2)
      stats::rnorm(n, lado, 0.5)
    },
    f = function(x) 0.5 * stats::dnorm(x, -2, 0.5) + 0.5 * stats::dnorm(x, 2, 0.5),
    F = function(x) 0.5 * stats::pnorm(x, -2, 0.5) + 0.5 * stats::pnorm(x, 2, 0.5),
    suporte = c(-4.5, 4.5), limites = c(-Inf, Inf)
  ),
  uniforme = list(
    rotulo = tr("Uniforme (0, 1)", "Uniform (0, 1)"),
    gerar = function(n) stats::runif(n),
    f = function(x) stats::dunif(x),
    F = function(x) stats::punif(x),
    suporte = c(0, 1), limites = c(0, 1)
  )
)

# Integral numérica pela regra do trapézio numa grade regular
integrar <- function(xs, y) sum((y[-1] + y[-length(y)]) / 2 * diff(xs))

# Intervalo [a; b) no padrão do idioma; a última classe fecha à direita
fmt_intervalo <- function(inicio, fim, digitos, ultima = FALSE) {
  sprintf(
    "[%s%s%s%s",
    fmt_num(inicio, digitos), tr("; ", ", "), fmt_num(fim, digitos),
    ifelse(ultima, "]", ")")
  )
}

# Tabela de frequências de uma variável categórica ou discreta, já contada.
# A frequência acumulada só faz sentido quando as categorias têm ordem.
tabela_frequencias <- function(categorias, contagens) {
  n <- sum(contagens)
  data.frame(
    categoria = categorias,
    n_i = contagens,
    f_i = contagens / n,
    N_i = cumsum(contagens),
    F_i = cumsum(contagens) / n
  )
}

# Quebras de largura h a partir da origem, cobrindo todos os dados
quebras_largura <- function(x, h, origem) {
  inicio <- origem - h * ceiling(max(0, origem - min(x)) / h)
  fim <- inicio + h * (floor((max(x) - inicio) / h) + 1)
  seq(inicio, fim, by = h)
}

# Tabela de classes [a; b) com frequência, amplitude e densidade f_i / Δ_i
# (Morettin e Bussab, seção 2.3). A última classe inclui o limite superior.
tabela_classes_densidade <- function(x, quebras, digitos) {
  contagem <- as.vector(table(cut(x, quebras, right = FALSE, include.lowest = TRUE)))
  inicio <- utils::head(quebras, -1)
  fim <- utils::tail(quebras, -1)
  f_i <- contagem / length(x)
  data.frame(
    classe = fmt_intervalo(inicio, fim, digitos, ultima = seq_along(contagem) == length(contagem)),
    inicio = inicio, fim = fim, n_i = contagem,
    f_i = f_i, delta = fim - inicio, densidade = f_i / (fim - inicio),
    F_i = cumsum(f_i)
  )
}
