# =============================================================================
# Script:  populacao.R (população simulada e funções de sorteio do tema
#          População, amostra, parâmetro e estatística)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-25
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/apps/populacao-amostra/app.R)
# Saída:
#   - objetos em memória: populacao, variaveis_pop, medidas, selecoes e funções
#     sortear_amostra(), calcular_medida(), valor_parametro(), fmt_medida()
# =============================================================================

# A população é fixa: 2.000 adultos de uma cidade fictícia, gerados sempre com a
# mesma semente, para o parâmetro ser o mesmo em qualquer sessão. Depois da
# geração, a semente volta a ser aleatória, para os sorteios variarem.
gerar_populacao <- function(N = 2000, semente = 2026) {
  set.seed(semente)
  idade <- floor(18 + 62 * stats::rbeta(N, 1.3, 1.8))
  renda <- round(exp(7.55 + 0.008 * (idade - 40) + stats::rnorm(N, 0, 0.7)), -1)
  chance_internet <- stats::plogis(3.4 - 0.075 * (idade - 18) + 1.3 * log(renda / 2000))
  internet <- stats::rbinom(N, 1, chance_internet)
  set.seed(NULL)

  data.frame(
    id = seq_len(N),
    coluna = (seq_len(N) - 1) %% 50 + 1,
    linha = (seq_len(N) - 1) %/% 50 + 1,
    idade = idade,
    renda = renda,
    internet = internet
  )
}

populacao <- gerar_populacao()

# Variáveis da população. `tipo` decide quais medidas fazem sentido.
variaveis_pop <- list(
  idade = list(
    rotulo = tr("Idade (anos)", "Age (years)"),
    tipo = "quanti", unidade = tr(" anos", " years"), digitos = 1
  ),
  renda = list(
    rotulo = tr("Renda mensal (R$)", "Monthly income (R$)"),
    tipo = "quanti", unidade = "", prefixo = "R$ ", digitos = 0
  ),
  internet = list(
    rotulo = tr("Usa internet (sim ou não)", "Uses the internet (yes or no)"),
    tipo = "binaria"
  )
)

# Medidas e seus símbolos: parâmetro (população) e estatística (amostra).
# Notação de Morettin e Bussab em pt e de Agresti e Finlay em en.
medidas <- list(
  media = list(
    rotulo = tr("Média", "Mean"),
    parametro = "μ", estatistica = tr("x̄", "ȳ"),
    calcular = \(x) mean(x)
  ),
  mediana = list(
    rotulo = tr("Mediana", "Median"),
    parametro = "Md", estatistica = "md",
    calcular = \(x) stats::median(x)
  ),
  dp = list(
    rotulo = tr("Desvio padrão", "Standard deviation"),
    parametro = "σ", estatistica = "s",
    calcular = \(x) stats::sd(x)
  ),
  proporcao = list(
    rotulo = tr("Proporção", "Proportion"),
    parametro = tr("p", "π"), estatistica = tr("p̂", "π̂"),
    calcular = \(x) mean(x)
  )
)

medidas_da_variavel <- function(variavel) {
  if (identical(variaveis_pop[[variavel]]$tipo, "binaria")) "proporcao" else c("media", "mediana", "dp")
}

selecoes <- list(
  aas = tr("Amostra aleatória simples", "Simple random sample"),
  online = tr("Pesquisa online (só quem usa internet)", "Online survey (internet users only)")
)

# Sorteio sem reposição. Na pesquisa online, só quem usa internet pode entrar;
# dentro desse grupo, o sorteio é aleatório.
sortear_amostra <- function(n, selecao = "aas", pop = populacao) {
  elegiveis <- if (identical(selecao, "online")) pop$id[pop$internet == 1] else pop$id
  sample(elegiveis, min(n, length(elegiveis)))
}

# Parâmetro: a medida calculada com todos os N. O desvio padrão da população
# divide por N; o da amostra, por n − 1 (stats::sd).
valor_parametro <- function(variavel, medida, pop = populacao) {
  x <- pop[[variavel]]
  if (identical(medida, "dp")) return(sqrt(mean((x - mean(x))^2)))
  medidas[[medida]]$calcular(x)
}

calcular_medida <- function(ids, variavel, medida, pop = populacao) {
  medidas[[medida]]$calcular(pop[[variavel]][ids])
}

fmt_medida <- function(valor, variavel, medida) {
  v <- variaveis_pop[[variavel]]
  if (identical(medida, "proporcao")) return(fmt_pct(valor, 1))
  paste0(v$prefixo %||% "", fmt_num(valor, v$digitos), v$unidade)
}
