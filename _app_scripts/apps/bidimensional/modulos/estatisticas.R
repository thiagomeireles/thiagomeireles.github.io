# =============================================================================
# Script:  estatisticas.R (dados simulados e medidas do tema Análise bidimensional)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-19
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/apps/bidimensional/app.R)
# Saída:
#   - objetos em memória: pares_tipo, exemplos_* e funções de simulação, de
#     associação e de interface usadas pelos módulos mod_*.R
# =============================================================================

# Textos em pt e en via tr() (comum/utilitarios.R)

# Variâncias e covariâncias deste tema dividem por n, como Morettin e Bussab no
# Cap. 4: assim corr = cov / (dp(X) dp(Y)) e R² = 1 − var̄/var batem com o
# livro. Agresti e Finlay dividem por n − 1; a correlação não muda com o divisor.
var_n <- function(x) mean((x - mean(x))^2)
dp_n <- function(x) sqrt(var_n(x))
cov_n <- function(x, y) mean((x - mean(x)) * (y - mean(y)))

# Os três pares de tipos, usados na visão geral e no topo de cada aba
pares_tipo <- list(
  quali_quali = list(
    nome = tr("Qualitativa × qualitativa", "Categorical × categorical"),
    aba = tr("Quali × quali", "Cat. × cat."),
    pergunta = tr(
      "A distribuição de uma variável muda conforme a categoria da outra?",
      "Does the distribution of one variable change across the categories of the other?"
    ),
    tabela = tr(
      "tabela de dupla entrada, com porcentagens na linha, na coluna ou no total",
      "contingency table, with row, column or total percentages"
    ),
    grafico = tr("barras empilhadas com os perfis de cada categoria", "stacked bars with the profile of each category"),
    medida = tr("χ², coeficiente de contingência C e coeficiente T", "χ², contingency coefficient C and coefficient T")
  ),
  quanti_quanti = list(
    nome = tr("Quantitativa × quantitativa", "Quantitative × quantitative"),
    aba = tr("Quanti × quanti", "Quant. × quant."),
    pergunta = tr(
      "Quando uma variável cresce, a outra tende a crescer, a diminuir ou nenhum dos dois?",
      "When one variable goes up, does the other tend to go up, go down, or neither?"
    ),
    tabela = tr("médias, desvios padrão e covariância", "means, standard deviations and covariance"),
    grafico = tr("gráfico de dispersão", "scatterplot"),
    medida = tr("coeficiente de correlação (e reta de regressão)", "correlation coefficient (and regression line)")
  ),
  quali_quanti = list(
    nome = tr("Qualitativa × quantitativa", "Categorical × quantitative"),
    aba = tr("Quali × quanti", "Cat. × quant."),
    pergunta = tr(
      "Os valores da variável quantitativa mudam de um grupo para outro?",
      "Do the values of the quantitative variable change from one group to another?"
    ),
    tabela = tr("medidas-resumo da quantitativa em cada grupo", "summary measures of the quantitative variable in each group"),
    grafico = tr("box plots lado a lado (e gráfico q × q para dois grupos)", "side-by-side box plots (and Q-Q plot for two groups)"),
    medida = tr("R² = 1 − variância média dos grupos / variância total", "R² = 1 − mean within-group variance / total variance")
  )
)

opcoes_par <- stats::setNames(names(pares_tipo), purrr::map_chr(pares_tipo, "nome"))

opcoes_exemplo <- function(exemplos) {
  stats::setNames(names(exemplos), purrr::map_chr(exemplos, "rotulo"))
}

# Qualitativa × qualitativa
# A explicativa fica nas linhas. Com força 0, a resposta segue prob_colunas em
# todas as linhas (independência); com força 1, segue o perfil de cada linha.
exemplos_quali_quali <- list(
  escolaridade_internet = list(
    rotulo = tr("Escolaridade × uso de internet", "Education × internet use"),
    nome_linhas = tr("Escolaridade", "Education"),
    nome_colunas = tr("Uso de internet", "Internet use"),
    linhas = tr(c("Fundamental", "Médio", "Superior"), c("Elementary/middle", "High school", "College")),
    colunas = tr(c("Usa", "Não usa"), c("Uses it", "Does not use it")),
    prob_linhas = c(0.35, 0.42, 0.23),
    prob_colunas = c(0.80, 0.20),
    perfil = rbind(c(0.55, 0.45), c(0.88, 0.12), c(0.98, 0.02))
  ),
  idade_informacao = list(
    rotulo = tr("Faixa etária × principal fonte de notícias", "Age group × main news source"),
    nome_linhas = tr("Faixa etária", "Age group"),
    nome_colunas = tr("Fonte de notícias", "News source"),
    linhas = tr(c("16 a 29 anos", "30 a 49 anos", "50 anos ou mais"), c("16 to 29", "30 to 49", "50 and over")),
    colunas = tr(c("Redes sociais", "Televisão", "Sites e jornais"), c("Social media", "Television", "News sites and papers")),
    prob_linhas = c(0.28, 0.37, 0.35),
    prob_colunas = c(0.40, 0.40, 0.20),
    perfil = rbind(c(0.75, 0.10, 0.15), c(0.40, 0.35, 0.25), c(0.12, 0.70, 0.18))
  ),
  escola_aprovacao = list(
    rotulo = tr("Tipo de escola × aprovação no vestibular", "School type × university admission"),
    nome_linhas = tr("Tipo de escola", "School type"),
    nome_colunas = tr("Vestibular", "Admission"),
    linhas = tr(c("Pública", "Privada"), c("Public", "Private")),
    colunas = tr(c("Aprovado", "Não aprovado"), c("Admitted", "Not admitted")),
    prob_linhas = c(0.75, 0.25),
    prob_colunas = c(0.30, 0.70),
    perfil = rbind(c(0.18, 0.82), c(0.65, 0.35))
  )
)

gerar_quali_quali <- function(ex, forca, n) {
  k <- length(ex$colunas)
  condicional <- (1 - forca) * matrix(ex$prob_colunas, nrow = length(ex$linhas), ncol = k, byrow = TRUE) + forca * ex$perfil
  linha <- sample(seq_along(ex$linhas), n, replace = TRUE, prob = ex$prob_linhas)
  coluna <- purrr::map_int(linha, \(i) sample.int(k, 1, prob = condicional[i, ]))
  table(
    factor(ex$linhas[linha], levels = ex$linhas),
    factor(ex$colunas[coluna], levels = ex$colunas)
  ) |>
    unclass()
}

# χ², C e T de Morettin e Bussab (4.4 a 4.6). Linhas ou colunas sem nenhuma
# observação saem da conta, porque a frequência esperada delas seria zero.
# Máximos: χ²/n não passa de min(r, s) − 1, o que limita C e T.
associacao_quali <- function(m) {
  m <- m[rowSums(m) > 0, colSums(m) > 0, drop = FALSE]
  r <- nrow(m)
  s <- ncol(m)
  n <- sum(m)
  esperado <- outer(rowSums(m), colSums(m)) / n
  if (r < 2 || s < 2) {
    return(list(n = n, r = r, s = s, esperado = esperado, qui2 = NA, C = NA, T = NA, C_max = NA, T_max = NA))
  }
  qui2 <- sum((m - esperado)^2 / esperado)
  k <- min(r, s)
  list(
    n = n, r = r, s = s, esperado = esperado, qui2 = qui2,
    C = sqrt(qui2 / (qui2 + n)),
    T = sqrt((qui2 / n) / ((r - 1) * (s - 1))),
    C_max = sqrt((k - 1) / k),
    T_max = sqrt((k - 1) / ((r - 1) * (s - 1)))
  )
}

# Leitura da força da associação pela proporção do máximo possível. Os cortes
# são uma regra prática do app, não vêm de Morettin e Bussab nem de Agresti e Finlay.
leitura_forca <- function(valor, maximo = 1) {
  if (is.na(valor)) return("")
  p <- abs(valor) / maximo
  if (p < 0.1) return(tr("praticamente nula", "practically none"))
  if (p < 0.3) return(tr("fraca", "weak"))
  if (p < 0.6) return(tr("moderada", "moderate"))
  tr("forte", "strong")
}

# Quantitativa × quantitativa
# Variáveis padronizadas: linear com correlação ρ (x normal) ou relação em U
# com força f. No U, x é uniforme: com x normal, a correlação amostral entre x
# e x² oscila muito (z³ tem caudas pesadas) e esconde o "r perto de zero".
exemplos_quanti_quanti <- list(
  altura_peso = list(
    rotulo = tr("Altura × peso", "Height × weight"),
    nome_x = tr("Altura", "Height"), un_x = "cm", media_x = 170, dp_x = 9, dig_x = 1,
    nome_y = tr("Peso", "Weight"), un_y = "kg", media_y = 70, dp_y = 12, dig_y = 1,
    forma = "linear", forca = 0.7
  ),
  estudo_nota = list(
    rotulo = tr("Horas de estudo × nota na prova", "Study hours × test score"),
    nome_x = tr("Horas de estudo por semana", "Weekly study hours"), un_x = "h", media_x = 12, dp_x = 3, dig_x = 1,
    nome_y = tr("Nota na prova", "Test score"), un_y = tr("pontos", "points"), media_y = 60, dp_y = 12, dig_y = 1,
    forma = "linear", forca = 0.5
  ),
  preco_vendas = list(
    rotulo = tr("Preço × vendas por dia", "Price × daily sales"),
    nome_x = tr("Preço", "Price"), un_x = "R$", media_x = 20, dp_x = 4, dig_x = 2,
    nome_y = tr("Unidades vendidas por dia", "Units sold per day"), un_y = tr("unidades", "units"), media_y = 120, dp_y = 20, dig_y = 1,
    forma = "linear", forca = -0.6
  ),
  temperatura_energia = list(
    rotulo = tr("Temperatura × consumo de energia (relação em U)", "Temperature × energy use (U-shaped)"),
    nome_x = tr("Temperatura média do dia", "Daily mean temperature"), un_x = "°C", media_x = 22, dp_x = 6, dig_x = 1,
    nome_y = tr("Consumo de energia da casa", "Household energy use"), un_y = "kWh", media_y = 30, dp_y = 6, dig_y = 1,
    forma = "u", forca = 0.8
  )
)

gerar_quanti_quanti <- function(ex, forca, n) {
  em_u <- identical(ex$forma, "u")
  # Uniforme em [−√3, √3]: média 0, variância 1 e var(z²) = 0,8
  z <- if (em_u) stats::runif(n, -sqrt(3), sqrt(3)) else stats::rnorm(n)
  ruido <- stats::rnorm(n)
  w <- if (em_u) {
    sqrt(forca) * (z^2 - 1) / sqrt(0.8) + sqrt(1 - forca) * ruido
  } else {
    forca * z + sqrt(1 - forca^2) * ruido
  }
  data.frame(x = ex$media_x + ex$dp_x * z, y = ex$media_y + ex$dp_y * w)
}

# Ponto atípico na contramão da tendência: 3 desvios à direita e 3 abaixo
# (ou acima, quando a relação é negativa)
ponto_atipico <- function(ex, forca) {
  sinal <- if (identical(ex$forma, "linear") && forca < 0) -1 else 1
  data.frame(x = ex$media_x + 3 * ex$dp_x, y = ex$media_y - 3 * sinal * ex$dp_y)
}

resumo_quanti_quanti <- function(x, y) {
  covariancia <- cov_n(x, y)
  b <- covariancia / var_n(x)
  list(
    n = length(x), media_x = mean(x), media_y = mean(y), dp_x = dp_n(x), dp_y = dp_n(y),
    cov = covariancia, cov_n1 = stats::cov(x, y), r = stats::cor(x, y),
    b = b, a = mean(y) - b * mean(x)
  )
}

# Qualitativa × quantitativa
# `desvios` desloca cada grupo; `gerar(deslocamento, n)` sorteia os valores
exemplos_quali_quanti <- list(
  escolaridade_salario = list(
    rotulo = tr("Escolaridade × salário", "Education × salary"),
    nome_grupo = tr("Escolaridade", "Education"),
    grupos = tr(c("Fundamental", "Médio", "Superior"), c("Elementary/middle", "High school", "College")),
    prob = c(0.33, 0.45, 0.22),
    desvios = c(-0.5, 0, 0.8),
    nome_y = tr("Salário mensal", "Monthly salary"), un_y = "R$", dig_y = 0,
    gerar = \(d, n) stats::rlnorm(n, log(2200) + d, 0.5)
  ),
  escola_nota = list(
    rotulo = tr("Tipo de escola × nota na prova", "School type × test score"),
    nome_grupo = tr("Tipo de escola", "School type"),
    grupos = tr(c("Pública", "Privada"), c("Public", "Private")),
    prob = c(0.75, 0.25),
    desvios = c(-0.3, 1.2),
    nome_y = tr("Nota na prova", "Test score"), un_y = tr("pontos", "points"), dig_y = 1,
    gerar = \(d, n) stats::rnorm(n, 60 + 12 * d, 12)
  ),
  regiao_tempo = list(
    rotulo = tr("Região × tempo de deslocamento ao trabalho", "Region × commute time"),
    nome_grupo = tr("Região", "Region"),
    grupos = tr(
      c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul"),
      c("North", "Northeast", "Central-West", "Southeast", "South")
    ),
    prob = c(0.085, 0.269, 0.080, 0.418, 0.147),
    desvios = c(-0.1, 0, -0.05, 0.25, -0.15),
    nome_y = tr("Tempo de deslocamento", "Commute time"), un_y = "min", dig_y = 1,
    gerar = \(d, n) stats::rgamma(n, shape = 2, rate = 2 / (30 * exp(d)))
  )
)

gerar_quali_quanti <- function(ex, efeito, n) {
  g <- sample(seq_along(ex$grupos), n, replace = TRUE, prob = ex$prob)
  data.frame(
    grupo = factor(ex$grupos[g], levels = ex$grupos),
    y = ex$gerar(efeito * ex$desvios[g], n)
  )
}

# Medidas por grupo e R² de Morettin e Bussab (4.12): grupos sem observação saem
resumo_grupos <- function(dados) {
  grupos <- dados$y |> split(dados$grupo) |> purrr::keep(\(v) length(v) > 0)
  por_grupo <- data.frame(
    grupo = names(grupos),
    n = purrr::map_int(grupos, length),
    media = purrr::map_dbl(grupos, mean),
    mediana = purrr::map_dbl(grupos, stats::median),
    dp = purrr::map_dbl(grupos, dp_n),
    var = purrr::map_dbl(grupos, var_n)
  )
  var_total <- var_n(dados$y)
  var_media <- sum(por_grupo$n * por_grupo$var) / sum(por_grupo$n)
  list(
    por_grupo = por_grupo,
    n = nrow(dados), media = mean(dados$y), mediana = stats::median(dados$y),
    dp = dp_n(dados$y), var = var_total, var_media = var_media,
    r2 = 1 - var_media / var_total
  )
}

# Gráfico q × q (Morettin e Bussab, 4.7): duas amostras de mesmo tamanho; o
# quantil de ordem p_i = (i − 0,5)/n de cada uma é a i-ésima menor observação
formas_qq <- list(
  normal = list(rotulo = tr("Simétrica (normal)", "Symmetric (normal)"), gerar = \(n) stats::rnorm(n)),
  assimetrica = list(rotulo = tr("Assimétrica à direita", "Right-skewed"), gerar = \(n) stats::rexp(n) - 1)
)

gerar_qq <- function(forma_a, forma_b, deslocamento, razao, n) {
  list(
    a = 60 + 10 * formas_qq[[forma_a]]$gerar(n),
    b = 60 + 10 * deslocamento + 10 * razao * formas_qq[[forma_b]]$gerar(n)
  )
}

# Valor com unidade: "R$ 2.500" ou "168,3 cm"
fmt_valor <- function(x, unidade, digitos = 1, quadrado = FALSE) {
  un <- if (quadrado) paste0(unidade, "²") else unidade
  if (identical(unidade, "R$")) paste(un, fmt_num(x, digitos)) else paste(fmt_num(x, digitos), un)
}

# Cores das categorias da resposta (zephyr: primary, warning, success, info)
cores_categorias <- function(k) {
  c("#3459e6", "#f4bd61", "#2fb380", "#287bb5", "#da292e")[seq_len(k)]
}

nomes_estatisticas <- function() tr(c("Medida", "Valor", "Como ler"), c("Measure", "Value", "How to read it"))

linha_estatistica <- function(medida, valor, leitura) {
  data.frame(medida = medida, valor = valor, leitura = leitura)
}

# Topo de cada aba: nome do par e a pergunta que a análise responde
cabecalho_par <- function(par) {
  p <- pares_tipo[[par]]
  shiny::div(
    class = "mb-2",
    shiny::tags$h4(class = "mb-1", p$nome),
    shiny::p(class = "mb-0", p$pergunta, " ", shiny::span(class = "badge text-bg-primary ms-1", p$medida))
  )
}

# Lista de cuidados: o nome vai em negrito, o valor é o texto
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

botao_sortear <- function(id) {
  shiny::actionButton(id, tr("Sortear nova amostra", "Draw a new sample"), class = "btn-primary", icon = shiny::icon("dice"))
}

slider_amostra <- function(id, min, max, value, step = 10) {
  shiny::sliderInput(id, tr("Tamanho da amostra", "Sample size"), min = min, max = max, value = value, step = step, sep = sep_milhar())
}

seletor_exemplo <- function(id, exemplos) {
  shiny::selectInput(id, tr("Par de variáveis", "Pair of variables"), choices = opcoes_exemplo(exemplos))
}
