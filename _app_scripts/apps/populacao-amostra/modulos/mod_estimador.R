# =============================================================================
# Script:  mod_estimador.R (módulo: estimador e estimativa; divisor n e n − 1)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/apps/populacao-amostra/app.R)
# Saída:
#   - funções mod_estimador_ui() e mod_estimador_server()
# =============================================================================

# Variáveis usadas aqui; a renda vai em milhares de reais para a variância
# (em reais ao quadrado) não virar um número de dez dígitos
variaveis_estimador <- list(
  idade = list(
    rotulo = tr("Idade (anos)", "Age (years)"),
    valores = populacao$idade,
    unidade = tr("anos²", "years²"), digitos = 1
  ),
  renda = list(
    rotulo = tr("Renda mensal (R$ mil)", "Monthly income (R$ thousand)"),
    valores = populacao$renda / 1000,
    unidade = tr("(R$ mil)²", "(R$ thousand)²"), digitos = 2
  )
)

# Variância da população: média dos desvios ao quadrado, divisor N
variancia_populacao <- function(x) mean((x - mean(x))^2)

# k amostras de tamanho n, COM reposição: as observações são independentes e
# têm a mesma distribuição (a da população), como na definição formal de
# amostra aleatória. Cada linha da matriz é uma amostra.
simular_variancias <- function(x, n, k) {
  m <- matrix(sample(x, n * k, replace = TRUE), nrow = k)
  soma_quadrados <- rowSums((m - rowMeans(m))^2)
  list(s2 = soma_quadrados / (n - 1), s2_n = soma_quadrados / n, ultima = m[k, ])
}

# A última amostra, valor por valor, e as duas estimativas que ela produz:
# o estimador é a regra; a estimativa é o número que a regra dá nesta amostra
ultima_amostra_ui <- function(valores, s2, s2_n, digitos, fmt_var) {
  shiny::tagList(
    shiny::p(
      class = "small text-muted mb-1",
      sprintf(tr("Os %d valores sorteados:", "The %d values drawn:"), length(valores))
    ),
    shiny::p(class = "font-monospace", paste(fmt_num(valores, digitos), collapse = tr("; ", ", "))),
    shiny::tags$ul(
      class = "mb-2",
      shiny::tags$li(tr("Regra com n − 1 (S²) aplicada a eles: ", "Rule with n − 1 (s²) applied to them: "), shiny::strong(fmt_var(s2))),
      shiny::tags$li(tr("Regra com n aplicada a eles: ", "Rule with n applied to them: "), shiny::strong(fmt_var(s2_n)))
    ),
    shiny::p(
      class = "small text-muted mb-0",
      tr(
        "Cada número acima é uma estimativa. A regra que o produziu é o estimador, e é ela que o gráfico avalia ao longo de muitas amostras.",
        "Each number above is an estimate. The rule that produced it is the estimator, and the chart judges that rule over many samples."
      )
    )
  )
}

resumo_estimador <- function(estimativas, alvo) {
  c(
    media = mean(estimativas),
    vies = mean(estimativas) - alvo,
    dp = sqrt(mean((estimativas - mean(estimativas))^2)),
    eqm = mean((estimativas - alvo)^2)
  )
}

texto_estimador <- function() {
  tr(
    shiny::withMathJax(
      shiny::p(
        "Antes de sortear qualquer amostra, a fórmula da variância amostral já existe. Essa regra é o estimador.",
        "Aplicada a uma amostra concreta, ela dá um número, a estimativa. Como cada amostra dá uma estimativa",
        "diferente, faz sentido perguntar como o estimador se comporta ao longo de muitas amostras, e é isso que a",
        "aba simula."
      ),
      shiny::p(
        "Dividir a soma dos desvios ao quadrado por \\(n\\) parece natural, mas os desvios são medidos em torno de",
        "\\(\\bar{x}\\), que fica no meio da própria amostra, e não em torno de \\(\\mu\\). Por isso a soma sai menor,",
        "em média, do que sairia em torno de \\(\\mu\\). Com divisor \\(n\\), a média das estimativas fica em",
        "\\(\\frac{n-1}{n}\\sigma^2\\): o estimador é viesado. Com divisor \\(n-1\\), a média das estimativas fica em",
        "\\(\\sigma^2\\): \\(S^2\\) é não viesado."
      ),
      shiny::p(
        "Ser não viesado não quer dizer ser melhor em tudo. A tabela mostra também o erro quadrático médio (EQM), que",
        "soma variância e viés ao quadrado. Nas duas variáveis do app, o divisor \\(n\\) tem EQM menor em qualquer",
        "\\(n\\) do controle: erra um pouco para baixo, mas varia menos (Casella e Berger, Ex. 7.3.4, mostram isso para",
        "a normal). A vantagem é maior com \\(n\\) pequeno e na renda, que tem cauda pesada, e encolhe à medida que",
        "\\(n\\) cresce."
      ),
      shiny::p(
        shiny::strong("Experimente:"),
        "comece com \\(n = 2\\) e repare que a linha cinza estaciona bem abaixo da vermelha. Aumente \\(n\\) e as duas",
        "linhas se aproximam. Depois troque a idade pela renda e confira, na nota da tabela, quanto o divisor \\(n\\)",
        "reduz o EQM. Como as unidades das duas variáveis são diferentes, compare esse percentual, e não o EQM em si."
      )
    ),
    shiny::withMathJax(
      shiny::p(
        "Before any sample is drawn, the formula for the sample variance already exists. That rule is the estimator.",
        "Applied to an actual sample, it gives a number, the estimate. Since each sample gives a different estimate,",
        "it makes sense to ask how the estimator behaves over many samples, and that is what this tab simulates."
      ),
      shiny::p(
        "Dividing the sum of squared deviations by \\(n\\) seems natural, but the deviations are measured around",
        "\\(\\bar{y}\\), which sits in the middle of the sample itself, not around \\(\\mu\\). So the sum comes out",
        "smaller, on average, than it would around \\(\\mu\\). With divisor \\(n\\), the mean of the estimates settles at",
        "\\(\\frac{n-1}{n}\\sigma^2\\): the estimator is biased. With divisor \\(n-1\\), it settles at \\(\\sigma^2\\):",
        "\\(s^2\\) is unbiased."
      ),
      shiny::p(
        "Being unbiased does not mean being better in every way. The table also shows the mean squared error (MSE),",
        "which adds the variance and the squared bias. For both variables in the app, divisor \\(n\\) has the smaller",
        "MSE at every \\(n\\) on the slider: it misses slightly low but varies less (Casella and Berger, Ex. 7.3.4, show",
        "this for the normal). The advantage is largest with small \\(n\\) and for income, which is heavy-tailed, and it",
        "shrinks as \\(n\\) grows."
      ),
      shiny::p(
        shiny::strong("Try it:"),
        "start with \\(n = 2\\) and watch the grey line settle well below the red one. Increase \\(n\\) and the two",
        "lines come together. Then switch from age to income and check, in the note under the table, how much divisor",
        "\\(n\\) cuts the MSE. The two variables are in different units, so compare that percentage, not the MSE itself."
      )
    )
  )
}

texto_para_ir_alem <- function() {
  tr(
    shiny::withMathJax(
      shiny::p(
        "Na definição formal, uma amostra aleatória de tamanho \\(n\\) é um conjunto de variáveis aleatórias",
        "\\(X_1, \\dots, X_n\\) independentes e com a mesma distribuição da população (i.i.d.). Uma estatística é",
        "qualquer função \\(T(X_1, \\dots, X_n)\\) que não dependa de parâmetros desconhecidos. Por ser função de",
        "variáveis aleatórias, ela também é uma variável aleatória, com distribuição própria, a distribuição amostral",
        "(Casella e Berger, Def. 5.1.1 e 5.2.1)."
      ),
      shiny::p(
        "Usada para estimar um parâmetro \\(\\theta\\), a estatística se chama estimador, \\(W = h(X_1, \\dots, X_n)\\),",
        "uma regra fixada antes de ver os dados. A estimativa é o número \\(w = h(x_1, \\dots, x_n)\\) obtido na amostra",
        "observada (Casella e Berger, Def. 7.1.1; Wooldridge, apêndice C.2). Viés e variância são propriedades do estimador, nunca de uma estimativa:",
        "\\(\\text{viés}(W) = E(W) - \\theta\\) e \\(\\text{EQM}(W) = \\text{Var}(W) + \\text{viés}(W)^2\\)",
        "(Casella e Berger, seção 7.3.1)."
      ),
      shiny::p(
        "Esta aba sorteia com reposição para que as observações sejam i.i.d. e \\(E(S^2) = \\sigma^2\\) valha exatamente",
        "(Casella e Berger, Teo. 5.2.6). Sem reposição, numa população finita, as observações deixam de ser",
        "independentes, e \\(S^2\\) passa a estimar \\(\\frac{N}{N-1}\\sigma^2\\). Com \\(N = 2.000\\), a diferença é de 0,05%."
      ),
      shiny::p(
        class = "mb-0",
        "Nenhum divisor conserta uma amostra que não é aleatória. Na pesquisa online da aba anterior, \\(\\bar{X}\\) se",
        "aproxima da média de quem usa internet, e não de \\(\\mu\\), por maior que seja \\(n\\) (Wooldridge, seção 9.5)."
      )
    ),
    shiny::withMathJax(
      shiny::p(
        "In the formal definition, a random sample of size \\(n\\) is a set of random variables \\(Y_1, \\dots, Y_n\\)",
        "that are independent and share the population's distribution (i.i.d.). A statistic is any function",
        "\\(T(Y_1, \\dots, Y_n)\\) that does not depend on unknown parameters. Being a function of random variables, it",
        "is itself a random variable, with a distribution of its own, the sampling distribution (Casella and Berger,",
        "Def. 5.1.1 and 5.2.1)."
      ),
      shiny::p(
        "When used to estimate a parameter \\(\\theta\\), the statistic is called an estimator, \\(W = h(Y_1, \\dots, Y_n)\\),",
        "a rule fixed before seeing the data. The estimate is the number \\(w = h(y_1, \\dots, y_n)\\) obtained from the",
        "observed sample (Casella and Berger, Def. 7.1.1; Wooldridge, appendix C.2). Bias and variance are properties of the estimator, never of a single",
        "estimate: \\(\\text{Bias}(W) = E(W) - \\theta\\) and \\(\\text{MSE}(W) = \\text{Var}(W) + \\text{Bias}(W)^2\\)",
        "(Casella and Berger, section 7.3.1)."
      ),
      shiny::p(
        "This tab draws with replacement so that the observations are i.i.d. and \\(E(S^2) = \\sigma^2\\) holds exactly",
        "(Casella and Berger, Thm. 5.2.6). Without replacement, in a finite population, the observations are no longer",
        "independent, and \\(S^2\\) estimates \\(\\frac{N}{N-1}\\sigma^2\\) instead. With \\(N = 2{,}000\\), the difference is 0.05%."
      ),
      shiny::p(
        class = "mb-0",
        "No divisor fixes a sample that is not random. In the online survey of the previous tab, \\(\\bar{Y}\\) approaches",
        "the mean among internet users, not \\(\\mu\\), however large \\(n\\) is (Wooldridge, section 9.5)."
      )
    )
  )
}

mod_estimador_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::selectInput(
        ns("variavel"), tr("Variável", "Variable"),
        choices = stats::setNames(names(variaveis_estimador), purrr::map_chr(variaveis_estimador, "rotulo"))
      ),
      shiny::sliderInput(ns("n"), tr("Tamanho de cada amostra (n)", "Size of each sample (n)"), min = 2, max = 30, value = 4, step = 1),
      shiny::sliderInput(ns("k"), tr("Número de amostras", "Number of samples"), min = 100, max = 5000, value = 2000, step = 100, sep = sep_milhar()),
      shiny::actionButton(ns("sortear"), tr("Sortear novamente", "Draw again"), class = "btn-primary", icon = shiny::icon("dice")),
      shiny::helpText(tr(
        "Aqui o sorteio é com reposição: a mesma pessoa pode sair mais de uma vez na amostra.",
        "Here the draw is with replacement: the same person can appear more than once in a sample."
      ))
    ),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(
        title = tr("Parâmetro: σ² da população", "Parameter: population σ²"),
        value = shiny::textOutput(ns("sigma2")),
        tr("Divisor N = 2.000", "Divisor N = 2,000"),
        theme = "primary"
      ),
      bslib::value_box(
        title = tr("Média das estimativas com n − 1 (S²)", "Mean of the estimates with n − 1 (s²)"),
        value = shiny::textOutput(ns("media_s2")),
        shiny::textOutput(ns("vies_s2")),
        theme = "info"
      ),
      bslib::value_box(
        title = tr("Média das estimativas com n", "Mean of the estimates with n"),
        value = shiny::textOutput(ns("media_s2_n")),
        shiny::textOutput(ns("vies_s2_n")),
        theme = NULL
      )
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico_acumulado"), height = "400px"))),
      shiny::div(
        bslib::card(
          bslib::card_header(tr("Uma amostra, duas estimativas", "One sample, two estimates")),
          bslib::card_body(shiny::uiOutput(ns("ultima")))
        ),
        bslib::card(
          bslib::card_header(tr("Os dois estimadores lado a lado", "The two estimators side by side")),
          bslib::card_body(
            shiny::uiOutput(ns("tabela")),
            shiny::p(class = "small text-muted mb-0", shiny::textOutput(ns("nota_tabela"), inline = TRUE))
          )
        )
      )
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)),
      bslib::card(bslib::card_header(rotulo_entenda()), bslib::card_body(texto_estimador())),
      bslib::card(bslib::card_header(tr("Para ir além", "Going further")), bslib::card_body(texto_para_ir_alem()))
    )
  )
}

mod_estimador_server <- function(id, modo = shiny::reactive("light")) {
  shiny::moduleServer(id, function(input, output, session) {

    variavel <- shiny::reactive(variaveis_estimador[[input$variavel]])
    sigma2 <- shiny::reactive(variancia_populacao(variavel()$valores))

    simulacao <- shiny::reactive({
      input$sortear
      simular_variancias(variavel()$valores, input$n, input$k)
    })

    fmt_var <- function(x) paste(fmt_num(x, variavel()$digitos), variavel()$unidade)
    fmt_vies <- function(x) {
      sinal <- if (x > 0) "+" else if (x < 0) "−" else ""
      paste0(sinal, fmt_num(abs(x), variavel()$digitos))
    }

    output$sigma2 <- shiny::renderText(fmt_var(sigma2()))
    output$media_s2 <- shiny::renderText(fmt_var(mean(simulacao()$s2)))
    output$media_s2_n <- shiny::renderText(fmt_var(mean(simulacao()$s2_n)))

    output$vies_s2 <- shiny::renderText({
      sprintf(tr("Diferença para σ²: %s (o esperado é 0)", "Difference from σ²: %s (expected: 0)"), fmt_vies(mean(simulacao()$s2) - sigma2()))
    })

    output$vies_s2_n <- shiny::renderText({
      esperado <- -sigma2() / input$n
      sprintf(
        tr("Diferença para σ²: %s (o esperado é −σ²/n = %s)", "Difference from σ²: %s (expected: −σ²/n = %s)"),
        fmt_vies(mean(simulacao()$s2_n) - sigma2()), fmt_vies(esperado)
      )
    })

    output$ultima <- shiny::renderUI({
      s <- simulacao()
      k <- length(s$s2)
      ultima_amostra_ui(s$ultima, s$s2[k], s$s2_n[k], variavel()$digitos, fmt_var)
    })

    output$grafico_acumulado <- shiny::renderPlot({
      pal <- paleta_modo(modo())
      s <- simulacao()
      k <- length(s$s2)
      rotulos <- tr(c("Divisor n − 1 (S²)", "Divisor n"), c("Divisor n − 1 (s²)", "Divisor n"))
      dados <- data.frame(
        amostra = rep(seq_len(k), 2),
        valor = c(cumsum(s$s2) / seq_len(k), cumsum(s$s2_n) / seq_len(k)),
        estimador = factor(rep(rotulos, each = k), levels = rotulos)
      )
      alvo_n <- sigma2() * (input$n - 1) / input$n

      ggplot2::ggplot(dados, ggplot2::aes(x = amostra, y = valor, colour = estimador)) +
        ggplot2::geom_hline(yintercept = sigma2(), colour = cores_app$referencia, linewidth = 1) +
        ggplot2::geom_hline(yintercept = alvo_n, colour = pal$secundario, linetype = "dashed") +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::scale_colour_manual(values = c(cores_app$destaque, pal$secundario), name = NULL) +
        ggplot2::scale_x_continuous(labels = rotulos_eixo()) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = tr("Média das estimativas, amostra após amostra", "Mean of the estimates, sample after sample"),
          subtitle = sprintf(tr("%s amostras de tamanho n = %d", "%s samples of size n = %d"), fmt_num(k, 0), input$n),
          x = tr("Número de amostras", "Number of samples"), y = variavel()$unidade,
          caption = tr(
            "Linha vermelha: σ². Tracejada: (n − 1)/n · σ², para onde vai a média com divisor n",
            "Red line: σ². Dashed: (n − 1)/n · σ², where the mean with divisor n settles"
          )
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(legend.position = "top", legend.justification = "left")
    })

    output$tabela <- shiny::renderUI({
      s <- simulacao()
      linhas <- list(resumo_estimador(s$s2, sigma2()), resumo_estimador(s$s2_n, sigma2()))
      tabela <- data.frame(
        tr(c("Divisor n − 1 (S²)", "Divisor n"), c("Divisor n − 1 (s²)", "Divisor n")),
        purrr::map_chr(linhas, \(l) fmt_num(l[["media"]], variavel()$digitos)),
        purrr::map_chr(linhas, \(l) fmt_vies(l[["vies"]])),
        purrr::map_chr(linhas, \(l) fmt_num(l[["dp"]], variavel()$digitos)),
        purrr::map_chr(linhas, \(l) fmt_num(l[["eqm"]], variavel()$digitos))
      )
      names(tabela) <- tr(
        c("Estimador", "Média", "Viés", "Desvio padrão", "EQM"),
        c("Estimator", "Mean", "Bias", "Standard deviation", "MSE")
      )
      tabela_html(tabela, classes = c("", "num", "num", "num", "num"))
    })

    output$nota_tabela <- shiny::renderText({
      s <- simulacao()
      eqm_n <- mean((s$s2_n - sigma2())^2)
      eqm_n1 <- mean((s$s2 - sigma2())^2)
      menor <- if (eqm_n < eqm_n1) "n" else "n − 1"
      sprintf(
        tr(
          paste(
            "Viés: média das estimativas menos σ². EQM: média de (estimativa − σ²)², igual a variância + viés². Nesta",
            "simulação, o menor EQM ficou com o divisor %s, e o EQM do divisor n equivale a %s do EQM do divisor n − 1."
          ),
          paste(
            "Bias: mean of the estimates minus σ². MSE: mean of (estimate − σ²)², equal to variance + bias². In this",
            "simulation, the smaller MSE went to divisor %s, and the MSE with divisor n is %s of the MSE with divisor n − 1."
          )
        ),
        menor, fmt_pct(eqm_n / eqm_n1, 0)
      )
    })
  })
}
