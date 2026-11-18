# =============================================================================
# Script:  mod_tcl.R (módulo: Teorema Central do Limite)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-18
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/app_estatistica/app.R)
# Saída:
#   - funções mod_tcl_ui() e mod_tcl_server()
# =============================================================================

# Populações com média e desvio padrão teóricos conhecidos
populacoes_tcl <- list(
  uniforme = list(
    rotulo = "Uniforme (0, 1)",
    gerar  = \(n) stats::runif(n),
    media  = 0.5,
    dp     = sqrt(1 / 12)
  ),
  exponencial = list(
    rotulo = "Exponencial (λ = 1), assimétrica",
    gerar  = \(n) stats::rexp(n, rate = 1),
    media  = 1,
    dp     = 1
  ),
  bimodal = list(
    rotulo = "Bimodal (duas normais misturadas)",
    gerar  = \(n) stats::rnorm(n, mean = sample(c(-2, 2), n, replace = TRUE), sd = 0.7),
    media  = 0,
    dp     = sqrt(0.7^2 + 4)
  ),
  lognormal = list(
    rotulo = "Log-normal, muito assimétrica",
    gerar  = \(n) stats::rlnorm(n, meanlog = 0, sdlog = 0.75),
    media  = exp(0.75^2 / 2),
    dp     = sqrt((exp(0.75^2) - 1) * exp(0.75^2))
  )
)

# k amostras de tamanho n de uma vez: cada linha da matriz é uma amostra
medias_amostrais <- function(pop, n, k) {
  matrix(pop$gerar(n * k), nrow = k) |> rowMeans()
}

texto_tcl <- function() {
  shiny::withMathJax(
    shiny::p(
      "Tire muitas amostras de tamanho \\(n\\) de uma população e calcule a média de cada uma.",
      "O Teorema Central do Limite diz que, para \\(n\\) grande o bastante, essas médias se distribuem",
      "aproximadamente como uma normal com média \\(\\mu\\) e desvio padrão \\(\\sigma / \\sqrt{n}\\),",
      "chamado de erro padrão. Isso vale qualquer que seja o formato da população, desde que ela tenha",
      "variância finita."
    ),
    shiny::p(
      "É por causa do TCL que intervalos de confiança e testes baseados na normal funcionam para médias",
      "de dados que não são normais."
    ),
    shiny::p(
      shiny::strong("Experimente:"),
      "escolha a população log-normal com \\(n = 2\\) e aumente \\(n\\) devagar. O histograma das médias",
      "começa torto e vai ficando simétrico. Compare com a uniforme, que já parece normal com \\(n\\) pequeno:",
      "quanto mais assimétrica a população, maior o \\(n\\) necessário."
    )
  )
}

mod_tcl_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::selectInput(
        ns("populacao"), "População",
        choices = stats::setNames(names(populacoes_tcl), purrr::map_chr(populacoes_tcl, "rotulo"))
      ),
      shiny::sliderInput(ns("n"), "Tamanho de cada amostra (n)", min = 1, max = 100, value = 5, step = 1),
      shiny::sliderInput(ns("k"), "Número de amostras", min = 100, max = 5000, value = 1000, step = 100),
      shiny::actionButton(ns("sortear"), "Sortear novamente", class = "btn-primary", icon = shiny::icon("dice"))
    ),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(
        title = "Média das médias amostrais",
        value = shiny::textOutput(ns("media")),
        shiny::textOutput(ns("media_desc")),
        theme = "primary"
      ),
      bslib::value_box(
        title = "Erro padrão observado",
        value = shiny::textOutput(ns("ep")),
        shiny::textOutput(ns("ep_desc")),
        theme = "secondary"
      )
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(5, 7)),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico_pop"), height = "380px"))),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico_medias"), height = "380px")))
    ),
    bslib::card(
      bslib::card_header("Entenda"),
      bslib::card_body(texto_tcl())
    )
  )
}

mod_tcl_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    sorteio <- shiny::reactive({
      input$sortear
      pop <- populacoes_tcl[[input$populacao]]
      list(
        pop     = pop,
        n       = input$n,
        k       = input$k,
        medias  = medias_amostrais(pop, input$n, input$k),
        valores = pop$gerar(20000)
      )
    })

    output$media      <- shiny::renderText(fmt_num(mean(sorteio()$medias), 3))
    output$media_desc <- shiny::renderText(sprintf("Média da população (μ): %s", fmt_num(sorteio()$pop$media, 3)))

    output$ep      <- shiny::renderText(fmt_num(stats::sd(sorteio()$medias), 3))
    output$ep_desc <- shiny::renderText({
      s <- sorteio()
      sprintf("Previsto pelo TCL (σ/√n): %s", fmt_num(s$pop$dp / sqrt(s$n), 3))
    })

    output$grafico_pop <- shiny::renderPlot({
      s <- sorteio()
      limites <- stats::quantile(s$valores, c(0.001, 0.995))

      ggplot2::ggplot(data.frame(x = s$valores), ggplot2::aes(x = x)) +
        ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), bins = 60, fill = cores_app$neutro, colour = "white") +
        ggplot2::geom_vline(xintercept = s$pop$media, linetype = "dashed", colour = cores_app$texto) +
        ggplot2::coord_cartesian(xlim = limites) +
        ggplot2::labs(
          title    = "População",
          subtitle = sprintf("μ = %s | σ = %s", fmt_num(s$pop$media, 2), fmt_num(s$pop$dp, 2)),
          x = "Valor", y = "Densidade",
          caption  = "Tracejado: média da população"
        )
    })

    output$grafico_medias <- shiny::renderPlot({
      s <- sorteio()
      ep <- s$pop$dp / sqrt(s$n)
      grade <- seq(min(s$medias), max(s$medias), length.out = 400)
      curva <- data.frame(x = grade, y = stats::dnorm(grade, mean = s$pop$media, sd = ep))

      ggplot2::ggplot(data.frame(x = s$medias), ggplot2::aes(x = x)) +
        ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), bins = 40, fill = cores_app$destaque, colour = "white", alpha = 0.85) +
        ggplot2::geom_line(data = curva, ggplot2::aes(x = x, y = y), linewidth = 1.1, colour = cores_app$referencia) +
        ggplot2::labs(
          title    = "Distribuição das médias amostrais",
          subtitle = sprintf("%s amostras de tamanho n = %d", fmt_num(s$k, 0), s$n),
          x = "Média da amostra", y = "Densidade",
          caption  = "Curva azul: normal prevista pelo TCL"
        )
    })
  })
}
