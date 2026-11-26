# =============================================================================
# Script:  mod_lgn.R (módulo: Lei dos Grandes Números)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-25
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/apps/teoremas-limite/app.R)
# Saída:
#   - funções mod_lgn_ui() e mod_lgn_server()
# =============================================================================

experimentos_lgn <- list(
  moeda = list(
    rotulo = tr("Moeda honesta (proporção de caras)", "Fair coin (proportion of heads)"),
    gerar  = \(n) stats::rbinom(n, size = 1, prob = 0.5),
    media  = 0.5,
    dp     = 0.5
  ),
  moeda_viciada = list(
    rotulo = tr("Moeda viciada, p = 0,7 (proporção de caras)", "Biased coin, p = 0.7 (proportion of heads)"),
    gerar  = \(n) stats::rbinom(n, size = 1, prob = 0.7),
    media  = 0.7,
    dp     = sqrt(0.7 * 0.3)
  ),
  dado = list(
    rotulo = tr("Dado de seis faces (média das faces)", "Six-sided die (mean of the faces)"),
    gerar  = \(n) sample(1:6, n, replace = TRUE),
    media  = 3.5,
    dp     = sqrt(35 / 12)
  ),
  exponencial = list(
    rotulo = tr("Tempo de espera exponencial (média = 1)", "Exponential waiting time (mean = 1)"),
    gerar  = \(n) stats::rexp(n, rate = 1),
    media  = 1,
    dp     = 1
  )
)

# Pontos usados no gráfico (~600 por trajetória, para não pesar na renderização)
pontos_grafico <- function(n_max, escala_log) {
  pontos <- if (escala_log) exp(seq(0, log(n_max), length.out = 600)) else seq(1, n_max, length.out = 600)
  unique(round(pontos))
}

simular_trajetoria <- function(id_trajetoria, experimento, n_max, pontos) {
  media_acumulada <- cumsum(experimento$gerar(n_max)) / seq_len(n_max)
  data.frame(
    trajetoria = factor(id_trajetoria),
    n          = pontos,
    media      = media_acumulada[pontos]
  )
}

texto_lgn <- function() {
  tr(
    shiny::withMathJax(
      shiny::p(
        "Quando um experimento é repetido sob as mesmas condições, de forma independente e com valor esperado finito",
        "\\(\\mu\\), a média acumulada \\(\\bar{X}_n = \\frac{1}{n}\\sum_{i=1}^{n} X_i\\) aproxima-se de \\(\\mu\\) à",
        "medida que \\(n\\) cresce (Casella e Berger, Teo. 5.5.9 e 5.5.14; Morettin e Bussab, cap. 10). Essa é a",
        "propriedade de consistência que garante que frequências relativas observadas numa amostra aleatória grande",
        "recuperam probabilidades e médias da população."
      ),
      shiny::p(
        "A convergência, no entanto, ocorre por diluição e não por compensação: dez caras seguidas no início não",
        "tornam coroas mais prováveis nos lançamentos seguintes; elas apenas pesam cada vez menos no denominador",
        "\\(n\\). Além disso, o estreitamento em torno de \\(\\mu\\) segue a escala \\(\\sigma / \\sqrt{n}\\),",
        "ilustrada pela faixa \\(\\mu \\pm 1{,}96\\,\\sigma/\\sqrt{n}\\): para reduzir a incerteza pela metade é",
        "preciso multiplicar o tamanho da amostra por quatro."
      ),
      shiny::p(
        shiny::strong("Experimente:"),
        "observe as primeiras 10 ou 20 repetições com eixo logarítmico ativado: trajetórias curtas oscilam longe de",
        "\\(\\mu\\) com frequência. Depois desative a escala logarítmica para ver quão devagar a faixa afunila",
        "depois das primeiras centenas de sorteios."
      )
    ),
    shiny::withMathJax(
      shiny::p(
        "When an experiment is repeated independently under identical conditions with finite expected value",
        "\\(\\mu\\), the running mean \\(\\bar{X}_n = \\frac{1}{n}\\sum_{i=1}^{n} X_i\\) settles toward \\(\\mu\\)",
        "as \\(n\\) grows (Casella and Berger, Thms. 5.5.9 and 5.5.14; Morettin and Bussab, ch. 10). That",
        "consistency property is why sample proportions and averages from large random samples recover population",
        "probabilities and means."
      ),
      shiny::p(
        "Convergence works by dilution rather than compensation, however: ten heads in a row at the start do not",
        "make tails more likely next; they simply carry less weight as the denominator \\(n\\) grows. Moreover,",
        "the band \\(\\mu \\pm 1.96\\,\\sigma/\\sqrt{n}\\) narrows at rate \\(1/\\sqrt{n}\\): halving the margin",
        "of error requires quadrupling the sample size."
      ),
      shiny::p(
        shiny::strong("Try it:"),
        "inspect the first 10 or 20 repetitions on the log axis: short paths stray far from \\(\\mu\\) routinely.",
        "Then untick the logarithmic scale to see how slowly the band narrows past the first few hundred draws."
      )
    )
  )
}

cuidados_lgn <- function() {
  tr(
    c(
      "Falácia do apostador" = "a moeda não tem memória. A média converge porque novos sorteios diluem o desvio inicial, não porque resultados opostos passem a ser \"devidos\".",
      "Amostra viciada não converge para μ" = "se o mecanismo de seleção exclui parte da população, x̄ₙ converge para a média do subgrupo amostrado, perpetuando o viés.",
      "Esperança finita é obrigatória" = "distribuições de cauda extrema como a Cauchy não têm valor esperado; a aba \"Para ir além\" mostra a média acumulada saltar sem nunca estabilizar."
    ),
    c(
      "Gambler's fallacy" = "a fair coin has no memory. The mean converges because fresh draws dilute early swings, not because opposite outcomes become \"due\".",
      "Biased sampling misses μ" = "if selection excludes part of the population, x̄ₙ converges to the sampled subgroup's mean rather than μ, locking in the bias.",
      "Finite expectation is required" = "heavy-tailed distributions such as the Cauchy lack a finite mean; the \"Going further\" tab shows the running average jumping forever."
    )
  )
}

cartao_cuidados_tl <- function(titulo, itens) {
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

mod_lgn_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::selectInput(
        ns("experimento"), tr("Experimento", "Experiment"),
        choices = stats::setNames(names(experimentos_lgn), purrr::map_chr(experimentos_lgn, "rotulo"))
      ),
      shiny::sliderInput(ns("n_max"), tr("Número de repetições", "Number of repetitions"), min = 100, max = 10000, value = 2000, step = 100, sep = sep_milhar()),
      shiny::sliderInput(ns("trajetorias"), tr("Número de trajetórias", "Number of paths"), min = 1, max = 20, value = 5, step = 1),
      shiny::checkboxInput(ns("escala_log"), tr("Eixo x em escala logarítmica", "Logarithmic x axis"), value = TRUE),
      shiny::checkboxInput(ns("faixa"), tr("Mostrar faixa pontual de 95%", "Show pointwise 95% band"), value = TRUE),
      shiny::actionButton(ns("sortear"), tr("Sortear novamente", "Draw again"), class = "btn-primary", icon = shiny::icon("dice"))
    ),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(
        title = tr("Valor esperado (μ)", "Expected value (μ)"),
        value = shiny::textOutput(ns("mu")),
        theme = "primary"
      ),
      bslib::value_box(
        title = tr("Médias ao final das trajetórias", "Means at the end of the paths"),
        value = shiny::textOutput(ns("finais")),
        shiny::textOutput(ns("finais_desc")),
        theme = "info"
      )
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_body(shiny::plotOutput(ns("grafico"), height = "440px"))
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
      bslib::card(
        bslib::card_header(rotulo_entenda()),
        bslib::card_body(texto_lgn())
      ),
      cartao_cuidados_tl(tr("Cuidados", "Watch out"), cuidados_lgn())
    )
  )
}

mod_lgn_server <- function(id, modo = shiny::reactive("light")) {
  shiny::moduleServer(id, function(input, output, session) {

    simulacao <- shiny::reactive({
      input$sortear
      experimento <- experimentos_lgn[[input$experimento]]
      pontos <- pontos_grafico(input$n_max, input$escala_log)

      trajetorias <- seq_len(input$trajetorias) |>
        purrr::map(\(j) simular_trajetoria(j, experimento, input$n_max, pontos)) |>
        purrr::list_rbind()

      list(experimento = experimento, pontos = pontos, trajetorias = trajetorias, n_max = input$n_max)
    })

    medias_finais <- shiny::reactive({
      s <- simulacao()
      s$trajetorias$media[s$trajetorias$n == max(s$pontos)]
    })

    output$mu <- shiny::renderText(fmt_num(simulacao()$experimento$media, 3))

    output$finais <- shiny::renderText({
      f <- medias_finais()
      if (length(f) == 1) return(fmt_num(f, 3))
      sprintf(tr("%s a %s", "%s to %s"), fmt_num(min(f), 3), fmt_num(max(f), 3))
    })

    output$finais_desc <- shiny::renderText({
      desvio <- max(abs(medias_finais() - simulacao()$experimento$media))
      sprintf(
        tr("Maior distância de μ após %s repetições: %s", "Largest distance from μ after %s repetitions: %s"),
        fmt_num(simulacao()$n_max, 0), fmt_num(desvio, 3)
      )
    })

    output$grafico <- shiny::renderPlot({
      s <- simulacao()
      mu <- s$experimento$media
      pal <- paleta_modo(modo())
      faixa <- data.frame(
        n   = s$pontos,
        inf = mu - 1.96 * s$experimento$dp / sqrt(s$pontos),
        sup = mu + 1.96 * s$experimento$dp / sqrt(s$pontos)
      )
      rotulos <- rotulos_eixo()

      grafico <- ggplot2::ggplot()

      if (isTRUE(input$faixa)) {
        grafico <- grafico +
          ggplot2::geom_ribbon(data = faixa, ggplot2::aes(x = n, ymin = inf, ymax = sup), fill = cores_app$destaque, alpha = 0.18)
      }

      grafico <- grafico +
        ggplot2::geom_hline(yintercept = mu, linetype = "dashed", linewidth = 0.9, colour = pal$linha) +
        ggplot2::geom_line(
          data = s$trajetorias,
          ggplot2::aes(x = n, y = media, group = trajetoria, colour = trajetoria),
          linewidth = 0.7, alpha = 0.85
        ) +
        ggplot2::scale_colour_viridis_d(option = "mako", begin = pal$trajetorias[1], end = pal$trajetorias[2]) +
        ggplot2::scale_y_continuous(labels = rotulos) +
        ggplot2::labs(
          title    = tr("Média acumulada conforme as repetições aumentam", "Running mean as repetitions increase"),
          subtitle = s$experimento$rotulo,
          x = tr("Número de repetições (n)", "Number of repetitions (n)"), y = tr("Média acumulada", "Running mean"),
          caption  = tr("Tracejado: valor esperado μ | Faixa: μ ± 1,96 σ/√n", "Dashed: expected value μ | Band: μ ± 1.96 σ/√n")
        ) +
        tema_grafico(modo())

      if (isTRUE(input$escala_log)) {
        grafico + ggplot2::scale_x_log10(labels = rotulos)
      } else {
        grafico + ggplot2::scale_x_continuous(labels = rotulos)
      }
    })
  })
}
