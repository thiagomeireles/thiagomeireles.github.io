# =============================================================================
# Script:  mod_lgn.R (módulo: Lei dos Grandes Números)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-18
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

# Pontos usados no gráfico (~600 por trajetória, para não pesar no navegador)
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
        "Repita um experimento várias vezes e acompanhe a média acumulada dos resultados.",
        "A Lei dos Grandes Números diz que essa média se aproxima do valor esperado \\(\\mu\\) à medida que",
        "o número de repetições \\(n\\) cresce. Cada linha do gráfico é uma sequência independente de repetições."
      ),
      shiny::p(
        "A faixa sombreada mostra \\(\\mu \\pm 1{,}96\\,\\sigma / \\sqrt{n}\\). Para \\(n\\) não muito pequeno,",
        "cerca de 95% das trajetórias passam dentro dela em cada ponto. A faixa estreita devagar: para cortar a",
        "largura pela metade, é preciso quadruplicar \\(n\\)."
      ),
      shiny::p(
        "LGN e TCL se complementam. A LGN diz para onde a média vai (\\(\\mu\\)). O TCL descreve a forma",
        "da variação da média em torno de \\(\\mu\\)."
      ),
      shiny::p(
        shiny::strong("Experimente:"),
        "olhe as primeiras 10 ou 20 jogadas da moeda. Sequências curtas se afastam bastante de 0,5, e isso",
        "é esperado. A lei não promete que os resultados \"se compensam\" no curto prazo."
      )
    ),
    shiny::withMathJax(
      shiny::p(
        "Repeat an experiment many times and follow the running mean of the results.",
        "The Law of Large Numbers says this mean approaches the expected value \\(\\mu\\) as the",
        "number of repetitions \\(n\\) grows. Each line in the chart is an independent sequence of repetitions."
      ),
      shiny::p(
        "The shaded band shows \\(\\mu \\pm 1.96\\,\\sigma / \\sqrt{n}\\). For \\(n\\) that is not too small,",
        "about 95% of the paths are inside it at each point. The band narrows slowly: to halve its width,",
        "you need to quadruple \\(n\\)."
      ),
      shiny::p(
        "The LLN and the CLT complement each other. The LLN says where the mean is heading (\\(\\mu\\)). The CLT",
        "describes the shape of the mean's variation around \\(\\mu\\)."
      ),
      shiny::p(
        shiny::strong("Try it:"),
        "look at the first 10 or 20 coin tosses. Short sequences stray quite far from 0.5, and that is",
        "expected. The law does not promise that results \"even out\" in the short run."
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
      shiny::checkboxInput(ns("faixa"), tr("Mostrar faixa de 95%", "Show 95% band"), value = TRUE),
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
      bslib::card_body(saida_grafico(ns("grafico"), height = "440px"))
    ),
    bslib::card(
      bslib::card_header(rotulo_entenda()),
      bslib::card_body(texto_lgn())
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

    output$grafico <- render_grafico({
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
