# =============================================================================
# Script:  mod_discreta.R (módulo: variável quantitativa discreta)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-18
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/apps/variaveis/app.R)
# Saída:
#   - funções mod_discreta_ui() e mod_discreta_server()
# =============================================================================

texto_discreta <- function() {
  tr(
    shiny::tagList(
      shiny::p(
        "Numa variável discreta, os valores possíveis são separados entre si. O caso mais comum é a contagem:",
        "0, 1, 2 filhos, nunca 1,5. Como os valores são quantidades de verdade, as contas fazem sentido e todas",
        "as estatísticas descritivas se aplicam: medidas de posição (média, mediana, moda, quartis) e de",
        "dispersão (amplitude, distância interquartil, variância, desvio padrão)."
      ),
      shiny::p(
        "A média pode dar um valor impossível, como 1,6 filho, e isso não é erro, uma vez que a média resume o",
        "conjunto e não descreve nenhuma observação. Em outras palavras, 100 famílias como essas têm, juntas,",
        "cerca de 160 filhos."
      ),
      shiny::p(
        shiny::strong("Experimente:"),
        "marque \"Incluir um valor extremo\". Uma única observação muda a média, o desvio padrão e a amplitude,",
        "enquanto a mediana, a moda e a distância interquartil quase não se mexem. Diminua o tamanho da amostra",
        "e veja o efeito ficar maior."
      )
    ),
    shiny::tagList(
      shiny::p(
        "In a discrete variable, the possible values are separate from each other. The most common case is a",
        "count: 0, 1, 2 children, never 1.5. Because the values are actual quantities, arithmetic makes sense and",
        "every descriptive statistic applies: measures of center and position (mean, median, mode, quartiles) and",
        "of variability (range, interquartile range, variance, standard deviation)."
      ),
      shiny::p(
        "The mean can be an impossible value, such as 1.6 children, and that is not a mistake, since the mean",
        "summarizes the group and does not describe any single observation. In other words, 100 families like",
        "these have about 160 children in total."
      ),
      shiny::p(
        shiny::strong("Try it:"),
        "check \"Add an extreme value\". A single observation moves the mean, the standard deviation and the range,",
        "while the median, the mode and the interquartile range barely change. Reduce the sample size and watch",
        "the effect grow."
      )
    )
  )
}

cuidados_discreta <- function() {
  tr(
    c(
      "Valores extremos" = "puxam a média e inflam o desvio padrão. Quando houver, informe também a mediana e a distância interquartil.",
      "Média com casas decimais" = "é normal numa contagem. Não arredonde para inteiro só porque os dados são inteiros.",
      "Histograma com classes largas" = "pode esconder que só existem valores inteiros. Com poucos valores distintos, use barras por valor.",
      "Muitos valores distintos" = "(como número de seguidores) fazem a discreta se comportar como contínua. Nesse caso, o histograma volta a ser útil."
    ),
    c(
      "Extreme values" = "pull the mean and inflate the standard deviation. When they occur, also report the median and the interquartile range.",
      "A mean with decimals" = "is normal for a count. Do not round it to a whole number just because the data are whole numbers.",
      "Histograms with wide bins" = "can hide the fact that only whole values exist. With few distinct values, use one bar per value.",
      "Many distinct values" = "(such as follower counts) make a discrete variable behave like a continuous one. In that case, the histogram is useful again."
    )
  )
}

mod_discreta_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      seletor_exemplo(ns("exemplo"), exemplos_discreta),
      slider_amostra(ns("n"), min = 10, max = 2000, value = 100),
      caixa_extremo(ns("extremo")),
      shiny::uiOutput(ns("extremo_valor")),
      botao_sortear(ns("sortear"))
    ),
    cabecalho_tipo("discreta"),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(title = tr("Média", "Mean"), value = shiny::textOutput(ns("media")), shiny::textOutput(ns("media_desc")), theme = "primary"),
      bslib::value_box(title = tr("Mediana", "Median"), value = shiny::textOutput(ns("mediana")), shiny::textOutput(ns("mediana_desc")), theme = "primary"),
      bslib::value_box(title = tr("Moda", "Mode"), value = shiny::textOutput(ns("moda")), tr("Valor mais frequente", "Most frequent value"), theme = "info"),
      bslib::value_box(title = tr("Desvio padrão", "Standard deviation"), value = shiny::textOutput(ns("dp")), shiny::textOutput(ns("dp_desc")), theme = "info")
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico"), height = "420px"))),
      bslib::card(
        bslib::card_header(tr("Tabela de frequências", "Frequency table")),
        max_height = "500px",
        bslib::card_body(shiny::uiOutput(ns("tabela")))
      )
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(bslib::card_header(tr("Estatísticas aplicáveis", "Applicable statistics")), bslib::card_body(shiny::uiOutput(ns("estatisticas")))),
      cartao_cuidados(tr("Cuidados", "Watch out"), cuidados_discreta())
    ),
    bslib::card(bslib::card_header(rotulo_entenda()), bslib::card_body(texto_discreta()))
  )
}

mod_discreta_server <- function(id, modo = shiny::reactive("light")) {
  shiny::moduleServer(id, function(input, output, session) {

    exemplo <- shiny::reactive(exemplos_discreta[[input$exemplo]])

    output$extremo_valor <- shiny::renderUI({
      shiny::req(input$extremo)
      entrada_extremo(session, exemplo()$extremo, step = 1)
    })

    base <- shiny::reactive({
      input$sortear
      exemplo()$gerar(input$n)
    })

    incluir <- shiny::reactive(isTRUE(input$extremo) && !is.null(input$valor_extremo) && !is.na(input$valor_extremo))

    dados <- shiny::reactive({
      if (incluir()) c(base(), round(input$valor_extremo)) else base()
    })

    resumo <- shiny::reactive(resumo_quantitativo(dados()))
    resumo_sem <- shiny::reactive(resumo_quantitativo(base()))
    un <- shiny::reactive(exemplo()$unidade)
    moda <- shiny::reactive(juntar(fmt_num(moda_valores(dados()), 0)))
    digitos_mediana <- \(m) if (m %% 1 == 0) 0 else 1

    output$media <- shiny::renderText(fmt_valor(resumo()$media, un(), 2))
    output$media_desc <- shiny::renderText(descricao_sem_extremo(resumo_sem()$media, un(), 2, incluir()))
    output$mediana <- shiny::renderText(fmt_valor(resumo()$mediana, un(), digitos_mediana(resumo()$mediana)))
    output$mediana_desc <- shiny::renderText({
      descricao_sem_extremo(resumo_sem()$mediana, un(), digitos_mediana(resumo_sem()$mediana), incluir())
    })
    output$moda <- shiny::renderText(moda())
    output$dp <- shiny::renderText(fmt_valor(resumo()$dp, un(), 2))
    output$dp_desc <- shiny::renderText(descricao_sem_extremo(resumo_sem()$dp, un(), 2, incluir()))

    output$tabela <- shiny::renderUI({
      f <- tabela_frequencias(factor(dados(), levels = sort(unique(dados()))))
      tabela <- data.frame(f$categoria, fmt_num(f$n, 0), fmt_pct(f$prop), fmt_pct(f$acum))
      names(tabela) <- nomes_freq(tr("Valor", "Value"))
      tabela_html(tabela, classes = c("num", "num", "num", "num"))
    })

    output$estatisticas <- shiny::renderUI({
      extras <- linha_estatistica(tr("Moda", "Mode"), moda(), tr("Valor mais frequente", "Most frequent value"))
      tabela_estatisticas_quant(resumo(), un(), 2, extras) |>
        tabela_html(classes = c("", "num", "leitura"))
    })

    output$grafico <- shiny::renderPlot({
      pal <- paleta_modo(modo())
      r <- resumo()
      contagem <- table(dados())
      f <- data.frame(valor = as.numeric(names(contagem)), n = as.vector(contagem))
      topo <- max(f$n)

      ggplot2::ggplot(f, ggplot2::aes(x = valor, y = n)) +
        ggplot2::geom_col(width = 0.6, fill = cores_app$destaque, alpha = 0.85) +
        ggplot2::geom_vline(xintercept = r$media, colour = cores_app$referencia, linetype = "dashed", linewidth = 1) +
        ggplot2::geom_vline(xintercept = r$mediana, colour = pal$linha, linewidth = 1) +
        ggplot2::annotate("label", x = r$media, y = topo * 1.08, label = tr("média", "mean"), colour = cores_app$referencia, fill = pal$fundo, size = 4) +
        ggplot2::annotate("label", x = r$mediana, y = topo * 1.18, label = tr("mediana", "median"), colour = pal$linha, fill = pal$fundo, size = 4) +
        ggplot2::scale_x_continuous(breaks = function(l) unique(round(scales::breaks_pretty()(l)))) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
        ggplot2::labs(
          title = tr("Uma barra para cada valor possível", "One bar for each possible value"),
          subtitle = exemplo()$rotulo,
          x = exemplo()$unidade, y = tr("Frequência (n)", "Frequency (n)"),
          caption = sprintf(
            tr("Dados simulados, n = %s | Tracejada: média | Contínua: mediana", "Simulated data, n = %s | Dashed: mean | Solid: median"),
            fmt_num(r$n, 0)
          )
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
    })
  })
}
