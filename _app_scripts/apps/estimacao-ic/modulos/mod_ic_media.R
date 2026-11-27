# =============================================================================
# Script:  mod_ic_media.R (Aba 2: IC para a média e cobertura frequentista)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Tamanho amostral n, confiança γ e método (Z com σ conhecido vs. t com S)
# Saída:
#   - 100 intervalos de confiança simultâneos e taxa de cobertura observada
# =============================================================================

mod_ic_media_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::sliderInput(ns("n_ic"), tr("Tamanho da amostra n", "Sample size n"), min = 5, max = 60, value = 15, step = 5),
      shiny::sliderInput(ns("gama_ic"), tr("Confiança γ", "Confidence γ"), min = 0.80, max = 0.99, value = 0.95, step = 0.01),
      shiny::radioButtons(
        ns("metodo_ic"),
        tr("Desvio padrão usado", "Standard deviation used"),
        choices = stats::setNames(
          c("t", "z"),
          c(tr("S desconhecido (t de Student)", "Unknown σ, using s (Student's t)"),
            tr("σ conhecido (Normal Z)", "Known σ (Normal Z)"))
        )
      ),
      shiny::actionButton(ns("sortear"), tr("Gerar 100 novos intervalos", "Draw 100 new intervals"), class = "btn-sm btn-primary mt-2")
    ),
    bslib::layout_columns(
      col_widths = c(8, 4),
      bslib::card(
        bslib::card_header(tr("100 intervalos de confiança para μ = 50", "100 confidence intervals for μ = 50")),
        bslib::card_body(shiny::plotOutput(ns("grafico_ics"), height = "420px"))
      ),
      bslib::card(
        bslib::card_header(tr("Interpretação frequentista", "Frequentist interpretation")),
        bslib::card_body(
          shiny::uiOutput(ns("resumo_ic")),
          shiny::hr(),
          shiny::p(class = "mb-0", tr(
            "Em γ% das amostras repetidas, o intervalo aleatório [L, U] captura o parâmetro fixo μ. Um intervalo já observado contém ou não contém μ — a confiança está no procedimento.",
            "Across γ% of repeated samples, the random interval [L, U] captures the fixed parameter μ. A single observed interval either contains μ or does not — confidence lies in the procedure."
          ))
        )
      )
    )
  )
}

mod_ic_media_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    ics <- shiny::reactive({
      input$sortear
      shiny::req(is.finite(input$n_ic), is.finite(input$gama_ic))
      simular_cobertura_media(input$n_ic, input$gama_ic, sigma = 5, usar_t = identical(input$metodo_ic, "t"), n_ics = 100L)
    })

    output$grafico_ics <- shiny::renderPlot({
      df <- ics()
      p <- paleta_modo(modo())
      cob <- mean(df$cobre)
      ggplot2::ggplot(df, ggplot2::aes(y = id, x = xbar, xmin = inf, xmax = sup, colour = cobre)) +
        ggplot2::geom_errorbarh(height = 0, linewidth = 0.7) +
        ggplot2::geom_point(size = 1.2) +
        ggplot2::geom_vline(xintercept = 50, colour = p$linha, linetype = "dashed", linewidth = 0.9) +
        ggplot2::scale_colour_manual(
          values = c("TRUE" = cores_app$destaque, "FALSE" = cores_app$referencia),
          labels = c("TRUE" = tr("Contém μ", "Covers μ"), "FALSE" = tr("Erra μ", "Misses μ"))
        ) +
        ggplot2::labs(
          title = sprintf(tr("Cobertura observada: %s (nominal: %s)", "Observed coverage: %s (nominal: %s)"), fmt_pct(cob, 0), fmt_pct(input$gama_ic, 0)),
          x = tr("Intervalo para μ", "Interval for μ"),
          y = tr("Amostra (1 a 100)", "Sample (1 to 100)"),
          colour = NULL
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(legend.position = "top")
    })

    output$resumo_ic <- shiny::renderUI({
      df <- ics()
      larg <- mean(df$sup - df$inf)
      n_ok <- sum(df$cobre)
      shiny::tagList(
        shiny::p(sprintf(tr("Intervalos que contêm μ = 50: %d de 100 (%s).", "Intervals covering μ = 50: %d out of 100 (%s)."), n_ok, fmt_pct(n_ok / 100, 0))),
        shiny::p(class = "mb-0", sprintf(tr("Largura média dos intervalos: %s.", "Average interval width: %s."), fmt_num(larg, 3)))
      )
    })
  })
}
