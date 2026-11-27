# =============================================================================
# Script:  mod_mqo.R (Aba 1: Mínimos quadrados e R²)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Controles de intercepto e inclinação manuais vs. reta MQO
# Saída:
#   - Dispersão com reta manual, reta MQO, resíduos verticais e SQE comparadas
# =============================================================================

mod_mqo_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::sliderInput(ns("a_man"), tr("Intercepto manual (a)", "Manual intercept (a)"), min = 0, max = 25, value = 12, step = 0.5),
      shiny::sliderInput(ns("b_man"), tr("Inclinação manual (b)", "Manual slope (b)"), min = -1, max = 4, value = 1.2, step = 0.1),
      shiny::checkboxInput(ns("mostrar_mqo"), tr("Mostrar reta de mínimos quadrados (MQO)", "Show OLS regression line"), value = TRUE),
      shiny::actionButton(ns("sortear"), tr("Gerar nova amostra (n = 30)", "Generate new sample (n = 30)"), class = "btn-sm btn-primary mt-2")
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header(tr("Dispersão, resíduos e reta ajustada", "Scatterplot, residuals and fitted line")),
        bslib::card_body(shiny::plotOutput(ns("grafico_mqo"), height = "340px"))
      ),
      bslib::card(
        bslib::card_header(tr("Soma dos quadrados dos resíduos (SQE) e R²", "Residual sum of squares (SSE) and R²")),
        bslib::card_body(
          shiny::uiOutput(ns("tabela_mqo")),
          shiny::hr(),
          shiny::p(class = "mb-0", tr(
            "Os estimadores de mínimos quadrados α̂ e β̂ minimizam exatamente Σ(yᵢ − ŷᵢ)². Qualquer outra reta gera uma SQE maior ou igual à do MQO.",
            "The OLS estimators α̂ and β̂ strictly minimize Σ(yᵢ − ŷᵢ)². Any other line produces an SSE greater than or equal to OLS."
          ))
        )
      )
    )
  )
}

mod_mqo_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    reg <- shiny::reactive({
      input$sortear
      simular_regressao(30L, beta0 = 10, beta1 = 2.0, sigma_e = 4, cenario = "linear")
    })

    output$grafico_mqo <- shiny::renderPlot({
      r <- reg()
      p <- paleta_modo(modo())
      shiny::req(is.finite(input$a_man), is.finite(input$b_man))
      df <- r$df
      df$y_man <- input$a_man + input$b_man * df$x

      g <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_segment(ggplot2::aes(xend = x, yend = y_man), colour = p$neutro, linetype = "dotted") +
        ggplot2::geom_point(colour = cores_app$destaque, size = 2.3) +
        ggplot2::geom_abline(intercept = input$a_man, slope = input$b_man, colour = cores_app$referencia, linewidth = 1.0)

      if (isTRUE(input$mostrar_mqo)) {
        g <- g + ggplot2::geom_abline(intercept = r$alpha_hat, slope = r$beta_hat, colour = cores_app$destaque, linetype = "dashed", linewidth = 1.0)
      }

      g +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = sprintf("MQO: ŷ = %s + %s·x (R² = %s)", fmt_num(r$alpha_hat, 2), fmt_num(r$beta_hat, 2), fmt_num(r$r2, 3)),
          x = "X", y = "Y",
          caption = tr("Vermelha sólida: reta manual | Azul tracejada: MQO", "Solid red: manual line | Dashed blue: OLS")
        ) +
        tema_grafico(modo())
    })

    output$tabela_mqo <- shiny::renderUI({
      r <- reg()
      shiny::req(is.finite(input$a_man), is.finite(input$b_man))
      sqe_man <- sum((r$df$y - (input$a_man + input$b_man * r$df$x))^2)
      df <- data.frame(
        Reta = c(tr("Reta manual (a, b)", "Manual line (a, b)"), tr("Reta MQO (α̂, β̂)", "OLS line (α̂, β̂)")),
        Equacao = c(sprintf("%s + %s·x", fmt_num(input$a_man, 1), fmt_num(input$b_man, 1)), sprintf("%s + %s·x", fmt_num(r$alpha_hat, 2), fmt_num(r$beta_hat, 2))),
        SQE = c(fmt_num(sqe_man, 2), fmt_num(r$sq_res, 2)),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Modelo", "Model"), tr("Equação", "Equation"), tr("SQE", "SSE"))
      tabela_html(df, c("", "", "num"))
    })
  })
}
