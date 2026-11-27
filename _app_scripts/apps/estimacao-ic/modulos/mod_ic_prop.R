# =============================================================================
# Script:  mod_ic_prop.R (Aba 3: IC para a proporção — Wald vs. Wilson)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Tamanho amostral n, proporção verdadeira p e confiança γ
# Saída:
#   - Comparação de cobertura e largura entre Wald e Wilson/Score
# =============================================================================

mod_ic_prop_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::sliderInput(ns("n_prop"), tr("Tamanho da amostra n", "Sample size n"), min = 10, max = 200, value = 25, step = 5),
      shiny::sliderInput(ns("p_prop"), tr("Proporção verdadeira p", "True proportion p"), min = 0.02, max = 0.98, value = 0.08, step = 0.02),
      shiny::sliderInput(ns("gama_prop"), tr("Confiança γ", "Confidence γ"), min = 0.80, max = 0.99, value = 0.95, step = 0.01),
      shiny::actionButton(ns("simular"), tr("Simular 400 amostras", "Simulate 400 samples"), class = "btn-sm btn-primary mt-2")
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header(tr("Cobertura real: Wald clássico vs. Wilson (Score)", "Actual coverage: classic Wald vs. Wilson (Score)")),
        bslib::card_body(shiny::plotOutput(ns("grafico_prop"), height = "340px"))
      ),
      bslib::card(
        bslib::card_header(tr("Quando o intervalo de Wald falha", "When the Wald interval fails")),
        bslib::card_body(
          shiny::uiOutput(ns("tabela_prop")),
          shiny::hr(),
          shiny::p(class = "mb-0", tr(
            "Quando n·p ou n·(1 − p) é pequeno, o intervalo de Wald p̂ ± z·√(p̂(1−p̂)/n) fica estreito demais e cobre bem menos que γ%. O intervalo de Wilson (base de stats::prop.test) corrige a cobertura.",
            "When n·p or n·(1 − p) is small, the Wald interval p̂ ± z·√(p̂(1−p̂)/n) is too narrow and covers far less than γ%. The Wilson interval (used in stats::prop.test) restores coverage."
          ))
        )
      )
    )
  )
}

mod_ic_prop_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    sim <- shiny::reactive({
      input$simular
      shiny::req(is.finite(input$n_prop), is.finite(input$p_prop), is.finite(input$gama_prop))
      simular_cobertura_prop(input$n_prop, input$p_prop, input$gama_prop, 400L)
    })

    output$grafico_prop <- shiny::renderPlot({
      s <- sim()
      p <- paleta_modo(modo())
      df <- data.frame(
        metodo = factor(c("Wald", "Wilson"), levels = c("Wald", "Wilson")),
        cobertura = c(s$cob_wald, s$cob_wilson),
        stringsAsFactors = FALSE
      )

      ggplot2::ggplot(df, ggplot2::aes(x = metodo, y = cobertura, fill = metodo)) +
        ggplot2::geom_col(width = 0.52) +
        ggplot2::geom_hline(yintercept = input$gama_prop, colour = cores_app$referencia, linetype = "dashed", linewidth = 0.9) +
        ggplot2::geom_text(ggplot2::aes(label = fmt_pct(cobertura, 1)), vjust = -0.4, colour = p$texto, fontface = "bold") +
        ggplot2::scale_fill_manual(values = c(p$neutro, cores_app$destaque)) +
        ggplot2::scale_y_continuous(limits = c(0, 1.08), labels = rotulos_eixo()) +
        ggplot2::labs(
          title = sprintf(tr("Cobertura empírica em 400 amostras (n = %d, p = %s)", "Empirical coverage in 400 samples (n = %d, p = %s)"), input$n_prop, fmt_num(input$p_prop, 2)),
          subtitle = sprintf(tr("Linha tracejada: nível nominal γ = %s", "Dashed line: nominal level γ = %s"), fmt_pct(input$gama_prop, 0)),
          x = NULL, y = tr("Cobertura", "Coverage")
        ) +
        tema_grafico(modo())
    })

    output$tabela_prop <- shiny::renderUI({
      s <- sim()
      df <- data.frame(
        Metodo = c("Wald", "Wilson"),
        Cobertura = c(fmt_pct(s$cob_wald, 1), fmt_pct(s$cob_wilson, 1)),
        Largura = c(fmt_num(s$larg_wald, 4), fmt_num(s$larg_wilson, 4)),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Método", "Method"), tr("Cobertura", "Coverage"), tr("Largura média", "Mean width"))
      tabela_html(df, c("", "num", "num"))
    })
  })
}
