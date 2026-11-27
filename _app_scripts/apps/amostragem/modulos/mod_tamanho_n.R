# =============================================================================
# Script:  mod_tamanho_n.R (Aba 3: Tamanho de amostra)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Parâmetro (média ou proporção), confiança γ, margem ε, σ ou p e N
# Saída:
#   - Cálculo exato de n (arredondado para cima) e curva n × margem de erro ε
# =============================================================================

mod_tamanho_n_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 330,
      shiny::radioButtons(
        ns("tipo_param"),
        tr("Parâmetro-alvo", "Target parameter"),
        choices = stats::setNames(c("media", "proporcao"), c(tr("Média (μ)", "Mean (μ)"), tr("Proporção (p)", "Proportion (p)"))),
        inline = TRUE
      ),
      shiny::sliderInput(ns("gama"), tr("Confiança γ", "Confidence γ"), min = 0.80, max = 0.99, value = 0.95, step = 0.01),
      shiny::sliderInput(ns("eps"), tr("Margem de erro ε", "Margin of error ε"), min = 0.01, max = 2.00, value = 0.50, step = 0.01),
      shiny::sliderInput(ns("sigma_val"), tr("Desvio padrão σ (para média)", "Standard deviation σ (for mean)"), min = 0.5, max = 20, value = 4, step = 0.5),
      shiny::sliderInput(ns("p_val"), tr("Proporção esperada p (para proporção)", "Expected proportion p (for proportion)"), min = 0.05, max = 0.95, value = 0.50, step = 0.05),
      shiny::numericInput(ns("pop_finita"), tr("População N (0 = infinita)", "Population N (0 = infinite)"), value = 0, min = 0, step = 100)
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header(tr("Tamanho de amostra necessário vs. margem de erro ε", "Required sample size vs. margin of error ε")),
        bslib::card_body(shiny::plotOutput(ns("grafico_n"), height = "340px"))
      ),
      bslib::card(
        bslib::card_header(tr("Dimensionamento de n", "Sample size calculation")),
        bslib::card_body(
          shiny::uiOutput(ns("tabela_n")),
          shiny::hr(),
          shiny::p(class = "mb-0", tr(
            "Como ε entra ao quadrado no denominador, cortar a margem de erro pela metade exige quadruplicar n. Arredonde sempre para cima.",
            "Because ε enters squared in the denominator, halving the margin of error requires quadrupling n. Always round up."
          ))
        )
      )
    )
  )
}

mod_tamanho_n_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$tipo_param, {
      if (identical(input$tipo_param, "proporcao")) {
        shiny::updateSliderInput(session, "eps", min = 0.01, max = 0.15, value = 0.03, step = 0.005)
      } else {
        shiny::updateSliderInput(session, "eps", min = 0.10, max = 2.00, value = 0.50, step = 0.05)
      }
    }, ignoreInit = TRUE)

    res <- shiny::reactive({
      shiny::req(is.finite(input$gama), is.finite(input$eps), input$eps > 0, is.finite(input$sigma_val), is.finite(input$p_val))
      pop_n <- if (is.finite(input$pop_finita) && input$pop_finita > 1) input$pop_finita else Inf
      calcular_n_amostra(input$tipo_param, input$gama, input$eps, input$sigma_val, input$p_val, pop_n)
    })

    output$grafico_n <- shiny::renderPlot({
      r <- res()
      p <- paleta_modo(modo())
      eps_seq <- seq(max(0.005, input$eps * 0.4), input$eps * 2.0, length.out = 60L)
      pop_n <- if (is.finite(input$pop_finita) && input$pop_finita > 1) input$pop_finita else Inf
      ns_seq <- purrr::map_dbl(eps_seq, \(e) calcular_n_amostra(input$tipo_param, input$gama, e, input$sigma_val, input$p_val, pop_n)$n_fin)
      df <- data.frame(eps = eps_seq, n = ns_seq)

      ggplot2::ggplot(df, ggplot2::aes(x = eps, y = n)) +
        ggplot2::geom_line(colour = cores_app$destaque, linewidth = 1.1) +
        ggplot2::geom_point(ggplot2::aes(x = input$eps, y = r$n_fin), colour = cores_app$referencia, size = 3.2) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = sprintf(tr("Ponto atual: ε = %s → n = %s", "Current point: ε = %s → n = %s"), fmt_num(input$eps, 3), fmt_num(r$n_fin, 0)),
          x = tr("Margem de erro ε", "Margin of error ε"),
          y = tr("Tamanho da amostra n", "Sample size n")
        ) +
        tema_grafico(modo())
    })

    output$tabela_n <- shiny::renderUI({
      r <- res()
      df <- data.frame(
        Item = c(
          "z_γ",
          tr("n exato (fórmula)", "Exact n (formula)"),
          tr("n (população infinita)", "n (infinite population)"),
          tr("n (com correção finita)", "n (with finite correction)")
        ),
        Valor = c(fmt_num(r$z, 4), fmt_num(r$n0, 2), fmt_num(r$n_inf, 0), fmt_num(r$n_fin, 0)),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Item", "Item"), tr("Valor", "Value"))
      tabela_html(df, c("", "num"))
    })
  })
}
