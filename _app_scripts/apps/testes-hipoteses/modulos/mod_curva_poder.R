# =============================================================================
# Script:  mod_curva_poder.R (Aba 3: Função poder)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Tamanho amostral base n, α e tipo de teste
# Saída:
#   - Curvas de função poder π(μ) para n/2, n e 2n
# =============================================================================

mod_curva_poder_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::sliderInput(ns("n_pod"), tr("Tamanho amostral base n", "Base sample size n"), min = 10, max = 100, value = 30, step = 10),
      shiny::sliderInput(ns("alpha_pod"), tr("Significância α", "Significance α"), min = 0.01, max = 0.10, value = 0.05, step = 0.01),
      shiny::radioButtons(
        ns("alt_pod"),
        tr("Tipo de teste", "Test type"),
        choices = stats::setNames(c("bilateral", "maior"), c(tr("Bilateral (μ ≠ 50)", "Two-sided (μ ≠ 50)"), tr("Unilateral (μ > 50)", "One-sided (μ > 50)"))),
        inline = TRUE
      )
    ),
    bslib::card(
      bslib::card_header(tr("Função poder π(μ) = P(rejeitar H₀ | μ) para três tamanhos de amostra", "Power function π(μ) = P(reject H₀ | μ) for three sample sizes")),
      bslib::card_body(
        shiny::plotOutput(ns("grafico_curva_poder"), height = "380px"),
        shiny::p(class = "mt-2 mb-0", tr(
          "Em μ = μ₀ = 50, a função poder vale exatamente α. À medida que μ se afasta de μ₀ ou que n cresce, a probabilidade de rejeitar H₀ aproxima-se de 1.",
          "At μ = μ₀ = 50, the power function equals α. As μ moves away from μ₀ or as n grows, the probability of rejecting H₀ approaches 1."
        ))
      )
    )
  )
}

mod_curva_poder_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    output$grafico_curva_poder <- shiny::renderPlot({
      shiny::req(is.finite(input$n_pod), is.finite(input$alpha_pod), input$alt_pod)
      df <- curva_poder_n(50, 6, input$n_pod, input$alpha_pod, input$alt_pod)
      p <- paleta_modo(modo())

      ggplot2::ggplot(df, ggplot2::aes(x = mu, y = poder, colour = n_rot)) +
        ggplot2::geom_line(linewidth = 1.0) +
        ggplot2::geom_hline(yintercept = input$alpha_pod, colour = cores_app$referencia, linetype = "dashed") +
        ggplot2::geom_vline(xintercept = 50, colour = p$secundario, linetype = "dotted") +
        ggplot2::scale_colour_manual(values = c(p$neutro, cores_app$destaque, "#2b8a3e")) +
        ggplot2::scale_y_continuous(limits = c(0, 1.03), labels = rotulos_eixo()) +
        ggplot2::labs(
          title = tr("Curva de poder por distância até μ₀ = 50", "Power curve by distance from μ₀ = 50"),
          x = tr("Valor verdadeiro μ", "True value μ"),
          y = tr("Poder π(μ)", "Power π(μ)"),
          colour = NULL
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(legend.position = "top")
    })
  })
}
