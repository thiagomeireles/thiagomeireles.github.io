# =============================================================================
# Script:  mod_erros_poder.R (Aba 2: Erros Tipo I, Tipo II e Poder)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Média sob H1 (μ₁), tamanho amostral n e nível de significância α
# Saída:
#   - Curvas sobrepostas de H0 e H1 destacando α, β e Poder (1 − β)
# =============================================================================

mod_erros_poder_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::sliderInput(ns("mu1"), tr("Média verdadeira sob H₁ (μ₁, com μ₀ = 50)", "True mean under H₁ (μ₁, with μ₀ = 50)"), min = 50.5, max = 56.0, value = 52.5, step = 0.5),
      shiny::sliderInput(ns("n_ep"), tr("Tamanho da amostra n", "Sample size n"), min = 10, max = 100, value = 25, step = 5),
      shiny::sliderInput(ns("alpha_ep"), tr("Significância α (erro tipo I)", "Significance α (type I error)"), min = 0.01, max = 0.15, value = 0.05, step = 0.01),
      shiny::radioButtons(
        ns("alt_ep"),
        tr("Tipo de teste", "Test type"),
        choices = stats::setNames(c("maior", "bilateral"), c(tr("Unilateral (μ > 50)", "One-sided (μ > 50)"), tr("Bilateral (μ ≠ 50)", "Two-sided (μ ≠ 50)"))),
        inline = TRUE
      )
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header(tr("Distribuições de X̄ sob H₀ e sob H₁", "Distributions of X̄ under H₀ and H₁")),
        bslib::card_body(shiny::plotOutput(ns("grafico_ep"), height = "340px"))
      ),
      bslib::card(
        bslib::card_header(tr("Trade-off entre α, β e Poder (1 − β)", "Trade-off between α, β and Power (1 − β)")),
        bslib::card_body(
          shiny::uiOutput(ns("tabela_ep")),
          shiny::hr(),
          shiny::p(class = "mb-0", tr(
            "Para n fixo, reduzir α diminui falsos positivos, mas empurra a linha crítica para a direita e aumenta β (reduzindo o poder). A única forma de reduzir α e β ao mesmo tempo é aumentar n.",
            "For fixed n, lowering α cuts false positives, but pushes the critical line right and raises β (lowering power). The only way to shrink both α and β simultaneously is to increase n."
          ))
        )
      )
    )
  )
}

mod_erros_poder_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    res <- shiny::reactive({
      shiny::req(is.finite(input$mu1), is.finite(input$n_ep), is.finite(input$alpha_ep), input$alt_ep)
      calcular_erros_poder(50, input$mu1, 6, input$n_ep, input$alpha_ep, input$alt_ep)
    })

    output$grafico_ep <- shiny::renderPlot({
      r <- res()
      p <- paleta_modo(modo())
      xs <- seq(50 - 3.5 * r$ep, input$mu1 + 3.5 * r$ep, length.out = 400L)
      df <- data.frame(
        x = xs,
        h0 = stats::dnorm(xs, mean = 50, sd = r$ep),
        h1 = stats::dnorm(xs, mean = input$mu1, sd = r$ep)
      )

      df_alpha <- df[df$x >= r$xc_sup, , drop = FALSE]
      df_beta <- df[df$x <= r$xc_sup & df$x >= r$xc_inf, , drop = FALSE]

      ggplot2::ggplot(df, ggplot2::aes(x = x)) +
        ggplot2::geom_area(data = df_beta, ggplot2::aes(y = h1), fill = cores_app$destaque, alpha = 0.25) +
        ggplot2::geom_area(data = df_alpha, ggplot2::aes(y = h0), fill = cores_app$referencia, alpha = 0.35) +
        ggplot2::geom_line(ggplot2::aes(y = h0), colour = p$secundario, linewidth = 0.9, linetype = "dashed") +
        ggplot2::geom_line(ggplot2::aes(y = h1), colour = cores_app$destaque, linewidth = 1.0) +
        ggplot2::geom_vline(xintercept = r$xc_sup, colour = cores_app$referencia, linewidth = 0.9) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = sprintf(tr("Poder = %s | β = %s", "Power = %s | β = %s"), fmt_pct(r$poder, 1), fmt_pct(r$beta, 1)),
          x = "X̄", y = tr("Densidade", "Density"),
          caption = tr("Tracejada: H₀ | Sólida azul: H₁ | Vermelho: α | Azul sombreado: β", "Dashed: H₀ | Solid blue: H₁ | Red: α | Shaded blue: β")
        ) +
        tema_grafico(modo())
    })

    output$tabela_ep <- shiny::renderUI({
      r <- res()
      df <- data.frame(
        Medida = c(
          tr("Erro tipo I (α)", "Type I error (α)"),
          tr("Erro tipo II (β)", "Type II error (β)"),
          tr("Poder (1 − β)", "Power (1 − β)"),
          tr("Corte crítico superior", "Upper critical cutoff")
        ),
        Valor = c(fmt_pct(r$alpha, 1), fmt_pct(r$beta, 1), fmt_pct(r$poder, 1), fmt_num(r$xc_sup, 3)),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Quantidade", "Quantity"), tr("Valor", "Value"))
      tabela_html(df, c("", "num"))
    })
  })
}
