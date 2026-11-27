# =============================================================================
# Script:  mod_ks_corr.R (Aba 3: Kolmogorov-Smirnov e correlação ρ = 0)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Distribuição geradora para K-S e correlação verdadeira ρ para cor.test()
# Saída:
#   - Gráfico Fₑ(x) vs. F₀(x) com distância máxima D e resumo de teste t para ρ
# =============================================================================

mod_ks_corr_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_columns(
    col_widths = c(7, 5),
    bslib::card(
      bslib::card_header(tr("Teste de aderência de Kolmogorov-Smirnov contra N(0, 1)", "Kolmogorov-Smirnov goodness-of-fit test against N(0, 1)")),
      bslib::card_body(
        bslib::layout_columns(
          col_widths = c(5, 4, 3),
          shiny::selectInput(
            ns("dist_ks"),
            tr("População real", "True population"),
            choices = stats::setNames(c("normal", "assim", "unif"), c("Normal N(0, 1)", tr("Assimétrica (Exp − 1)", "Skewed (Exp − 1)"), tr("Uniforme (mesma var.)", "Uniform (same var.)")))
          ),
          shiny::sliderInput(ns("n_ks"), "n", min = 15, max = 120, value = 40, step = 15),
          shiny::actionButton(ns("sim_ks"), tr("Sortear", "Draw"), class = "btn-sm btn-primary mt-4")
        ),
        shiny::plotOutput(ns("grafico_ks"), height = "300px")
      )
    ),
    bslib::card(
      bslib::card_header(tr("Teste t para correlação linear (H₀: ρ = 0)", "t test for linear correlation (H₀: ρ = 0)")),
      bslib::card_body(
        shiny::sliderInput(ns("rho_c"), tr("Correlação verdadeira ρ", "True correlation ρ"), min = 0.0, max = 0.8, value = 0.35, step = 0.05),
        shiny::sliderInput(ns("n_c"), tr("Tamanho da amostra n", "Sample size n"), min = 10, max = 100, value = 30, step = 5),
        shiny::actionButton(ns("sim_c"), tr("Sortear amostra", "Draw sample"), class = "btn-sm btn-primary mb-3"),
        shiny::uiOutput(ns("tabela_corr"))
      )
    )
  )
}

mod_ks_corr_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    ks_res <- shiny::reactive({
      input$sim_ks
      shiny::req(input$dist_ks, is.finite(input$n_ks))
      simular_ks_dados(input$n_ks, input$dist_ks)
    })

    output$grafico_ks <- shiny::renderPlot({
      r <- ks_res()
      p <- paleta_modo(modo())
      ggplot2::ggplot(r$df, ggplot2::aes(x = x)) +
        ggplot2::geom_step(ggplot2::aes(y = fe), colour = cores_app$destaque, linewidth = 1.0) +
        ggplot2::geom_line(ggplot2::aes(y = f0), colour = p$secundario, linetype = "dashed", linewidth = 0.9) +
        ggplot2::geom_segment(
          ggplot2::aes(x = r$x_max, xend = r$x_max, y = r$f0_max, yend = r$fe_max),
          colour = cores_app$referencia, linewidth = 1.2
        ) +
        ggplot2::scale_y_continuous(limits = c(0, 1.02), labels = rotulos_eixo()) +
        ggplot2::labs(
          title = sprintf("D = sup |Fₑ(x) − F₀(x)| = %s | valor-p = %s", fmt_num(r$d_stat, 3), fmt_num(r$valor_p, 4)),
          x = "x", y = "F(x)",
          caption = tr("Escada azul: Fₑ(x) | Tracejada: N(0, 1) | Segmento vermelho: D", "Blue step: Fₑ(x) | Dashed: N(0, 1) | Red segment: D")
        ) +
        tema_grafico(modo())
    })

    corr_res <- shiny::reactive({
      input$sim_c
      shiny::req(is.finite(input$n_c), is.finite(input$rho_c))
      simular_corr_teste(input$n_c, input$rho_c)
    })

    output$tabela_corr <- shiny::renderUI({
      r <- corr_res()
      df <- data.frame(
        Medida = c("r", "T = r√(n−2)/√(1−r²)", tr("gl (n − 2)", "df (n − 2)"), tr("Valor-p", "p-value")),
        Valor = c(fmt_num(r$r, 3), fmt_num(r$t_stat, 3), as.character(r$gl), fmt_num(r$valor_p, 4)),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Quantidade", "Quantity"), tr("Valor", "Value"))
      tabela_html(df, c("", "num"))
    })
  })
}
