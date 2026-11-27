# =============================================================================
# Script:  mod_poisson.R (Aba 3: Poisson e aproximação binomial)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Controles de n, p (λ = np) e horizonte t do processo de Poisson
# Saída:
#   - Gráfico comparando Binomial(n, p) e Poisson(λ = np) e tabela de erro máximo
# =============================================================================

mod_poisson_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::sliderInput(ns("n_pois"), tr("Número de ensaios n", "Number of trials n"), min = 10, max = 200, value = 80, step = 5),
      shiny::sliderInput(ns("p_pois"), tr("Probabilidade p", "Probability p"), min = 0.005, max = 0.400, value = 0.030, step = 0.005),
      shiny::hr(),
      shiny::sliderInput(ns("t_proc"), tr("Horizonte t do processo Po(λ·t)", "Horizon t of Poisson process Po(λ·t)"), min = 1, max = 10, value = 3, step = 1)
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header(tr("Aproximação Binomial(n, p) → Poisson(λ = n·p)", "Binomial(n, p) → Poisson(λ = n·p) approximation")),
        bslib::card_body(shiny::plotOutput(ns("grafico_pois"), height = "350px"))
      ),
      bslib::card(
        bslib::card_header(tr("Comparação e processo de Poisson", "Comparison and Poisson process")),
        bslib::card_body(
          shiny::uiOutput(ns("tabela_pois")),
          shiny::hr(),
          shiny::uiOutput(ns("texto_processo"))
        )
      )
    )
  )
}

mod_poisson_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    res <- shiny::reactive({
      shiny::req(is.finite(input$n_pois), is.finite(input$p_pois))
      dist_poisson_vs_binomial(input$n_pois, input$p_pois)
    })

    output$grafico_pois <- shiny::renderPlot({
      r <- res()
      df <- r$df
      p <- paleta_modo(modo())

      ggplot2::ggplot(df, ggplot2::aes(x = k)) +
        ggplot2::geom_col(ggplot2::aes(y = binomial), fill = p$neutro, width = 0.65) +
        ggplot2::geom_line(ggplot2::aes(y = poisson), colour = cores_app$referencia, linewidth = 0.9) +
        ggplot2::geom_point(ggplot2::aes(y = poisson), colour = cores_app$referencia, size = 2.5) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = sprintf("Binomial(%d, %s) vs. Poisson(λ = %s)", input$n_pois, fmt_num(input$p_pois, 3), fmt_num(r$lambda, 2)),
          subtitle = sprintf(tr("Erro máximo |P_Bin − P_Pois| = %s", "Max error |P_Bin − P_Pois| = %s"), fmt_num(r$erro_max, 5)),
          x = "k", y = "P(X = k)",
          caption = tr("Barras: Binomial | Linha/pontos vermelhos: Poisson", "Bars: Binomial | Red line/dots: Poisson")
        ) +
        tema_grafico(modo())
    })

    output$tabela_pois <- shiny::renderUI({
      r <- res()
      df <- data.frame(
        Medida = c("λ = n·p", "Var(Binomial) = n·p·(1−p)", "Var(Poisson) = λ", tr("Diferença máxima |Δ|", "Max difference |Δ|")),
        Valor = c(fmt_num(r$lambda, 3), fmt_num(r$var_bin, 4), fmt_num(r$var_pois, 4), fmt_num(r$erro_max, 5)),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Quantidade", "Quantity"), tr("Valor", "Value"))
      tabela_html(df, c("", "num"))
    })

    output$texto_processo <- shiny::renderUI({
      shiny::req(is.finite(input$t_proc))
      r <- res()
      lam_t <- r$lambda * input$t_proc
      shiny::p(class = "mb-0", sprintf(
        tr(
          "Num processo de Poisson com taxa λ = %s eventos por unidade, em t = %d unidades a contagem segue Po(λ·t = %s), com média e variância iguais a %s e P(X = 0) = %s.",
          "In a Poisson process with rate λ = %s events per unit, over t = %d units the count follows Po(λ·t = %s), with mean and variance equal to %s and P(X = 0) = %s."
        ),
        fmt_num(r$lambda, 2), input$t_proc, fmt_num(lam_t, 2), fmt_num(lam_t, 2), fmt_pct(stats::dpois(0, lam_t), 2)
      ))
    })
  })
}
