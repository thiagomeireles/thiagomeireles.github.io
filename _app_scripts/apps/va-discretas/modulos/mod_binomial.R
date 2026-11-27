# =============================================================================
# Script:  mod_binomial.R (Aba 2: Bernoulli e Binomial b(n, p))
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Parâmetros n, p e intervalo de contagens [k_inf, k_sup]
# Saída:
#   - Gráfico de barras b(n, p) com destaque do intervalo, simulação e tabela
# =============================================================================

mod_binomial_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::sliderInput(ns("n_bin"), tr("Número de ensaios n", "Number of trials n"), min = 1, max = 40, value = 12, step = 1),
      shiny::sliderInput(ns("p_bin"), tr("Probabilidade de sucesso p", "Success probability p"), min = 0.02, max = 0.98, value = 0.35, step = 0.01),
      shiny::sliderInput(ns("faixa_k"), tr("Intervalo k₁ ≤ X ≤ k₂", "Interval k₁ ≤ X ≤ k₂"), min = 0, max = 40, value = c(3, 6), step = 1),
      shiny::actionButton(ns("simular"), tr("Simular 1.000 amostras", "Simulate 1,000 samples"), class = "btn-sm btn-primary mt-2")
    ),
    bslib::layout_columns(
      col_widths = c(8, 4),
      bslib::card(
        bslib::card_header(tr("Distribuição binomial b(n, p)", "Binomial distribution b(n, p)")),
        bslib::card_body(shiny::plotOutput(ns("grafico_bin"), height = "350px"))
      ),
      bslib::card(
        bslib::card_header(tr("Resumo do intervalo e momentos", "Interval summary and moments")),
        bslib::card_body(
          shiny::uiOutput(ns("tabela_bin")),
          shiny::hr(),
          shiny::p(class = "mb-0", tr(
            "Lembrete: para P(X ≥ k) em variáveis discretas, use 1 − P(X ≤ k − 1) (em R: 1 - pbinom(k - 1, n, p)).",
            "Reminder: for P(X ≥ k) in discrete variables, use 1 − P(X ≤ k − 1) (in R: 1 - pbinom(k - 1, n, p))."
          ))
        )
      )
    )
  )
}

mod_binomial_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$n_bin, {
      shiny::req(is.finite(input$n_bin))
      fx <- if (length(input$faixa_k) == 2) input$faixa_k else c(0, input$n_bin)
      novo <- c(min(fx[1], input$n_bin), min(fx[2], input$n_bin))
      shiny::updateSliderInput(session, "faixa_k", max = input$n_bin, value = novo)
    })

    sim_bin <- shiny::reactiveVal(NULL)
    shiny::observeEvent(list(input$n_bin, input$p_bin), sim_bin(NULL))
    shiny::observeEvent(input$simular, {
      shiny::req(is.finite(input$n_bin), is.finite(input$p_bin))
      draws <- stats::rbinom(1000, size = input$n_bin, prob = input$p_bin)
      tab <- table(factor(draws, levels = 0:input$n_bin))
      sim_bin(as.numeric(tab) / 1000)
    })

    res <- shiny::reactive({
      shiny::req(is.finite(input$n_bin), is.finite(input$p_bin), length(input$faixa_k) == 2)
      dist_binomial(input$n_bin, input$p_bin, input$faixa_k[1], input$faixa_k[2])
    })

    output$grafico_bin <- shiny::renderPlot({
      r <- res()
      df <- r$df
      p <- paleta_modo(modo())
      sim <- sim_bin()

      g <- ggplot2::ggplot(df, ggplot2::aes(x = k, y = pmf, fill = destaque)) +
        ggplot2::geom_col(width = 0.7) +
        ggplot2::scale_fill_manual(values = c("FALSE" = p$neutro, "TRUE" = cores_app$destaque))

      if (!is.null(sim)) {
        df_s <- data.frame(k = df$k, sim = sim)
        g <- g + ggplot2::geom_point(
          data = df_s,
          ggplot2::aes(x = k, y = sim),
          inherit.aes = FALSE,
          colour = cores_app$referencia, size = 2.2
        )
      }

      g +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = sprintf("b(n = %d, p = %s)", input$n_bin, fmt_num(input$p_bin, 2)),
          subtitle = sprintf(
            tr("P(%d ≤ X ≤ %d) = %s", "P(%d ≤ X ≤ %d) = %s"),
            r$a, r$b, fmt_pct(r$prob_interv, 2)
          ),
          x = tr("Número de sucessos k", "Number of successes k"),
          y = "P(X = k)",
          caption = if (is.null(sim)) tr("Barras azuis: intervalo selecionado", "Blue bars: selected interval") else tr("Pontos vermelhos: frequência simulada (1.000)", "Red dots: simulated frequency (1,000)")
        ) +
        tema_grafico(modo())
    })

    output$tabela_bin <- shiny::renderUI({
      r <- res()
      df <- data.frame(
        Medida = c(
          sprintf("P(%d ≤ X ≤ %d)", r$a, r$b),
          "E(X) = n·p",
          "Var(X) = n·p·(1 − p)",
          "DP(X)"
        ),
        Valor = c(
          fmt_num(r$prob_interv, 4),
          fmt_num(r$media, 3),
          fmt_num(r$variancia, 3),
          fmt_num(r$dp, 3)
        ),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Quantidade", "Quantity"), tr("Valor", "Value"))
      tabela_html(df, c("", "num"))
    })
  })
}
