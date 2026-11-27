# =============================================================================
# Script:  mod_alem_discretas.R (Para ir além: hipergeométrica e geométrica)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Parâmetros da população finita (N, r, n) e probabilidade geométrica (p)
# Saída:
#   - Comparação Hipergeométrica vs. Binomial e convenções da Geométrica
# =============================================================================

mod_alem_discretas_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_columns(
    col_widths = c(6, 6),
    bslib::card(
      bslib::card_header(tr("Hipergeométrica vs. Binomial (sem vs. com reposição)", "Hypergeometric vs. Binomial (without vs. with replacement)")),
      bslib::card_body(
        bslib::layout_columns(
          col_widths = c(4, 4, 4),
          shiny::sliderInput(ns("pop_n"), tr("População N", "Population N"), min = 15, max = 200, value = 30, step = 5),
          shiny::sliderInput(ns("r_suc"), tr("Sucessos r", "Successes r"), min = 2, max = 30, value = 12, step = 1),
          shiny::sliderInput(ns("n_am"), tr("Amostra n", "Sample n"), min = 2, max = 25, value = 10, step = 1)
        ),
        shiny::plotOutput(ns("grafico_hiper"), height = "270px"),
        shiny::uiOutput(ns("tabela_hiper"))
      )
    ),
    bslib::card(
      bslib::card_header(tr("Geométrica: duas convenções e falta de memória", "Geometric: two conventions and memorylessness")),
      bslib::card_body(
        bslib::layout_columns(
          col_widths = c(6, 6),
          shiny::sliderInput(ns("p_geom"), tr("Probabilidade p", "Probability p"), min = 0.10, max = 0.80, value = 0.30, step = 0.05),
          shiny::sliderInput(ns("s_esp"), tr("Fracassos já vistos s", "Already seen failures s"), min = 1, max = 8, value = 3, step = 1)
        ),
        shiny::plotOutput(ns("grafico_geom"), height = "270px"),
        shiny::uiOutput(ns("resumo_geom"))
      )
    )
  )
}

mod_alem_discretas_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$pop_n, {
      shiny::req(is.finite(input$pop_n))
      shiny::updateSliderInput(session, "r_suc", max = input$pop_n - 1L, value = min(input$r_suc, input$pop_n - 1L))
      shiny::updateSliderInput(session, "n_am", max = min(25L, input$pop_n - 1L), value = min(input$n_am, input$pop_n - 1L))
    })

    hiper <- shiny::reactive({
      shiny::req(is.finite(input$pop_n), is.finite(input$r_suc), is.finite(input$n_am))
      dist_hiper_vs_bin(input$pop_n, min(input$r_suc, input$pop_n), min(input$n_am, input$pop_n))
    })

    output$grafico_hiper <- shiny::renderPlot({
      h <- hiper()
      df <- h$df
      p <- paleta_modo(modo())
      ggplot2::ggplot(df, ggplot2::aes(x = k)) +
        ggplot2::geom_col(ggplot2::aes(y = hiper), fill = cores_app$destaque, width = 0.6) +
        ggplot2::geom_point(ggplot2::aes(y = binomial), colour = cores_app$referencia, size = 2.4) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = tr("Barras: Hipergeométrica | Pontos: Binomial", "Bars: Hypergeometric | Dots: Binomial"),
          x = "k", y = "P(X = k)"
        ) +
        tema_grafico(modo())
    })

    output$tabela_hiper <- shiny::renderUI({
      h <- hiper()
      df <- data.frame(
        Modelo = c(tr("Hipergeométrica (sem reposição)", "Hypergeometric (without replacement)"), tr("Binomial (com reposição)", "Binomial (with replacement)")),
        Media = c(fmt_num(h$media, 3), fmt_num(h$media, 3)),
        Variancia = c(fmt_num(h$var_hiper, 3), fmt_num(h$var_bin, 3)),
        Fator = c(fmt_num(h$fator_fpc, 3), "1.000"),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Modelo", "Model"), "E(X)", "Var(X)", tr("Fator (N−n)/(N−1)", "Factor (N−n)/(N−1)"))
      tabela_html(df, c("", "num", "num", "num"))
    })

    geom_res <- shiny::reactive({
      shiny::req(is.finite(input$p_geom), is.finite(input$s_esp))
      dist_geometrica_comp(input$p_geom, input$s_esp, 2L)
    })

    output$grafico_geom <- shiny::renderPlot({
      g <- geom_res()
      df <- g$df
      ggplot2::ggplot(df, ggplot2::aes(x = valor, y = pmf_ensaios)) +
        ggplot2::geom_col(fill = cores_app$destaque, width = 0.65) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = tr("Ensaios até o 1º sucesso (x = 1, 2, ...)", "Trials until 1st success (x = 1, 2, ...)"),
          subtitle = tr("Em R, stats::dgeom(x - 1, p) conta fracassos (0, 1, ...)", "In R, stats::dgeom(x - 1, p) counts failures (0, 1, ...)"),
          x = "x", y = "P(X = x)"
        ) +
        tema_grafico(modo())
    })

    output$resumo_geom <- shiny::renderUI({
      g <- geom_res()
      shiny::tagList(
        shiny::p(sprintf(
          tr("Média: E(X) = 1/p = %s ensaios (ou (1 − p)/p = %s fracassos no R).", "Mean: E(X) = 1/p = %s trials (or (1 − p)/p = %s failures in R)."),
          fmt_num(g$media_ensaios, 2), fmt_num(g$media_frac, 2)
        )),
        shiny::p(class = "mb-0", sprintf(
          tr("Falta de memória: P(X > %d + 2 | X > %d) = P(X > 2) = %s.", "Memorylessness: P(X > %d + 2 | X > %d) = P(X > 2) = %s."),
          input$s_esp, input$s_esp, fmt_num(g$p_cond, 4)
        ))
      )
    })
  })
}
