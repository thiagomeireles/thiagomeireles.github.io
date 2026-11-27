# =============================================================================
# Script:  mod_aas_fpc.R (Aba 1: AAS com vs. sem reposição)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Tamanho populacional N, tamanho da amostra n e botão de novo sorteio
# Saída:
#   - Densidades das médias amostrais com/sem reposição e tabela do fator FPC
# =============================================================================

mod_aas_fpc_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::sliderInput(ns("pop_n"), tr("Tamanho da população N", "Population size N"), min = 30, max = 300, value = 80, step = 10),
      shiny::sliderInput(ns("n_am"), tr("Tamanho da amostra n", "Sample size n"), min = 5, max = 60, value = 25, step = 5),
      shiny::actionButton(ns("sortear"), tr("Sortear 500 amostras", "Draw 500 samples"), class = "btn-sm btn-primary mt-2")
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header(tr("Distribuição de X̄: com vs. sem reposição", "Distribution of X̄: with vs. without replacement")),
        bslib::card_body(shiny::plotOutput(ns("grafico_fpc"), height = "340px"))
      ),
      bslib::card(
        bslib::card_header(tr("Fator de correção para população finita", "Finite population correction factor")),
        bslib::card_body(
          shiny::uiOutput(ns("tabela_fpc")),
          shiny::hr(),
          shiny::p(class = "mb-0", tr(
            "Quando a fração amostral n/N é grande, amostrar sem reposição evita repetir unidades e reduz Var(X̄) pelo fator (N − n)/(N − 1). Se n/N é pequeno, os dois esquemas praticamente coincidem.",
            "When the sampling fraction n/N is large, sampling without replacement avoids repeating units and shrinks Var(X̄) by (N − n)/(N − 1). If n/N is small, both schemes practically coincide."
          ))
        )
      )
    )
  )
}

mod_aas_fpc_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$pop_n, {
      shiny::req(is.finite(input$pop_n))
      max_n <- max(5L, input$pop_n - 5L)
      shiny::updateSliderInput(session, "n_am", max = max_n, value = min(input$n_am, max_n))
    })

    sim <- shiny::reactive({
      input$sortear
      shiny::req(is.finite(input$pop_n), is.finite(input$n_am))
      simular_aas_fpc(input$pop_n, min(input$n_am, input$pop_n - 1L), 500L)
    })

    output$grafico_fpc <- shiny::renderPlot({
      s <- sim()
      p <- paleta_modo(modo())
      df <- data.frame(
        media = c(s$medias_com, s$medias_sem),
        esquema = factor(
          c(rep(tr("Com reposição", "With replacement"), length(s$medias_com)),
            rep(tr("Sem reposição", "Without replacement"), length(s$medias_sem))),
          levels = c(tr("Com reposição", "With replacement"), tr("Sem reposição", "Without replacement"))
        ),
        stringsAsFactors = FALSE
      )

      ggplot2::ggplot(df, ggplot2::aes(x = media, fill = esquema, colour = esquema)) +
        ggplot2::geom_density(alpha = 0.28, linewidth = 0.9) +
        ggplot2::geom_vline(xintercept = s$pop$mu, colour = p$linha, linetype = "dashed") +
        ggplot2::scale_fill_manual(values = c(p$neutro, cores_app$destaque)) +
        ggplot2::scale_colour_manual(values = c(p$secundario, cores_app$destaque)) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = sprintf("N = %d, n = %d (n/N = %s)", input$pop_n, input$n_am, fmt_pct(input$n_am / input$pop_n, 1)),
          subtitle = sprintf(tr("Fator (N − n)/(N − 1) = %s", "Factor (N − n)/(N − 1) = %s"), fmt_num(s$fpc, 3)),
          x = "X̄", y = tr("Densidade", "Density"), fill = NULL, colour = NULL
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(legend.position = "top")
    })

    output$tabela_fpc <- shiny::renderUI({
      s <- sim()
      df <- data.frame(
        Esquema = c(tr("Com reposição", "With replacement"), tr("Sem reposição", "Without replacement")),
        VarTeo = c(fmt_num(s$var_teo_com, 4), fmt_num(s$var_teo_sem, 4)),
        VarEmp = c(fmt_num(s$var_emp_com, 4), fmt_num(s$var_emp_sem, 4)),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Esquema", "Scheme"), tr("Var teórica", "Theoretical var."), tr("Var simulada", "Simulated var."))
      tabela_html(df, c("", "num", "num"))
    })
  })
}
