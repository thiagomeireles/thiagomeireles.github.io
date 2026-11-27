# =============================================================================
# Script:  mod_aderencia.R (Aba 1: Teste de aderência)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Contagens observadas em 6 faces de um dado (ou geração multinomial) e n_par
# Saída:
#   - Comparação de observados vs. esperados, contribuição (O − E)²/E e valor-p
# =============================================================================

mod_aderencia_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::radioButtons(
        ns("cenario_ad"),
        tr("Dado simulado", "Simulated die"),
        choices = stats::setNames(c("honesto", "viciado"), c(tr("Dado honesto (1/6 cada)", "Fair die (1/6 each)"), tr("Dado viciado (face 6 favorecida)", "Loaded die (face 6 favored)"))),
        inline = FALSE
      ),
      shiny::sliderInput(ns("n_lanc"), tr("Número de lançamentos n", "Number of rolls n"), min = 30, max = 300, value = 120, step = 30),
      shiny::sliderInput(ns("n_par_est"), tr("Parâmetros estimados da amostra", "Parameters estimated from sample"), min = 0, max = 2, value = 0, step = 1),
      shiny::actionButton(ns("sortear"), tr("Lançar dado n vezes", "Roll die n times"), class = "btn-sm btn-primary mt-2")
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header(tr("Frequências observadas (Oᵢ) vs. esperadas (Eᵢ)", "Observed (Oᵢ) vs. expected (Eᵢ) frequencies")),
        bslib::card_body(shiny::plotOutput(ns("grafico_ad"), height = "340px"))
      ),
      bslib::card(
        bslib::card_header(tr("Teste qui-quadrado de aderência", "Chi-squared goodness-of-fit test")),
        bslib::card_body(
          shiny::uiOutput(ns("tabela_ad")),
          shiny::hr(),
          shiny::p(class = "mb-0", tr(
            "Quando parâmetros do modelo são estimados a partir da própria amostra, os graus de liberdade caem para k − 1 − (nº de parâmetros). A função chisq.test() do R não faz esse desconto automaticamente.",
            "When model parameters are estimated from the sample itself, degrees of freedom drop to k − 1 − (number of parameters). R's chisq.test() does not subtract them automatically."
          ))
        )
      )
    )
  )
}

mod_aderencia_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    obs <- shiny::reactive({
      input$sortear
      shiny::req(input$cenario_ad, is.finite(input$n_lanc))
      p_real <- if (identical(input$cenario_ad, "honesto")) rep(1 / 6, 6) else c(0.12, 0.12, 0.12, 0.12, 0.17, 0.35)
      as.numeric(stats::rmultinom(1L, size = input$n_lanc, prob = p_real))
    })

    res <- shiny::reactive({
      shiny::req(obs(), is.finite(input$n_par_est))
      calcular_aderencia(obs(), rep(1 / 6, 6), input$n_par_est)
    })

    output$grafico_ad <- shiny::renderPlot({
      r <- res()
      p <- paleta_modo(modo())
      df <- data.frame(
        face = rep(factor(1:6), 2L),
        tipo = factor(
          c(rep(tr("Observado (Oᵢ)", "Observed (Oᵢ)"), 6L), rep(tr("Esperado (Eᵢ)", "Expected (Eᵢ)"), 6L)),
          levels = c(tr("Observado (Oᵢ)", "Observed (Oᵢ)"), tr("Esperado (Eᵢ)", "Expected (Eᵢ)"))
        ),
        freq = c(r$o, r$e),
        stringsAsFactors = FALSE
      )

      ggplot2::ggplot(df, ggplot2::aes(x = face, y = freq, fill = tipo)) +
        ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.65), width = 0.6) +
        ggplot2::scale_fill_manual(values = c(cores_app$destaque, p$neutro)) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = sprintf("χ² = %s | gl = %d | valor-p = %s", fmt_num(r$chi2, 2), r$gl, fmt_num(r$valor_p, 4)),
          x = tr("Face do dado", "Die face"), y = tr("Frequência", "Frequency"), fill = NULL
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(legend.position = "top")
    })

    output$tabela_ad <- shiny::renderUI({
      r <- res()
      df <- data.frame(
        Face = as.character(1:6),
        Obs = fmt_num(r$o, 0),
        Esp = fmt_num(r$e, 1),
        Contrib = fmt_num(r$contrib, 2),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Face", "Face"), "Oᵢ", "Eᵢ", "(Oᵢ − Eᵢ)²/Eᵢ")
      tabela_html(df, c("", "num", "num", "num"))
    })
  })
}
