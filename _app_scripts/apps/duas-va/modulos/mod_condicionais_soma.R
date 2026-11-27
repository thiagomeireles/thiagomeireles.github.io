# =============================================================================
# Script:  mod_condicionais_soma.R (Aba 2: Condicionais e função Z = X + Y)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Valor condicionante Y = y_j selecionado pelo usuário
# Saída:
#   - Comparação entre P(X | Y = y_j) e marginal P(X), mais distribuição de X + Y
# =============================================================================

mod_condicionais_soma_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_columns(
    col_widths = c(6, 6),
    bslib::card(
      bslib::card_header(tr("Distribuição condicional P(X = x | Y = y)", "Conditional distribution P(X = x | Y = y)")),
      bslib::card_body(
        shiny::uiOutput(ns("seletor_y")),
        shiny::plotOutput(ns("grafico_cond"), height = "310px"),
        shiny::uiOutput(ns("texto_cond"))
      )
    ),
    bslib::card(
      bslib::card_header(tr("Distribuição da soma Z = X + Y", "Distribution of the sum Z = X + Y")),
      bslib::card_body(
        shiny::plotOutput(ns("grafico_soma"), height = "310px"),
        shiny::uiOutput(ns("tabela_soma"))
      )
    )
  )
}

mod_condicionais_soma_server <- function(id, modo, conjunta_reativa) {
  shiny::moduleServer(id, function(input, output, session) {
    output$seletor_y <- shiny::renderUI({
      an <- conjunta_reativa()
      shiny::req(an)
      ns <- session$ns
      shiny::radioButtons(
        ns("y_esc"),
        tr("Condicionar em Y =", "Condition on Y ="),
        choices = stats::setNames(as.character(an$ys), paste0("Y = ", an$ys)),
        selected = as.character(an$ys[1]),
        inline = TRUE
      )
    })

    cond_sel <- shiny::reactive({
      an <- conjunta_reativa()
      shiny::req(an, input$y_esc)
      idx <- which(as.character(an$ys) == as.character(input$y_esc))
      if (length(idx) == 0L) idx <- 1L
      an$cond_y[[idx[1]]]
    })

    output$grafico_cond <- shiny::renderPlot({
      an <- conjunta_reativa()
      c_y <- cond_sel()
      shiny::req(an, c_y)
      p <- paleta_modo(modo())

      df <- data.frame(
        x = rep(factor(an$xs), 2),
        tipo = factor(
          c(rep(tr("Marginal P(X)", "Marginal P(X)"), length(an$xs)),
            rep(sprintf("P(X | Y = %s)", c_y$y), length(an$xs))),
          levels = c(tr("Marginal P(X)", "Marginal P(X)"), sprintf("P(X | Y = %s)", c_y$y))
        ),
        prob = c(an$px, c_y$p_x_dado_y),
        stringsAsFactors = FALSE
      )

      ggplot2::ggplot(df, ggplot2::aes(x = x, y = prob, fill = tipo)) +
        ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.65), width = 0.6) +
        ggplot2::scale_fill_manual(values = c(p$neutro, cores_app$destaque)) +
        ggplot2::scale_y_continuous(limits = c(0, 1.05), labels = rotulos_eixo()) +
        ggplot2::labs(
          title = sprintf("P(X = x | Y = %s) vs. P(X = x)", c_y$y),
          x = "x", y = tr("Probabilidade", "Probability"), fill = NULL
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(legend.position = "top")
    })

    output$texto_cond <- shiny::renderUI({
      c_y <- cond_sel()
      shiny::req(c_y)
      shiny::p(class = "mt-2 mb-0", sprintf(
        tr("Dado Y = %s: E(X | Y = %s) = %s e Var(X | Y = %s) = %s.", "Given Y = %s: E(X | Y = %s) = %s and Var(X | Y = %s) = %s."),
        c_y$y, c_y$y, fmt_num(c_y$media, 3), c_y$y, fmt_num(c_y$var, 3)
      ))
    })

    output$grafico_soma <- shiny::renderPlot({
      an <- conjunta_reativa()
      shiny::req(an)
      df_z <- distribuicao_soma(an$grade)
      ggplot2::ggplot(df_z, ggplot2::aes(x = factor(z), y = prob)) +
        ggplot2::geom_col(fill = cores_app$destaque, width = 0.6) +
        ggplot2::scale_y_continuous(limits = c(0, max(df_z$prob) * 1.18), labels = rotulos_eixo()) +
        ggplot2::labs(
          title = tr("Soma das probabilidades nos pares com x + y = z", "Sum of probabilities over pairs with x + y = z"),
          x = "z = x + y", y = "P(Z = z)"
        ) +
        tema_grafico(modo())
    })

    output$tabela_soma <- shiny::renderUI({
      an <- conjunta_reativa()
      shiny::req(an)
      df_z <- distribuicao_soma(an$grade)
      df_z$prob <- fmt_num(df_z$prob, 4)
      names(df_z) <- c("z = x + y", "P(Z = z)")
      tabela_html(df_z, c("num", "num"))
    })
  })
}
