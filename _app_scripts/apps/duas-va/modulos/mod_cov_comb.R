# =============================================================================
# Script:  mod_cov_comb.R (Aba 3: Covariância, correlação e aX + bY)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Coeficientes a e b da combinação linear aX + bY
# Saída:
#   - Decomposição de Var(aX + bY) em a²Var(X) + b²Var(Y) e 2ab·Cov(X, Y)
# =============================================================================

mod_cov_comb_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::sliderInput(ns("coef_a"), tr("Coeficiente a em aX + bY", "Coefficient a in aX + bY"), min = -3, max = 3, value = 1, step = 0.5),
      shiny::sliderInput(ns("coef_b"), tr("Coeficiente b em aX + bY", "Coefficient b in aX + bY"), min = -3, max = 3, value = 1, step = 0.5)
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header(tr("Decomposição de Var(aX + bY)", "Decomposition of Var(aX + bY)")),
        bslib::card_body(shiny::plotOutput(ns("grafico_comb"), height = "340px"))
      ),
      bslib::card(
        bslib::card_header(tr("Esperança e variância da combinação linear", "Expected value and variance of linear combination")),
        bslib::card_body(
          shiny::uiOutput(ns("tabela_comb")),
          shiny::hr(),
          shiny::p(class = "mb-0", tr(
            "Quando a = 1 e b = −1 (diferença X − Y), o termo 2ab·Cov(X, Y) entra com sinal negativo: variáveis positivamente correlacionadas reduzem a variância da diferença.",
            "When a = 1 and b = −1 (difference X − Y), the 2ab·Cov(X, Y) term enters negatively: positively correlated variables reduce the variance of the difference."
          ))
        )
      )
    )
  )
}

mod_cov_comb_server <- function(id, modo, conjunta_reativa) {
  shiny::moduleServer(id, function(input, output, session) {
    comb <- shiny::reactive({
      an <- conjunta_reativa()
      shiny::req(an, is.finite(input$coef_a), is.finite(input$coef_b))
      combinacao_linear_vars(an$vx, an$vy, an$cov_xy, an$ex, an$ey, input$coef_a, input$coef_b)
    })

    output$grafico_comb <- shiny::renderPlot({
      c_lin <- comb()
      p <- paleta_modo(modo())
      df <- data.frame(
        comp = factor(
          c("a²Var(X) + b²Var(Y)", "2ab·Cov(X, Y)", "Var(aX + bY)"),
          levels = c("a²Var(X) + b²Var(Y)", "2ab·Cov(X, Y)", "Var(aX + bY)")
        ),
        valor = c(c_lin$parte_ind, c_lin$termo_cov, c_lin$var_total),
        stringsAsFactors = FALSE
      )

      ggplot2::ggplot(df, ggplot2::aes(x = comp, y = valor, fill = comp)) +
        ggplot2::geom_col(width = 0.58) +
        ggplot2::geom_hline(yintercept = 0, colour = p$secundario) +
        ggplot2::scale_fill_manual(values = c(p$neutro, cores_app$referencia, cores_app$destaque)) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = sprintf("Var(%s·X + %s·Y) = %s", fmt_num(input$coef_a, 1), fmt_num(input$coef_b, 1), fmt_num(c_lin$var_total, 4)),
          x = NULL, y = tr("Variância", "Variance")
        ) +
        tema_grafico(modo())
    })

    output$tabela_comb <- shiny::renderUI({
      c_lin <- comb()
      df <- data.frame(
        Medida = c("E(aX + bY)", "a²Var(X) + b²Var(Y)", "2ab·Cov(X, Y)", "Var(aX + bY)", "DP(aX + bY)"),
        Valor = c(
          fmt_num(c_lin$media, 4),
          fmt_num(c_lin$parte_ind, 4),
          fmt_num(c_lin$termo_cov, 4),
          fmt_num(c_lin$var_total, 4),
          fmt_num(c_lin$dp_total, 4)
        ),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Quantidade", "Quantity"), tr("Valor", "Value"))
      tabela_html(df, c("", "num"))
    })
  })
}
