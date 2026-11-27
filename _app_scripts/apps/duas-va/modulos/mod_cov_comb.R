# =============================================================================
# Script:  mod_cov_comb.R (Aba 3: Covariância, correlação e aX + bY)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Conjunta escolhida na aba 1 e coeficientes a e b de aX + bY
# Saída:
#   - Decomposição de Var(aX + bY) em a²Var(X) + b²Var(Y) e 2ab·Cov(X, Y)
# =============================================================================

mod_cov_comb_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::uiOutput(ns("nota_conjunta")),
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
        bslib::card_header(tr("Esperança e variância da combinação linear", "Expected value and variance of the linear combination")),
        bslib::card_body(shiny::uiOutput(ns("tabela_comb")))
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header(rotulo_entenda()),
        bslib::card_body(
          shiny::p(tr(
            "A média de uma combinação linear segue os coeficientes: E(aX + bY) = aE(X) + bE(Y). A variância tem um termo a mais: Var(aX + bY) = a²Var(X) + b²Var(Y) + 2ab·Cov(X, Y) (Casella e Berger, Teorema 4.5.6; Wooldridge, apêndice B, propriedade VAR.3). Com a = b = 1, é o Teorema 8.3 de Morettin e Bussab.",
            "The mean of a linear combination follows the coefficients: E(aX + bY) = aE(X) + bE(Y). The variance has one extra term: Var(aX + bY) = a²Var(X) + b²Var(Y) + 2ab·Cov(X, Y) (Casella and Berger, Theorem 4.5.6; Wooldridge, appendix B, property VAR.3). With a = b = 1, this is Theorem 8.3 in Morettin and Bussab."
          )),
          shiny::p(tr(
            "O sinal de 2ab·Cov(X, Y) decide se a covariância soma ou desconta. Com a = 1 e b = −1 (a diferença X − Y), variáveis com covariância positiva têm uma diferença menos variável do que teriam se fossem independentes.",
            "The sign of 2ab·Cov(X, Y) decides whether the covariance adds or subtracts. With a = 1 and b = −1 (the difference X − Y), variables with positive covariance have a less variable difference than they would if independent."
          )),
          shiny::p(
            class = "mb-0",
            shiny::strong(tr("Experimente:", "Try this:")),
            tr(
              "com o Ex. 8.1 da aba 1, compare a = b = 1 (Var = 1,5) com a = 1 e b = −1 (Var = 0,5). Nos dois casos, a²Var(X) + b²Var(Y) = 1,0; só o sinal de 2ab·Cov(X, Y) = ±0,5 muda.",
              "with Ex. 8.1 from tab 1, compare a = b = 1 (Var = 1.5) with a = 1 and b = −1 (Var = 0.5). In both cases, a²Var(X) + b²Var(Y) = 1.0; only the sign of 2ab·Cov(X, Y) = ±0.5 changes."
            )
          )
        )
      ),
      cartao_cuidados(
        tr("Cuidados", "Watch out"),
        stats::setNames(
          list(
            tr(
              "a variância de X − Y não é Var(X) − Var(Y). Os coeficientes entram ao quadrado, então Var(X − Y) = Var(X) + Var(Y) − 2·Cov(X, Y).",
              "the variance of X − Y is not Var(X) − Var(Y). The coefficients enter squared, so Var(X − Y) = Var(X) + Var(Y) − 2·Cov(X, Y)."
            ),
            tr(
              "a covariância depende das unidades: dobrar X dobra Cov(X, Y). A correlação não muda com a escala, só com o sinal dos coeficientes (fórmula 8.20).",
              "covariance depends on units: doubling X doubles Cov(X, Y). Correlation does not change with scale, only with the sign of the coefficients (formula 8.20)."
            )
          ),
          c(
            tr("Diferença não subtrai variâncias", "A difference does not subtract variances"),
            tr("Covariância tem unidade", "Covariance has units")
          )
        )
      )
    )
  )
}

mod_cov_comb_server <- function(id, modo, conjunta_reativa) {
  shiny::moduleServer(id, function(input, output, session) {
    output$nota_conjunta <- shiny::renderUI({
      an <- conjunta_reativa()
      shiny::req(an)
      nota_conjunta_em_uso(an)
    })

    comb <- shiny::reactive({
      an <- conjunta_reativa()
      shiny::req(an, is.finite(input$coef_a), is.finite(input$coef_b))
      combinacao_linear_vars(an$vx, an$vy, an$cov_xy, an$ex, an$ey, input$coef_a, input$coef_b)
    })

    output$grafico_comb <- shiny::renderPlot({
      c_lin <- comb()
      p <- paleta_modo(modo())
      niveis <- c("a²Var(X) + b²Var(Y)", "2ab·Cov(X, Y)", "Var(aX + bY)")
      df <- data.frame(
        comp = factor(niveis, levels = niveis),
        valor = c(c_lin$parte_ind, c_lin$termo_cov, c_lin$var_total),
        stringsAsFactors = FALSE
      )

      ggplot2::ggplot(df, ggplot2::aes(x = comp, y = valor, fill = comp)) +
        ggplot2::geom_col(width = 0.58) +
        ggplot2::geom_hline(yintercept = 0, colour = p$secundario) +
        ggplot2::scale_fill_manual(values = c(p$neutro, cores_app$referencia, cores_app$destaque)) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = sprintf("Var(%s·X + %s·Y) = %s", fmt_coef(input$coef_a), fmt_coef(input$coef_b), fmt_num(c_lin$var_total, 4)),
          x = NULL, y = tr("Variância", "Variance")
        ) +
        tema_grafico(modo())
    })

    output$tabela_comb <- shiny::renderUI({
      an <- conjunta_reativa()
      c_lin <- comb()
      df <- data.frame(
        Medida = c(
          "Var(X)", "Var(Y)", "Cov(X, Y)",
          "E(aX + bY)", "a²Var(X) + b²Var(Y)", "2ab·Cov(X, Y)", "Var(aX + bY)", tr("DP(aX + bY)", "SD(aX + bY)")
        ),
        Valor = c(
          fmt_num(an$vx, 4), fmt_num(an$vy, 4), fmt_num(an$cov_xy, 4),
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
