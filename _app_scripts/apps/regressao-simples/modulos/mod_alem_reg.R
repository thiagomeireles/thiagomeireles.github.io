# =============================================================================
# Script:  mod_alem_reg.R (Para ir além: CEF, causalidade e White/HC)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Força da heterocedasticidade e tamanho amostral n
# Saída:
#   - Comparação de erro-padrão clássico (OLS) vs. robusto de White (HC1)
# =============================================================================

mod_alem_reg_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::sliderInput(ns("n_hc"), tr("Tamanho da amostra n", "Sample size n"), min = 25, max = 200, value = 60, step = 25),
      shiny::sliderInput(ns("se_hc"), tr("Escala da heterocedasticidade", "Heteroscedasticity scale"), min = 2, max = 12, value = 6, step = 1),
      shiny::actionButton(ns("sim_hc"), tr("Nova amostra heterocedástica", "New heteroscedastic sample"), class = "btn-sm btn-primary mt-2")
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header(tr("Dispersão sob heterocedasticidade e aproximação linear da CEF", "Scatter under heteroscedasticity and linear CEF approximation")),
        bslib::card_body(shiny::plotOutput(ns("grafico_hc"), height = "340px"))
      ),
      bslib::card(
        bslib::card_header(tr("Erro-padrão clássico (OLS) vs. robusto de White (HC1)", "Classic OLS vs. White robust (HC1) standard error")),
        bslib::card_body(
          shiny::uiOutput(ns("tabela_hc")),
          shiny::hr(),
          shiny::p(class = "mb-0", tr(
            "Sob heterocedasticidade, β̂ continua não viesado para a melhor aproximação linear da esperança condicional E(Y | X), mas o erro-padrão clássico de OLS fica errado. Use erros-padrão robustos (White/HC). Lembrete: regressão mede associação, não causalidade.",
            "Under heteroscedasticity, β̂ remains unbiased for the best linear approximation to E(Y | X), yet classic OLS standard errors are wrong. Use robust (White/HC) standard errors. Reminder: regression measures association, not causation."
          ))
        )
      )
    )
  )
}

mod_alem_reg_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    reg <- shiny::reactive({
      input$sim_hc
      shiny::req(is.finite(input$n_hc), is.finite(input$se_hc))
      simular_regressao(input$n_hc, beta0 = 10, beta1 = 2.0, sigma_e = input$se_hc, cenario = "heteroced")
    })

    output$grafico_hc <- shiny::renderPlot({
      r <- reg()
      ggplot2::ggplot(r$df, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point(colour = cores_app$destaque, alpha = 0.7, size = 2.1) +
        ggplot2::geom_abline(intercept = r$alpha_hat, slope = r$beta_hat, colour = cores_app$referencia, linewidth = 1.0) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = sprintf("β̂ = %s (EP OLS = %s vs. EP HC1 = %s)", fmt_num(r$beta_hat, 3), fmt_num(r$ep_ols, 3), fmt_num(r$ep_hc1, 3)),
          x = "X", y = "Y"
        ) +
        tema_grafico(modo())
    })

    output$tabela_hc <- shiny::renderUI({
      r <- reg()
      t_ols <- r$beta_hat / r$ep_ols
      t_hc1 <- r$beta_hat / r$ep_hc1
      df <- data.frame(
        Metodo = c(tr("OLS clássico (homocedástico)", "Classic OLS (homoscedastic)"), tr("White robusto (HC1)", "White robust (HC1)")),
        EP = c(fmt_num(r$ep_ols, 4), fmt_num(r$ep_hc1, 4)),
        StatT = c(fmt_num(t_ols, 3), fmt_num(t_hc1, 3)),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Método", "Method"), tr("EP(β̂)", "SE(β̂)"), "t = β̂ / EP")
      tabela_html(df, c("", "num", "num"))
    })
  })
}
