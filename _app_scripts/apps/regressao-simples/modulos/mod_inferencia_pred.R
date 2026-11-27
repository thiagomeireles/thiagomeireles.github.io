# =============================================================================
# Script:  mod_inferencia_pred.R (Aba 2: Inferência e predição)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Tamanho da amostra n, erro σ_e e ponto de predição z
# Saída:
#   - Faixas de confiança para E(Y | X) vs. predição para nova observação Y
# =============================================================================

mod_inferencia_pred_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::sliderInput(ns("n_reg"), tr("Tamanho da amostra n", "Sample size n"), min = 15, max = 100, value = 35, step = 5),
      shiny::sliderInput(ns("se_reg"), tr("Desvio padrão dos erros σ_e", "Error standard deviation σ_e"), min = 1, max = 10, value = 4, step = 1),
      shiny::sliderInput(ns("z_pred"), tr("Ponto de predição z", "Prediction point z"), min = 1, max = 10, value = 6, step = 0.5),
      shiny::actionButton(ns("sortear"), tr("Nova amostra", "New sample"), class = "btn-sm btn-primary mt-2")
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header(tr("Intervalo de confiança para a média vs. intervalo de predição", "Confidence band for the mean vs. prediction band")),
        bslib::card_body(shiny::plotOutput(ns("grafico_pred"), height = "340px"))
      ),
      bslib::card(
        bslib::card_header(tr("Estimativas, ANOVA e predição em X = z", "Estimates, ANOVA and prediction at X = z")),
        bslib::card_body(
          shiny::uiOutput(ns("tabela_pred")),
          shiny::hr(),
          shiny::p(class = "mb-0", tr(
            "O intervalo de predição para uma nova observação individual é sempre mais largo que o IC da média E(Y | X = z), porque soma a variância individual σ_e² à incerteza da reta.",
            "The prediction interval for a new individual observation is always wider than the CI for E(Y | X = z), because it adds individual error variance σ_e² to line uncertainty."
          ))
        )
      )
    )
  )
}

mod_inferencia_pred_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    reg <- shiny::reactive({
      input$sortear
      shiny::req(is.finite(input$n_reg), is.finite(input$se_reg))
      simular_regressao(input$n_reg, beta0 = 10, beta1 = 2.0, sigma_e = input$se_reg, cenario = "linear")
    })

    faixas <- shiny::reactive({
      shiny::req(reg(), is.finite(input$z_pred))
      calcular_faixas_reg(reg(), input$z_pred)
    })

    output$grafico_pred <- shiny::renderPlot({
      r <- reg()
      fx <- faixas()
      p <- paleta_modo(modo())

      ggplot2::ggplot(r$df, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_ribbon(data = fx$faixas, ggplot2::aes(x = x, ymin = ip_inf, ymax = ip_sup), inherit.aes = FALSE, fill = p$neutro, alpha = 0.25) +
        ggplot2::geom_ribbon(data = fx$faixas, ggplot2::aes(x = x, ymin = ic_inf, ymax = ic_sup), inherit.aes = FALSE, fill = cores_app$destaque, alpha = 0.30) +
        ggplot2::geom_point(colour = p$texto, alpha = 0.7, size = 2.0) +
        ggplot2::geom_line(data = fx$faixas, ggplot2::aes(x = x, y = fit), colour = cores_app$destaque, linewidth = 1.0) +
        ggplot2::geom_vline(xintercept = input$z_pred, colour = cores_app$referencia, linetype = "dashed") +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = sprintf("β̂ = %s | IC 95%% = [%s; %s]", fmt_num(r$beta_hat, 2), fmt_num(r$ci["x", 1], 2), fmt_num(r$ci["x", 2], 2)),
          x = "X", y = "Y",
          caption = tr("Faixa azul: IC 95% de E(Y|X) | Faixa cinza: predição 95% de Y", "Blue band: 95% CI of E(Y|X) | Gray band: 95% prediction of Y")
        ) +
        tema_grafico(modo())
    })

    output$tabela_pred <- shiny::renderUI({
      r <- reg()
      fx <- faixas()
      df <- data.frame(
        Item = c(
          "α̂", "β̂", "R²",
          sprintf("ŷ(z = %s)", fmt_num(fx$z_pred, 1)),
          tr("IC 95% para E(Y | z)", "95% CI for E(Y | z)"),
          tr("Intervalo de predição 95%", "95% prediction interval")
        ),
        Valor = c(
          fmt_num(r$alpha_hat, 2), fmt_num(r$beta_hat, 2), fmt_num(r$r2, 3),
          fmt_num(fx$y_hat_z, 2),
          sprintf("[%s; %s]", fmt_num(fx$ic_z[1], 2), fmt_num(fx$ic_z[2], 2)),
          sprintf("[%s; %s]", fmt_num(fx$ip_z[1], 2), fmt_num(fx$ip_z[2], 2))
        ),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Item", "Item"), tr("Valor", "Value"))
      tabela_html(df, c("", "num"))
    })
  })
}
