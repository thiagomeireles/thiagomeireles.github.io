# =============================================================================
# Script:  mod_pressupostos_anova.R (Aba 2: Pressupostos e Bartlett)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Objeto reativo anova_reativa() da Aba 1
# Saída:
#   - Gráfico de resíduos vs. ajustados, Q-Q plot e teste de Bartlett
# =============================================================================

mod_pressupostos_anova_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_columns(
    col_widths = c(6, 6),
    bslib::card(
      bslib::card_header(tr("Resíduos vs. valores ajustados (homocedasticidade)", "Residuals vs. fitted values (homoscedasticity)")),
      bslib::card_body(shiny::plotOutput(ns("grafico_res"), height = "330px"))
    ),
    bslib::card(
      bslib::card_header(tr("Normalidade dos resíduos (Q-Q) e teste de Bartlett", "Normality of residuals (Q-Q) and Bartlett test")),
      bslib::card_body(
        shiny::plotOutput(ns("grafico_qq"), height = "270px"),
        shiny::hr(),
        shiny::uiOutput(ns("texto_bartlett"))
      )
    )
  )
}

mod_pressupostos_anova_server <- function(id, modo, anova_reativa) {
  shiny::moduleServer(id, function(input, output, session) {
    output$grafico_res <- shiny::renderPlot({
      an <- anova_reativa()
      shiny::req(an)
      p <- paleta_modo(modo())
      df <- data.frame(ajust = an$ajustados, res = an$residuos)
      ggplot2::ggplot(df, ggplot2::aes(x = ajust, y = res)) +
        ggplot2::geom_hline(yintercept = 0, colour = cores_app$referencia, linetype = "dashed") +
        ggplot2::geom_jitter(width = 0.25, colour = cores_app$destaque, alpha = 0.7, size = 2.0) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = tr("Dispersão dos resíduos por média de grupo", "Residual spread across group means"),
          x = tr("Média ajustada ȳᵢ", "Fitted group mean ȳᵢ"),
          y = tr("Resíduo eᵢⱼ", "Residual eᵢⱼ")
        ) +
        tema_grafico(modo())
    })

    output$grafico_qq <- shiny::renderPlot({
      an <- anova_reativa()
      shiny::req(an)
      df <- data.frame(res = an$residuos)
      ggplot2::ggplot(df, ggplot2::aes(sample = res)) +
        ggplot2::stat_qq(colour = cores_app$destaque, alpha = 0.75) +
        ggplot2::stat_qq_line(colour = cores_app$referencia) +
        ggplot2::labs(
          title = tr("Gráfico Q-Q dos resíduos", "Normal Q-Q plot of residuals"),
          x = tr("Quantis teóricos", "Theoretical quantiles"),
          y = tr("Resíduos observados", "Observed residuals")
        ) +
        tema_grafico(modo())
    })

    output$texto_bartlett <- shiny::renderUI({
      an <- anova_reativa()
      shiny::req(an)
      shiny::p(class = "mb-0", sprintf(
        tr(
          "Teste de Bartlett para H₀: σ₁² = … = σ_I²: K² = %s, valor-p = %s. Lembrete: o teste de Bartlett é sensível à não normalidade.",
          "Bartlett test for H₀: σ₁² = … = σ_I²: K² = %s, p-value = %s. Reminder: Bartlett's test is sensitive to non-normality."
        ),
        fmt_num(an$bart_k2, 3), fmt_num(an$bart_p, 4)
      ))
    })
  })
}
