# =============================================================================
# Script:  mod_residuos.R (Aba 3: Diagnóstico de resíduos)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Processo gerador (linear, quadrático, heterocedástico)
# Saída:
#   - Resíduos vs. ajustados e Q-Q normal para detectar violações do modelo
# =============================================================================

mod_residuos_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::radioButtons(
        ns("cen_res"),
        tr("Processo gerador real", "True data generating process"),
        choices = stats::setNames(
          c("linear", "quadratico", "heteroced"),
          c(
            tr("Linear homocedástico (ideal)", "Linear homoscedastic (ideal)"),
            tr("Curva quadrática (não linearidade)", "Quadratic curve (non-linearity)"),
            tr("Variância crescente (heterocedasticidade)", "Increasing variance (heteroscedasticity)")
          )
        )
      ),
      shiny::actionButton(ns("sortear"), tr("Nova amostra (n = 60)", "New sample (n = 60)"), class = "btn-sm btn-primary mt-2")
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header(tr("Resíduos ê vs. valores ajustados ŷ", "Residuals e vs. fitted values ŷ")),
        bslib::card_body(shiny::plotOutput(ns("grafico_res_fit"), height = "340px"))
      ),
      bslib::card(
        bslib::card_header(tr("Gráfico Q-Q normal dos resíduos", "Normal Q-Q plot of residuals")),
        bslib::card_body(
          shiny::plotOutput(ns("grafico_res_qq"), height = "280px"),
          shiny::p(class = "mt-2 mb-0", tr(
            "Um R² alto não garante que a forma funcional esteja certa: padrão em U nos resíduos revela não linearidade; abertura em funil revela heterocedasticidade.",
            "A high R² does not guarantee a correct functional form: a U-shape in residuals reveals non-linearity; a funnel shape reveals heteroscedasticity."
          ))
        )
      )
    )
  )
}

mod_residuos_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    reg <- shiny::reactive({
      input$sortear
      shiny::req(input$cen_res)
      simular_regressao(60L, beta0 = 10, beta1 = 2.0, sigma_e = 4, cenario = input$cen_res)
    })

    output$grafico_res_fit <- shiny::renderPlot({
      r <- reg()
      df <- data.frame(fit = r$ajustados, res = r$residuos)
      ggplot2::ggplot(df, ggplot2::aes(x = fit, y = res)) +
        ggplot2::geom_hline(yintercept = 0, colour = cores_app$referencia, linetype = "dashed") +
        ggplot2::geom_point(colour = cores_app$destaque, alpha = 0.75, size = 2.1) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = sprintf("R² = %s | S_e = %s", fmt_num(r$r2, 3), fmt_num(r$s_e, 2)),
          x = tr("Valores ajustados ŷ", "Fitted values ŷ"),
          y = tr("Resíduos ê", "Residuals ê")
        ) +
        tema_grafico(modo())
    })

    output$grafico_res_qq <- shiny::renderPlot({
      r <- reg()
      df <- data.frame(res = r$residuos)
      ggplot2::ggplot(df, ggplot2::aes(sample = res)) +
        ggplot2::stat_qq(colour = cores_app$destaque, alpha = 0.75) +
        ggplot2::stat_qq_line(colour = cores_app$referencia) +
        ggplot2::labs(
          x = tr("Quantis teóricos", "Theoretical quantiles"),
          y = tr("Resíduos observados", "Observed residuals")
        ) +
        tema_grafico(modo())
    })
  })
}
