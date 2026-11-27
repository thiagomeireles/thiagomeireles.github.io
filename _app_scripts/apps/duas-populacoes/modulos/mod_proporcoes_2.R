# =============================================================================
# Script:  mod_proporcoes_2.R (Aba 3: Duas proporções)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Contagens de sucessos k1, k2 e tamanhos n1, n2
# Saída:
#   - Gráfico comparativo de p̂1 e p̂2, teste Z combinado e IC 95% para p1 − p2
# =============================================================================

mod_proporcoes_2_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::sliderInput(ns("n1_p"), tr("Tamanho grupo 1 (n₁)", "Group 1 size (n₁)"), min = 20, max = 300, value = 80, step = 10),
      shiny::sliderInput(ns("k1_p"), tr("Sucessos grupo 1 (k₁)", "Group 1 successes (k₁)"), min = 0, max = 300, value = 42, step = 1),
      shiny::sliderInput(ns("n2_p"), tr("Tamanho grupo 2 (n₂)", "Group 2 size (n₂)"), min = 20, max = 300, value = 80, step = 10),
      shiny::sliderInput(ns("k2_p"), tr("Sucessos grupo 2 (k₂)", "Group 2 successes (k₂)"), min = 0, max = 300, value = 26, step = 1)
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header(tr("Proporções amostrais p̂₁ e p̂₂", "Sample proportions p̂₁ and p̂₂")),
        bslib::card_body(shiny::plotOutput(ns("grafico_p2"), height = "330px"))
      ),
      bslib::card(
        bslib::card_header(tr("Teste Z (p̂ combinado) e IC 95% (variâncias separadas)", "Z test (pooled p̂) and 95% CI (separate variances)")),
        bslib::card_body(shiny::uiOutput(ns("tabela_p2")))
      )
    )
  )
}

mod_proporcoes_2_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$n1_p, {
      shiny::req(is.finite(input$n1_p))
      shiny::updateSliderInput(session, "k1_p", max = input$n1_p, value = min(input$k1_p, input$n1_p))
    })
    shiny::observeEvent(input$n2_p, {
      shiny::req(is.finite(input$n2_p))
      shiny::updateSliderInput(session, "k2_p", max = input$n2_p, value = min(input$k2_p, input$n2_p))
    })

    res <- shiny::reactive({
      shiny::req(is.finite(input$k1_p), is.finite(input$n1_p), is.finite(input$k2_p), is.finite(input$n2_p))
      calcular_duas_prop(min(input$k1_p, input$n1_p), input$n1_p, min(input$k2_p, input$n2_p), input$n2_p, 0.95)
    })

    output$grafico_p2 <- shiny::renderPlot({
      r <- res()
      p <- paleta_modo(modo())
      rot <- c(tr("Grupo 1", "Group 1"), tr("Grupo 2", "Group 2"))
      df <- data.frame(
        grupo = factor(rot, levels = rot),
        prop = c(r$p1, r$p2),
        stringsAsFactors = FALSE
      )

      ggplot2::ggplot(df, ggplot2::aes(x = grupo, y = prop, fill = grupo)) +
        ggplot2::geom_col(width = 0.5) +
        ggplot2::geom_hline(yintercept = r$p_comb, colour = cores_app$referencia, linetype = "dashed") +
        ggplot2::geom_text(ggplot2::aes(label = fmt_pct(prop, 1)), vjust = -0.4, colour = p$texto, fontface = "bold") +
        ggplot2::scale_fill_manual(values = c(cores_app$destaque, p$neutro)) +
        ggplot2::scale_y_continuous(limits = c(0, 1.08), labels = rotulos_eixo()) +
        ggplot2::labs(
          title = sprintf("p̂₁ − p̂₂ = %s", fmt_pct(r$p1 - r$p2, 1)),
          x = NULL, y = tr("Proporção", "Proportion"),
          caption = sprintf(tr("Linha tracejada: p̂ combinado sob H₀ = %s", "Dashed line: pooled p̂ under H₀ = %s"), fmt_pct(r$p_comb, 1))
        ) +
        tema_grafico(modo())
    })

    output$tabela_p2 <- shiny::renderUI({
      r <- res()
      df <- data.frame(
        Medida = c("p̂₁", "p̂₂", tr("p̂ combinado (H₀)", "Pooled p̂ (H₀)"), "Z", tr("Valor-p (bilateral)", "p-value (two-sided)"), tr("IC 95% para p₁ − p₂", "95% CI for p₁ − p₂")),
        Valor = c(
          fmt_num(r$p1, 4), fmt_num(r$p2, 4), fmt_num(r$p_comb, 4),
          fmt_num(r$z, 3), fmt_num(r$valor_p, 4),
          sprintf("[%s; %s]", fmt_num(r$ic[1], 3), fmt_num(r$ic[2], 3))
        ),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Medida", "Measure"), tr("Valor", "Value"))
      tabela_html(df, c("", "num"))
    })
  })
}
