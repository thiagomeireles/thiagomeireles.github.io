# =============================================================================
# Script:  mod_alem_duas_va.R (Para ir além: leis totais e normal bidimensional)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Conjunta reativa (para leis totais) e ρ da normal bidimensional
# Saída:
#   - Verificação numérica da Esperança Total e Variância Total e dispersão
#     simulada da normal bidimensional com reta E(Y | X = x) = ρ·x
# =============================================================================

mod_alem_duas_va_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_columns(
    col_widths = c(5, 7),
    bslib::card(
      bslib::card_header(tr("Esperança Total e Variância Total (Gelman Eqs. 1.8–1.9)", "Law of Total Expectation and Variance (Gelman Eqs. 1.8–1.9)")),
      bslib::card_body(
        shiny::p(tr(
          "A partir da conjunta da Aba 1, conferimos que E(X) = E[E(X | Y)] e que Var(X) se decompõe exatamente na média da variância condicional mais a variância da média condicional.",
          "Using the joint table from Tab 1, we verify E(X) = E[E(X | Y)] and that Var(X) decomposes exactly into the mean of conditional variance plus the variance of conditional mean."
        )),
        shiny::uiOutput(ns("tabela_totais"))
      )
    ),
    bslib::card(
      bslib::card_header(tr("Normal bidimensional: ρ = 0 equivale à independência", "Bivariate normal: ρ = 0 means independence")),
      bslib::card_body(
        bslib::layout_columns(
          col_widths = c(7, 5),
          shiny::sliderInput(ns("rho_bv"), tr("Correlação ρ", "Correlation ρ"), min = -0.95, max = 0.95, value = 0.70, step = 0.05),
          shiny::actionButton(ns("sim_bv"), tr("Sortear 400 pares", "Draw 400 pairs"), class = "btn-sm btn-primary mt-4")
        ),
        shiny::plotOutput(ns("grafico_bv"), height = "310px")
      )
    )
  )
}

mod_alem_duas_va_server <- function(id, modo, conjunta_reativa) {
  shiny::moduleServer(id, function(input, output, session) {
    output$tabela_totais <- shiny::renderUI({
      an <- conjunta_reativa()
      shiny::req(an)
      lt <- leis_totais(an)
      df <- data.frame(
        Identidade = c(
          "E(X)",
          "E[E(X | Y)]",
          "Var(X)",
          "E[Var(X | Y)]",
          "Var[E(X | Y)]",
          "E[Var(X | Y)] + Var[E(X | Y)]"
        ),
        Valor = c(
          fmt_num(lt$ex, 4),
          fmt_num(lt$e_da_cond, 4),
          fmt_num(lt$vx, 4),
          fmt_num(lt$e_da_var_cond, 4),
          fmt_num(lt$var_da_media_cond, 4),
          fmt_num(lt$soma_vars, 4)
        ),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Identidade", "Identity"), tr("Valor", "Value"))
      tabela_html(df, c("", "num"))
    })

    amostra_bv <- shiny::reactiveVal(simular_normal_bivariada(400L, 0.70))
    shiny::observeEvent(list(input$rho_bv, input$sim_bv), {
      shiny::req(is.finite(input$rho_bv))
      amostra_bv(simular_normal_bivariada(400L, input$rho_bv))
    })

    output$grafico_bv <- shiny::renderPlot({
      df <- amostra_bv()
      shiny::req(df, is.finite(input$rho_bv))
      p <- paleta_modo(modo())
      ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point(colour = cores_app$destaque, alpha = 0.55, size = 1.9) +
        ggplot2::geom_abline(intercept = 0, slope = input$rho_bv, colour = cores_app$referencia, linewidth = 1.0) +
        ggplot2::coord_fixed(ratio = 1, xlim = c(-3.5, 3.5), ylim = c(-3.5, 3.5)) +
        ggplot2::labs(
          title = sprintf("Normal(0, 0, 1, 1, ρ = %s)", fmt_num(input$rho_bv, 2)),
          subtitle = sprintf(
            tr("E(Y | X = x) = %s·x | DP condicional = √(1 − ρ²) = %s", "E(Y | X = x) = %s·x | Conditional SD = √(1 − ρ²) = %s"),
            fmt_num(input$rho_bv, 2), fmt_num(sqrt(1 - input$rho_bv^2), 3)
          ),
          x = "X", y = "Y",
          caption = tr("Linha vermelha: E(Y | X = x)", "Red line: E(Y | X = x)")
        ) +
        tema_grafico(modo())
    })
  })
}
