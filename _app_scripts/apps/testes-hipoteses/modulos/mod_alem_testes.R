# =============================================================================
# Script:  mod_alem_testes.R (Para ir além: múltiplos testes e armadilhas)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Número de testes independentes sob H0 verdadeira e nível α
# Saída:
#   - Simulação de falsos positivos acumulados vs. correção de Bonferroni
# =============================================================================

mod_alem_testes_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::sliderInput(ns("m_testes"), tr("Número de testes simultâneos m (todos com H₀ verdadeira)", "Number of simultaneous tests m (all with H₀ true)"), min = 1, max = 50, value = 20, step = 1),
      shiny::sliderInput(ns("alpha_mult"), tr("Nível nominal α por teste", "Nominal α per test"), min = 0.01, max = 0.10, value = 0.05, step = 0.01),
      shiny::actionButton(ns("sim_mult"), tr("Simular m testes sob H₀", "Simulate m tests under H₀"), class = "btn-sm btn-primary mt-2")
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header(tr("Valores-p simulados quando H₀ é verdadeira em todos os testes", "Simulated p-values when H₀ is true in every test")),
        bslib::card_body(shiny::plotOutput(ns("grafico_mult"), height = "340px"))
      ),
      bslib::card(
        bslib::card_header(tr("Inflação do erro global e Bonferroni", "Family-wise error inflation and Bonferroni")),
        bslib::card_body(
          shiny::uiOutput(ns("tabela_mult")),
          shiny::hr(),
          shiny::p(class = "mb-0", tr(
            "Com α = 0,05 e 20 testes onde H₀ é sempre verdadeira, a chance de ao menos um falso positivo chega a 64,2%. A correção de Bonferroni usa α/m em cada teste para controlar o erro global.",
            "With α = 0.05 and 20 tests where H₀ is always true, the chance of at least one false positive reaches 64.2%. Bonferroni correction uses α/m per test to control family-wise error."
          ))
        )
      )
    )
  )
}

mod_alem_testes_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    sim <- shiny::reactive({
      input$sim_mult
      shiny::req(is.finite(input$m_testes), is.finite(input$alpha_mult))
      simular_multiplos_testes(input$m_testes, input$alpha_mult)
    })

    output$grafico_mult <- shiny::renderPlot({
      s <- sim()
      p <- paleta_modo(modo())
      df <- data.frame(
        teste = seq_along(s$ps),
        valor_p = s$ps,
        fp = s$ps <= input$alpha_mult
      )
      ggplot2::ggplot(df, ggplot2::aes(x = teste, y = valor_p, colour = fp)) +
        ggplot2::geom_point(size = 2.6) +
        ggplot2::geom_hline(yintercept = input$alpha_mult, colour = cores_app$referencia, linetype = "dashed") +
        ggplot2::geom_hline(yintercept = s$alpha_bonf, colour = "#2b8a3e", linetype = "dotted") +
        ggplot2::scale_colour_manual(values = c("FALSE" = cores_app$destaque, "TRUE" = cores_app$referencia)) +
        ggplot2::scale_y_continuous(limits = c(0, 1), labels = rotulos_eixo()) +
        ggplot2::labs(
          title = sprintf(tr("Falsos positivos: %d de %d testes (sem correção)", "False positives: %d of %d tests (uncorrected)"), s$falso_pos, input$m_testes),
          x = tr("Índice do teste", "Test index"),
          y = tr("Valor-p", "p-value"),
          caption = tr("Tracejada vermelha: α | Pontilhada verde: α/m (Bonferroni)", "Red dashed: α | Green dotted: α/m (Bonferroni)")
        ) +
        tema_grafico(modo())
    })

    output$tabela_mult <- shiny::renderUI({
      s <- sim()
      df <- data.frame(
        Item = c(
          tr("P(≥ 1 falso positivo) = 1 − (1 − α)^m", "P(≥ 1 false positive) = 1 − (1 − α)^m"),
          tr("Falsos positivos sem correção", "Uncorrected false positives"),
          tr("Corte de Bonferroni (α/m)", "Bonferroni cutoff (α/m)"),
          tr("Falsos positivos com Bonferroni", "Bonferroni false positives")
        ),
        Valor = c(fmt_pct(s$fwer_teo, 1), as.character(s$falso_pos), fmt_num(s$alpha_bonf, 4), as.character(s$falso_pos_bonf)),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Item", "Item"), tr("Valor", "Value"))
      tabela_html(df, c("", "num"))
    })
  })
}
