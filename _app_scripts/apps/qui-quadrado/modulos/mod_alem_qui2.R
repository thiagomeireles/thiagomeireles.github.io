# =============================================================================
# Script:  mod_alem_qui2.R (Para ir além: Qui-quadrado vs. Teste de Fisher)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Contagens de uma tabela 2 × 2 com células pequenas
# Saída:
#   - Comparação de valor-p entre χ² (sem correção), Yates e Exato de Fisher
# =============================================================================

mod_alem_qui2_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::sliderInput(ns("c_a"), tr("Célula (1, 1)", "Cell (1, 1)"), min = 0, max = 20, value = 1, step = 1),
      shiny::sliderInput(ns("c_b"), tr("Célula (1, 2)", "Cell (1, 2)"), min = 0, max = 20, value = 7, step = 1),
      shiny::sliderInput(ns("c_c"), tr("Célula (2, 1)", "Cell (2, 1)"), min = 0, max = 20, value = 6, step = 1),
      shiny::sliderInput(ns("c_d"), tr("Célula (2, 2)", "Cell (2, 2)"), min = 0, max = 20, value = 2, step = 1)
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header(tr("Amostras pequenas em tabela 2 × 2", "Small counts in a 2 × 2 table")),
        bslib::card_body(shiny::uiOutput(ns("tabela_fisher")))
      ),
      bslib::card(
        bslib::card_header(tr("Quando a aproximação χ² deixa de valer", "When the χ² approximation breaks down")),
        bslib::card_body(
          shiny::uiOutput(ns("alerta_fisher")),
          shiny::hr(),
          shiny::p(class = "mb-0", tr(
            "Quando alguma frequência esperada fica abaixo de 5, a distribuição assintótica χ² pode divergir da distribuição discreta exata. O teste exato de Fisher condiciona nas margens e usa a hipergeométrica.",
            "When any expected frequency falls below 5, the asymptotic χ² distribution can diverge from the exact discrete distribution. Fisher's exact test conditions on the margins via the hypergeometric."
          ))
        )
      )
    )
  )
}

mod_alem_qui2_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    res <- shiny::reactive({
      shiny::req(is.finite(input$c_a), is.finite(input$c_b), is.finite(input$c_c), is.finite(input$c_d))
      shiny::req((input$c_a + input$c_b) > 0, (input$c_c + input$c_d) > 0, (input$c_a + input$c_c) > 0, (input$c_b + input$c_d) > 0)
      comparar_fisher_qui2(input$c_a, input$c_b, input$c_c, input$c_d)
    })

    output$tabela_fisher <- shiny::renderUI({
      r <- res()
      df <- data.frame(
        Teste = c(
          tr("χ² sem correção (Morettin & Bussab)", "χ² uncorrected (Morettin & Bussab)"),
          tr("χ² com correção de Yates (padrão R)", "χ² with Yates' correction (R default)"),
          tr("Teste exato de Fisher", "Fisher's exact test")
        ),
        ValorP = c(fmt_num(r$p_qui2, 4), fmt_num(r$p_yates, 4), fmt_num(r$p_fisher, 4)),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Método", "Method"), tr("Valor-p", "p-value"))
      tabela_html(df, c("", "num"))
    })

    output$alerta_fisher <- shiny::renderUI({
      r <- res()
      msg <- if (r$min_esp < 5) {
        sprintf(tr("Atenção: menor frequência esperada = %s (< 5). Prefira o Teste Exato de Fisher.", "Warning: smallest expected count = %s (< 5). Prefer Fisher's exact test."), fmt_num(r$min_esp, 2))
      } else {
        sprintf(tr("Menor frequência esperada = %s (≥ 5): aproximação χ² adequada.", "Smallest expected count = %s (≥ 5): χ² approximation is adequate."), fmt_num(r$min_esp, 2))
      }
      shiny::p(class = "mb-0 fw-semibold", msg)
    })
  })
}
