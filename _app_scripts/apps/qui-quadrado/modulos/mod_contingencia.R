# =============================================================================
# Script:  mod_contingencia.R (Aba 2: Independência e homogeneidade)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Força da associação na tabela 3 × 3 e desenho (independência vs. homogeneidade)
# Saída:
#   - Mapa de calor das contribuições (O − E)²/E e resumo de χ², gl, valor-p e V
# =============================================================================

mod_contingencia_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::radioButtons(
        ns("desenho_tab"),
        tr("Desenho da coleta", "Sampling design"),
        choices = stats::setNames(
          c("indep", "homog"),
          c(tr("Independência (1 amostra de tamanho n)", "Independence (1 sample of size n)"),
            tr("Homogeneidade (margens fixadas por grupo)", "Homogeneity (fixed margins per group)"))
        )
      ),
      shiny::sliderInput(ns("forca_assoc"), tr("Intensidade da associação", "Association strength"), min = 0, max = 1, value = 0.6, step = 0.1),
      shiny::sliderInput(ns("n_tab"), tr("Total de observações n", "Total observations n"), min = 60, max = 300, value = 150, step = 30)
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header(tr("Contribuição de cada célula para χ²", "Each cell's contribution to χ²")),
        bslib::card_body(shiny::plotOutput(ns("grafico_tab"), height = "340px"))
      ),
      bslib::card(
        bslib::card_header(tr("Resultado do teste e leitura pelo desenho", "Test result and design interpretation")),
        bslib::card_body(
          shiny::uiOutput(ns("tabela_tab")),
          shiny::hr(),
          shiny::uiOutput(ns("texto_desenho"))
        )
      )
    )
  )
}

mod_contingencia_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    res <- shiny::reactive({
      shiny::req(is.finite(input$forca_assoc), is.finite(input$n_tab))
      p_base <- matrix(1 / 9, nrow = 3, ncol = 3)
      p_diag <- diag(0.25, 3) + matrix((1 - 0.75) / 6, nrow = 3, ncol = 3) - diag((1 - 0.75) / 6, 3)
      p_mix <- (1 - input$forca_assoc) * p_base + input$forca_assoc * p_diag
      mat <- round(input$n_tab * p_mix)
      linhas <- c(tr("Baixa", "Low"), tr("Média", "Medium"), tr("Alta", "High"))
      colunas <- c("C1", "C2", "C3")
      calcular_contingencia(mat, linhas, colunas)
    })

    output$grafico_tab <- shiny::renderPlot({
      r <- res()
      g <- r$grade
      p <- paleta_modo(modo())
      g$rotulo <- sprintf("O=%d\nE=%.1f", g$obs, g$esp)
      g$linha <- factor(g$linha, levels = rev(unique(g$linha)))

      ggplot2::ggplot(g, ggplot2::aes(x = coluna, y = linha, fill = contrib)) +
        ggplot2::geom_tile(colour = p$fundo, linewidth = 1.2) +
        ggplot2::geom_text(ggplot2::aes(label = rotulo), colour = "#ffffff", fontface = "bold", size = 3.8) +
        ggplot2::scale_fill_gradient(low = p$neutro, high = cores_app$referencia) +
        ggplot2::labs(
          title = sprintf("χ² = %s | gl = %d | valor-p = %s", fmt_num(r$chi2, 2), r$gl, fmt_num(r$valor_p, 4)),
          x = tr("Coluna", "Column"), y = tr("Linha", "Row"),
          caption = tr("Cor indica (O − E)²/E em cada célula", "Color shows (O − E)²/E in each cell")
        ) +
        tema_grafico(modo())
    })

    output$tabela_tab <- shiny::renderUI({
      r <- res()
      df <- data.frame(
        Medida = c("χ²", tr("Graus de liberdade", "Degrees of freedom"), tr("Valor-p", "p-value"), tr("V de Cramér", "Cramér's V"), tr("Menor Eᵢⱼ", "Smallest Eᵢⱼ")),
        Valor = c(fmt_num(r$chi2, 3), as.character(r$gl), fmt_num(r$valor_p, 4), fmt_num(r$cramer_v, 3), fmt_num(r$min_esp, 1)),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Medida", "Measure"), tr("Valor", "Value"))
      tabela_html(df, c("", "num"))
    })

    output$texto_desenho <- shiny::renderUI({
      txt <- if (identical(input$desenho_tab, "indep")) {
        tr(
          "Teste de independência: H₀ afirma que as duas variáveis categóricas são independentes na população (apenas n é fixado no desenho).",
          "Test of independence: H₀ states that the two categorical variables are independent in the population (only n is fixed by design)."
        )
      } else {
        tr(
          "Teste de homogeneidade: H₀ afirma que todas as populações (linhas) têm a mesma distribuição nas colunas. O cálculo de χ² é idêntico, mas o desenho fixa os totais de linha.",
          "Test of homogeneity: H₀ states that all populations (rows) share the same distribution across columns. The χ² calculation is identical, but the design fixes row totals."
        )
      }
      shiny::p(class = "mb-0", txt)
    })
  })
}
