# =============================================================================
# Script:  mod_conjunta.R (Aba 1: Conjunta, marginais e independência)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Preset da conjunta e peso de mistura com o produto das marginais
# Saída:
#   - Mapa de calor da distribuição conjunta p(x, y), marginais e teste de
#     independência célula a célula
# =============================================================================

mod_conjunta_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::selectInput(
        ns("caso"),
        tr("Distribuição conjunta de partida", "Starting joint distribution"),
        choices = stats::setNames(
          c("dependente", "independente", "cov_nula_dep"),
          c(
            tr("Associação positiva (3 × 3)", "Positive association (3 × 3)"),
            tr("Variáveis independentes (3 × 3)", "Independent variables (3 × 3)"),
            tr("Covariância nula, mas dependentes (Y = X²)", "Zero covariance, yet dependent (Y = X²)")
          )
        )
      ),
      shiny::sliderInput(
        ns("mistura_indep"),
        tr("Aproximar de p_X(x)·p_Y(y) (0 = original, 1 = indep.)", "Blend toward p_X(x)·p_Y(y) (0 = original, 1 = indep.)"),
        min = 0, max = 1, value = 0, step = 0.1
      )
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header(tr("Distribuição conjunta p(x, y) e marginais", "Joint distribution p(x, y) and marginals")),
        bslib::card_body(shiny::plotOutput(ns("grafico_conjunta"), height = "340px"))
      ),
      bslib::card(
        bslib::card_header(tr("Verificação de independência e momentos", "Independence check and moments")),
        bslib::card_body(
          shiny::uiOutput(ns("tabela_resumo")),
          shiny::hr(),
          shiny::uiOutput(ns("diagnostico_indep"))
        )
      )
    )
  )
}

mod_conjunta_server <- function(id, modo, conjunta_reativa) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      shiny::req(input$caso, is.finite(input$mistura_indep))
      base_p <- presets_conjunta(input$caso)
      px <- rowSums(base_p$mat)
      py <- colSums(base_p$mat)
      mat_mista <- (1 - input$mistura_indep) * base_p$mat + input$mistura_indep * outer(px, py)
      conjunta_reativa(analisar_conjunta(mat_mista, base_p$xs, base_p$ys))
    })

    output$grafico_conjunta <- shiny::renderPlot({
      an <- conjunta_reativa()
      shiny::req(an)
      p <- paleta_modo(modo())
      g <- an$grade
      g$rotulo <- paste0(fmt_num(g$p, 3), "\n(", fmt_num(g$p_indep, 3), ")")

      ggplot2::ggplot(g, ggplot2::aes(x = factor(x), y = factor(y), fill = p)) +
        ggplot2::geom_tile(colour = p$fundo, linewidth = 1.2) +
        ggplot2::geom_text(ggplot2::aes(label = rotulo), colour = "#ffffff", fontface = "bold", size = 3.8) +
        ggplot2::scale_fill_gradient(low = p$neutro, high = cores_app$destaque) +
        ggplot2::labs(
          title = tr("Células: p(x, y) e, entre parênteses, p_X(x)·p_Y(y)", "Cells: p(x, y) and, in parentheses, p_X(x)·p_Y(y)"),
          x = "X", y = "Y"
        ) +
        tema_grafico(modo())
    })

    output$tabela_resumo <- shiny::renderUI({
      an <- conjunta_reativa()
      shiny::req(an)
      df <- data.frame(
        Medida = c("E(X)", "E(Y)", "Var(X)", "Var(Y)", "Cov(X, Y)", "ρ(X, Y)", tr("Desvio máx. |p − p_X·p_Y|", "Max deviation |p − p_X·p_Y|")),
        Valor = c(
          fmt_num(an$ex, 3), fmt_num(an$ey, 3),
          fmt_num(an$vx, 3), fmt_num(an$vy, 3),
          fmt_num(an$cov_xy, 4), fmt_num(an$rho, 4),
          fmt_num(an$desvio_max, 4)
        ),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Medida", "Measure"), tr("Valor", "Value"))
      tabela_html(df, c("", "num"))
    })

    output$diagnostico_indep <- shiny::renderUI({
      an <- conjunta_reativa()
      shiny::req(an)
      txt <- if (an$independentes) {
        tr(
          "Independentes: p(x, y) = p_X(x)·p_Y(y) em todas as células, logo Cov(X, Y) = 0.",
          "Independent: p(x, y) = p_X(x)·p_Y(y) in every cell, hence Cov(X, Y) = 0."
        )
      } else if (abs(an$cov_xy) < 1e-6) {
        tr(
          "Covariância nula, mas NÃO independentes: há células com p(x, y) ≠ p_X(x)·p_Y(y). Independência exige a fatoração completa, não apenas ρ = 0.",
          "Zero covariance, yet NOT independent: some cells have p(x, y) ≠ p_X(x)·p_Y(y). Independence requires full factorization, not just ρ = 0."
        )
      } else {
        tr(
          "Dependentes: ao menos uma célula viola p(x, y) = p_X(x)·p_Y(y).",
          "Dependent: at least one cell violates p(x, y) = p_X(x)·p_Y(y)."
        )
      }
      shiny::p(class = "mb-0", txt)
    })
  })
}
