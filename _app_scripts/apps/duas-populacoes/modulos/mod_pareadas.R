# =============================================================================
# Script:  mod_pareadas.R (Aba 2: Amostras pareadas)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Correlação entre pares, diferença média δ e tamanho n
# Saída:
#   - Gráfico de pares ligados (antes/depois) e comparação pareado vs. indep.
# =============================================================================

mod_pareadas_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::sliderInput(ns("diff_par"), tr("Diferença média δ = μ₁ − μ₂", "Mean difference δ = μ₁ − μ₂"), min = 0, max = 6, value = 2.5, step = 0.5),
      shiny::sliderInput(ns("corr_par"), tr("Correlação entre pares ρ", "Within-pair correlation ρ"), min = 0.0, max = 0.95, value = 0.80, step = 0.05),
      shiny::sliderInput(ns("n_par"), tr("Número de pares n", "Number of pairs n"), min = 10, max = 60, value = 25, step = 5),
      shiny::actionButton(ns("sortear"), tr("Sortear novos pares", "Draw new pairs"), class = "btn-sm btn-primary mt-2")
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header(tr("Mesmas unidades medidas antes e depois", "Same units measured before and after")),
        bslib::card_body(shiny::plotOutput(ns("grafico_par"), height = "340px"))
      ),
      bslib::card(
        bslib::card_header(tr("Ganho de precisão ao parear", "Precision gain from pairing")),
        bslib::card_body(
          shiny::uiOutput(ns("tabela_par")),
          shiny::hr(),
          shiny::p(class = "mb-0", tr(
            "Quando os pares são positivamente correlacionados, trabalhar com Dᵢ = Xᵢ − Yᵢ elimina a variação entre indivíduos, reduz o erro padrão e aumenta o poder do teste.",
            "When pairs are positively correlated, working with Dᵢ = Xᵢ − Yᵢ removes between-subject variation, shrinks the standard error and increases test power."
          ))
        )
      )
    )
  )
}

mod_pareadas_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    sim <- shiny::reactive({
      input$sortear
      shiny::req(is.finite(input$n_par), is.finite(input$diff_par), is.finite(input$corr_par))
      simular_pareado_dados(input$n_par, input$diff_par, input$corr_par)
    })

    output$grafico_par <- shiny::renderPlot({
      s <- sim()
      p <- paleta_modo(modo())
      n <- length(s$x)
      df_seg <- data.frame(id = seq_len(n), y_antes = s$y, y_depois = s$x)
      df_pts <- data.frame(
        etapa = factor(
          c(rep(tr("Antes (Y)", "Before (Y)"), n), rep(tr("Depois (X)", "After (X)"), n)),
          levels = c(tr("Antes (Y)", "Before (Y)"), tr("Depois (X)", "After (X)"))
        ),
        valor = c(s$y, s$x),
        stringsAsFactors = FALSE
      )

      ggplot2::ggplot() +
        ggplot2::geom_segment(
          data = df_seg,
          ggplot2::aes(x = 1, xend = 2, y = y_antes, yend = y_depois),
          colour = p$neutro, alpha = 0.65
        ) +
        ggplot2::geom_point(data = df_pts, ggplot2::aes(x = as.numeric(etapa), y = valor, colour = etapa), size = 2.2) +
        ggplot2::scale_x_continuous(breaks = c(1, 2), labels = levels(df_pts$etapa), limits = c(0.7, 2.3)) +
        ggplot2::scale_colour_manual(values = c(p$secundario, cores_app$destaque)) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = sprintf("D̄ = %s | S_D = %s", fmt_num(mean(s$d), 2), fmt_num(stats::sd(s$d), 2)),
          x = NULL, y = tr("Medida", "Measurement")
        ) +
        tema_grafico(modo())
    })

    output$tabela_par <- shiny::renderUI({
      s <- sim()
      df <- data.frame(
        Analise = c(tr("t pareado (correto)", "Paired t (correct)"), tr("t independente (ignora pareamento)", "Independent t (ignores pairing)")),
        EP = c(fmt_num(s$t_par$stderr, 3), fmt_num(s$t_ind$stderr, 3)),
        Estat = c(fmt_num(unname(s$t_par$statistic), 3), fmt_num(unname(s$t_ind$statistic), 3)),
        ValorP = c(fmt_num(s$t_par$p.value, 4), fmt_num(s$t_ind$p.value, 4)),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Análise", "Analysis"), tr("Erro padrão", "Std. error"), "t", tr("Valor-p", "p-value"))
      tabela_html(df, c("", "num", "num", "num"))
    })
  })
}
