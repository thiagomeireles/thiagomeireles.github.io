# =============================================================================
# Script:  mod_independentes.R (Aba 1: Amostras independentes)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Tamanhos n1, n2, diferença de médias e desvios padrão dos dois grupos
# Saída:
#   - Box plots lado a lado e comparação de F, t (pooled), Welch e Wilcoxon
# =============================================================================

mod_independentes_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::sliderInput(ns("diff_mu"), tr("Diferença real μ₁ − μ₂", "True difference μ₁ − μ₂"), min = 0, max = 8, value = 3, step = 0.5),
      shiny::sliderInput(ns("sd1"), tr("Desvio padrão grupo 1 (σ₁)", "Group 1 SD (σ₁)"), min = 2, max = 12, value = 5, step = 1),
      shiny::sliderInput(ns("sd2"), tr("Desvio padrão grupo 2 (σ₂)", "Group 2 SD (σ₂)"), min = 2, max = 12, value = 5, step = 1),
      shiny::sliderInput(ns("n_ind"), tr("Tamanho por grupo (n₁ = n₂)", "Sample size per group (n₁ = n₂)"), min = 10, max = 80, value = 25, step = 5),
      shiny::actionButton(ns("sortear"), tr("Sortear novas amostras", "Draw new samples"), class = "btn-sm btn-primary mt-2")
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header(tr("Comparação dos dois grupos", "Comparison of the two groups")),
        bslib::card_body(shiny::plotOutput(ns("grafico_ind"), height = "340px"))
      ),
      bslib::card(
        bslib::card_header(tr("Testes F, t (pooled), Welch e Mann-Whitney", "F, t (pooled), Welch and Mann-Whitney tests")),
        bslib::card_body(
          shiny::uiOutput(ns("tabela_ind")),
          shiny::hr(),
          shiny::p(class = "mb-0", tr(
            "Nota: o R usa graus de liberdade fracionários no teste de Welch; Morettin & Bussab arredondam ν para o inteiro mais próximo. O teste F é sensível à não normalidade.",
            "Note: R reports fractional degrees of freedom for Welch's test; Morettin & Bussab round ν to the nearest integer. The F test is sensitive to non-normality."
          ))
        )
      )
    )
  )
}

mod_independentes_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    sim <- shiny::reactive({
      input$sortear
      shiny::req(is.finite(input$n_ind), is.finite(input$diff_mu), is.finite(input$sd1), is.finite(input$sd2))
      simular_indep(input$n_ind, input$n_ind, input$diff_mu, input$sd1, input$sd2)
    })

    output$grafico_ind <- shiny::renderPlot({
      s <- sim()
      p <- paleta_modo(modo())
      rot <- c(tr("Grupo 1", "Group 1"), tr("Grupo 2", "Group 2"))
      df <- data.frame(
        grupo = factor(c(rep(rot[1], length(s$x)), rep(rot[2], length(s$y))), levels = rot),
        valor = c(s$x, s$y),
        stringsAsFactors = FALSE
      )

      ggplot2::ggplot(df, ggplot2::aes(x = grupo, y = valor, fill = grupo)) +
        ggplot2::geom_boxplot(alpha = 0.72, width = 0.48) +
        ggplot2::scale_fill_manual(values = c(cores_app$destaque, p$neutro)) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = sprintf("X̄₁ − X̄₂ = %s", fmt_num(mean(s$x) - mean(s$y), 2)),
          x = NULL, y = tr("Valor observado", "Observed value")
        ) +
        tema_grafico(modo())
    })

    output$tabela_ind <- shiny::renderUI({
      s <- sim()
      df <- data.frame(
        Teste = c(
          tr("F (variâncias)", "F (variances)"),
          tr("t (variâncias iguais)", "t (equal variances)"),
          "t Welch",
          "Wilcoxon / Mann-Whitney"
        ),
        Estat = c(
          fmt_num(unname(s$f_res$statistic), 3),
          fmt_num(unname(s$t_eq$statistic), 3),
          fmt_num(unname(s$t_we$statistic), 3),
          fmt_num(unname(s$w_res$statistic), 1)
        ),
        GL = c(
          sprintf("%d, %d", s$f_res$parameter[1], s$f_res$parameter[2]),
          fmt_num(s$t_eq$parameter, 0),
          fmt_num(s$t_we$parameter, 1),
          "—"
        ),
        ValorP = c(
          fmt_num(s$f_res$p.value, 4),
          fmt_num(s$t_eq$p.value, 4),
          fmt_num(s$t_we$p.value, 4),
          fmt_num(s$w_res$p.value, 4)
        ),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Teste", "Test"), tr("Estatística", "Statistic"), tr("gl", "df"), tr("Valor-p", "p-value"))
      tabela_html(df, c("", "num", "num", "num"))
    })
  })
}
