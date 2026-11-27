# =============================================================================
# Script:  mod_desenhos.R (Aba 2: Desenhos amostrais)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Tamanho da amostra n_tot e botão de novo sorteio
# Saída:
#   - Comparação de AAS, Estratificada e Conglomerados na mesma população
# =============================================================================

mod_desenhos_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::sliderInput(ns("n_tot"), tr("Tamanho total da amostra n", "Total sample size n"), min = 60, max = 180, value = 90, step = 30),
      shiny::actionButton(ns("sortear"), tr("Simular 300 amostras por desenho", "Simulate 300 samples per design"), class = "btn-sm btn-primary mt-2")
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header(tr("Variabilidade de X̄ por desenho amostral", "Variability of X̄ by sampling design")),
        bslib::card_body(shiny::plotOutput(ns("grafico_desenhos"), height = "340px"))
      ),
      bslib::card(
        bslib::card_header(tr("Resumo comparativo e efeito de desenho", "Comparative summary and design effect")),
        bslib::card_body(
          shiny::uiOutput(ns("tabela_desenhos")),
          shiny::hr(),
          shiny::p(class = "mb-0", tr(
            "Estratificar reduz a variância quando os estratos são internamente homogêneos e diferentes entre si. Conglomerados reduzem custo de campo, mas aumentam a variância quando unidades do mesmo grupo se parecem (DEFF > 1).",
            "Stratification reduces variance when strata are internally homogeneous and differ from one another. Cluster sampling lowers field cost, but inflates variance when units in the same cluster resemble each other (DEFF > 1)."
          ))
        )
      )
    )
  )
}

mod_desenhos_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    pop <- criar_pop_desenhos()

    sim <- shiny::reactive({
      input$sortear
      shiny::req(is.finite(input$n_tot))
      simular_desenhos(pop, input$n_tot, 300L)
    })

    output$grafico_desenhos <- shiny::renderPlot({
      s <- sim()
      p <- paleta_modo(modo())
      rot <- c(
        tr("AAS", "SRS"),
        tr("Estratificada", "Stratified"),
        tr("Conglomerados", "Cluster")
      )
      df <- data.frame(
        desenho = factor(
          c(rep(rot[1], length(s$aas)), rep(rot[2], length(s$estratificada)), rep(rot[3], length(s$conglomerados))),
          levels = rot
        ),
        media = c(s$aas, s$estratificada, s$conglomerados),
        stringsAsFactors = FALSE
      )

      ggplot2::ggplot(df, ggplot2::aes(x = desenho, y = media, fill = desenho)) +
        ggplot2::geom_boxplot(alpha = 0.75, width = 0.5) +
        ggplot2::geom_hline(yintercept = s$mu, colour = cores_app$referencia, linetype = "dashed", linewidth = 0.9) +
        ggplot2::scale_fill_manual(values = c(p$neutro, cores_app$destaque, "#f59f00")) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = sprintf(tr("Distribuição das médias em 300 sorteios (n ≈ %d)", "Distribution of means across 300 draws (n ≈ %d)"), input$n_tot),
          subtitle = sprintf(tr("Conglomerados: %d grupos sorteados de 20 | Linha vermelha: μ = %s", "Clusters: %d sampled out of 20 | Red line: μ = %s"), s$m_congl, fmt_num(s$mu, 1)),
          x = NULL, y = "X̄"
        ) +
        tema_grafico(modo())
    })

    output$tabela_desenhos <- shiny::renderUI({
      s <- sim()
      v_aas <- stats::var(s$aas)
      v_est <- stats::var(s$estratificada)
      v_con <- stats::var(s$conglomerados)
      df <- data.frame(
        Desenho = c(tr("AAS", "SRS"), tr("Estratificada", "Stratified"), tr("Conglomerados", "Cluster")),
        Media = c(fmt_num(mean(s$aas), 2), fmt_num(mean(s$estratificada), 2), fmt_num(mean(s$conglomerados), 2)),
        EP = c(fmt_num(sqrt(v_aas), 3), fmt_num(sqrt(v_est), 3), fmt_num(sqrt(v_con), 3)),
        DEFF = c("1.00", fmt_num(v_est / v_aas, 2), fmt_num(v_con / v_aas, 2)),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Desenho", "Design"), tr("Média de X̄", "Mean of X̄"), tr("Erro padrão", "Standard error"), "DEFF")
      tabela_html(df, c("", "num", "num", "num"))
    })
  })
}
