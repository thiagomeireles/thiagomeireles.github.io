# =============================================================================
# Script:  mod_decomposicao_anova.R (Aba 1: Decomposição da variância)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Número de grupos, separação entre médias, dispersão interna e botão
# Saída:
#   - Box plots por grupo e tabela completa de ANOVA (Entre, Dentro, Total)
# =============================================================================

mod_decomposicao_anova_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::sliderInput(ns("k_grp"), tr("Número de grupos I", "Number of groups I"), min = 3, max = 5, value = 3, step = 1),
      shiny::sliderInput(ns("sep_m"), tr("Separação entre médias dos grupos", "Separation between group means"), min = 0, max = 8, value = 3, step = 0.5),
      shiny::sliderInput(ns("sd_d"), tr("Desvio padrão dentro dos grupos (σ_e)", "Within-group SD (σ_e)"), min = 2, max = 12, value = 4, step = 1),
      shiny::sliderInput(ns("n_grp"), tr("Tamanho por grupo (nᵢ)", "Size per group (nᵢ)"), min = 5, max = 30, value = 15, step = 5),
      shiny::actionButton(ns("sortear"), tr("Sortear novas amostras", "Draw new samples"), class = "btn-sm btn-primary mt-2")
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header(tr("Variação entre grupos vs. dentro dos grupos", "Between-group vs. within-group variation")),
        bslib::card_body(shiny::plotOutput(ns("grafico_anova"), height = "340px"))
      ),
      bslib::card(
        bslib::card_header(tr("Tabela de Análise de Variância (ANOVA)", "Analysis of Variance (ANOVA) table")),
        bslib::card_body(
          shiny::uiOutput(ns("tabela_anova")),
          shiny::hr(),
          shiny::uiOutput(ns("resumo_r2"))
        )
      )
    )
  )
}

mod_decomposicao_anova_server <- function(id, modo, anova_reativa) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      input$sortear
      shiny::req(is.finite(input$k_grp), is.finite(input$n_grp), is.finite(input$sep_m), is.finite(input$sd_d))
      anova_reativa(simular_anova(input$k_grp, input$n_grp, input$sep_m, input$sd_d))
    })

    output$grafico_anova <- shiny::renderPlot({
      an <- anova_reativa()
      shiny::req(an)
      p <- paleta_modo(modo())
      mu_g <- mean(an$df$y)

      ggplot2::ggplot(an$df, ggplot2::aes(x = grupo, y = y, fill = grupo)) +
        ggplot2::geom_boxplot(alpha = 0.65, width = 0.5) +
        ggplot2::geom_jitter(width = 0.12, alpha = 0.6, size = 1.6, colour = p$texto) +
        ggplot2::geom_hline(yintercept = mu_g, colour = cores_app$referencia, linetype = "dashed", linewidth = 0.9) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = sprintf("F = %s | valor-p = %s", fmt_num(an$f_val, 2), fmt_num(an$p_val, 4)),
          x = tr("Grupo", "Group"), y = "Y",
          caption = tr("Linha vermelha: média geral ȳ", "Red line: grand mean ȳ")
        ) +
        tema_grafico(modo())
    })

    output$tabela_anova <- shiny::renderUI({
      an <- anova_reativa()
      shiny::req(an)
      df <- data.frame(
        Fonte = c(tr("Entre grupos", "Between groups"), tr("Dentro (resíduo)", "Within (residual)"), tr("Total", "Total")),
        GL = c(as.character(an$gl_ent), as.character(an$gl_den), as.character(an$gl_ent + an$gl_den)),
        SQ = c(fmt_num(an$sq_ent, 2), fmt_num(an$sq_den, 2), fmt_num(an$sq_tot, 2)),
        QM = c(fmt_num(an$qm_ent, 2), fmt_num(an$qm_den, 2), "—"),
        Fval = c(fmt_num(an$f_val, 3), "—", "—"),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Fonte", "Source"), tr("gl", "df"), tr("SQ", "SS"), tr("QM", "MS"), "F")
      tabela_html(df, c("", "num", "num", "num", "num"))
    })

    output$resumo_r2 <- shiny::renderUI({
      an <- anova_reativa()
      shiny::req(an)
      shiny::p(class = "mb-0", sprintf(
        tr("R² = SQEntre / SQTotal = %s (%s da variação total de Y é explicada pelos grupos).", "R² = SSBetween / SSTotal = %s (%s of total Y variation is explained by groups)."),
        fmt_num(an$r2, 3), fmt_pct(an$r2, 1)
      ))
    })
  })
}
