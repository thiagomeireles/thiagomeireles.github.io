# =============================================================================
# Script:  mod_alem_anova.R (Para ir além: Partial Pooling / Gelman Cap. 5)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Desvio padrão entre grupos τ (controle da força do encolhimento)
# Saída:
#   - Comparação visual de sem pooling, pooling completo e agrupamento parcial
# =============================================================================

mod_alem_anova_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::sliderInput(ns("tau_pp"), tr("Heterogeneidade entre grupos τ", "Between-group heterogeneity τ"), min = 1, max = 15, value = 4, step = 0.5),
      shiny::actionButton(ns("sim_pp"), tr("Sortear 5 grupos desiguais", "Draw 5 unequal groups"), class = "btn-sm btn-primary mt-2")
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header(tr("Da ANOVA ao agrupamento parcial (Gelman et al., Cap. 5)", "From ANOVA to partial pooling (Gelman et al., Ch. 5)")),
        bslib::card_body(shiny::plotOutput(ns("grafico_pp"), height = "340px"))
      ),
      bslib::card(
        bslib::card_header(tr("Por que grupos pequenos encolhem mais", "Why small groups shrink more")),
        bslib::card_body(
          shiny::uiOutput(ns("tabela_pp")),
          shiny::hr(),
          shiny::p(class = "mb-0", tr(
            "Grupos com poucas observações (G1, n = 4) têm estimativas ruidosas e são puxados com mais força em direção à média geral. Grupos grandes (G5, n = 40) mantêm quase intacta sua média amostral.",
            "Small groups (G1, n = 4) have noisy estimates and are pulled more strongly toward the grand mean. Large groups (G5, n = 40) keep their sample mean nearly intact."
          ))
        )
      )
    )
  )
}

mod_alem_anova_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    df_pp <- shiny::reactive({
      input$sim_pp
      shiny::req(is.finite(input$tau_pp))
      simular_partial_pooling(input$tau_pp, 6)
    })

    output$grafico_pp <- shiny::renderPlot({
      df <- df_pp()
      p <- paleta_modo(modo())
      df_plot <- data.frame(
        grupo = rep(df$grupo, 2L),
        tipo = factor(
          c(rep(tr("Sem pooling (ȳᵢ)", "No pooling (ȳᵢ)"), 5L), rep(tr("Agrupamento parcial", "Partial pooling"), 5L)),
          levels = c(tr("Sem pooling (ȳᵢ)", "No pooling (ȳᵢ)"), tr("Agrupamento parcial", "Partial pooling"))
        ),
        valor = c(df$sem_pooling, df$parcial),
        stringsAsFactors = FALSE
      )

      ggplot2::ggplot(df_plot, ggplot2::aes(x = grupo, y = valor, colour = tipo)) +
        ggplot2::geom_hline(yintercept = df$geral[1], colour = cores_app$referencia, linetype = "dashed") +
        ggplot2::geom_point(size = 3.2, position = ggplot2::position_dodge(width = 0.3)) +
        ggplot2::scale_colour_manual(values = c(p$neutro, cores_app$destaque)) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = tr("Encolhimento (shrinkage) proporcional ao tamanho do grupo", "Shrinkage proportional to group size"),
          x = tr("Grupo (n = 4, 6, 12, 25, 40)", "Group (n = 4, 6, 12, 25, 40)"),
          y = tr("Estimativa da média", "Mean estimate"),
          colour = NULL
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(legend.position = "top")
    })

    output$tabela_pp <- shiny::renderUI({
      df <- df_pp()
      out <- data.frame(
        Grupo = as.character(df$grupo),
        N = as.character(df$n),
        Sem = fmt_num(df$sem_pooling, 2),
        Parcial = fmt_num(df$parcial, 2),
        stringsAsFactors = FALSE
      )
      names(out) <- c(tr("Grupo", "Group"), "n", tr("Sem pooling", "No pooling"), tr("Parcial", "Partial"))
      tabela_html(out, c("", "num", "num", "num"))
    })
  })
}
