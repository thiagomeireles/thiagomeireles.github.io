# =============================================================================
# Script:  mod_alem_amostragem.R (Para ir além: coleta e ignorabilidade)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Taxa de não resposta seletiva (quando Y > 55) e tamanho amostral n
# Saída:
#   - Comparação de AAS ignorável vs. coleta não ignorável
# =============================================================================

mod_alem_amostragem_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 330,
      shiny::sliderInput(ns("n_nr"), tr("Tamanho da amostra inicial n", "Initial sample size n"), min = 50, max = 600, value = 300, step = 50),
      shiny::sliderInput(ns("vies_nr"), tr("Não resposta entre valores altos (Y > 55)", "Nonresponse among high values (Y > 55)"), min = 0.05, max = 0.90, value = 0.65, step = 0.05),
      shiny::actionButton(ns("sim_nr"), tr("Simular 300 pesquisas", "Simulate 300 surveys"), class = "btn-sm btn-primary mt-2")
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header(tr("Coleta ignorável vs. não ignorável (Gelman et al., Cap. 8)", "Ignorable vs. nonignorable collection (Gelman et al., Ch. 8)")),
        bslib::card_body(shiny::plotOutput(ns("grafico_nr"), height = "340px"))
      ),
      bslib::card(
        bslib::card_header(tr("Por que aumentar n não corrige viés de seleção", "Why larger n does not fix selection bias")),
        bslib::card_body(
          shiny::uiOutput(ns("tabela_nr")),
          shiny::hr(),
          shiny::p(class = "mb-0", tr(
            "Quando a ausência de resposta depende diretamente do próprio valor Y que queremos medir (mecanismo não ignorável), aumentar n apenas estreita a distribuição em torno do valor errado.",
            "When nonresponse depends directly on the very value Y we want to measure (nonignorable mechanism), increasing n merely narrows the distribution around the wrong value."
          ))
        )
      )
    )
  )
}

mod_alem_amostragem_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    sim <- shiny::reactive({
      input$sim_nr
      shiny::req(is.finite(input$n_nr), is.finite(input$vies_nr))
      simular_nao_ignoravel(input$n_nr, input$vies_nr, 300L)
    })

    output$grafico_nr <- shiny::renderPlot({
      s <- sim()
      p <- paleta_modo(modo())
      df <- data.frame(
        media = c(s$medias_aas, s$medias_nr),
        mecanismo = factor(
          c(rep(tr("AAS (ignorável)", "SRS (ignorable)"), length(s$medias_aas)),
            rep(tr("Não resposta seletiva", "Selective nonresponse"), length(s$medias_nr))),
          levels = c(tr("AAS (ignorável)", "SRS (ignorable)"), tr("Não resposta seletiva", "Selective nonresponse"))
        ),
        stringsAsFactors = FALSE
      )

      ggplot2::ggplot(df, ggplot2::aes(x = media, fill = mecanismo, colour = mecanismo)) +
        ggplot2::geom_density(alpha = 0.30, linewidth = 0.9) +
        ggplot2::geom_vline(xintercept = s$mu, colour = p$linha, linetype = "dashed", linewidth = 0.9) +
        ggplot2::scale_fill_manual(values = c(cores_app$destaque, cores_app$referencia)) +
        ggplot2::scale_colour_manual(values = c(cores_app$destaque, cores_app$referencia)) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = tr("Média amostral sob dois mecanismos de coleta", "Sample mean under two data collection mechanisms"),
          subtitle = sprintf(tr("Linha tracejada: parâmetro verdadeiro μ = %s", "Dashed line: true parameter μ = %s"), fmt_num(s$mu, 2)),
          x = "X̄", y = tr("Densidade", "Density"), fill = NULL, colour = NULL
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(legend.position = "top")
    })

    output$tabela_nr <- shiny::renderUI({
      s <- sim()
      df <- data.frame(
        Mecanismo = c(tr("AAS (ignorável)", "SRS (ignorable)"), tr("Não resposta seletiva", "Selective nonresponse")),
        Media = c(fmt_num(mean(s$medias_aas), 2), fmt_num(mean(s$medias_nr), 2)),
        Vies = c(fmt_num(mean(s$medias_aas) - s$mu, 2), fmt_num(mean(s$medias_nr) - s$mu, 2)),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Mecanismo", "Mechanism"), tr("Média de X̄", "Mean of X̄"), tr("Viés", "Bias"))
      tabela_html(df, c("", "num", "num"))
    })
  })
}
