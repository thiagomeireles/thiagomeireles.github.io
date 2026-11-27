# =============================================================================
# Script:  mod_propriedades.R (Aba 1: Propriedades de estimadores)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Tamanho amostral n, desvio padrão σ e botão de novo sorteio
# Saída:
#   - Comparação de eficiência (média vs. mediana) e viés/EQM (S² vs. σ̂²)
# =============================================================================

mod_propriedades_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::sliderInput(ns("n_est"), tr("Tamanho da amostra n", "Sample size n"), min = 5, max = 80, value = 20, step = 5),
      shiny::sliderInput(ns("sigma_est"), tr("Desvio padrão σ", "Standard deviation σ"), min = 1, max = 15, value = 5, step = 1),
      shiny::actionButton(ns("sortear"), tr("Simular 500 amostras", "Simulate 500 samples"), class = "btn-sm btn-primary mt-2")
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header(tr("Eficiência: média amostral X̄ vs. mediana (μ = 50)", "Efficiency: sample mean X̄ vs. median (μ = 50)")),
        bslib::card_body(shiny::plotOutput(ns("grafico_est"), height = "340px"))
      ),
      bslib::card(
        bslib::card_header(tr("Viés, variância e EQM = Var + Viés²", "Bias, variance and MSE = Var + Bias²")),
        bslib::card_body(
          shiny::uiOutput(ns("tabela_est")),
          shiny::hr(),
          shiny::p(class = "mb-0", tr(
            "Na distribuição normal, X̄ e a mediana são não viesados para μ, mas X̄ tem menor variância (mais eficiente). Já na variância amostral, o divisor n tem viés negativo, mas pode apresentar EQM menor que o divisor n − 1.",
            "Under normality, both X̄ and the median are unbiased for μ, but X̄ has smaller variance (higher efficiency). For sample variance, divisor n has negative bias, yet can yield a smaller MSE than divisor n − 1."
          ))
        )
      )
    )
  )
}

mod_propriedades_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    sim <- shiny::reactive({
      input$sortear
      shiny::req(is.finite(input$n_est), is.finite(input$sigma_est))
      simular_propriedades(input$n_est, input$sigma_est, 500L)
    })

    output$grafico_est <- shiny::renderPlot({
      s <- sim()
      p <- paleta_modo(modo())
      rot <- c(tr("Média X̄", "Mean X̄"), tr("Mediana", "Median"))
      df <- data.frame(
        estimativa = c(s$medias, s$medianas),
        estimador = factor(
          c(rep(rot[1], length(s$medias)), rep(rot[2], length(s$medianas))),
          levels = rot
        ),
        stringsAsFactors = FALSE
      )

      ggplot2::ggplot(df, ggplot2::aes(x = estimativa, fill = estimador, colour = estimador)) +
        ggplot2::geom_density(alpha = 0.28, linewidth = 0.9) +
        ggplot2::geom_vline(xintercept = s$mu, colour = p$linha, linetype = "dashed") +
        ggplot2::scale_fill_manual(values = c(cores_app$destaque, cores_app$referencia)) +
        ggplot2::scale_colour_manual(values = c(cores_app$destaque, cores_app$referencia)) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = sprintf(tr("Distribuição amostral de dois estimadores de μ (n = %d)", "Sampling distribution of two estimators of μ (n = %d)"), input$n_est),
          x = tr("Estimativa", "Estimate"), y = tr("Densidade", "Density"), fill = NULL, colour = NULL
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(legend.position = "top")
    })

    output$tabela_est <- shiny::renderUI({
      s <- sim()
      df <- data.frame(
        Estimador = c("X̄ (μ = 50)", tr("Mediana (μ = 50)", "Median (μ = 50)"), "S² [n − 1]", "σ̂² [n]"),
        Vies = c(fmt_num(s$res_media["vies"], 3), fmt_num(s$res_mediana["vies"], 3), fmt_num(s$res_s2_n1["vies"], 2), fmt_num(s$res_s2_n["vies"], 2)),
        Var = c(fmt_num(s$res_media["var"], 3), fmt_num(s$res_mediana["var"], 3), fmt_num(s$res_s2_n1["var"], 2), fmt_num(s$res_s2_n["var"], 2)),
        EQM = c(fmt_num(s$res_media["eqm"], 3), fmt_num(s$res_mediana["eqm"], 3), fmt_num(s$res_s2_n1["eqm"], 2), fmt_num(s$res_s2_n["eqm"], 2)),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Estimador", "Estimator"), tr("Viés", "Bias"), tr("Variância", "Variance"), tr("EQM", "MSE"))
      tabela_html(df, c("", "num", "num", "num"))
    })
  })
}
