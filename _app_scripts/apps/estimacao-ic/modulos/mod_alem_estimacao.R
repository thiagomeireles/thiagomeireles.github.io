# =============================================================================
# Script:  mod_alem_estimacao.R (Para ir além: MLE e intervalo credível Beta)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Dados (n, y) e parâmetros da priori Beta(α₀, β₀)
# Saída:
#   - Curva de verossimilhança com MLE e posteriori Beta com intervalo credível
# =============================================================================

mod_alem_estimacao_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 330,
      shiny::sliderInput(ns("n_bayes"), tr("Ensaios observados n", "Observed trials n"), min = 5, max = 100, value = 20, step = 5),
      shiny::sliderInput(ns("y_bayes"), tr("Sucessos observados y", "Observed successes y"), min = 0, max = 100, value = 6, step = 1),
      shiny::hr(),
      shiny::sliderInput(ns("alpha0"), tr("Priori Beta: α₀", "Beta prior: α₀"), min = 0.5, max = 20, value = 1, step = 0.5),
      shiny::sliderInput(ns("beta0"), tr("Priori Beta: β₀", "Beta prior: β₀"), min = 0.5, max = 20, value = 1, step = 0.5)
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header(tr("Verossimilhança (MLE) vs. Posteriori Beta(α₀ + y, β₀ + n − y)", "Likelihood (MLE) vs. Beta(α₀ + y, β₀ + n − y) Posterior")),
        bslib::card_body(shiny::plotOutput(ns("grafico_bayes_est"), height = "340px"))
      ),
      bslib::card(
        bslib::card_header(tr("Intervalo de confiança vs. intervalo credível", "Confidence interval vs. credible interval")),
        bslib::card_body(
          shiny::uiOutput(ns("tabela_bayes_est")),
          shiny::hr(),
          shiny::p(class = "mb-0", tr(
            "Sob priori Beta(α₀, β₀), a média a posteriori (α₀ + y)/(α₀ + β₀ + n) é um compromisso entre a priori e a MLE y/n. O intervalo credível de 95% tem interpretação direta: P(p ∈ [L, U] | y) = 0,95.",
            "Under a Beta(α₀, β₀) prior, the posterior mean (α₀ + y)/(α₀ + β₀ + n) compromises between the prior and the MLE y/n. The 95% credible interval has direct probability interpretation: P(p ∈ [L, U] | y) = 0.95."
          ))
        )
      )
    )
  )
}

mod_alem_estimacao_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$n_bayes, {
      shiny::req(is.finite(input$n_bayes))
      shiny::updateSliderInput(session, "y_bayes", max = input$n_bayes, value = min(input$y_bayes, input$n_bayes))
    })

    res <- shiny::reactive({
      shiny::req(is.finite(input$n_bayes), is.finite(input$y_bayes), is.finite(input$alpha0), is.finite(input$beta0))
      comparar_mle_bayes(input$n_bayes, min(input$y_bayes, input$n_bayes), input$alpha0, input$beta0, 0.95)
    })

    output$grafico_bayes_est <- shiny::renderPlot({
      r <- res()
      df <- r$df
      p <- paleta_modo(modo())
      escala_lik <- max(r$df$post)
      df_plot <- data.frame(
        p = rep(df$p, 3L),
        curva = factor(
          c(rep(tr("Priori", "Prior"), nrow(df)),
            rep(tr("Verossimilhança (reescalada)", "Likelihood (rescaled)"), nrow(df)),
            rep(tr("Posteriori", "Posterior"), nrow(df))),
          levels = c(tr("Priori", "Prior"), tr("Verossimilhança (reescalada)", "Likelihood (rescaled)"), tr("Posteriori", "Posterior"))
        ),
        y = c(df$priori, df$lik_norm * escala_lik, df$post),
        stringsAsFactors = FALSE
      )

      ggplot2::ggplot(df_plot, ggplot2::aes(x = p, y = y, colour = curva, linetype = curva)) +
        ggplot2::geom_line(linewidth = 1.0) +
        ggplot2::scale_colour_manual(values = c(p$neutro, cores_app$referencia, cores_app$destaque)) +
        ggplot2::scale_linetype_manual(values = c("dashed", "dotted", "solid")) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = sprintf("MLE = %s | E(p | y) = %s", fmt_num(r$mle, 3), fmt_num(r$media_post, 3)),
          x = "p", y = tr("Densidade", "Density"), colour = NULL, linetype = NULL
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(legend.position = "top")
    })

    output$tabela_bayes_est <- shiny::renderUI({
      r <- res()
      df <- data.frame(
        Abordagem = c(tr("Frequentista (MLE + Wald)", "Frequentist (MLE + Wald)"), tr("Bayesiana (Beta-Binomial)", "Bayesian (Beta-Binomial)")),
        Centro = c(fmt_num(r$mle, 4), fmt_num(r$media_post, 4)),
        IC95 = c(
          sprintf("[%s; %s]", fmt_num(r$ic_wald[1], 3), fmt_num(r$ic_wald[2], 3)),
          sprintf("[%s; %s]", fmt_num(r$ic_bayes[1], 3), fmt_num(r$ic_bayes[2], 3))
        ),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Abordagem", "Approach"), tr("Estimativa pontual", "Point estimate"), tr("Intervalo 95%", "95% Interval"))
      tabela_html(df, c("", "num", "num"))
    })
  })
}
