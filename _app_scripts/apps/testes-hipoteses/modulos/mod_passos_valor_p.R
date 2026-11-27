# =============================================================================
# Script:  mod_passos_valor_p.R (Aba 1: Hipóteses, região crítica e valor-p)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Tipo de teste, H0, H1, estatística observada, n e nível α
# Saída:
#   - Distribuição sob H0 com região crítica, valor-p e decisão do teste
# =============================================================================

mod_passos_valor_p_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 330,
      shiny::selectInput(
        ns("tipo_t"),
        tr("Teste", "Test"),
        choices = stats::setNames(
          c("media_z", "media_t", "prop"),
          c(tr("Média (σ conhecido, Z)", "Mean (known σ, Z)"),
            tr("Média (σ desconhecido, t)", "Mean (unknown σ, t)"),
            tr("Proporção (Z)", "Proportion (Z)"))
        )
      ),
      shiny::radioButtons(
        ns("alt"),
        tr("Hipótese alternativa H₁", "Alternative hypothesis H₁"),
        choices = stats::setNames(c("bilateral", "maior", "menor"), c("≠ (bilateral)", "> (direita / right)", "< (esquerda / left)")),
        inline = TRUE
      ),
      shiny::sliderInput(ns("est_obs"), tr("Estimativa observada (x̄ ou p̂)", "Observed estimate (x̄ or p̂)"), min = 45, max = 58, value = 52.2, step = 0.2),
      shiny::sliderInput(ns("h0_val"), tr("Valor sob H₀ (μ₀ ou p₀)", "Null value H₀ (μ₀ or p₀)"), min = 48, max = 52, value = 50, step = 0.5),
      shiny::sliderInput(ns("disp"), tr("Desvio padrão (σ ou S)", "Standard deviation (σ or s)"), min = 1, max = 15, value = 6, step = 0.5),
      shiny::sliderInput(ns("n_t"), tr("Tamanho da amostra n", "Sample size n"), min = 5, max = 120, value = 30, step = 5),
      shiny::sliderInput(ns("alpha_t"), tr("Significância α", "Significance α"), min = 0.01, max = 0.10, value = 0.05, step = 0.01)
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header(tr("Distribuição da estatística sob H₀", "Test statistic distribution under H₀")),
        bslib::card_body(shiny::plotOutput(ns("grafico_h0"), height = "340px"))
      ),
      bslib::card(
        bslib::card_header(tr("Resultado dos 5 passos e valor-p", "5-step result and p-value")),
        bslib::card_body(
          shiny::uiOutput(ns("tabela_teste")),
          shiny::hr(),
          shiny::uiOutput(ns("decisao_teste"))
        )
      )
    )
  )
}

mod_passos_valor_p_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$tipo_t, {
      if (identical(input$tipo_t, "prop")) {
        shiny::updateSliderInput(session, "h0_val", min = 0.10, max = 0.90, value = 0.50, step = 0.05)
        shiny::updateSliderInput(session, "est_obs", min = 0.10, max = 0.90, value = 0.62, step = 0.01)
      } else {
        shiny::updateSliderInput(session, "h0_val", min = 48, max = 52, value = 50, step = 0.5)
        shiny::updateSliderInput(session, "est_obs", min = 45, max = 58, value = 52.2, step = 0.2)
      }
    }, ignoreInit = TRUE)

    res <- shiny::reactive({
      shiny::req(
        input$tipo_t, input$alt,
        is.finite(input$est_obs), is.finite(input$h0_val),
        is.finite(input$disp), is.finite(input$n_t), is.finite(input$alpha_t)
      )
      calcular_teste_hipotese(input$tipo_t, input$est_obs, input$h0_val, input$disp, input$n_t, input$alt, input$alpha_t)
    })

    output$grafico_h0 <- shiny::renderPlot({
      r <- res()
      p <- paleta_modo(modo())
      xs <- seq(-4.2, 4.2, length.out = 400L)
      ys <- if (r$tipo == "media_t") stats::dt(xs, df = r$gl) else stats::dnorm(xs)
      df <- data.frame(x = xs, y = ys)

      g <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_line(colour = cores_app$destaque, linewidth = 1.0)

      if (!is.na(r$crit[1])) {
        df_esq <- df[df$x <= r$crit[1], , drop = FALSE]
        g <- g + ggplot2::geom_area(data = df_esq, fill = cores_app$referencia, alpha = 0.35)
      }
      if (!is.na(r$crit[2])) {
        df_dir <- df[df$x >= r$crit[2], , drop = FALSE]
        g <- g + ggplot2::geom_area(data = df_dir, fill = cores_app$referencia, alpha = 0.35)
      }

      stat_lim <- min(max(r$stat, -4.1), 4.1)
      g +
        ggplot2::geom_vline(xintercept = stat_lim, colour = p$linha, linetype = "dashed", linewidth = 1.0) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = sprintf(tr("Estatística observada = %s | valor-p = %s", "Observed statistic = %s | p-value = %s"), fmt_num(r$stat, 3), fmt_num(r$valor_p, 4)),
          x = if (r$tipo == "media_t") sprintf("t(%d)", r$gl) else "Z",
          y = tr("Densidade", "Density"),
          caption = tr("Área vermelha: região crítica α | Linha tracejada: estatística observada", "Red area: critical region α | Dashed line: observed statistic")
        ) +
        tema_grafico(modo())
    })

    output$tabela_teste <- shiny::renderUI({
      r <- res()
      crit_txt <- if (r$alt == "bilateral") {
        sprintf("±%s", fmt_num(r$crit[2], 3))
      } else if (r$alt == "maior") {
        fmt_num(r$crit[2], 3)
      } else {
        fmt_num(r$crit[1], 3)
      }
      df <- data.frame(
        Item = c(
          tr("Erro padrão sob H₀", "Standard error under H₀"),
          tr("Estatística observada", "Observed statistic"),
          tr("Valor crítico (α)", "Critical value (α)"),
          tr("Valor-p", "p-value")
        ),
        Valor = c(fmt_num(r$ep, 4), fmt_num(r$stat, 4), crit_txt, fmt_num(r$valor_p, 4)),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Item", "Item"), tr("Valor", "Value"))
      tabela_html(df, c("", "num"))
    })

    output$decisao_teste <- shiny::renderUI({
      r <- res()
      txt <- if (r$rejeita) {
        sprintf(
          tr("Decisão (α = %s): rejeitar H₀, pois valor-p (%s) ≤ α.", "Decision (α = %s): reject H₀, since p-value (%s) ≤ α."),
          fmt_num(input$alpha_t, 2), fmt_num(r$valor_p, 4)
        )
      } else {
        sprintf(
          tr("Decisão (α = %s): não rejeitar H₀, pois valor-p (%s) > α.", "Decision (α = %s): do not reject H₀, since p-value (%s) > α."),
          fmt_num(input$alpha_t, 2), fmt_num(r$valor_p, 4)
        )
      }
      shiny::p(class = "mb-0 fw-semibold", txt)
    })
  })
}
