# =============================================================================
# Script:  mod_fp_fda.R (Aba 1: Função de probabilidade, FDA e momentos)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Seleção de distribuição discreta e parâmetros da transformação linear aX + b
# Saída:
#   - Gráficos da FP p(x) e da FDA em escada F(x) e tabela de momentos
# =============================================================================

mod_fp_fda_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::selectInput(
        ns("modelo"),
        tr("Distribuição de partida", "Starting distribution"),
        choices = stats::setNames(
          c("gss", "dado", "assim"),
          c(
            tr("Nº ideal de filhos (Agresti, Ex. 4.1)", "Ideal number of children (Agresti, Ex. 4.1)"),
            tr("Uniforme discreta {1, ..., 6} (dado)", "Discrete uniform {1, ..., 6} (die)"),
            tr("Assimétrica à direita {0, ..., 4}", "Right-skewed {0, ..., 4}")
          )
        )
      ),
      shiny::hr(),
      shiny::sliderInput(ns("a_lin"), tr("Multiplicador a em Y = aX + b", "Multiplier a in Y = aX + b"), min = -3, max = 3, value = 2, step = 0.5),
      shiny::sliderInput(ns("b_lin"), tr("Deslocamento b em Y = aX + b", "Shift b in Y = aX + b"), min = -5, max = 5, value = 1, step = 0.5)
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header(tr("Função de probabilidade p(x) = P(X = x)", "Probability mass function p(x) = P(X = x)")),
        bslib::card_body(shiny::plotOutput(ns("grafico_fp"), height = "310px"))
      ),
      bslib::card(
        bslib::card_header(tr("Função de distribuição acumulada F(x) = P(X ≤ x)", "Cumulative distribution function F(x) = P(X ≤ x)")),
        bslib::card_body(shiny::plotOutput(ns("grafico_fda"), height = "310px"))
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header(tr("Momentos e transformação linear Y = aX + b", "Moments and linear transformation Y = aX + b")),
        bslib::card_body(shiny::uiOutput(ns("tabela_momentos")))
      ),
      bslib::card(
        bslib::card_header(rotulo_entenda()),
        bslib::card_body(
          shiny::p(tr(
            "A função de distribuição F(x) = P(X ≤ x) é uma escada contínua à direita: em cada valor xᵢ com probabilidade positiva, F(x) dá um salto de altura p(xᵢ). Somar uma constante b desloca E(X) sem alterar a dispersão; multiplicar por a multiplica E(X) por a e a variância por a².",
            "The CDF F(x) = P(X ≤ x) is a right-continuous step function: at each value xᵢ with positive mass, F(x) jumps by p(xᵢ). Adding a constant b shifts E(X) without changing spread; multiplying by a scales E(X) by a and the variance by a²."
          ))
        )
      )
    )
  )
}

mod_fp_fda_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    dados_mom <- shiny::reactive({
      shiny::req(input$modelo)
      if (identical(input$modelo, "gss")) {
        momentos_discreta(0:5, c(0.01, 0.03, 0.60, 0.23, 0.12, 0.01))
      } else if (identical(input$modelo, "dado")) {
        momentos_discreta(1:6, rep(1 / 6, 6))
      } else {
        momentos_discreta(0:4, c(0.45, 0.28, 0.15, 0.08, 0.04))
      }
    })

    output$grafico_fp <- shiny::renderPlot({
      m <- dados_mom()
      p <- paleta_modo(modo())
      df <- data.frame(x = m$x, prob = m$prob)
      ggplot2::ggplot(df, ggplot2::aes(x = x, y = prob)) +
        ggplot2::geom_col(fill = cores_app$destaque, width = 0.55) +
        ggplot2::geom_vline(xintercept = m$media, colour = cores_app$referencia, linetype = "dashed", linewidth = 0.9) +
        ggplot2::scale_x_continuous(breaks = m$x) +
        ggplot2::scale_y_continuous(limits = c(0, max(m$prob) * 1.18), labels = rotulos_eixo()) +
        ggplot2::labs(
          title = tr("Massa de probabilidade", "Probability mass"),
          subtitle = sprintf("E(X) = %s | DP(X) = %s", fmt_num(m$media, 2), fmt_num(m$dp, 2)),
          x = "x", y = "p(x)",
          caption = tr("Linha tracejada: E(X)", "Dashed line: E(X)")
        ) +
        tema_grafico(modo())
    })

    output$grafico_fda <- shiny::renderPlot({
      m <- dados_mom()
      p <- paleta_modo(modo())
      xs <- m$x
      fs <- m$fda
      n <- length(xs)
      segs <- data.frame(
        x = c(min(xs) - 1, xs),
        xend = c(xs, max(xs) + 1),
        y = c(0, fs),
        yend = c(0, fs)
      )
      pontos_abertos <- data.frame(x = xs, y = c(0, fs[-n]))
      pontos_fechados <- data.frame(x = xs, y = fs)

      ggplot2::ggplot() +
        ggplot2::geom_segment(
          data = segs,
          ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
          colour = cores_app$destaque, linewidth = 1
        ) +
        ggplot2::geom_point(data = pontos_fechados, ggplot2::aes(x = x, y = y), colour = cores_app$destaque, size = 2.8) +
        ggplot2::geom_point(data = pontos_abertos, ggplot2::aes(x = x, y = y), colour = cores_app$destaque, fill = p$fundo, shape = 21, size = 2.8, stroke = 1) +
        ggplot2::scale_x_continuous(breaks = xs) +
        ggplot2::scale_y_continuous(limits = c(0, 1.05), labels = rotulos_eixo()) +
        ggplot2::labs(
          title = tr("Escada da FDA (contínua à direita)", "Step CDF (right-continuous)"),
          x = "x", y = "F(x)"
        ) +
        tema_grafico(modo())
    })

    output$tabela_momentos <- shiny::renderUI({
      shiny::req(is.finite(input$a_lin), is.finite(input$b_lin))
      m <- dados_mom()
      trf <- transformar_linear(m, input$a_lin, input$b_lin)
      df <- data.frame(
        Variavel = c("X", sprintf("Y = %s·X + (%s)", fmt_num(input$a_lin, 1), fmt_num(input$b_lin, 1))),
        Media = c(fmt_num(m$media, 4), fmt_num(trf$media_y, 4)),
        Variancia = c(fmt_num(m$variancia, 4), fmt_num(trf$variancia_y, 4)),
        DP = c(fmt_num(m$dp, 4), fmt_num(trf$dp_y, 4)),
        stringsAsFactors = FALSE
      )
      names(df) <- c(
        tr("Variável", "Variable"),
        tr("Esperança E(·)", "Expected value E(·)"),
        tr("Variância Var(·)", "Variance Var(·)"),
        tr("Desvio padrão", "Standard deviation")
      )
      tabela_html(df, c("", "num", "num", "num"))
    })
  })
}
