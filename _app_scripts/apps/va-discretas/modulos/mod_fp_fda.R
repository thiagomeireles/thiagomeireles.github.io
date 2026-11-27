# =============================================================================
# Script:  mod_fp_fda.R (Aba 1: Função de probabilidade, FDA e momentos)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Exemplo dos livros e parâmetros da transformação linear aX + b
# Saída:
#   - Gráficos da FP p(x) e da FDA em escada F(x) de X ou de Y = aX + b e
#     tabela de esperança, variância, desvio padrão e mediana
# =============================================================================

mod_fp_fda_ui <- function(id) {
  ns <- shiny::NS(id)
  exemplos <- exemplos_fp()

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::selectInput(
        ns("modelo"),
        tr("Distribuição de partida", "Starting distribution"),
        choices = stats::setNames(names(exemplos), purrr::map_chr(exemplos, "rotulo"))
      ),
      shiny::uiOutput(ns("descricao")),
      shiny::hr(),
      shiny::sliderInput(ns("a_lin"), tr("Multiplicador a em Y = aX + b", "Multiplier a in Y = aX + b"), min = -3, max = 3, value = 2, step = 0.5),
      shiny::sliderInput(ns("b_lin"), tr("Deslocamento b em Y = aX + b", "Shift b in Y = aX + b"), min = -5, max = 5, value = 0, step = 0.5),
      shiny::radioButtons(
        ns("mostrar"),
        tr("Mostrar nos gráficos", "Show in the plots"),
        choices = stats::setNames(c("x", "y"), c("X", "Y = aX + b")),
        inline = TRUE
      )
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
    bslib::card(
      bslib::card_header(tr("Momentos, mediana e transformação linear Y = aX + b", "Moments, median and linear transformation Y = aX + b")),
      bslib::card_body(
        shiny::uiOutput(ns("tabela_momentos")),
        shiny::uiOutput(ns("leitura_momentos"))
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header(rotulo_entenda()),
        bslib::card_body(
          shiny::p(tr(
            "A função de probabilidade lista cada valor xᵢ e sua probabilidade p(xᵢ), com soma 1. A esperança E(X) = Σ xᵢ p(xᵢ) é a média que se espera numa longa série de repetições, e a variância mede o quanto X costuma se afastar dela, Var(X) = E(X²) − [E(X)]² (Morettin e Bussab, seções 6.2 a 6.4; Agresti e Finlay, seção 4.2). No lucro por montagem, E(X) = 9,85 e Var(X) = 57,23.",
            "The probability mass function lists each value xᵢ with its probability p(xᵢ), summing to 1. The expected value E(X) = Σ xᵢ p(xᵢ) is the mean you expect over a long run of repetitions, and the variance measures how far X tends to stray from it, Var(X) = E(X²) − [E(X)]² (Morettin and Bussab, sections 6.2 to 6.4; Agresti and Finlay, section 4.2). For the profit per assembly, E(X) = 9.85 and Var(X) = 57.23."
          )),
          shiny::p(tr(
            "A FDA F(x) = P(X ≤ x) é uma escada contínua à direita: em cada xᵢ ela salta p(xᵢ) (seção 6.5). Somar b desloca a esperança e não mexe na dispersão, porque todos os valores andam a mesma distância; multiplicar por a multiplica a esperança por a e a variância por a² (fórmulas 6.4 e 6.5).",
            "The CDF F(x) = P(X ≤ x) is a right-continuous step function: at each xᵢ it jumps by p(xᵢ) (section 6.5). Adding b shifts the expected value and leaves the spread alone, because every value moves the same distance; multiplying by a multiplies the expected value by a and the variance by a² (formulas 6.4 and 6.5)."
          )),
          shiny::p(
            shiny::strong(tr("Experimente:", "Try this:")),
            tr(
              "escolha o lucro por montagem e mostre Y nos gráficos. Com a = 2 e b = 0, os preços dobram, como no Ex. 6.7, e E(Y) = 19,70. Depois ponha a = −1: as barras se espelham, a esperança troca de sinal e a variância fica igual, porque (−1)² = 1.",
              "pick the profit per assembly and show Y in the plots. With a = 2 and b = 0, prices double, as in Ex. 6.7, and E(Y) = 19.70. Then set a = −1: the bars flip, the expected value changes sign and the variance stays the same, because (−1)² = 1."
            )
          )
        )
      ),
      cartao_cuidados(
        tr("Cuidados", "Watch out"),
        stats::setNames(
          list(
            tr(
              "a esperança não precisa ser um valor possível de X. No dado, E(X) = 3,5, e nenhuma face mostra 3,5.",
              "the expected value need not be a possible value of X. For the die, E(X) = 3.5, and no face shows 3.5."
            ),
            tr(
              "como F(x) anda em degraus, pode não existir x com F(x) = 0,5. Por isso Morettin e Bussab (seção 6.8) definem a mediana por P(X ≤ Md) ≥ 0,5 e P(X ≥ Md) ≥ 0,5. No dado, qualquer valor entre 3 e 4 é mediana.",
              "since F(x) moves in steps, there may be no x with F(x) = 0.5. That is why Morettin and Bussab (section 6.8) define the median by P(X ≤ Md) ≥ 0.5 and P(X ≥ Md) ≥ 0.5. For the die, any value between 3 and 4 is a median."
            ),
            tr(
              "E(aX + b) = aE(X) + b vale para funções lineares. Para X², calcule E(X²) = Σ xᵢ² p(xᵢ); ela não é [E(X)]² (Ex. 6.7: 154,25 contra 9,85² = 97,02).",
              "E(aX + b) = aE(X) + b holds for linear functions. For X², compute E(X²) = Σ xᵢ² p(xᵢ); it is not [E(X)]² (Ex. 6.7: 154.25 against 9.85² = 97.02)."
            )
          ),
          c(
            tr("Esperança não é valor típico", "The expected value is not a typical value"),
            tr("Mediana discreta", "Discrete median"),
            tr("Só vale para o linear", "Linear only")
          )
        )
      )
    )
  )
}

mod_fp_fda_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    exemplo <- shiny::reactive({
      shiny::req(input$modelo)
      exemplos_fp()[[input$modelo]]
    })

    dados_mom <- shiny::reactive(momentos_discreta(exemplo()$x, exemplo()$p))

    transf <- shiny::reactive({
      shiny::req(is.finite(input$a_lin), is.finite(input$b_lin))
      transformar_linear(dados_mom(), input$a_lin, input$b_lin)
    })

    rotulo_y <- shiny::reactive({
      sprintf("Y = %s·X %s %s", fmt_num(input$a_lin, 1), if (input$b_lin < 0) "−" else "+", fmt_num(abs(input$b_lin), 1))
    })

    # Distribuição exibida nos gráficos: X ou Y = aX + b
    exibida <- shiny::reactive({
      if (identical(input$mostrar, "y")) {
        list(m = transf()$dist_y, nome = "Y", titulo = rotulo_y())
      } else {
        list(m = dados_mom(), nome = "X", titulo = "X")
      }
    })

    output$descricao <- shiny::renderUI({
      shiny::p(class = "small text-muted", exemplo()$descricao)
    })

    output$grafico_fp <- shiny::renderPlot({
      e <- exibida()
      m <- e$m
      df <- data.frame(x = m$x, prob = m$prob)
      largura <- if (length(m$x) > 1) 0.55 * min(diff(m$x)) else 0.55
      ggplot2::ggplot(df, ggplot2::aes(x = x, y = prob)) +
        ggplot2::geom_col(fill = cores_app$destaque, width = largura) +
        ggplot2::geom_vline(xintercept = m$media, colour = cores_app$referencia, linetype = "dashed", linewidth = 0.9) +
        ggplot2::scale_x_continuous(breaks = m$x, labels = rotulos_eixo()) +
        ggplot2::scale_y_continuous(limits = c(0, max(m$prob) * 1.18), labels = rotulos_eixo()) +
        ggplot2::labs(
          title = e$titulo,
          subtitle = sprintf("E(%s) = %s | DP(%s) = %s", e$nome, fmt_num(m$media, 2), e$nome, fmt_num(m$dp, 2)),
          x = tolower(e$nome), y = sprintf("P(%s = %s)", e$nome, tolower(e$nome)),
          caption = sprintf(tr("Linha tracejada: E(%s)", "Dashed line: E(%s)"), e$nome)
        ) +
        tema_grafico(modo())
    })

    output$grafico_fda <- shiny::renderPlot({
      e <- exibida()
      m <- e$m
      p <- paleta_modo(modo())
      xs <- m$x
      fs <- m$fda
      n <- length(xs)
      folga <- if (n > 1) min(diff(xs)) else 1
      segs <- data.frame(
        x = c(min(xs) - folga, xs),
        xend = c(xs, max(xs) + folga),
        y = c(0, fs),
        yend = c(0, fs)
      )
      pontos_abertos <- data.frame(x = xs, y = c(0, fs[-n]))
      pontos_fechados <- data.frame(x = xs, y = fs)

      ggplot2::ggplot() +
        ggplot2::geom_hline(yintercept = 0.5, colour = p$secundario, linetype = "dotted") +
        ggplot2::geom_segment(
          data = segs,
          ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
          colour = cores_app$destaque, linewidth = 1
        ) +
        ggplot2::geom_point(data = pontos_fechados, ggplot2::aes(x = x, y = y), colour = cores_app$destaque, size = 2.8) +
        ggplot2::geom_point(data = pontos_abertos, ggplot2::aes(x = x, y = y), colour = cores_app$destaque, fill = p$fundo, shape = 21, size = 2.8, stroke = 1) +
        ggplot2::scale_x_continuous(breaks = xs, labels = rotulos_eixo()) +
        ggplot2::scale_y_continuous(limits = c(0, 1.05), labels = rotulos_eixo()) +
        ggplot2::labs(
          title = tr("Escada da FDA (contínua à direita)", "Step CDF (right-continuous)"),
          x = tolower(e$nome), y = sprintf("F(%s)", tolower(e$nome)),
          caption = tr("Linha pontilhada: F = 0,5 (mediana)", "Dotted line: F = 0.5 (median)")
        ) +
        tema_grafico(modo())
    })

    fmt_mediana <- function(md) {
      if (md$unico) {
        fmt_num(md$inf, 2)
      } else {
        sprintf(tr("qualquer valor em [%s; %s]", "any value in [%s, %s]"), fmt_num(md$inf, 2), fmt_num(md$sup, 2))
      }
    }

    output$tabela_momentos <- shiny::renderUI({
      m <- dados_mom()
      trf <- transf()
      df <- data.frame(
        Variavel = c("X", rotulo_y()),
        Media = c(fmt_num(m$media, 4), fmt_num(trf$media_y, 4)),
        Variancia = c(fmt_num(m$variancia, 4), fmt_num(trf$variancia_y, 4)),
        DP = c(fmt_num(m$dp, 4), fmt_num(trf$dp_y, 4)),
        Mediana = c(fmt_mediana(m$mediana), fmt_mediana(trf$dist_y$mediana)),
        stringsAsFactors = FALSE
      )
      names(df) <- c(
        tr("Variável", "Variable"),
        tr("Esperança E(·)", "Expected value E(·)"),
        tr("Variância Var(·)", "Variance Var(·)"),
        tr("Desvio padrão", "Standard deviation"),
        tr("Mediana Md", "Median Md")
      )
      tabela_html(df, c("", "num", "num", "num", "num"))
    })

    output$leitura_momentos <- shiny::renderUI({
      m <- dados_mom()
      trf <- transf()
      shiny::p(class = "small text-muted mb-0", sprintf(
        tr(
          "Pelas fórmulas: E(Y) = %s × %s %s %s = %s e Var(Y) = %s² × %s = %s. Calculando direto na distribuição de Y, as contas batem: E(Y) = %s e Var(Y) = %s. Para X, E(X²) = %s.",
          "By the formulas: E(Y) = %s × %s %s %s = %s and Var(Y) = %s² × %s = %s. Computing directly from the distribution of Y, the results match: E(Y) = %s and Var(Y) = %s. For X, E(X²) = %s."
        ),
        fmt_num(trf$a, 1), fmt_num(m$media, 4), if (trf$b < 0) "−" else "+", fmt_num(abs(trf$b), 1), fmt_num(trf$media_y, 4),
        fmt_num(trf$a, 1), fmt_num(m$variancia, 4), fmt_num(trf$variancia_y, 4),
        fmt_num(trf$dist_y$media, 4), fmt_num(trf$dist_y$variancia, 4), fmt_num(m$ex2, 4)
      ))
    })
  })
}
