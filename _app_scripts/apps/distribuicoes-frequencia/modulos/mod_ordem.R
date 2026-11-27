# =============================================================================
# Script:  mod_ordem.R (módulo: estatísticas de ordem e função de distribuição empírica)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/apps/distribuicoes-frequencia/app.R)
# Saída:
#   - funções mod_ordem_ui() e mod_ordem_server()
# =============================================================================

# pdf da j-ésima estatística de ordem de uma amostra contínua de tamanho n
# (Casella e Berger, Teo. 5.4.4):
# f_(j)(x) = n! / ((j − 1)! (n − j)!) f(x) F(x)^(j − 1) [1 − F(x)]^(n − j)
densidade_ordem <- function(x, modelo, n, j) {
  constante <- exp(lgamma(n + 1) - lgamma(j) - lgamma(n - j + 1))
  constante * modelo$f(x) * modelo$F(x)^(j - 1) * (1 - modelo$F(x))^(n - j)
}

# E X_(j) = ∫ x f_(j)(x) dx. Uma grade larga acha onde está a massa de X_(j)
# (o máximo da gama com n = 100 passa de 11), e stats::integrate() integra só
# nesse trecho, cortado no suporte verdadeiro: na uniforme, f_(n) tem um degrau
# de altura n em x = 1, que a regra do trapézio numa grade não capta bem.
media_ordem <- function(modelo, n, j) {
  s <- modelo$suporte
  xs <- seq(s[1] - diff(s), s[2] + 2 * diff(s), length.out = 20001)
  fx <- densidade_ordem(xs, modelo, n, j)
  massa <- range(xs[fx > max(fx) * 1e-12]) + c(-1, 1) * diff(xs[1:2])
  de <- max(massa[1], modelo$limites[1])
  ate <- min(massa[2], modelo$limites[2])
  stats::integrate(\(x) x * densidade_ordem(x, modelo, n, j), de, ate, rel.tol = 1e-8, subdivisions = 500L)$value
}

# k amostras de tamanho n; devolve a j-ésima menor de cada uma
simular_ordem <- function(modelo, n, j, k) {
  m <- matrix(modelo$gerar(n * k), nrow = k)
  purrr::map_dbl(seq_len(k), \(i) sort(m[i, ])[j])
}

texto_ordem <- function() {
  tr(
    shiny::withMathJax(
      shiny::p(
        "Ordenar os dados é o passo escondido atrás do ramo-e-folhas, da mediana e dos quantis. Em notação formal,",
        "os valores da amostra em ordem crescente são as estatísticas de ordem \\(X_{(1)} \\le X_{(2)} \\le \\dots \\le X_{(n)}\\):",
        "\\(X_{(1)}\\) é o mínimo, \\(X_{(n)}\\) o máximo, e a mediana é \\(X_{((n+1)/2)}\\) com \\(n\\) ímpar ou a média",
        "das duas centrais com \\(n\\) par (Casella e Berger, Def. 5.4.1 e eq. 5.4.1). Morettin e Bussab usam as",
        "mesmas estatísticas nos quantis empíricos: \\(X_{(i)}\\) é o quantil de ordem \\(p_i = (i - 0{,}5)/n\\)."
      ),
      shiny::p(
        "A função de distribuição empírica \\(\\hat F_n(x) = \\frac{1}{n}\\#\\{i : X_i \\le x\\}\\) é a frequência",
        "acumulada da tabela levada ao extremo, sem agrupar nada. É uma escada que sobe \\(1/n\\) em cada",
        "\\(X_{(i)}\\): em \\(X_{(j)}\\), vale \\(j/n\\). Ao contrário do histograma, ela não depende de largura",
        "nem de origem de classe."
      ),
      shiny::p(
        "Para cada \\(x\\) fixo, a contagem \\(Y = n\\hat F_n(x)\\) é binomial \\((n, F(x))\\): cada observação cai",
        "abaixo de \\(x\\) com probabilidade \\(F(x)\\), de forma independente. Logo \\(E\\,\\hat F_n(x) = F(x)\\) e",
        "\\(\\text{Var}\\,\\hat F_n(x) = F(x)[1 - F(x)]/n\\), que dá a faixa do gráfico. A mesma contagem dá a",
        "distribuição das estatísticas de ordem: \\(X_{(j)} \\le x\\) se e só se pelo menos \\(j\\) observações",
        "ficam abaixo de \\(x\\), isto é, \\(Y \\ge j\\) (prova do Teo. 5.4.3). Derivando em \\(x\\), chega-se à",
        "densidade \\(f_{X_{(j)}}(x) = \\frac{n!}{(j-1)!(n-j)!} f(x) F(x)^{j-1}[1-F(x)]^{n-j}\\) (Teo. 5.4.4),",
        "a curva vermelha do segundo gráfico. Na uniforme (0, 1), \\(X_{(j)}\\) é beta \\((j, n - j + 1)\\), com",
        "média \\(j/(n+1)\\) (Ex. 5.4.5)."
      ),
      shiny::p(
        shiny::strong("Experimente:"),
        "com \\(j = 1\\) e depois \\(j = n\\), veja as distribuições do mínimo e do máximo irem para as pontas e",
        "perderem a simetria. Na uniforme, ponha \\(j\\) no meio e compare a média simulada com \\(j/(n+1)\\).",
        "Aumente \\(n\\) e veja a escada se aproximar da curva verdadeira."
      )
    ),
    shiny::withMathJax(
      shiny::p(
        "Sorting the data is the hidden step behind the stem-and-leaf plot, the median and the quantiles. In formal",
        "notation, the sample values in increasing order are the order statistics \\(X_{(1)} \\le X_{(2)} \\le \\dots \\le X_{(n)}\\):",
        "\\(X_{(1)}\\) is the minimum, \\(X_{(n)}\\) the maximum, and the median is \\(X_{((n+1)/2)}\\) for odd \\(n\\)",
        "or the average of the two middle values for even \\(n\\) (Casella and Berger, Def. 5.4.1 and eq. 5.4.1).",
        "Morettin and Bussab use the same statistics for empirical quantiles: \\(X_{(i)}\\) is the quantile of order",
        "\\(p_i = (i - 0.5)/n\\)."
      ),
      shiny::p(
        "The empirical distribution function \\(\\hat F_n(x) = \\frac{1}{n}\\#\\{i : X_i \\le x\\}\\) is the cumulative",
        "frequency of the table taken to the limit, with no grouping at all. It is a staircase that rises \\(1/n\\) at",
        "each \\(X_{(i)}\\): at \\(X_{(j)}\\), it equals \\(j/n\\). Unlike the histogram, it does not depend on a bin",
        "width or origin."
      ),
      shiny::p(
        "For each fixed \\(x\\), the count \\(Y = n\\hat F_n(x)\\) is binomial \\((n, F(x))\\): each observation falls",
        "below \\(x\\) with probability \\(F(x)\\), independently. So \\(E\\,\\hat F_n(x) = F(x)\\) and",
        "\\(\\text{Var}\\,\\hat F_n(x) = F(x)[1 - F(x)]/n\\), which gives the band in the plot. The same count gives",
        "the distribution of the order statistics: \\(X_{(j)} \\le x\\) if and only if at least \\(j\\) observations fall",
        "below \\(x\\), that is, \\(Y \\ge j\\) (proof of Thm. 5.4.3). Differentiating in \\(x\\) gives the density",
        "\\(f_{X_{(j)}}(x) = \\frac{n!}{(j-1)!(n-j)!} f(x) F(x)^{j-1}[1-F(x)]^{n-j}\\) (Thm. 5.4.4), the red curve in",
        "the second plot. For the uniform (0, 1), \\(X_{(j)}\\) is beta \\((j, n - j + 1)\\), with mean \\(j/(n+1)\\)",
        "(Ex. 5.4.5)."
      ),
      shiny::p(
        shiny::strong("Try it:"),
        "with \\(j = 1\\) and then \\(j = n\\), watch the distributions of the minimum and the maximum move to the",
        "edges and lose their symmetry. For the uniform, put \\(j\\) in the middle and compare the simulated mean",
        "with \\(j/(n+1)\\). Increase \\(n\\) and watch the staircase approach the true curve."
      )
    )
  )
}

mod_ordem_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::selectInput(
        ns("modelo"), tr("População", "Population"),
        choices = stats::setNames(names(modelos_densidade), purrr::map_chr(modelos_densidade, "rotulo")),
        selected = "uniforme"
      ),
      shiny::sliderInput(ns("n"), tr("Tamanho da amostra (n)", "Sample size (n)"), min = 3, max = 100, value = 10, step = 1),
      shiny::uiOutput(ns("controle_j")),
      shiny::checkboxInput(ns("faixa"), tr("Mostrar F(x) ± 2 desvios padrão de F̂ₙ(x)", "Show F(x) ± 2 standard deviations of F̂ₙ(x)"), value = TRUE),
      shiny::sliderInput(ns("k"), tr("Amostras na simulação de X(j)", "Samples in the simulation of X(j)"), min = 500, max = 10000, value = 3000, step = 500, sep = sep_milhar()),
      shiny::actionButton(ns("sortear"), tr("Sortear nova amostra", "Draw a new sample"), class = "btn-primary", icon = shiny::icon("dice"))
    ),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(title = shiny::textOutput(ns("titulo_xj")), value = shiny::textOutput(ns("xj")), shiny::textOutput(ns("xj_desc")), theme = "primary"),
      bslib::value_box(title = tr("F verdadeira nesse ponto", "True F at that point"), value = shiny::textOutput(ns("F_xj")), tr("Quanto da população fica abaixo", "Share of the population below it"), theme = "info"),
      bslib::value_box(title = shiny::textOutput(ns("titulo_media")), value = shiny::textOutput(ns("media_sim")), shiny::textOutput(ns("media_teo")), theme = NULL)
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico_fda"), height = "420px"))),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico_ordem"), height = "420px")))
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(5, 7)),
      bslib::card(
        bslib::card_header(tr("A amostra ordenada", "The sorted sample")),
        max_height = "520px",
        bslib::card_body(shiny::uiOutput(ns("tabela")))
      ),
      bslib::card(bslib::card_header(rotulo_entenda()), bslib::card_body(texto_ordem()))
    )
  )
}

mod_ordem_server <- function(id, modo = shiny::reactive("light")) {
  shiny::moduleServer(id, function(input, output, session) {

    modelo <- shiny::reactive(modelos_densidade[[input$modelo]])

    output$controle_j <- shiny::renderUI({
      atual <- shiny::isolate(input$j)
      valor <- if (is.null(atual)) ceiling(input$n / 2) else min(atual, input$n)
      shiny::sliderInput(session$ns("j"), tr("Estatística de ordem j", "Order statistic j"), min = 1, max = input$n, value = valor, step = 1)
    })

    j <- shiny::reactive({
      shiny::req(input$j)
      min(input$j, input$n)
    })

    amostra <- shiny::reactive({
      input$sortear
      sort(modelo()$gerar(input$n))
    })

    simulacao <- shiny::reactive({
      input$sortear
      simular_ordem(modelo(), input$n, j(), input$k)
    })

    media_teorica <- shiny::reactive(media_ordem(modelo(), input$n, j()))

    rotulo_j <- function(jj) sprintf("X(%s)", jj)

    output$titulo_xj <- shiny::renderText(sprintf(tr("%s nesta amostra", "%s in this sample"), rotulo_j(j())))
    output$xj <- shiny::renderText(fmt_num(amostra()[j()], 3))
    output$xj_desc <- shiny::renderText(sprintf(
      tr("F̂ₙ salta para j/n = %d/%d = %s", "F̂ₙ jumps to j/n = %d/%d = %s"),
      j(), input$n, fmt_num(j() / input$n, 3)
    ))
    output$F_xj <- shiny::renderText(fmt_num(modelo()$F(amostra()[j()]), 3))
    output$titulo_media <- shiny::renderText(sprintf(tr("Média de %s em %s amostras", "Mean of %s over %s samples"), rotulo_j(j()), fmt_num(input$k, 0)))
    output$media_sim <- shiny::renderText(fmt_num(mean(simulacao()), 3))
    output$media_teo <- shiny::renderText({
      extra <- if (identical(input$modelo, "uniforme")) sprintf(tr(" = j/(n + 1) = %d/%d", " = j/(n + 1) = %d/%d"), j(), input$n + 1) else ""
      sprintf(tr("Pela densidade do Teo. 5.4.4: %s%s", "From the density in Thm. 5.4.4: %s%s"), fmt_num(media_teorica(), 3), extra)
    })

    output$grafico_fda <- shiny::renderPlot({
      pal <- paleta_modo(modo())
      x <- amostra()
      n <- length(x)
      m <- modelo()
      lim <- range(c(m$suporte, x))
      xs <- seq(lim[1], lim[2], length.out = 500)
      verdade <- data.frame(x = xs, F = m$F(xs))
      verdade$inf <- pmax(0, verdade$F - 2 * sqrt(verdade$F * (1 - verdade$F) / n))
      verdade$sup <- pmin(1, verdade$F + 2 * sqrt(verdade$F * (1 - verdade$F) / n))
      escada <- data.frame(x = c(lim[1], x, lim[2]), y = c(0, seq_len(n) / n, 1))
      pontos <- data.frame(x = x, y = seq_len(n) / n, destaque = seq_len(n) == j())

      grafico <- ggplot2::ggplot()
      if (isTRUE(input$faixa)) {
        grafico <- grafico + ggplot2::geom_ribbon(data = verdade, ggplot2::aes(x = x, ymin = inf, ymax = sup), fill = cores_app$referencia, alpha = 0.12)
      }
      grafico +
        ggplot2::geom_line(data = verdade, ggplot2::aes(x = x, y = F), colour = cores_app$referencia, linewidth = 1) +
        ggplot2::geom_step(data = escada, ggplot2::aes(x = x, y = y), colour = cores_app$destaque, linewidth = 1) +
        ggplot2::geom_point(data = pontos[!pontos$destaque, ], ggplot2::aes(x = x, y = y), colour = cores_app$destaque, size = if (n > 40) 1.2 else 2.2) +
        ggplot2::geom_segment(
          data = pontos[pontos$destaque, ], ggplot2::aes(x = x, xend = x, y = 0, yend = y),
          colour = pal$linha, linetype = "dashed"
        ) +
        ggplot2::geom_point(data = pontos[pontos$destaque, ], ggplot2::aes(x = x, y = y), colour = pal$linha, size = 3.5) +
        # Na metade de cima, o rótulo vai para a esquerda do ponto, para não sair do painel
        ggplot2::annotate(
          "label", x = x[j()], y = j() / n, label = sprintf("%s ; %d/%d", rotulo_j(j()), j(), n),
          hjust = if (j() / n > 0.5) 1.1 else -0.1, vjust = if (j() / n > 0.5) 1.2 else -0.4,
          fill = pal$fundo, colour = pal$linha, size = 4
        ) +
        ggplot2::scale_x_continuous(labels = rotulos_eixo()) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo(), limits = c(0, 1)) +
        ggplot2::labs(
          title = tr("Função de distribuição empírica", "Empirical distribution function"),
          subtitle = sprintf("%s | n = %d", m$rotulo, n),
          x = "x", y = tr("Proporção ≤ x", "Proportion ≤ x"),
          caption = if (isTRUE(input$faixa)) {
            tr("Azul: F̂ₙ | Vermelha: F verdadeira, com faixa F ± 2√(F(1 − F)/n)", "Blue: F̂ₙ | Red: true F, with band F ± 2√(F(1 − F)/n)")
          } else {
            tr("Azul: F̂ₙ | Vermelha: F verdadeira", "Blue: F̂ₙ | Red: true F")
          }
        ) +
        tema_grafico(modo())
    })

    output$grafico_ordem <- shiny::renderPlot({
      pal <- paleta_modo(modo())
      sim <- simulacao()
      m <- modelo()
      lim <- range(c(sim, m$suporte[1], m$suporte[2]))
      lim <- c(max(lim[1], min(sim) - 0.5), min(lim[2], max(sim) + 0.5))
      xs <- seq(lim[1], lim[2], length.out = 500)
      curva <- data.frame(x = xs, y = densidade_ordem(xs, m, input$n, j()))

      ggplot2::ggplot() +
        ggplot2::geom_histogram(
          data = data.frame(x = sim), ggplot2::aes(x = x, y = ggplot2::after_stat(density)),
          bins = 40, fill = cores_app$destaque, colour = pal$fundo, alpha = 0.7
        ) +
        ggplot2::geom_line(data = curva, ggplot2::aes(x = x, y = y), colour = cores_app$referencia, linewidth = 1.1) +
        ggplot2::geom_vline(xintercept = amostra()[j()], colour = pal$linha, linetype = "dashed") +
        ggplot2::scale_x_continuous(labels = rotulos_eixo()) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = sprintf(tr("%s varia de amostra para amostra", "%s varies from sample to sample"), rotulo_j(j())),
          subtitle = sprintf(tr("%s amostras de tamanho n = %d", "%s samples of size n = %d"), fmt_num(input$k, 0), input$n),
          x = "x", y = tr("Densidade", "Density"),
          caption = tr(
            "Barras: simulação | Vermelha: densidade do Teo. 5.4.4 | Tracejada: valor na amostra atual",
            "Bars: simulation | Red: density from Thm. 5.4.4 | Dashed: value in the current sample"
          )
        ) +
        tema_grafico(modo())
    })

    output$tabela <- shiny::renderUI({
      x <- amostra()
      n <- length(x)
      tabela <- data.frame(
        purrr::map_chr(seq_len(n), rotulo_j),
        fmt_num(x, 3),
        fmt_num(seq_len(n) / n, 3),
        fmt_num((seq_len(n) - 0.5) / n, 3),
        fmt_num(modelo()$F(x), 3)
      )
      names(tabela) <- c(tr("Ordem", "Order"), tr("Valor", "Value"), "F̂ₙ = i/n", tr("pᵢ = (i − 0,5)/n", "pᵢ = (i − 0.5)/n"), "F(x(i))")
      shiny::tagList(
        shiny::p(class = "small text-muted", tr(
          "pᵢ: ordem do quantil empírico de Morettin e Bussab que cai exatamente em X(i).",
          "pᵢ: order of Morettin and Bussab's empirical quantile that falls exactly on X(i)."
        )),
        tabela_html(tabela, classes = c("", "num", "num", "num", "num"))
      )
    })
  })
}
