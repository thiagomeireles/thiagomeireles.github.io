# =============================================================================
# Script:  mod_qq.R (módulo: gráfico q × q para comparar duas amostras)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-19
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/apps/bidimensional/app.R)
# Saída:
#   - funções mod_qq_ui() e mod_qq_server()
# =============================================================================

texto_qq <- function() {
  tr(
    shiny::withMathJax(
      shiny::p(
        "O gráfico q × q compara duas distribuições ponto a ponto, em vez de resumir cada uma num número. Morettin e",
        "Bussab o apresentam no fim do Cap. 4: com duas amostras de mesmo tamanho \\(n\\), ordenam-se os valores de",
        "cada uma, e a \\(i\\)-ésima menor observação de A forma um par com a \\(i\\)-ésima menor de B. As duas são",
        "o quantil de ordem \\(p_i = (i - 0{,}5)/n\\) da sua amostra, e cada par vira um ponto do gráfico."
      ),
      shiny::p(
        "A leitura parte da reta \\(y = x\\). Se as duas distribuições forem iguais, os pontos ficam sobre ela. Se B",
        "for A deslocada, os pontos formam uma reta paralela, acima (B maior) ou abaixo (B menor). Se B for mais",
        "dispersa, a inclinação passa de 1; se for menos, fica abaixo de 1. Quando as formas diferem, como uma",
        "simétrica contra uma assimétrica, os pontos se curvam, e a curva aparece sobretudo nas pontas."
      ),
      shiny::p(
        shiny::strong("Experimente:"),
        "comece com deslocamento 0 e razão 1, e os pontos ficam em torno da reta. Mexa só no deslocamento e depois",
        "só na razão, para ver as duas assinaturas separadas. Por fim, troque a forma de B para assimétrica."
      )
    ),
    shiny::withMathJax(
      shiny::p(
        "The Q-Q plot compares two distributions point by point instead of summarizing each in a single number.",
        "Morettin and Bussab present it at the end of ch. 4: with two samples of the same size \\(n\\), the values of",
        "each one are sorted, and the \\(i\\)-th smallest observation of A is paired with the \\(i\\)-th smallest of B.",
        "Both are the quantile of order \\(p_i = (i - 0.5)/n\\) of their sample, and each pair becomes a point."
      ),
      shiny::p(
        "Reading starts from the line \\(y = x\\). If the two distributions are equal, the points sit on it. If B is",
        "A shifted, the points form a parallel line, above it (B larger) or below it (B smaller). If B is more spread",
        "out, the slope is above 1; if less, below 1. When the shapes differ, such as a symmetric against a skewed",
        "one, the points bend, and the bend shows mostly at the ends."
      ),
      shiny::p(
        shiny::strong("Try it:"),
        "start with shift 0 and ratio 1, and the points stay around the line. Move only the shift and then only the",
        "ratio, to see the two signatures separately. Finally, switch the shape of B to skewed."
      )
    )
  )
}

cuidados_qq <- function() {
  tr(
    c(
      "Mesmo tamanho" = paste(
        "com n iguais, cada ponto é um par de observações ordenadas. Com tamanhos diferentes, é preciso",
        "calcular os quantis de uma das amostras por interpolação."
      ),
      "Amostras pequenas" = "oscilam bastante em torno da reta, mesmo vindas da mesma distribuição. Sorteie de novo antes de concluir.",
      "Pontas do gráfico" = "os quantis extremos se apoiam em poucas observações e variam mais que os centrais.",
      "Não é o gráfico de normalidade" = paste(
        "aqui as duas amostras são comparadas entre si. O gráfico de quantis contra a normal, usado para checar",
        "normalidade, segue a mesma ideia, mas compara uma amostra com uma distribuição teórica."
      )
    ),
    c(
      "Same size" = paste(
        "with equal n, each point is a pair of sorted observations. With different sizes, the quantiles of one",
        "sample must be interpolated."
      ),
      "Small samples" = "wander quite a bit around the line, even when they come from the same distribution. Draw again before concluding.",
      "Ends of the plot" = "extreme quantiles rest on few observations and vary more than the central ones.",
      "Not the normality plot" = paste(
        "here the two samples are compared with each other. The normal quantile plot, used to check normality,",
        "follows the same idea but compares one sample with a theoretical distribution."
      )
    )
  )
}

mod_qq_ui <- function(id) {
  ns <- shiny::NS(id)
  opcoes_forma <- stats::setNames(names(formas_qq), purrr::map_chr(formas_qq, "rotulo"))

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::helpText(tr(
        "Notas de duas turmas numa prova, em pontos. A turma A tem média 60 e desvio padrão 10.",
        "Scores of two classes on a test, in points. Class A has mean 60 and standard deviation 10."
      )),
      slider_amostra(ns("n"), min = 10, max = 500, value = 200),
      shiny::selectInput(ns("forma_a"), tr("Forma da turma A", "Shape of class A"), choices = opcoes_forma),
      shiny::selectInput(ns("forma_b"), tr("Forma da turma B", "Shape of class B"), choices = opcoes_forma),
      shiny::sliderInput(ns("deslocamento"), tr("Deslocamento de B (pontos)", "Shift of B (points)"), min = -20, max = 20, value = 5, step = 1),
      shiny::sliderInput(ns("razao"), tr("Dispersão de B ÷ dispersão de A", "Spread of B ÷ spread of A"), min = 0.5, max = 2, value = 1, step = 0.1),
      botao_sortear(ns("sortear"))
    ),
    shiny::div(
      class = "mb-2",
      shiny::tags$h4(class = "mb-1", tr("Gráfico q × q", "Q-Q plot")),
      shiny::p(
        class = "mb-0",
        tr(
          "As duas amostras têm a mesma distribuição? Se não, diferem na posição, na dispersão ou na forma?",
          "Do the two samples have the same distribution? If not, do they differ in location, spread or shape?"
        ),
        " ", shiny::span(class = "badge text-bg-primary ms-1", tr("quantis de mesma ordem das duas amostras", "same-order quantiles of both samples"))
      )
    ),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(title = tr("Diferença das medianas (B − A)", "Difference in medians (B − A)"), value = shiny::textOutput(ns("dif_med")), tr("Posição", "Location"), theme = "primary"),
      bslib::value_box(title = tr("Razão das distâncias interquartis (B ÷ A)", "Ratio of interquartile ranges (B ÷ A)"), value = shiny::textOutput(ns("razao_dq")), tr("Dispersão; perto da inclinação dos pontos", "Spread; close to the slope of the points"), theme = "info")
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico"), height = "460px"))),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("caixas"), height = "460px")))
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(bslib::card_header(tr("Quantis das duas turmas", "Quantiles of the two classes")), bslib::card_body(shiny::uiOutput(ns("tabela")))),
      cartao_cuidados(tr("Cuidados", "Watch out"), cuidados_qq())
    ),
    bslib::card(bslib::card_header(rotulo_entenda()), bslib::card_body(texto_qq()))
  )
}

mod_qq_server <- function(id, modo = shiny::reactive("light")) {
  shiny::moduleServer(id, function(input, output, session) {

    amostras <- shiny::reactive({
      input$sortear
      gerar_qq(input$forma_a, input$forma_b, input$deslocamento / 10, input$razao, input$n)
    })

    # Quantil de ordem p_i = (i − 0,5)/n, com interpolação entre eles (type = 5)
    quantil_mb <- function(x, p) stats::quantile(x, p, type = 5, names = FALSE)
    dq <- function(x) diff(quantil_mb(x, c(0.25, 0.75)))

    output$dif_med <- shiny::renderText({
      a <- amostras()
      fmt_num(quantil_mb(a$b, 0.5) - quantil_mb(a$a, 0.5), 1)
    })
    output$razao_dq <- shiny::renderText({
      a <- amostras()
      fmt_num(dq(a$b) / dq(a$a), 2)
    })

    output$tabela <- shiny::renderUI({
      a <- amostras()
      p <- c(0.1, 0.25, 0.5, 0.75, 0.9)
      qa <- quantil_mb(a$a, p)
      qb <- quantil_mb(a$b, p)
      tabela <- data.frame(
        tr(c("q(0,10)", "q(0,25): 1º quartil", "q(0,50): mediana", "q(0,75): 3º quartil", "q(0,90)"), c("q(0.10)", "q(0.25): 1st quartile", "q(0.50): median", "q(0.75): 3rd quartile", "q(0.90)")),
        fmt_num(qa, 1), fmt_num(qb, 1), fmt_num(qb - qa, 1)
      )
      names(tabela) <- tr(c("Quantil", "Turma A", "Turma B", "B − A"), c("Quantile", "Class A", "Class B", "B − A"))
      shiny::tagList(
        tabela_html(tabela, classes = c("", "num", "num", "num")),
        shiny::p(class = "small text-muted mt-2 mb-0", tr(
          paste(
            "Quantis como em Morettin e Bussab: o i-ésimo menor valor é o quantil de ordem (i − 0,5)/n, com interpolação",
            "linear entre eles. Se B − A for parecido em todas as linhas, a diferença é só de posição; se crescer",
            "de cima para baixo, B é mais dispersa."
          ),
          paste(
            "Quantiles as in Morettin and Bussab: the i-th smallest value is the quantile of order (i − 0.5)/n, with linear",
            "interpolation between them. If B − A is similar in every row, the difference is only in location; if it",
            "grows from top to bottom, B is more spread out."
          )
        ))
      )
    })

    output$grafico <- shiny::renderPlot({
      pal <- paleta_modo(modo())
      a <- amostras()
      d <- data.frame(qa = sort(a$a), qb = sort(a$b))
      limites <- range(c(d$qa, d$qb))

      ggplot2::ggplot(d, ggplot2::aes(x = qa, y = qb)) +
        ggplot2::geom_abline(intercept = 0, slope = 1, colour = cores_app$referencia, linetype = "dashed", linewidth = 1) +
        ggplot2::geom_point(colour = cores_app$destaque, size = 2.6, alpha = 0.8) +
        ggplot2::coord_equal(xlim = limites, ylim = limites) +
        ggplot2::scale_x_continuous(labels = rotulos_eixo()) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = tr("Gráfico q × q", "Q-Q plot"),
          subtitle = tr("Cada ponto: quantis de mesma ordem das turmas A e B", "Each point: same-order quantiles of classes A and B"),
          x = tr("Quantis da turma A (pontos)", "Quantiles of class A (points)"),
          y = tr("Quantis da turma B (pontos)", "Quantiles of class B (points)"),
          caption = sprintf(tr("Dados simulados, n = %s em cada turma | Tracejada: y = x", "Simulated data, n = %s per class | Dashed: y = x"), fmt_num(input$n, 0))
        ) +
        tema_grafico(modo())
    }, bg = "transparent") # com eixos na mesma escala, sobram margens; o fundo do cartão as preenche

    output$caixas <- shiny::renderPlot({
      pal <- paleta_modo(modo())
      a <- amostras()
      rotulos <- tr(c("Turma A", "Turma B"), c("Class A", "Class B"))
      d <- data.frame(turma = factor(rep(rotulos, each = length(a$a)), levels = rev(rotulos)), nota = c(a$a, a$b))

      ggplot2::ggplot(d, ggplot2::aes(x = nota, y = turma, fill = turma)) +
        ggplot2::geom_boxplot(width = 0.5, colour = pal$texto, outlier.colour = cores_app$referencia) +
        ggplot2::scale_fill_manual(values = stats::setNames(c(pal$neutro, cores_app$destaque), rotulos)) +
        ggplot2::scale_x_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = tr("As mesmas amostras em box plots", "The same samples as box plots"),
          x = tr("Nota (pontos)", "Score (points)"), y = NULL,
          caption = tr("Pontos vermelhos: a mais de 1,5 × d_q da caixa", "Red dots: more than 1.5 × IQR beyond the box")
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_text(size = 13, colour = pal$texto))
    })
  })
}
