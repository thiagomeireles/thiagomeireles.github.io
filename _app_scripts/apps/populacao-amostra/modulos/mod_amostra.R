# =============================================================================
# Script:  mod_amostra.R (módulo: uma amostra tirada da população)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-25
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/apps/populacao-amostra/app.R)
# Saída:
#   - funções mod_amostra_ui() e mod_amostra_server()
# =============================================================================

limite_historico <- 300

opcoes_variavel <- function() {
  stats::setNames(names(variaveis_pop), purrr::map_chr(variaveis_pop, "rotulo"))
}

opcoes_medida <- function(variavel) {
  ids <- medidas_da_variavel(variavel)
  stats::setNames(ids, purrr::map_chr(medidas[ids], "rotulo"))
}

texto_amostra <- function() {
  tr(
    shiny::tagList(
      shiny::p(
        "A cidade simulada tem 2.000 adultos, e o app conhece todos eles. Por isso consegue mostrar o que, na prática,",
        "nunca se vê: o parâmetro (fixo, calculado com a população inteira) ao lado da estatística (calculada só com os",
        "sorteados). Com amostra aleatória, a diferença entre os dois é o erro amostral. Numa pesquisa de verdade ele é",
        "desconhecido, porque o parâmetro é desconhecido."
      ),
      shiny::p(
        "Com amostra aleatória simples, cada sorteio erra um pouco para cima ou para baixo, e os erros se compensam:",
        "os pontos do gráfico oscilam em torno da linha vermelha. Quanto maior o n, mais perto dela eles ficam."
      ),
      shiny::p(
        "A pesquisa online é diferente. Como quem não usa internet nunca é sorteado, e esse grupo é mais velho e tem",
        "renda menor, a estatística passa a errar, em média, para o mesmo lado. Isso é viés de seleção (Morettin e",
        "Bussab, seção 10.4), e ele não some com uma amostra maior: aumentar n só deixa o erro mais estável."
      ),
      shiny::p(
        shiny::strong("Experimente:"),
        "escolha a idade e a média, sorteie mais 20 amostras algumas vezes e observe o erro médio. Depois mude para a",
        "pesquisa online e repita. Por fim, ponha n = 500 e veja que o erro médio da pesquisa online continua lá."
      )
    ),
    shiny::tagList(
      shiny::p(
        "The simulated city has 2,000 adults, and the app knows every one of them. That is why it can show what, in",
        "practice, we never see: the parameter (fixed, computed from the whole population) next to the statistic",
        "(computed only from the people drawn). With a random sample, the difference between them is the sampling",
        "error. In a real survey it is unknown, because the parameter is unknown."
      ),
      shiny::p(
        "With a simple random sample, each draw misses a little high or a little low, and the errors balance out:",
        "the points in the chart scatter around the red line. The larger n is, the closer they stay to it."
      ),
      shiny::p(
        "The online survey is different. Since people who do not use the internet are never drawn, and that group is",
        "older and earns less, the statistic now misses, on average, to the same side. That is sampling bias",
        "(undercoverage), and a larger sample does not remove it: increasing n only makes the error more stable."
      ),
      shiny::p(
        shiny::strong("Try it:"),
        "choose age and the mean, draw 20 more samples a few times and watch the average error. Then switch to the",
        "online survey and repeat. Finally, set n = 500 and see that the average error of the online survey is still there."
      )
    )
  )
}

mod_amostra_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::selectInput(ns("variavel"), tr("Variável", "Variable"), choices = opcoes_variavel()),
      shiny::selectInput(ns("medida"), tr("Medida", "Measure"), choices = opcoes_medida("idade")),
      shiny::sliderInput(ns("n"), tr("Tamanho da amostra (n)", "Sample size (n)"), min = 10, max = 500, value = 50, step = 10, sep = sep_milhar()),
      shiny::radioButtons(ns("selecao"), tr("Como a amostra é escolhida", "How the sample is chosen"), choices = stats::setNames(names(selecoes), unlist(selecoes))),
      shiny::actionButton(ns("sortear"), tr("Sortear nova amostra", "Draw a new sample"), class = "btn-primary", icon = shiny::icon("dice")),
      shiny::actionButton(ns("sortear_varias"), tr("Sortear mais 20", "Draw 20 more"), icon = shiny::icon("forward")),
      shiny::helpText(tr(
        "Trocar a variável ou a medida mantém as mesmas pessoas sorteadas. Trocar n ou a forma de escolha recomeça o histórico.",
        "Changing the variable or the measure keeps the same people. Changing n or how the sample is chosen restarts the history."
      ))
    ),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(
        title = shiny::textOutput(ns("titulo_parametro")),
        value = shiny::textOutput(ns("parametro")),
        tr("Fixo: calculado com as 2.000 pessoas", "Fixed: computed from all 2,000 people"),
        theme = "primary"
      ),
      bslib::value_box(
        title = shiny::textOutput(ns("titulo_estatistica")),
        value = shiny::textOutput(ns("estatistica")),
        tr("Muda a cada amostra sorteada", "Changes with every sample drawn"),
        theme = "info"
      ),
      bslib::value_box(
        title = tr("Erro desta amostra", "Error in this sample"),
        value = shiny::textOutput(ns("erro")),
        shiny::textOutput(ns("erro_medio")),
        theme = NULL
      )
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico_grade"), height = "420px"))),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico_historico"), height = "420px")))
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico_comparacao"), height = "360px"))),
      bslib::card(
        bslib::card_header(rotulo_entenda()),
        bslib::card_body(texto_amostra())
      )
    )
  )
}

mod_amostra_server <- function(id, modo = shiny::reactive("light")) {
  shiny::moduleServer(id, function(input, output, session) {

    # Histórico: os ids de cada amostra sorteada com o n e a seleção atuais.
    # A estatística é recalculada para a variável e a medida escolhidas.
    historico <- shiny::reactiveVal(list())

    adicionar <- function(quantas = 1) {
      novas <- purrr::map(seq_len(quantas), \(i) sortear_amostra(input$n, input$selecao))
      historico(utils::tail(c(historico(), novas), limite_historico))
    }

    shiny::observeEvent(list(input$n, input$selecao), {
      historico(list())
      adicionar(1)
    })
    shiny::observeEvent(input$sortear, adicionar(1))
    shiny::observeEvent(input$sortear_varias, adicionar(20))

    shiny::observeEvent(input$variavel, {
      escolhas <- opcoes_medida(input$variavel)
      atual <- if (input$medida %in% escolhas) input$medida else escolhas[[1]]
      shiny::updateSelectInput(session, "medida", choices = escolhas, selected = atual)
    })

    # A medida só vale se combinar com a variável (evita um ciclo com valor velho)
    medida <- shiny::reactive({
      shiny::req(input$medida %in% medidas_da_variavel(input$variavel))
      input$medida
    })

    parametro <- shiny::reactive(valor_parametro(input$variavel, medida()))

    estatisticas <- shiny::reactive({
      shiny::req(length(historico()) > 0)
      purrr::map_dbl(historico(), \(ids) calcular_medida(ids, input$variavel, medida()))
    })

    ultima <- shiny::reactive(utils::tail(historico(), 1)[[1]])

    output$titulo_parametro <- shiny::renderText({
      m <- medidas[[medida()]]
      sprintf(tr("Parâmetro: %s da população (%s)", "Parameter: population %s (%s)"), tolower(m$rotulo), m$parametro)
    })

    output$titulo_estatistica <- shiny::renderText({
      m <- medidas[[medida()]]
      sprintf(
        tr("Estatística: %s da amostra (%s), n = %s", "Statistic: sample %s (%s), n = %s"),
        tolower(m$rotulo), m$estatistica, fmt_num(length(ultima()), 0)
      )
    })

    output$parametro <- shiny::renderText(fmt_medida(parametro(), input$variavel, medida()))
    output$estatistica <- shiny::renderText(fmt_medida(utils::tail(estatisticas(), 1), input$variavel, medida()))

    output$erro <- shiny::renderText({
      erro <- utils::tail(estatisticas(), 1) - parametro()
      sinal <- if (erro > 0) "+" else if (erro < 0) "−" else ""
      paste0(sinal, fmt_medida(abs(erro), input$variavel, medida()))
    })

    output$erro_medio <- shiny::renderText({
      k <- length(estatisticas())
      if (k < 2) return(tr("Sorteie mais amostras para ver o erro médio", "Draw more samples to see the average error"))
      erro <- mean(estatisticas()) - parametro()
      sinal <- if (erro > 0) "+" else if (erro < 0) "−" else ""
      sprintf(
        tr("Erro médio em %s amostras: %s", "Average error over %s samples: %s"),
        fmt_num(k, 0), paste0(sinal, fmt_medida(abs(erro), input$variavel, medida()))
      )
    })

    output$grafico_grade <- shiny::renderPlot({
      pal <- paleta_modo(modo())
      sorteados <- populacao$id %in% ultima()
      fora <- identical(input$selecao, "online") & populacao$internet == 0
      dados <- populacao
      dados$grupo <- ifelse(sorteados, "sorteado", ifelse(fora, "fora", "populacao"))

      ggplot2::ggplot(dados, ggplot2::aes(x = coluna, y = -linha)) +
        ggplot2::geom_point(
          data = dados[dados$grupo == "fora", ],
          shape = 1, size = 2.1, colour = pal$neutro, alpha = 0.45
        ) +
        ggplot2::geom_point(data = dados[dados$grupo == "populacao", ], size = 2.3, colour = pal$neutro) +
        ggplot2::geom_point(data = dados[dados$grupo == "sorteado", ], size = 2.8, colour = cores_app$destaque) +
        ggplot2::coord_fixed() +
        ggplot2::labs(
          title = sprintf(tr("A população: %s pessoas", "The population: %s people"), fmt_num(nrow(populacao), 0)),
          subtitle = sprintf(tr("Em azul, as %s sorteadas para a amostra", "In blue, the %s drawn into the sample"), fmt_num(sum(sorteados), 0)),
          caption = if (identical(input$selecao, "online")) {
            tr("Círculos vazados: quem não usa internet e nunca entra na pesquisa online", "Hollow circles: people who do not use the internet and never enter the online survey")
          }
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(axis.text = ggplot2::element_blank(), axis.title = ggplot2::element_blank(), panel.grid = ggplot2::element_blank())
    }, bg = "transparent")

    output$grafico_historico <- shiny::renderPlot({
      pal <- paleta_modo(modo())
      est <- estatisticas()
      dados <- data.frame(sorteio = seq_along(est), valor = est)
      m <- medidas[[medida()]]
      eixo <- if (identical(medida(), "proporcao")) scales::label_percent(decimal.mark = tr(",", ".")) else rotulos_eixo()

      ggplot2::ggplot(dados, ggplot2::aes(x = sorteio, y = valor)) +
        ggplot2::geom_hline(yintercept = parametro(), colour = cores_app$referencia, linewidth = 1) +
        ggplot2::geom_point(colour = cores_app$destaque, alpha = 0.45, size = 2.2) +
        ggplot2::geom_point(data = utils::tail(dados, 1), colour = cores_app$destaque, size = 4) +
        ggplot2::scale_x_continuous(labels = rotulos_eixo(), limits = c(1, max(10, nrow(dados)))) +
        ggplot2::scale_y_continuous(labels = eixo) +
        ggplot2::labs(
          title = tr("Uma estatística por amostra", "One statistic per sample"),
          subtitle = sprintf(tr("%s (%s) de cada amostra sorteada", "%s (%s) of each sample drawn"), m$rotulo, m$estatistica),
          x = tr("Amostra", "Sample"), y = NULL,
          caption = sprintf(tr("Linha vermelha: o parâmetro (%s)", "Red line: the parameter (%s)"), m$parametro)
        ) +
        tema_grafico(modo())
    })

    output$grafico_comparacao <- shiny::renderPlot({
      pal <- paleta_modo(modo())
      v <- input$variavel
      amostra <- populacao[populacao$id %in% ultima(), ]

      if (identical(variaveis_pop[[v]]$tipo, "binaria")) {
        dados <- data.frame(
          grupo = factor(
            c(tr("População", "Population"), tr("Amostra", "Sample")),
            levels = c(tr("População", "Population"), tr("Amostra", "Sample"))
          ),
          valor = c(mean(populacao$internet), mean(amostra$internet))
        )
        return(
          ggplot2::ggplot(dados, ggplot2::aes(x = grupo, y = valor, fill = grupo)) +
            ggplot2::geom_col(width = 0.55) +
            ggplot2::geom_text(ggplot2::aes(label = fmt_pct(valor, 1)), vjust = -0.5, colour = pal$texto, size = 5) +
            ggplot2::scale_fill_manual(values = c(pal$neutro, cores_app$destaque)) +
            ggplot2::scale_y_continuous(labels = scales::label_percent(decimal.mark = tr(",", ".")), limits = c(0, 1.08)) +
            ggplot2::labs(
              title = tr("Quem usa internet", "Who uses the internet"),
              subtitle = tr("Proporção na população e na amostra", "Proportion in the population and in the sample"),
              x = NULL, y = NULL
            ) +
            tema_grafico(modo())
        )
      }

      dados <- rbind(
        data.frame(grupo = "populacao", valor = populacao[[v]]),
        data.frame(grupo = "amostra", valor = amostra[[v]])
      )
      limites <- stats::quantile(populacao[[v]], c(0, 0.99))
      quebras <- seq(limites[1], limites[2], length.out = 26)

      ggplot2::ggplot(mapping = ggplot2::aes(x = valor)) +
        ggplot2::geom_histogram(
          data = dados[dados$grupo == "populacao", ], ggplot2::aes(y = ggplot2::after_stat(density)),
          breaks = quebras, fill = pal$neutro, colour = pal$fundo
        ) +
        ggplot2::geom_histogram(
          data = dados[dados$grupo == "amostra", ], ggplot2::aes(y = ggplot2::after_stat(density)),
          breaks = quebras, fill = NA, colour = cores_app$destaque, linewidth = 0.9
        ) +
        ggplot2::coord_cartesian(xlim = limites) +
        ggplot2::scale_x_continuous(labels = rotulos_eixo()) +
        ggplot2::scale_y_continuous(labels = NULL) +
        ggplot2::labs(
          title = tr("A amostra é um retrato menor da população", "The sample is a smaller picture of the population"),
          subtitle = tr("Barras cinza: população. Contorno azul: amostra.", "Grey bars: population. Blue outline: sample."),
          x = variaveis_pop[[v]]$rotulo, y = NULL,
          caption = tr("Alturas em densidade, para comparar grupos de tamanhos diferentes", "Heights in density, to compare groups of different sizes")
        ) +
        tema_grafico(modo())
    })
  })
}
