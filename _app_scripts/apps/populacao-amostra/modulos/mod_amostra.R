# =============================================================================
# Script:  mod_amostra.R (módulo: uma amostra tirada da população)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-26
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

# Erro com sinal. O sinal sai do valor já arredondado, para um erro desprezível
# (como o do censo) aparecer como 0, e não como +0 ou −0.
fmt_erro <- function(erro, variavel, medida) {
  texto <- fmt_medida(abs(erro), variavel, medida)
  zero <- fmt_medida(0, variavel, medida)
  if (identical(texto, zero)) return(zero)
  paste0(if (erro > 0) "+" else "−", texto)
}

# Palpite antes do teste (prever, observar, explicar)
opcoes_palpite <- stats::setNames(
  c("some", "fica", "cresce"),
  tr(
    c("Cai até quase zero", "Continua mais ou menos o mesmo", "Fica maior"),
    c("It drops to almost zero", "It stays about the same", "It gets larger")
  )
)

texto_palpite <- function() {
  tr(
    paste(
      "Na pesquisa online, a idade média estimada erra para o mesmo lado. Se o tamanho da amostra subir de 50 para",
      "500, o que acontece com o erro médio?"
    ),
    paste(
      "In the online survey, the estimated mean age misses to the same side. If the sample size goes from 50 to",
      "500, what happens to the average error?"
    )
  )
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
        "Com amostra aleatória simples, cada sorteio erra um pouco para cima ou para baixo. Para a média e a",
        "proporção, os erros se compensam: os pontos oscilam em torno da linha vermelha, e quanto maior o n, mais",
        "perto dela eles ficam. A mediana e o desvio padrão têm um pequeno erro sistemático com n pequeno (com",
        "n = 10, o desvio padrão da renda fica, em média, cerca de 8% abaixo de σ), que some quando n cresce."
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
        "With a simple random sample, each draw misses a little high or a little low. For the mean and the",
        "proportion, the errors balance out: the points scatter around the red line, and the larger n is, the closer",
        "they stay to it. The median and the standard deviation have a small systematic error when n is small (with",
        "n = 10, the income standard deviation falls, on average, about 8% below σ), which fades as n grows."
      ),
      shiny::p(
        "The online survey is different. Since people who do not use the internet are never drawn, and that group is",
        "older and earns less, the statistic now misses, on average, to the same side. That is sampling bias, here",
        "caused by undercoverage: the group that can be drawn leaves out part of the population (Agresti and Finlay,",
        "section 2.3). A larger sample does not remove it: increasing n only makes the error more stable."
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
      shiny::sliderInput(ns("n"), tr("Tamanho da amostra (n)", "Sample size (n)"), min = 10, max = nrow(populacao), value = 50, step = 10, sep = sep_milhar()),
      shiny::radioButtons(ns("selecao"), tr("Como a amostra é escolhida", "How the sample is chosen"), choices = stats::setNames(names(selecoes), unlist(selecoes))),
      shiny::actionButton(ns("sortear"), tr("Sortear nova amostra", "Draw a new sample"), class = "btn-primary", icon = shiny::icon("dice")),
      shiny::actionButton(ns("sortear_varias"), tr("Sortear mais 20", "Draw 20 more"), icon = shiny::icon("forward")),
      shiny::helpText(tr(
        paste(
          "Trocar a variável ou a medida mantém as mesmas pessoas sorteadas. Trocar n ou a forma de escolha recomeça o",
          "histórico. Com n = 2.000, a amostra aleatória simples inclui todo mundo: é um censo."
        ),
        paste(
          "Changing the variable or the measure keeps the same people. Changing n or how the sample is chosen restarts",
          "the history. With n = 2,000, the simple random sample includes everyone: it is a census."
        )
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
      bslib::navset_card_underline(
        full_screen = TRUE,
        bslib::nav_panel(
          tr("Sorteio a sorteio", "Draw by draw"),
          shiny::plotOutput(ns("grafico_historico"), height = "380px")
        ),
        bslib::nav_panel(
          tr("Distribuição amostral", "Sampling distribution"),
          shiny::plotOutput(ns("grafico_distribuicao"), height = "380px")
        )
      )
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico_comparacao"), height = "360px"))),
      shiny::div(
        bslib::card(
          bslib::card_header(tr("Antes de testar, dê seu palpite", "Before you test it, make a guess")),
          bslib::card_body(
            shiny::p(class = "mb-1", texto_palpite()),
            shiny::radioButtons(ns("palpite"), NULL, choices = opcoes_palpite, selected = character(0)),
            shiny::actionButton(ns("montar_teste"), tr("Registrar palpite e montar o teste", "Lock in my guess and set up the test"), class = "btn-outline-primary btn-sm", icon = shiny::icon("flask")),
            shiny::uiOutput(ns("resultado_palpite"))
          )
        ),
        bslib::card(
          bslib::card_header(rotulo_entenda()),
          bslib::card_body(texto_amostra())
        )
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
      n <- length(ultima())
      sprintf(
        tr("Estatística: %s da amostra (%s), n = %s%s", "Statistic: sample %s (%s), n = %s%s"),
        tolower(m$rotulo), m$estatistica, fmt_num(n, 0),
        if (n == nrow(populacao)) tr(" (censo)", " (census)") else ""
      )
    })

    output$parametro <- shiny::renderText(fmt_medida(parametro(), input$variavel, medida()))
    output$estatistica <- shiny::renderText(fmt_medida(utils::tail(estatisticas(), 1), input$variavel, medida()))

    output$erro <- shiny::renderText({
      fmt_erro(utils::tail(estatisticas(), 1) - parametro(), input$variavel, medida())
    })

    output$erro_medio <- shiny::renderText({
      k <- length(estatisticas())
      if (k < 2) return(tr("Sorteie mais amostras para ver o erro médio", "Draw more samples to see the average error"))
      sprintf(
        tr("Erro médio em %s amostras: %s", "Average error over %s samples: %s"),
        fmt_num(k, 0), fmt_erro(mean(estatisticas()) - parametro(), input$variavel, medida())
      )
    })

    # Palpite: registra a escolha e monta o teste; quem sorteia é o leitor
    palpite <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$montar_teste, {
      shiny::req(input$palpite)
      palpite(input$palpite)
      shiny::updateSelectInput(session, "variavel", selected = "idade")
      shiny::updateSelectInput(session, "medida", selected = "media")
      shiny::updateRadioButtons(session, "selecao", selected = "online")
      shiny::updateSliderInput(session, "n", value = 50)
    })

    output$resultado_palpite <- shiny::renderUI({
      shiny::req(palpite())
      teste_montado <- identical(input$selecao, "online") && identical(input$variavel, "idade") && identical(input$medida, "media")
      k <- length(estatisticas())

      passo <- if (!teste_montado) {
        tr("Volte para a pesquisa online, a idade e a média para fazer o teste.", "Switch back to the online survey, age and the mean to run the test.")
      } else if (input$n < 500 || k < 20) {
        tr(
          "Teste montado. Clique em \"Sortear mais 20\" e anote o erro médio. Depois leve n para 500 e sorteie mais 20.",
          "Test set up. Click \"Draw 20 more\" and note the average error. Then move n to 500 and draw 20 more."
        )
      }
      if (!is.null(passo)) return(shiny::div(class = "alert alert-info mt-3 mb-0 small", passo))

      acertou <- identical(palpite(), "fica")
      shiny::div(
        class = paste("alert mt-3 mb-0 small", if (acertou) "alert-success" else "alert-warning"),
        shiny::strong(if (acertou) tr("Seu palpite se confirmou. ", "Your guess held up. ") else tr("O resultado contraria o palpite. ", "The result goes against your guess. ")),
        sprintf(
          tr(
            "Com n = %s, o erro médio ficou em %s. Quem não usa internet continua de fora, e por isso o erro não some: amostra maior reduz a oscilação, não o viés.",
            "With n = %s, the average error was %s. People who do not use the internet are still left out, so the error does not go away: a larger sample reduces the scatter, not the bias."
          ),
          fmt_num(input$n, 0), fmt_erro(mean(estatisticas()) - parametro(), "idade", "media")
        )
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

    # A distribuição das estatísticas ao longo das amostras: a distribuição amostral
    output$grafico_distribuicao <- shiny::renderPlot({
      pal <- paleta_modo(modo())
      est <- estatisticas()
      m <- medidas[[medida()]]
      eixo <- if (identical(medida(), "proporcao")) scales::label_percent(decimal.mark = tr(",", ".")) else rotulos_eixo()

      base <- ggplot2::ggplot() +
        ggplot2::labs(
          title = sprintf(tr("Distribuição amostral de %s, aproximada", "Sampling distribution of %s, approximated"), m$estatistica),
          subtitle = sprintf(
            tr(
              "%s amostras sorteadas. Com todas as amostras possíveis, seria a distribuição exata.",
              "%s samples drawn. With every possible sample, it would be the exact distribution."
            ),
            fmt_num(length(est), 0)
          ),
          x = NULL, y = NULL
        ) +
        tema_grafico(modo())

      if (length(est) < 20) {
        return(
          base +
            ggplot2::annotate(
              "text", x = 0, y = 0, size = 5, colour = pal$secundario,
              label = tr("Clique em \"Sortear mais 20\" para\nver a forma da distribuição", "Click \"Draw 20 more\" to\nsee the shape of the distribution")
            ) +
            ggplot2::theme(axis.text = ggplot2::element_blank(), panel.grid = ggplot2::element_blank())
        )
      }

      base +
        ggplot2::geom_histogram(
          data = data.frame(valor = est), ggplot2::aes(x = valor),
          bins = 25, fill = cores_app$destaque, colour = pal$fundo, alpha = 0.8
        ) +
        ggplot2::geom_vline(xintercept = parametro(), colour = cores_app$referencia, linewidth = 1.2) +
        ggplot2::geom_vline(xintercept = mean(est), colour = pal$linha, linetype = "dashed", linewidth = 0.9) +
        ggplot2::scale_x_continuous(labels = eixo) +
        ggplot2::scale_y_continuous(labels = NULL) +
        ggplot2::labs(
          caption = sprintf(
            tr(
              "Linha vermelha: o parâmetro (%s). Tracejada: a média das estatísticas. A distância entre as duas é o viés.",
              "Red line: the parameter (%s). Dashed: the mean of the statistics. The gap between them is the bias."
            ),
            m$parametro
          )
        )
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
