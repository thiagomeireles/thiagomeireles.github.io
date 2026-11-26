# =============================================================================
# Script:  mod_contagem.R (módulo: técnicas de contagem 2 × 2, pôquer, loteria
#          e armadilha de equiprobabilidade na amostragem com reposição)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-26
#
# Entrada:
#   - objetos e funções de _app_scripts/apps/espaco-amostral/modulos/dados.R
# Saída:
#   - funções mod_contagem_ui() e mod_contagem_server()
# =============================================================================

texto_contagem <- function() {
  tr(
    shiny::withMathJax(
      shiny::p(
        "Em espaços amostrais finitos com resultados equiprováveis, calcular \\(P(A) = \\#A / \\#\\Omega\\) reduz-se",
        "a contar elementos sem precisar listá-los um a um. Pelo Teorema Fundamental da Contagem (Casella e Berger,",
        "Teo. 1.2.14; Morettin e Bussab, cap. 5), uma tarefa realizada em \\(k\\) etapas com \\(n_1, n_2, \\dots, n_k\\)",
        "escolhas possíveis admite \\(n_1 \\times n_2 \\times \\dots \\times n_k\\) sequências. Cruzar duas perguntas —",
        "se a ordem importa e se há reposição dos objetos — organiza todas as escolhas de \\(r\\) itens entre \\(n\\)",
        "disponíveis na tabela \\(2 \\times 2\\) de Casella e Berger (Tabela 1.2.1): ordenado sem reposição",
        "\\(n!/(n - r)!\\), ordenado com reposição \\(n^r\\), não ordenado sem reposição",
        "\\(\\binom{n}{r} = \\frac{n!}{r!(n - r)!}\\), e não ordenado com reposição \\(\\binom{n + r - 1}{r}\\)",
        "(obtido dispondo \\(r\\) marcadores e \\(n - 1\\) divisórias internas)."
      ),
      shiny::p(
        "Contar subconjuntos, no entanto, não garante que eles tenham a mesma probabilidade. Sem reposição, cada",
        "conjunto não ordenado de \\(r\\) itens corresponde a exatamente \\(r!\\) sequências ordenadas; por isso,",
        "em qualquer evento que não dependa da ordem (como uma mão de pôquer no Ex. 1.2.18), o fator \\(r!\\) se",
        "cancela entre numerador e denominador, e os dois espaços entregam a mesma probabilidade. Com reposição, por",
        "outro lado, a simetria entre conjuntos não ordenados desaparece: ao sortear \\(r = 2\\) bolas com reposição",
        "de \\(\\{1, 2, 3\\}\\) (Ex. 1.2.19), os \\(3^2 = 9\\) pares ordenados têm probabilidade \\(1/9\\) cada, mas",
        "o par não ordenado \\(\\{1, 3\\}\\) agrupa \\((1, 3)\\) e \\((3, 1)\\) (probabilidade \\(2/9\\)), enquanto",
        "\\(\\{1, 1\\}\\) corresponde só a \\((1, 1)\\) (probabilidade \\(1/9\\)). Em outras palavras, a probabilidade",
        "decorre do mecanismo físico de sorteio — que opera sobre sequências ordenadas —, base exata das reamostragens",
        "com reposição no *bootstrap* (Ex. 1.2.20)."
      ),
      shiny::p(
        shiny::strong("Experimente:"),
        "selecione a loteria 6 de 44 para comparar os quatro quadrantes da tabela \\(2 \\times 2\\); depois alterne o",
        "painel da direita entre os sorteios com reposição em \\(\\{1, 2, 3\\}\\), a distribuição exata das médias em",
        "\\(\\{2, 4, 9, 12\\}\\) e as mãos de pôquer."
      )
    ),
    shiny::withMathJax(
      shiny::p(
        "On finite sample spaces with equally likely outcomes, evaluating \\(P(A) = \\#A / \\#S\\) comes down to",
        "counting outcomes without listing them all. By the Fundamental Theorem of Counting (Casella and Berger,",
        "Thm. 1.2.14), a procedure with \\(k\\) stages offering \\(n_1, n_2, \\dots, n_k\\) choices produces",
        "\\(n_1 \\times n_2 \\times \\dots \\times n_k\\) sequences. Crossing two questions — whether order matters",
        "and whether items are replaced — organizes every selection of \\(r\\) objects from \\(n\\) into Casella and",
        "Berger's \\(2 \\times 2\\) table (Table 1.2.1): ordered without replacement \\(n!/(n - r)!\\), ordered with",
        "replacement \\(n^r\\), unordered without replacement \\(\\binom{n}{r} = \\frac{n!}{r!(n - r)!}\\), and",
        "unordered with replacement \\(\\binom{n + r - 1}{r}\\) (derived by placing \\(r\\) markers among \\(n - 1\\)",
        "interior walls)."
      ),
      shiny::p(
        "Counting distinct subsets, however, does not make those subsets equally likely. Without replacement, every",
        "unordered set of size \\(r\\) corresponds to \\(r!\\) ordered sequences; for any order-free event (such as a",
        "poker hand in Ex. 1.2.18), the factor \\(r!\\) cancels between numerator and denominator, so ordered and",
        "unordered spaces give the same probability. With replacement, on the other hand, unordered sets lose equal",
        "probability: when drawing \\(r = 2\\) numbers with replacement from \\(\\{1, 2, 3\\}\\) (Ex. 1.2.19), all",
        "\\(3^2 = 9\\) ordered pairs have probability \\(1/9\\), yet the unordered pair \\(\\{1, 3\\}\\) collects both",
        "\\((1, 3)\\) and \\((3, 1)\\) (probability \\(2/9\\)), whereas \\(\\{1, 1\\}\\) comes only from \\((1, 1)\\)",
        "(probability \\(1/9\\)). Put differently, probabilities are governed by the sampling mechanism — which acts",
        "on ordered sequences — the exact foundation of *bootstrap* resampling with replacement (Ex. 1.2.20)."
      ),
      shiny::p(
        shiny::strong("Try it:"),
        "load the 6-from-44 lottery to inspect all four cells of the \\(2 \\times 2\\) table; then switch the right",
        "panel across draws with replacement from \\(\\{1, 2, 3\\}\\), the exact distribution of sample means from",
        "\\(\\{2, 4, 9, 12\\}\\) and classic poker hands."
      )
    )
  )
}

cuidados_contagem <- function() {
  tr(
    c(
      "O erro n^r / r! com reposição" = paste(
        "dividir n^r por r! só funciona quando os r elementos sorteados são todos distintos. Na loteria 6 de 44 com",
        "reposição, 44⁶ / 6! = 10.078.213,7 sequer é inteiro; a contagem correta é C(49, 6) = 13.983.816."
      ),
      "Não ordenados com reposição não têm chance 1 / C(n+r−1, r)" = paste(
        "uma amostra com valores repetidos k₁, …, k_m vezes reúne r! / (k₁! ⋯ k_m!) sequências ordenadas de",
        "probabilidade 1 / n^r cada (coeficiente multinomial, Ex. 1.2.20)."
      ),
      "Estouro numérico em n!" = paste(
        "em R, factorial(171) já estoura para Inf. Para razões e combinações grandes, calcule diretamente com",
        "choose(), lchoose() ou lgamma() no espaço logarítmico."
      )
    ),
    c(
      "The n^r / r! trap under replacement" = paste(
        "dividing n^r by r! works only when all r drawn items are distinct. In the 6-from-44 lottery with",
        "replacement, 44⁶ / 6! = 10,078,213.7 is not even an integer; the true count is C(49, 6) = 13,983,816."
      ),
      "Unordered sets with replacement do not have chance 1 / C(n+r−1, r)" = paste(
        "an unordered sample with multiplicities k₁, …, k_m gathers r! / (k₁! ⋯ k_m!) ordered sequences of",
        "probability 1 / n^r each (multinomial count, Ex. 1.2.20)."
      ),
      "Numeric overflow in n!" = paste(
        "in R, factorial(171) overflows to Inf. For large combinations and ratios, compute with choose(), lchoose()",
        "or lgamma() on the log scale."
      )
    )
  )
}

mod_contagem_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 310,
      shiny::selectInput(
        ns("preset"), tr("Exemplo de referência (n e r)", "Reference example (n and r)"),
        choices = stats::setNames(names(exemplos_contagem), purrr::map_chr(exemplos_contagem, "rotulo")),
        selected = "loteria_cb"
      ),
      shiny::sliderInput(
        ns("n"), tr("Objetos disponíveis (n)", "Available objects (n)"),
        min = 2, max = 52, value = 44, step = 1
      ),
      shiny::sliderInput(
        ns("r"), tr("Tamanho da escolha (r)", "Selection size (r)"),
        min = 1, max = 12, value = 6, step = 1
      ),
      shiny::radioButtons(
        ns("celula"), tr("Esquema destacado nos cartões", "Scheme highlighted in cards"),
        choices = stats::setNames(
          c("nao_ordenado_sem", "ordenado_sem", "ordenado_com", "nao_ordenado_com"),
          tr(
            c(
              "Não ordenado, sem reposição: C(n, r)",
              "Ordenado, sem reposição: n! / (n − r)!",
              "Ordenado, com reposição: n^r",
              "Não ordenado, com reposição: C(n + r − 1, r)"
            ),
            c(
              "Unordered, without replacement: C(n, r)",
              "Ordered, without replacement: n! / (n − r)!",
              "Ordered, with replacement: n^r",
              "Unordered, with replacement: C(n + r − 1, r)"
            )
          )
        ),
        selected = "nao_ordenado_sem"
      ),
      shiny::hr(),
      shiny::selectInput(
        ns("estudo_caso"), tr("Estudo de equiprobabilidade (direita)", "Equal-probability case study (right)"),
        choices = stats::setNames(
          c("urnas3", "medias4", "poquer"),
          tr(
            c(
              "Ex. 1.2.19: r = 2 com reposição em {1, 2, 3}",
              "Ex. 1.2.20: médias de r = 4 com reposição em {2, 4, 9, 12}",
              "Ex. 1.2.18: mãos de pôquer em C(52, 5)"
            ),
            c(
              "Ex. 1.2.19: r = 2 with replacement from {1, 2, 3}",
              "Ex. 1.2.20: sample means of r = 4 from {2, 4, 9, 12}",
              "Ex. 1.2.18: poker hands in C(52, 5)"
            )
          )
        ),
        selected = "urnas3"
      )
    ),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(
        title = shiny::textOutput(ns("vb_cel_titulo")),
        value = shiny::textOutput(ns("vb_cel_valor")),
        shiny::textOutput(ns("vb_cel_formula")),
        theme = "primary"
      ),
      bslib::value_box(
        title = tr("Razão ordenado / não ordenado", "Ordered / unordered ratio"),
        value = shiny::textOutput(ns("vb_razao")),
        shiny::textOutput(ns("vb_razao_desc")),
        theme = "info"
      ),
      bslib::value_box(
        title = tr("Probabilidade de 1 resultado neste esquema", "Probability of 1 outcome in this scheme"),
        value = shiny::textOutput(ns("vb_prob1")),
        shiny::textOutput(ns("vb_prob1_desc")),
        theme = NULL
      )
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)),
      bslib::card(
        bslib::card_header(tr("Tabela 2 × 2 de contagem (Casella e Berger, Tabela 1.2.1)", "2 × 2 counting table (Casella and Berger, Table 1.2.1)")),
        bslib::card_body(
          shiny::uiOutput(ns("tabela_2x2")),
          shiny::p(class = "small text-muted mt-2 mb-0", shiny::textOutput(ns("nota_preset"), inline = TRUE))
        )
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(shiny::textOutput(ns("titulo_estudo"))),
        bslib::card_body(shiny::uiOutput(ns("painel_estudo")))
      )
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
      bslib::card(bslib::card_header(rotulo_entenda()), bslib::card_body(texto_contagem())),
      cartao_cuidados(tr("Cuidados", "Watch out"), cuidados_contagem())
    )
  )
}

mod_contagem_server <- function(id, modo = shiny::reactive("light")) {
  shiny::moduleServer(id, function(input, output, session) {

    shiny::observeEvent(input$preset, {
      ex <- exemplos_contagem[[input$preset]]
      if (!is.null(ex) && !identical(input$preset, "livre")) {
        shiny::updateSliderInput(session, "n", value = ex$n)
        shiny::updateSliderInput(session, "r", value = ex$r)
      }
    }, ignoreInit = TRUE)

    contagens <- shiny::reactive({
      shiny::req(input$n, input$r)
      contar_quatro_casos(input$n, input$r)
    })

    output$vb_cel_titulo <- shiny::renderText({
      switch(
        input$celula,
        ordenado_sem     = tr("Ordenado, sem reposição", "Ordered, without replacement"),
        ordenado_com     = tr("Ordenado, com reposição", "Ordered, with replacement"),
        nao_ordenado_sem = tr("Não ordenado, sem reposição", "Unordered, without replacement"),
        nao_ordenado_com = tr("Não ordenado, com reposição", "Unordered, with replacement")
      )
    })

    output$vb_cel_valor <- shiny::renderText({
      formatar_inteiro(contagens()[[input$celula]])
    })

    output$vb_cel_formula <- shiny::renderText({
      n <- input$n
      r <- input$r
      switch(
        input$celula,
        ordenado_sem     = if (r > n) tr("r > n: impossível sem reposição (0)", "r > n: impossible without replacement (0)") else sprintf("%d! / (%d − %d)!", n, n, r),
        ordenado_com     = sprintf("%d^%d", n, r),
        nao_ordenado_sem = if (r > n) tr("r > n: C(n, r) = 0", "r > n: C(n, r) = 0") else sprintf("C(%d, %d)", n, r),
        nao_ordenado_com = sprintf("C(%d + %d − 1, %d) = C(%d, %d)", n, r, r, n + r - 1L, r)
      )
    })

    output$vb_razao <- shiny::renderText({
      cts <- contagens()
      r <- input$r
      sem_rep <- input$celula %in% c("ordenado_sem", "nao_ordenado_sem")
      if (sem_rep) {
        if (r > input$n) return("—")
        paste0("r! = ", formatar_inteiro(factorial(r)))
      } else {
        raz <- cts[["ordenado_com"]] / cts[["nao_ordenado_com"]]
        fmt_num(raz, 2)
      }
    })

    output$vb_razao_desc <- shiny::renderText({
      r <- input$r
      sem_rep <- input$celula %in% c("ordenado_sem", "nao_ordenado_sem")
      if (sem_rep) {
        tr("Sem reposição: todo subconjunto gera r! sequências", "Without replacement: every subset yields r! sequences")
      } else {
        sprintf(tr("Com reposição: n^r / C(n+r−1, r) < %d! (para r ≥ 2)", "With replacement: n^r / C(n+r−1, r) < %d! (for r ≥ 2)"), r)
      }
    })

    output$vb_prob1 <- shiny::renderText({
      cts <- contagens()
      val <- cts[[input$celula]]
      if (val <= 0) return("0")
      if (identical(input$celula, "nao_ordenado_com")) {
        return(tr("Varia!", "Varies!"))
      }
      paste0("1 / ", formatar_inteiro(val))
    })

    output$vb_prob1_desc <- shiny::renderText({
      if (identical(input$celula, "nao_ordenado_com")) {
        tr("Não equiprovável: depende das repetições (Ex. 1.2.19)", "Not equally likely: depends on repeats (Ex. 1.2.19)")
      } else {
        tr("Equiprovável sob sorteio aleatório simples", "Equally likely under random sampling")
      }
    })

    output$tabela_2x2 <- shiny::renderUI({
      cts <- contagens()
      n <- input$n
      r <- input$r
      df <- data.frame(
        tr(c("Ordenado", "Não ordenado"), c("Ordered", "Unordered")),
        c(
          paste0("n!/(n−r)! = ", formatar_inteiro(cts[["ordenado_sem"]])),
          paste0("C(n, r) = ", formatar_inteiro(cts[["nao_ordenado_sem"]]))
        ),
        c(
          paste0("n^r = ", formatar_inteiro(cts[["ordenado_com"]])),
          paste0("C(n+r−1, r) = ", formatar_inteiro(cts[["nao_ordenado_com"]]))
        ),
        stringsAsFactors = FALSE
      )
      names(df) <- c(
        sprintf("n = %d, r = %d", n, r),
        tr("Sem reposição", "Without replacement"),
        tr("Com reposição", "With replacement")
      )
      tabela_html(df, classes = c("", "num", "num"))
    })

    output$nota_preset <- shiny::renderText({
      ex <- exemplos_contagem[[input$preset]] %||% exemplos_contagem$livre
      ex$nota
    })

    output$titulo_estudo <- shiny::renderText({
      switch(
        input$estudo_caso,
        urnas3  = tr("Ex. 1.2.19: não ordenados com reposição não são equiprováveis", "Ex. 1.2.19: unordered outcomes with replacement are not equally likely"),
        medias4 = tr("Ex. 1.2.20: distribuição exata de X̄ em 256 amostras com reposição", "Ex. 1.2.20: exact distribution of X̄ across 256 samples with replacement"),
        poquer  = tr("Ex. 1.2.18: probabilidades exatas em C(52, 5) = 2.598.960 mãos", "Ex. 1.2.18: exact probabilities across C(52, 5) = 2,598,960 hands")
      )
    })

    output$painel_estudo <- shiny::renderUI({
      if (identical(input$estudo_caso, "poquer")) {
        df <- maos_poquer()
        names(df) <- tr(
          c("Mão (5 cartas)", "Contagem por etapas", "Mãos favoráveis", "P(mão)"),
          c("Hand (5 cards)", "Stagewise count", "Favorable hands", "P(hand)")
        )
        return(shiny::tagList(
          tabela_html(df, classes = c("", "leitura", "num", "num")),
          shiny::p(class = "small text-muted mt-2 mb-0", tr(
            "Total de mãos não ordenadas equiprováveis: C(52, 5) = 2.598.960 (Casella e Berger, Ex. 1.2.18).",
            "Total equally likely unordered hands: C(52, 5) = 2,598,960 (Casella and Berger, Ex. 1.2.18)."
          ))
        ))
      }
      shiny::plotOutput(session$ns("grafico_estudo"), height = "340px")
    })

    output$grafico_estudo <- shiny::renderPlot({
      p_modo <- paleta_modo(modo())

      if (identical(input$estudo_caso, "urnas3")) {
        df <- distribuicao_com_reposicao_3()
        df$conjunto <- factor(df$conjunto, levels = df$conjunto)
        ggplot2::ggplot(df, ggplot2::aes(x = conjunto, y = prob_real)) +
          ggplot2::geom_col(fill = cores_app$destaque, width = 0.62) +
          ggplot2::geom_hline(yintercept = 1 / 6, colour = cores_app$referencia, linetype = "dashed", linewidth = 0.9) +
          ggplot2::geom_text(
            ggplot2::aes(label = paste0(n_ord, "/9")),
            vjust = -0.45, size = 3.9, colour = p_modo$texto
          ) +
          ggplot2::scale_y_continuous(labels = rotulos_eixo(), limits = c(0, 0.28), expand = ggplot2::expansion(mult = c(0, 0.02))) +
          tema_grafico(modo()) +
          ggplot2::labs(
            title = tr("Probabilidade real nos 6 conjuntos não ordenados", "True probability across the 6 unordered sets"),
            subtitle = tr("Tracejado vermelho: 1/6 ≈ 0,167 (erro de tratar os 6 conjuntos como equiprováveis)", "Dashed red: 1/6 ≈ 0.167 (mistake of treating the 6 sets as equally likely)"),
            x = tr("Conjunto não ordenado em r = 2 sorteios de {1, 2, 3}", "Unordered set in r = 2 draws from {1, 2, 3}"),
            y = tr("Probabilidade verdadeira", "True probability"),
            caption = tr("Casella e Berger, Exemplo 1.2.19", "Casella and Berger, Example 1.2.19")
          )
      } else {
        df <- distribuicao_medias_cb1220()
        ggplot2::ggplot(df, ggplot2::aes(x = media, y = prob_real)) +
          ggplot2::geom_col(fill = cores_app$destaque, width = 0.22) +
          ggplot2::scale_x_continuous(labels = rotulos_eixo()) +
          ggplot2::scale_y_continuous(labels = rotulos_eixo(), expand = ggplot2::expansion(mult = c(0, 0.08))) +
          tema_grafico(modo()) +
          ggplot2::labs(
            title = tr("Distribuição exata das médias de 4 sorteios em {2, 4, 9, 12}", "Exact distribution of means of 4 draws from {2, 4, 9, 12}"),
            subtitle = tr("Ex.: {2, 4, 4, 9} dá média 4,75 com P = 12/256 = 0,0469 (e não 1/35 = 0,0286)", "E.g., {2, 4, 4, 9} gives mean 4.75 with P = 12/256 = 0.0469 (not 1/35 = 0.0286)"),
            x = tr("Média da amostra com reposição", "With-replacement sample mean"),
            y = tr("Probabilidade P(média)", "Probability P(mean)"),
            caption = tr("Casella e Berger, Exemplo 1.2.20 e Figura 1.2.2", "Casella and Berger, Example 1.2.20 and Figure 1.2.2")
          )
      }
    })
  })
}
