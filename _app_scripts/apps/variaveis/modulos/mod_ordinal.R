# =============================================================================
# Script:  mod_ordinal.R (módulo: variável categórica ordinal)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-18
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/apps/variaveis/app.R)
# Saída:
#   - funções mod_ordinal_ui() e mod_ordinal_server()
# =============================================================================

texto_ordinal <- function() {
  tr(
    shiny::tagList(
      shiny::p(
        "Numa variável ordinal, as categorias têm uma ordem natural: \"Médio\" vem depois de \"Fundamental\".",
        "Isso libera tudo o que depende de ordenar as observações: frequência acumulada, mediana, quartis e",
        "categorias mínima e máxima. O que continua faltando é a distância. Não dá para dizer que a diferença",
        "entre \"Neutro\" e \"Satisfeito\" é igual à diferença entre \"Satisfeito\" e \"Muito satisfeito\"."
      ),
      shiny::p(
        "A mediana é a primeira categoria em que a frequência acumulada atinge 50%. No gráfico, é a barra em",
        "destaque: a primeira cujo ponto da frequência acumulada fica na linha tracejada de 50% ou acima dela."
      ),
      shiny::p(
        shiny::strong("Experimente:"),
        "escolha o formato \"Polarizada\". As modas ficam nos extremos, mas a mediana cai no meio, numa categoria",
        "que pouca gente escolheu. Nenhum resumo de um número só descreve bem esse caso: olhe a distribuição inteira."
      )
    ),
    shiny::tagList(
      shiny::p(
        "In an ordinal variable, the categories have a natural order: \"High school\" comes after \"Elementary/middle school\".",
        "That unlocks everything that depends on ranking the observations: cumulative frequency, median,",
        "quartiles and minimum and maximum categories. What is still missing is distance. You cannot say that",
        "the gap between \"Neutral\" and \"Satisfied\" equals the gap between \"Satisfied\" and \"Very satisfied\"."
      ),
      shiny::p(
        "The median is the first category where the cumulative frequency reaches 50%. In the chart it is the",
        "highlighted bar: the first one whose cumulative point is at or above the dashed 50% line."
      ),
      shiny::p(
        shiny::strong("Try it:"),
        "choose the \"Polarized\" shape. The modes sit at the extremes, but the median falls in the middle, in a",
        "category few people chose. No single-number summary describes this case well: look at the whole distribution."
      )
    )
  )
}

cuidados_ordinal <- function() {
  tr(
    c(
      "Média dos códigos" = paste(
        "não é indicada, mas é comum em escalas de 1 a 5. Ela pressupõe que as categorias são igualmente",
        "espaçadas, um pressuposto forte que a escala ordinal não garante. Se usar, mostre junto a distribuição",
        "e a mediana, nunca a média sozinha."
      ),
      "Desvio padrão e variância" = paste(
        "também não são indicados, porque dependem do mesmo pressuposto de distâncias iguais. Se a média",
        "for informada, o desvio padrão pode acompanhá-la, com a mesma ressalva."
      ),
      "Amplitude e distância interquartil" = paste(
        "como diferenças numéricas, não fazem sentido. Informe as categorias extremas e os quartis",
        "como categorias (\"de Insatisfeito a Satisfeito\")."
      ),
      "Ordem das barras" = "deve seguir a ordem das categorias. Reordenar pela frequência esconde a estrutura da variável."
    ),
    c(
      "Mean of the codes" = paste(
        "is not recommended, but it is common for 1-to-5 scales. It assumes the categories are equally spaced,",
        "a strong assumption that an ordinal scale does not guarantee. If you use it, show the distribution and",
        "the median alongside it, never the mean on its own."
      ),
      "Standard deviation and variance" = paste(
        "are not recommended either, because they rest on the same equal-spacing assumption. If the mean is",
        "reported, the standard deviation can go with it, with the same caveat."
      ),
      "Range and interquartile range" = paste(
        "make no sense as numeric differences. Report the extreme categories and the quartiles",
        "as categories (\"from Dissatisfied to Satisfied\")."
      ),
      "Bar order" = "must follow the category order. Sorting by frequency hides the structure of the variable."
    )
  )
}

mod_ordinal_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      seletor_exemplo(ns("exemplo"), exemplos_ordinal),
      shiny::selectInput(ns("formato"), tr("Formato da distribuição", "Distribution shape"), choices = opcoes_exemplo(formatos_ordinal)),
      slider_amostra(ns("n"), min = 20, max = 2000, value = 200),
      shiny::checkboxInput(ns("acumulada"), tr("Mostrar frequência acumulada", "Show cumulative frequency"), value = TRUE),
      botao_sortear(ns("sortear"))
    ),
    cabecalho_tipo("ordinal"),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(title = tr("Moda", "Mode"), value = shiny::textOutput(ns("moda")), shiny::textOutput(ns("moda_desc")), theme = "primary"),
      bslib::value_box(title = tr("Mediana", "Median"), value = shiny::textOutput(ns("mediana")), shiny::textOutput(ns("mediana_desc")), theme = "primary"),
      bslib::value_box(
        title = tr("Quartis (Q1 a Q3)", "Quartiles (Q1 to Q3)"), value = shiny::textOutput(ns("quartis")),
        tr("Onde estão os 50% centrais", "Where the middle 50% are"), theme = "info"
      )
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(full_screen = TRUE, bslib::card_body(saida_grafico(ns("grafico"), height = "420px"))),
      bslib::card(bslib::card_header(tr("Tabela de frequências", "Frequency table")), bslib::card_body(shiny::uiOutput(ns("tabela"))))
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(bslib::card_header(tr("Estatísticas aplicáveis", "Applicable statistics")), bslib::card_body(shiny::uiOutput(ns("estatisticas")))),
      cartao_cuidados(tr("O que não usar, e por quê", "What not to use, and why"), cuidados_ordinal())
    ),
    bslib::card(bslib::card_header(rotulo_entenda()), bslib::card_body(texto_ordinal()))
  )
}

mod_ordinal_server <- function(id, modo = shiny::reactive("light")) {
  shiny::moduleServer(id, function(input, output, session) {

    exemplo <- shiny::reactive(exemplos_ordinal[[input$exemplo]])

    dados <- shiny::reactive({
      input$sortear
      categorias <- exemplo()$categorias
      pesos <- formatos_ordinal[[input$formato]]$pesos(length(categorias))
      gerar_categorica(categorias, pesos, input$n)
    })

    freq <- shiny::reactive(tabela_frequencias(dados()))
    mediana <- shiny::reactive(quantil_ordinal(dados(), 0.5))
    quartis <- shiny::reactive(quantil_ordinal(dados(), c(0.25, 0.75)))
    de_a <- \(a, b) if (a == b) a else paste(a, tr("a", "to"), b)

    output$moda <- shiny::renderText(moda_tabela(freq()))
    output$moda_desc <- shiny::renderText(sprintf(tr("%s das observações", "%s of the observations"), fmt_pct(max(freq()$prop))))
    output$mediana <- shiny::renderText(mediana())
    output$mediana_desc <- shiny::renderText({
      sprintf(
        tr("%s das observações estão nesta categoria", "%s of the observations are in this category"),
        fmt_pct(freq()$prop[freq()$categoria == mediana()])
      )
    })
    output$quartis <- shiny::renderText(de_a(quartis()[1], quartis()[2]))

    output$tabela <- shiny::renderUI({
      f <- freq()
      tabela <- data.frame(f$categoria, fmt_num(f$n, 0), fmt_pct(f$prop), fmt_pct(f$acum))
      names(tabela) <- nomes_freq(tr("Categoria", "Category"))
      tabela_html(tabela, classes = c("", "num", "num", "num"))
    })

    output$estatisticas <- shiny::renderUI({
      f <- freq()
      observadas <- f$categoria[f$n > 0]
      q <- quartis()
      tabela <- data.frame(
        tr(
          c(
            "Tamanho da amostra (n)", "Moda", "Mediana", "1º quartil (Q1)", "3º quartil (Q3)",
            "Categorias mínima e máxima", "Média dos códigos (não indicada)"
          ),
          c(
            "Sample size (n)", "Mode", "Median", "1st quartile (Q1)", "3rd quartile (Q3)",
            "Minimum and maximum categories", "Mean of the codes (not recommended)"
          )
        ),
        c(
          fmt_num(sum(f$n), 0), moda_tabela(f), mediana(), q[1], q[2],
          de_a(observadas[1], observadas[length(observadas)]),
          fmt_num(mean(as.integer(dados())), 2)
        ),
        tr(
          c(
            "Número de observações",
            "A categoria mais frequente",
            "Pelo menos metade está nesta categoria ou abaixo, e pelo menos metade nesta ou acima",
            "Pelo menos 25% das observações estão até esta categoria",
            "Pelo menos 75% das observações estão até esta categoria",
            "Categorias extremas observadas na amostra",
            sprintf("Códigos de 1 a %d; pressupõe distâncias iguais entre categorias, um pressuposto forte", nrow(f))
          ),
          c(
            "Number of observations",
            "The most frequent category",
            "At least half are in this category or below, and at least half in this one or above",
            "At least 25% of the observations are up to this category",
            "At least 75% of the observations are up to this category",
            "Extreme categories observed in the sample",
            sprintf("Codes from 1 to %d; assumes equal distances between categories, a strong assumption", nrow(f))
          )
        )
      )
      names(tabela) <- nomes_estatisticas()
      tabela_html(tabela, classes = c("", "num", "leitura"))
    })

    output$grafico <- render_grafico({
      f <- freq()
      pal <- paleta_modo(modo())
      f$categoria <- factor(f$categoria, levels = f$categoria)
      f$e_mediana <- as.character(f$categoria) == mediana()
      f$rotulo <- ifelse(f$n == max(f$n), paste0(fmt_pct(f$prop), "\n", tr("moda", "mode")), fmt_pct(f$prop))

      grafico <- ggplot2::ggplot(f, ggplot2::aes(x = categoria)) +
        ggplot2::geom_col(ggplot2::aes(y = 100 * prop, fill = e_mediana), width = 0.7) +
        ggplot2::geom_text(ggplot2::aes(y = 100 * prop, label = rotulo), vjust = -0.3, size = 4.2, colour = pal$texto, lineheight = 0.9) +
        ggplot2::scale_fill_manual(values = c(`TRUE` = cores_app$destaque, `FALSE` = pal$neutro))

      if (isTRUE(input$acumulada)) {
        grafico <- grafico +
          ggplot2::geom_hline(yintercept = 50, linetype = "dashed", colour = pal$secundario) +
          ggplot2::geom_line(ggplot2::aes(y = 100 * acum, group = 1), colour = cores_app$referencia, linewidth = 1) +
          ggplot2::geom_point(ggplot2::aes(y = 100 * acum), colour = cores_app$referencia, size = 2.5)
      }

      grafico +
        ggplot2::scale_x_discrete(labels = scales::label_wrap(12)) +
        ggplot2::scale_y_continuous(limits = c(0, 108), breaks = seq(0, 100, 25)) +
        ggplot2::labs(
          title = tr("Frequência por categoria, na ordem natural", "Frequency by category, in natural order"),
          subtitle = exemplo()$rotulo,
          x = NULL, y = "%",
          caption = paste(
            tr("Dados simulados | Em destaque: categoria mediana", "Simulated data | Highlighted: median category"),
            if (isTRUE(input$acumulada)) tr("| Linha vermelha: % acumulada", "| Red line: cumulative %") else ""
          )
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
    })
  })
}
