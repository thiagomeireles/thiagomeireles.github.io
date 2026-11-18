# =============================================================================
# Script:  mod_ordinal.R (módulo: variável categórica ordinal)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-18
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/apps/descritiva/app.R)
# Saída:
#   - funções mod_ordinal_ui() e mod_ordinal_server()
# =============================================================================

texto_ordinal <- function() {
  shiny::tagList(
    shiny::p(
      "Numa variável ordinal, as categorias têm uma ordem natural: \"Médio\" vem depois de \"Fundamental\".",
      "Isso libera tudo o que depende de ordenar as observações: frequência acumulada, mediana, quartis e",
      "categorias mínima e máxima. O que continua faltando é a distância. Não dá para dizer que a diferença",
      "entre \"Neutro\" e \"Satisfeito\" é igual à diferença entre \"Satisfeito\" e \"Muito satisfeito\"."
    ),
    shiny::p(
      "A mediana é a categoria em que a frequência acumulada passa de 50%. No gráfico, é a barra em destaque:",
      "a linha da frequência acumulada cruza a marca de 50% sobre ela."
    ),
    shiny::p(
      shiny::strong("Experimente:"),
      "escolha o formato \"Polarizada\". As modas ficam nos extremos, mas a mediana cai no meio, numa categoria",
      "que pouca gente escolheu. Nenhum resumo de um número só descreve bem esse caso: olhe a distribuição inteira."
    )
  )
}

cuidados_ordinal <- list(
  "Média dos códigos" = paste(
    "é comum em escalas de 1 a 5, mas supõe que as categorias são igualmente espaçadas. Use com cautela,",
    "sempre ao lado da distribuição, e nunca como único resumo."
  ),
  "Desvio padrão e variância" = "dependem de distâncias entre valores, que a escala ordinal não define.",
  "Amplitude e intervalo interquartil" = paste(
    "como diferenças numéricas, não fazem sentido. Informe as categorias extremas e os quartis",
    "como categorias (\"de Insatisfeito a Satisfeito\")."
  ),
  "Ordem das barras" = "deve seguir a ordem das categorias. Reordenar pela frequência esconde a estrutura da variável."
)

mod_ordinal_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::selectInput(ns("exemplo"), "Variável", choices = opcoes_exemplo(exemplos_ordinal)),
      shiny::selectInput(ns("formato"), "Formato da distribuição", choices = opcoes_exemplo(formatos_ordinal)),
      shiny::sliderInput(ns("n"), "Tamanho da amostra", min = 20, max = 2000, value = 200, step = 10, sep = "."),
      shiny::checkboxInput(ns("acumulada"), "Mostrar frequência acumulada", value = TRUE),
      botao_sortear(ns("sortear"))
    ),
    cabecalho_tipo("ordinal"),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(title = "Moda", value = shiny::textOutput(ns("moda")), shiny::textOutput(ns("moda_desc")), theme = "primary"),
      bslib::value_box(title = "Mediana", value = shiny::textOutput(ns("mediana")), shiny::textOutput(ns("mediana_desc")), theme = "primary"),
      bslib::value_box(title = "Quartis (Q1 a Q3)", value = shiny::textOutput(ns("quartis")), "Onde estão os 50% centrais", theme = "info")
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico"), height = "420px"))),
      bslib::card(bslib::card_header("Tabela de frequências"), bslib::card_body(shiny::uiOutput(ns("tabela"))))
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(bslib::card_header("Estatísticas aplicáveis"), bslib::card_body(shiny::uiOutput(ns("estatisticas")))),
      cartao_cuidados("O que não usar, e por quê", cuidados_ordinal)
    ),
    bslib::card(bslib::card_header("Entenda"), bslib::card_body(texto_ordinal()))
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

    output$moda <- shiny::renderText(moda_tabela(freq()))
    output$moda_desc <- shiny::renderText(sprintf("%s das observações", fmt_pct(max(freq()$prop))))
    output$mediana <- shiny::renderText(mediana())
    output$mediana_desc <- shiny::renderText({
      sprintf("%s das observações estão nesta categoria", fmt_pct(freq()$prop[freq()$categoria == mediana()]))
    })
    output$quartis <- shiny::renderText({
      q <- quartis()
      if (q[1] == q[2]) q[1] else paste(q[1], "a", q[2])
    })

    output$tabela <- shiny::renderUI({
      f <- freq()
      tabela <- data.frame(
        "Categoria" = f$categoria,
        "n" = fmt_num(f$n, 0),
        "%" = fmt_pct(f$prop),
        "% acumulada" = fmt_pct(f$acum),
        check.names = FALSE
      )
      tabela_html(tabela, classes = c("", "num", "num", "num"))
    })

    output$estatisticas <- shiny::renderUI({
      f <- freq()
      observadas <- f$categoria[f$n > 0]
      q <- quartis()
      tabela <- data.frame(
        "Estatística" = c(
          "Tamanho da amostra (n)", "Moda", "Mediana", "1º quartil (Q1)", "3º quartil (Q3)",
          "Categorias mínima e máxima", "Média dos códigos (com cautela)"
        ),
        "Valor" = c(
          fmt_num(sum(f$n), 0), moda_tabela(f), mediana(), q[1], q[2],
          paste(observadas[1], "a", observadas[length(observadas)]),
          fmt_num(mean(as.integer(dados())), 2)
        ),
        "Como ler" = c(
          "Número de observações",
          "A categoria mais frequente",
          "Pelo menos metade está nesta categoria ou abaixo, e pelo menos metade nesta ou acima",
          "Pelo menos 25% das observações estão até esta categoria",
          "Pelo menos 75% das observações estão até esta categoria",
          "Categorias extremas observadas na amostra",
          sprintf("Códigos de 1 a %d; supõe distâncias iguais entre categorias", nrow(f))
        ),
        check.names = FALSE
      )
      tabela_html(tabela, classes = c("", "num", "leitura"))
    })

    output$grafico <- shiny::renderPlot({
      f <- freq()
      pal <- paleta_modo(modo())
      f$categoria <- factor(f$categoria, levels = f$categoria)
      f$e_mediana <- as.character(f$categoria) == mediana()
      f$rotulo <- ifelse(f$n == max(f$n), paste0(fmt_pct(f$prop), "\nmoda"), fmt_pct(f$prop))

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
          title = "Frequência por categoria, na ordem natural",
          subtitle = exemplo()$rotulo,
          x = NULL, y = "%",
          caption = paste(
            "Dados simulados | Em destaque: categoria mediana",
            if (isTRUE(input$acumulada)) "| Linha vermelha: % acumulada" else ""
          )
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
    })
  })
}
