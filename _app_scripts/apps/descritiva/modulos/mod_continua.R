# =============================================================================
# Script:  mod_continua.R (módulo: variável quantitativa contínua)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-18
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/apps/descritiva/app.R)
# Saída:
#   - funções mod_continua_ui() e mod_continua_server()
# =============================================================================

texto_continua <- function() {
  shiny::tagList(
    shiny::p(
      "Uma variável contínua vem de uma medição e pode assumir qualquer valor num intervalo: 168,3 cm,",
      "168,31 cm, e assim por diante, até onde o instrumento permitir. Valem as mesmas estatísticas da",
      "discreta. A diferença está na forma de olhar a distribuição: como quase nenhum valor se repete,",
      "os dados são agrupados em classes (histograma e tabela de frequências por classes)."
    ),
    shiny::p(
      "O coeficiente de assimetria resume a forma. Perto de 0, a distribuição é aproximadamente simétrica",
      "e média e mediana ficam próximas. Positivo, há uma cauda longa à direita (comum em renda e tempos de",
      "espera) e a média fica acima da mediana."
    ),
    shiny::p(
      shiny::strong("Experimente:"),
      "compare altura e renda. Na altura, média e mediana quase coincidem; na renda, a média fica bem acima,",
      "puxada pelos salários altos. Depois mude o número de classes e veja a forma do histograma mudar,",
      "sem que nenhum dado tenha mudado."
    )
  )
}

cuidados_continua <- list(
  "Moda" = paste(
    "quase nenhum valor contínuo se repete, então a moda dos valores brutos não informa nada.",
    "Use a classe modal, que depende da escolha das classes."
  ),
  "Número de classes" = "muda a cara do histograma. Poucas classes escondem detalhes; muitas mostram ruído.",
  "Distribuições assimétricas" = "afastam média e mediana. Para descrever o valor típico de renda, a mediana costuma ser melhor.",
  "Coeficiente de variação" = "só faz sentido quando o zero da escala significa ausência (altura, renda, tempo). Para temperatura em °C, não use.",
  "Agrupar em faixas" = "transforma a variável em ordinal: a leitura fica mais fácil, mas o detalhe dentro de cada faixa se perde."
)

# Limites das classes com números "redondos" (o número final é aproximado)
quebras_classes <- function(x, classes) {
  pretty(range(x), n = classes)
}

# Frequência por classe [a; b), com a última classe fechada à direita
tabela_classes <- function(x, quebras, digitos) {
  n <- as.vector(table(cut(x, quebras, right = FALSE, include.lowest = TRUE)))
  inicio <- utils::head(quebras, -1)
  fim <- utils::tail(quebras, -1)
  fecha <- ifelse(seq_along(n) == length(n), "]", ")")
  data.frame(
    classe = sprintf("[%s; %s%s", fmt_num(inicio, digitos), fmt_num(fim, digitos), fecha),
    inicio = inicio, fim = fim, n = n,
    prop = n / length(x), acum = cumsum(n) / length(x)
  )
}

mod_continua_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::selectInput(ns("exemplo"), "Variável", choices = opcoes_exemplo(exemplos_continua)),
      shiny::sliderInput(ns("n"), "Tamanho da amostra", min = 20, max = 5000, value = 300, step = 10, sep = "."),
      shiny::sliderInput(ns("classes"), "Número aproximado de classes", min = 3, max = 60, value = 15, step = 1),
      shiny::checkboxInput(ns("boxplot"), "Mostrar boxplot", value = TRUE),
      shiny::checkboxInput(ns("extremo"), "Incluir um valor extremo", value = FALSE),
      shiny::uiOutput(ns("extremo_valor")),
      botao_sortear(ns("sortear"))
    ),
    cabecalho_tipo("continua"),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(title = "Média", value = shiny::textOutput(ns("media")), shiny::textOutput(ns("media_desc")), theme = "primary"),
      bslib::value_box(title = "Mediana", value = shiny::textOutput(ns("mediana")), shiny::textOutput(ns("mediana_desc")), theme = "primary"),
      bslib::value_box(title = "Desvio padrão", value = shiny::textOutput(ns("dp")), shiny::textOutput(ns("dp_desc")), theme = "info"),
      bslib::value_box(title = "Assimetria", value = shiny::textOutput(ns("assimetria")), shiny::textOutput(ns("assimetria_desc")), theme = "info")
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico"), height = "440px"))),
      bslib::card(
        bslib::card_header("Tabela de frequências por classes"),
        max_height = "520px",
        bslib::card_body(shiny::uiOutput(ns("tabela")))
      )
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(bslib::card_header("Estatísticas aplicáveis"), bslib::card_body(shiny::uiOutput(ns("estatisticas")))),
      cartao_cuidados("Cuidados", cuidados_continua)
    ),
    bslib::card(bslib::card_header("Entenda"), bslib::card_body(texto_continua()))
  )
}

mod_continua_server <- function(id, modo = shiny::reactive("light")) {
  shiny::moduleServer(id, function(input, output, session) {

    exemplo <- shiny::reactive(exemplos_continua[[input$exemplo]])

    output$extremo_valor <- shiny::renderUI({
      shiny::req(input$extremo)
      shiny::numericInput(session$ns("valor_extremo"), "Valor extremo", value = exemplo()$extremo, min = 0)
    })

    base <- shiny::reactive({
      input$sortear
      exemplo()$gerar(input$n)
    })

    incluir <- shiny::reactive(isTRUE(input$extremo) && !is.null(input$valor_extremo) && !is.na(input$valor_extremo))
    dados <- shiny::reactive(if (incluir()) c(base(), input$valor_extremo) else base())

    resumo <- shiny::reactive(resumo_quantitativo(dados()))
    resumo_sem <- shiny::reactive(resumo_quantitativo(base()))
    un <- shiny::reactive(exemplo()$unidade)
    dig <- shiny::reactive(exemplo()$digitos)

    classes <- shiny::reactive({
      x <- dados()
      tabela_classes(x, quebras_classes(x, input$classes), dig())
    })

    output$media <- shiny::renderText(fmt_valor(resumo()$media, un(), dig()))
    output$media_desc <- shiny::renderText(descricao_sem_extremo(resumo_sem()$media, un(), dig(), incluir()))
    output$mediana <- shiny::renderText(fmt_valor(resumo()$mediana, un(), dig()))
    output$mediana_desc <- shiny::renderText(descricao_sem_extremo(resumo_sem()$mediana, un(), dig(), incluir()))
    output$dp <- shiny::renderText(fmt_valor(resumo()$dp, un(), dig()))
    output$dp_desc <- shiny::renderText(descricao_sem_extremo(resumo_sem()$dp, un(), dig(), incluir()))
    output$assimetria <- shiny::renderText(fmt_num(resumo()$assimetria, 2))
    output$assimetria_desc <- shiny::renderText(leitura_assimetria(resumo()$assimetria))

    output$tabela <- shiny::renderUI({
      cl <- classes()
      tabela <- data.frame(
        "Classe" = cl$classe, "n" = fmt_num(cl$n, 0), "%" = fmt_pct(cl$prop), "% acumulada" = fmt_pct(cl$acum),
        check.names = FALSE
      )
      shiny::tagList(
        shiny::p(class = "small text-muted", sprintf("Unidade: %s. Cada classe inclui o limite inferior e exclui o superior.", un())),
        tabela_html(tabela, classes = c("", "num", "num", "num"))
      )
    })

    output$estatisticas <- shiny::renderUI({
      cl <- classes()
      r <- resumo()
      extras <- data.frame(
        "Estatística" = c("Classe modal", "Coeficiente de assimetria"),
        "Valor" = c(paste(cl$classe[cl$n == max(cl$n)], collapse = " e "), fmt_num(r$assimetria, 2)),
        "Como ler" = c(
          "Classe com mais observações; muda com o número de classes",
          paste("Distribuição", leitura_assimetria(r$assimetria))
        ),
        check.names = FALSE
      )
      tabela_estatisticas_quant(r, un(), dig(), extras) |>
        tabela_html(classes = c("", "num", "leitura"))
    })

    output$grafico <- shiny::renderPlot({
      pal <- paleta_modo(modo())
      r <- resumo()
      cl <- classes()
      topo <- max(cl$n)
      rotulos <- scales::label_number(big.mark = ".", decimal.mark = ",")

      grafico <- ggplot2::ggplot() +
        ggplot2::geom_rect(
          data = cl,
          ggplot2::aes(xmin = inicio, xmax = fim, ymin = 0, ymax = n),
          fill = cores_app$destaque, colour = pal$fundo, alpha = 0.85
        ) +
        ggplot2::geom_vline(xintercept = r$media, colour = cores_app$referencia, linetype = "dashed", linewidth = 1) +
        ggplot2::geom_vline(xintercept = r$mediana, colour = pal$linha, linewidth = 1) +
        ggplot2::annotate("label", x = r$media, y = topo * 1.06, label = "média", colour = cores_app$referencia, fill = pal$fundo, size = 4) +
        ggplot2::annotate("label", x = r$mediana, y = topo * 1.16, label = "mediana", colour = pal$linha, fill = pal$fundo, size = 4)

      if (isTRUE(input$boxplot)) {
        grafico <- grafico +
          ggplot2::geom_boxplot(
            data = data.frame(x = dados()),
            ggplot2::aes(x = x, y = -topo * 0.12),
            orientation = "y", width = topo * 0.12,
            fill = pal$neutro, colour = pal$texto, outlier.colour = cores_app$referencia
          )
      }

      grafico +
        ggplot2::scale_x_continuous(labels = rotulos) +
        ggplot2::scale_y_continuous(breaks = function(l) scales::breaks_pretty()(c(0, l[2]))) +
        ggplot2::labs(
          title = "Histograma: a distribuição agrupada em classes",
          subtitle = exemplo()$rotulo,
          x = un(), y = "Frequência (n)",
          caption = sprintf(
            "Dados simulados, n = %s | Tracejada: média | Contínua: mediana%s",
            fmt_num(r$n, 0), if (isTRUE(input$boxplot)) " | Abaixo: boxplot" else ""
          )
        ) +
        tema_grafico(modo())
    })
  })
}
