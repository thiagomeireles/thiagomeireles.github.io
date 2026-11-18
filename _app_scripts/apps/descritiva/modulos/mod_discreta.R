# =============================================================================
# Script:  mod_discreta.R (módulo: variável quantitativa discreta)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-18
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/apps/descritiva/app.R)
# Saída:
#   - funções mod_discreta_ui() e mod_discreta_server()
# =============================================================================

texto_discreta <- function() {
  shiny::tagList(
    shiny::p(
      "Uma variável discreta vem de uma contagem: 0, 1, 2 filhos, nunca 1,5. Como os valores são números de",
      "verdade, as contas fazem sentido e todas as estatísticas descritivas se aplicam: medidas de posição",
      "(média, mediana, moda, quartis) e de dispersão (amplitude, intervalo interquartil, variância, desvio padrão)."
    ),
    shiny::p(
      "A média pode dar um valor impossível, como 1,6 filho. Não é erro: a média é um resumo do conjunto,",
      "não uma observação. Ela diz que 100 famílias como essas têm, juntas, cerca de 160 filhos."
    ),
    shiny::p(
      shiny::strong("Experimente:"),
      "marque \"Incluir um valor extremo\". Uma única observação muda a média, o desvio padrão e a amplitude,",
      "enquanto a mediana, a moda e o intervalo interquartil quase não se mexem. Diminua o tamanho da amostra",
      "e veja o efeito ficar maior."
    )
  )
}

cuidados_discreta <- list(
  "Valores extremos" = "puxam a média e inflam o desvio padrão. Quando houver, informe também a mediana e o intervalo interquartil.",
  "Média com casas decimais" = "é normal numa contagem. Não arredonde para inteiro só porque os dados são inteiros.",
  "Histograma com classes largas" = "pode esconder que só existem valores inteiros. Com poucos valores distintos, use barras por valor.",
  "Muitos valores distintos" = "(como número de seguidores) fazem a discreta se comportar como contínua. Aí o histograma volta a ser útil."
)

# Tabela de estatísticas de uma quantitativa: estatística, valor e como ler
tabela_estatisticas_quant <- function(r, unidade, digitos, extras = NULL) {
  v <- \(x, quadrado = FALSE) fmt_valor(x, unidade, digitos, quadrado)
  linhas <- data.frame(
    "Estatística" = c(
      "Tamanho da amostra (n)", "Média", "Mediana", "Mínimo e máximo", "Amplitude",
      "1º e 3º quartis", "Intervalo interquartil", "Variância", "Desvio padrão", "Coeficiente de variação"
    ),
    "Valor" = c(
      fmt_num(r$n, 0), v(r$media), v(r$mediana), paste(v(r$minimo), "e", v(r$maximo)), v(r$amplitude),
      paste(v(r$q1), "e", v(r$q3)), v(r$iiq), v(r$variancia, quadrado = TRUE), v(r$dp), fmt_pct(r$cv)
    ),
    "Como ler" = c(
      "Número de observações",
      "Soma dos valores dividida por n; sensível a extremos",
      "Metade das observações está abaixo e metade acima",
      "Menor e maior valor observados",
      "Máximo menos mínimo; depende só dos dois extremos",
      "25% das observações estão abaixo de Q1; 75% abaixo de Q3",
      "Q3 − Q1: largura dos 50% centrais; resiste a extremos",
      "Média dos desvios ao quadrado, em unidade ao quadrado",
      "Distância típica até a média, na unidade original",
      "Desvio padrão como % da média; compara variabilidade entre variáveis"
    ),
    check.names = FALSE
  )
  rbind(linhas, extras)
}

# Com o extremo incluído, mostra a mesma estatística calculada sem ele
descricao_sem_extremo <- function(sem, unidade, digitos, incluir) {
  if (!incluir) return("")
  paste("Sem o valor extremo:", fmt_valor(sem, unidade, digitos))
}

mod_discreta_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::selectInput(ns("exemplo"), "Variável", choices = opcoes_exemplo(exemplos_discreta)),
      shiny::sliderInput(ns("n"), "Tamanho da amostra", min = 10, max = 2000, value = 100, step = 10, sep = "."),
      shiny::checkboxInput(ns("extremo"), "Incluir um valor extremo", value = FALSE),
      shiny::uiOutput(ns("extremo_valor")),
      botao_sortear(ns("sortear"))
    ),
    cabecalho_tipo("discreta"),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(title = "Média", value = shiny::textOutput(ns("media")), shiny::textOutput(ns("media_desc")), theme = "primary"),
      bslib::value_box(title = "Mediana", value = shiny::textOutput(ns("mediana")), shiny::textOutput(ns("mediana_desc")), theme = "primary"),
      bslib::value_box(title = "Moda", value = shiny::textOutput(ns("moda")), "Valor mais frequente", theme = "info"),
      bslib::value_box(title = "Desvio padrão", value = shiny::textOutput(ns("dp")), shiny::textOutput(ns("dp_desc")), theme = "info")
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico"), height = "420px"))),
      bslib::card(
        bslib::card_header("Tabela de frequências"),
        max_height = "500px",
        bslib::card_body(shiny::uiOutput(ns("tabela")))
      )
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(bslib::card_header("Estatísticas aplicáveis"), bslib::card_body(shiny::uiOutput(ns("estatisticas")))),
      cartao_cuidados("Cuidados", cuidados_discreta)
    ),
    bslib::card(bslib::card_header("Entenda"), bslib::card_body(texto_discreta()))
  )
}

mod_discreta_server <- function(id, modo = shiny::reactive("light")) {
  shiny::moduleServer(id, function(input, output, session) {

    exemplo <- shiny::reactive(exemplos_discreta[[input$exemplo]])

    output$extremo_valor <- shiny::renderUI({
      shiny::req(input$extremo)
      shiny::numericInput(session$ns("valor_extremo"), "Valor extremo", value = exemplo()$extremo, min = 0, step = 1)
    })

    base <- shiny::reactive({
      input$sortear
      exemplo()$gerar(input$n)
    })

    incluir <- shiny::reactive(isTRUE(input$extremo) && !is.null(input$valor_extremo) && !is.na(input$valor_extremo))

    dados <- shiny::reactive({
      if (incluir()) c(base(), round(input$valor_extremo)) else base()
    })

    resumo <- shiny::reactive(resumo_quantitativo(dados()))
    resumo_sem <- shiny::reactive(resumo_quantitativo(base()))
    un <- shiny::reactive(exemplo()$unidade)

    output$media <- shiny::renderText(fmt_valor(resumo()$media, un(), 2))
    output$media_desc <- shiny::renderText(descricao_sem_extremo(resumo_sem()$media, un(), 2, incluir()))
    output$mediana <- shiny::renderText(fmt_valor(resumo()$mediana, un(), if (resumo()$mediana %% 1 == 0) 0 else 1))
    output$mediana_desc <- shiny::renderText(descricao_sem_extremo(resumo_sem()$mediana, un(), 1, incluir()))
    output$moda <- shiny::renderText(paste(fmt_num(moda_valores(dados()), 0), collapse = " e "))
    output$dp <- shiny::renderText(fmt_valor(resumo()$dp, un(), 2))
    output$dp_desc <- shiny::renderText(descricao_sem_extremo(resumo_sem()$dp, un(), 2, incluir()))

    output$tabela <- shiny::renderUI({
      f <- tabela_frequencias(factor(dados(), levels = sort(unique(dados()))))
      tabela <- data.frame(
        "Valor" = f$categoria, "n" = fmt_num(f$n, 0), "%" = fmt_pct(f$prop), "% acumulada" = fmt_pct(f$acum),
        check.names = FALSE
      )
      tabela_html(tabela, classes = c("num", "num", "num", "num"))
    })

    output$estatisticas <- shiny::renderUI({
      extras <- data.frame(
        "Estatística" = "Moda",
        "Valor" = paste(fmt_num(moda_valores(dados()), 0), collapse = " e "),
        "Como ler" = "Valor mais frequente",
        check.names = FALSE
      )
      tabela_estatisticas_quant(resumo(), un(), 2, extras) |>
        tabela_html(classes = c("", "num", "leitura"))
    })

    output$grafico <- shiny::renderPlot({
      pal <- paleta_modo(modo())
      r <- resumo()
      contagem <- table(dados())
      f <- data.frame(valor = as.numeric(names(contagem)), n = as.vector(contagem))
      topo <- max(f$n)

      ggplot2::ggplot(f, ggplot2::aes(x = valor, y = n)) +
        ggplot2::geom_col(width = 0.6, fill = cores_app$destaque, alpha = 0.85) +
        ggplot2::geom_vline(xintercept = r$media, colour = cores_app$referencia, linetype = "dashed", linewidth = 1) +
        ggplot2::geom_vline(xintercept = r$mediana, colour = pal$linha, linewidth = 1) +
        ggplot2::annotate("label", x = r$media, y = topo * 1.08, label = "média", colour = cores_app$referencia, fill = pal$fundo, size = 4) +
        ggplot2::annotate("label", x = r$mediana, y = topo * 1.18, label = "mediana", colour = pal$linha, fill = pal$fundo, size = 4) +
        ggplot2::scale_x_continuous(breaks = function(l) unique(round(scales::breaks_pretty()(l)))) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
        ggplot2::labs(
          title = "Uma barra para cada valor possível",
          subtitle = exemplo()$rotulo,
          x = exemplo()$unidade, y = "Frequência (n)",
          caption = sprintf("Dados simulados, n = %s | Tracejada: média | Contínua: mediana", fmt_num(r$n, 0))
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
    })
  })
}
