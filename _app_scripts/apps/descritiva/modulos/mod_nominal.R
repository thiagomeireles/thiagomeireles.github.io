# =============================================================================
# Script:  mod_nominal.R (módulo: variável categórica nominal)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-18
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/apps/descritiva/app.R)
# Saída:
#   - funções mod_nominal_ui() e mod_nominal_server()
# =============================================================================

texto_nominal <- function() {
  shiny::tagList(
    shiny::p(
      "Numa variável nominal, cada observação cai numa categoria, e as categorias não têm ordem:",
      "Sudeste não é \"maior\" que Norte. Por isso o resumo se limita a contar: quantas observações há",
      "em cada categoria (frequência absoluta), que fração do total isso representa (frequência relativa)",
      "e qual categoria aparece mais (moda)."
    ),
    shiny::p(
      "A razão de variação (1 menos a proporção da moda) faz o papel de medida de dispersão. Vale 0 quando",
      "todas as observações estão na mesma categoria e se aproxima de 1 quando elas se espalham por muitas categorias."
    ),
    shiny::p(
      shiny::strong("Experimente:"),
      "clique em \"Trocar os códigos\" várias vezes. A moda e as frequências não mudam, mas a \"média dos códigos\"",
      "muda a cada clique. Ela depende de uma escolha arbitrária, e não da variável."
    )
  )
}

cuidados_nominal <- list(
  "Média e desvio padrão" = paste(
    "exigem números com distância entre si. Códigos como 1 = Norte, 2 = Nordeste são só rótulos:",
    "trocar a codificação muda a \"média\", como mostra o botão ao lado."
  ),
  "Mediana e quartis" = "exigem ordem. Não existe uma região \"do meio\".",
  "Mínimo, máximo e amplitude" = "também exigem ordem. Não há categoria menor nem maior.",
  "Ordem das barras" = "é livre. Ordenar pela frequência costuma facilitar a leitura."
)

mod_nominal_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::selectInput(ns("exemplo"), "Variável", choices = opcoes_exemplo(exemplos_nominal)),
      shiny::sliderInput(ns("n"), "Tamanho da amostra", min = 20, max = 2000, value = 200, step = 10, sep = "."),
      shiny::radioButtons(
        ns("ordem"), "Ordem das barras",
        choices = c("Da mais à menos frequente" = "freq", "Ordem da lista" = "lista")
      ),
      shiny::radioButtons(ns("escala"), "Mostrar", choices = c("Porcentagem" = "pct", "Contagem" = "n"), inline = TRUE),
      botao_sortear(ns("sortear")),
      shiny::tags$hr(),
      shiny::strong("Teste da codificação"),
      shiny::helpText("Cada categoria recebe um código numérico, como num banco de dados."),
      shiny::uiOutput(ns("codigos")),
      shiny::actionButton(ns("recodificar"), "Trocar os códigos", icon = shiny::icon("shuffle"))
    ),
    cabecalho_tipo("nominal"),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(title = "Moda", value = shiny::textOutput(ns("moda")), shiny::textOutput(ns("moda_desc")), theme = "primary"),
      bslib::value_box(title = "Razão de variação", value = shiny::textOutput(ns("rv")), "1 − proporção da moda", theme = "info"),
      bslib::value_box(
        title = "Média dos códigos (não use!)", value = shiny::textOutput(ns("media_codigos")),
        "Muda quando os códigos mudam", theme = "danger"
      )
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico"), height = "400px"))),
      bslib::card(bslib::card_header("Tabela de frequências"), bslib::card_body(shiny::uiOutput(ns("tabela"))))
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(bslib::card_header("Estatísticas aplicáveis"), bslib::card_body(shiny::uiOutput(ns("estatisticas")))),
      cartao_cuidados("O que não usar, e por quê", cuidados_nominal)
    ),
    bslib::card(bslib::card_header("Entenda"), bslib::card_body(texto_nominal()))
  )
}

mod_nominal_server <- function(id, modo = shiny::reactive("light")) {
  shiny::moduleServer(id, function(input, output, session) {

    exemplo <- shiny::reactive(exemplos_nominal[[input$exemplo]])

    dados <- shiny::reactive({
      input$sortear
      gerar_categorica(exemplo()$categorias, exemplo()$prob, input$n)
    })

    freq <- shiny::reactive(tabela_frequencias(dados()))

    # Começa com 1, 2, 3... na ordem da lista; cada clique sorteia outra codificação
    codigos <- shiny::reactive({
      k <- length(exemplo()$categorias)
      if (input$recodificar == 0) seq_len(k) else sample(k)
    })

    output$codigos <- shiny::renderUI({
      shiny::p(
        class = "small",
        paste(exemplo()$categorias, "=", codigos(), collapse = " | ")
      )
    })

    output$moda <- shiny::renderText(moda_tabela(freq()))
    output$moda_desc <- shiny::renderText(sprintf("%s das observações", fmt_pct(max(freq()$prop))))
    output$rv <- shiny::renderText(fmt_num(1 - max(freq()$prop), 3))
    output$media_codigos <- shiny::renderText(fmt_num(mean(codigos()[as.integer(dados())]), 2))

    output$tabela <- shiny::renderUI({
      f <- freq()
      tabela <- data.frame(
        "Categoria" = c(f$categoria, "Total"),
        "Frequência (n)" = c(fmt_num(f$n, 0), fmt_num(sum(f$n), 0)),
        "Frequência relativa" = c(fmt_pct(f$prop), "100,0%"),
        check.names = FALSE
      )
      tabela_html(tabela, classes = c("", "num", "num"))
    })

    output$estatisticas <- shiny::renderUI({
      f <- freq()
      tabela <- data.frame(
        "Estatística" = c("Tamanho da amostra (n)", "Moda", "Proporção da moda", "Categorias observadas", "Razão de variação"),
        "Valor" = c(
          fmt_num(sum(f$n), 0), moda_tabela(f), fmt_pct(max(f$prop)),
          sprintf("%d de %d", sum(f$n > 0), nrow(f)), fmt_num(1 - max(f$prop), 3)
        ),
        "Como ler" = c(
          "Número de observações",
          "A categoria mais frequente",
          "Fração do total que está na moda",
          "Categorias com pelo menos uma observação",
          "Perto de 0: concentrada numa categoria; perto de 1: espalhada"
        ),
        check.names = FALSE
      )
      tabela_html(tabela, classes = c("", "num", "leitura"))
    })

    output$grafico <- shiny::renderPlot({
      f <- freq()
      pal <- paleta_modo(modo())
      em_pct <- identical(input$escala, "pct")

      niveis <- if (identical(input$ordem, "freq")) f$categoria[order(f$n)] else rev(f$categoria)
      f$categoria <- factor(f$categoria, levels = niveis)
      f$e_moda <- f$n == max(f$n)
      f$valor <- if (em_pct) 100 * f$prop else f$n
      f$rotulo <- if (em_pct) fmt_pct(f$prop) else fmt_num(f$n, 0)

      ggplot2::ggplot(f, ggplot2::aes(x = valor, y = categoria, fill = e_moda)) +
        ggplot2::geom_col(width = 0.7) +
        ggplot2::geom_text(ggplot2::aes(label = rotulo), hjust = -0.15, size = 4.5, colour = pal$texto) +
        ggplot2::scale_fill_manual(values = c(`TRUE` = cores_app$destaque, `FALSE` = pal$neutro)) +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.18))) +
        ggplot2::labs(
          title = "Frequência de cada categoria",
          subtitle = exemplo()$rotulo,
          x = if (em_pct) "Frequência relativa (%)" else "Frequência (n)", y = NULL,
          caption = sprintf("Dados simulados (%s), n = %s | Em destaque: moda", exemplo()$nota, fmt_num(input$n, 0))
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
    })
  })
}
