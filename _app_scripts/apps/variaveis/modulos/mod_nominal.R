# =============================================================================
# Script:  mod_nominal.R (módulo: variável categórica nominal)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-18
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/apps/variaveis/app.R)
# Saída:
#   - funções mod_nominal_ui() e mod_nominal_server()
# =============================================================================

texto_nominal <- function() {
  tr(
    shiny::tagList(
      shiny::p(
        "Numa variável nominal, cada observação cai numa categoria, e as categorias não têm ordem:",
        "Sudeste não é \"maior\" que Norte. Por isso o resumo se limita a contar: quantas observações há",
        "em cada categoria (frequência absoluta), que fração do total isso representa (frequência relativa)",
        "e qual categoria aparece mais (moda)."
      ),
      shiny::p(
        "Para a dispersão, o app usa a razão de variação, proposta por Freeman (1965): 1 menos a proporção da moda.",
        "Ela vale 0 quando todas as observações estão na mesma categoria e cresce à medida que elas se espalham",
        "por igual, até 1 − 1/k para k categorias (0,80 com cinco). Morettin e Bussab e Agresti e Finlay não",
        "a apresentam; nos dois livros, a nominal é descrita pela tabela de frequências e pela moda."
      ),
      shiny::p(
        shiny::strong("Experimente:"),
        "clique em \"Trocar os códigos\" várias vezes. A moda e as frequências não mudam, mas a \"média dos códigos\"",
        "muda a cada clique, porque depende da codificação, que é arbitrária, e não da variável."
      )
    ),
    shiny::tagList(
      shiny::p(
        "In a nominal variable, each observation falls into a category, and the categories have no order:",
        "Southeast is not \"greater\" than North. So the summary is limited to counting: how many observations",
        "fall in each category (absolute frequency), what share of the total that is (relative frequency)",
        "and which category appears most often (mode)."
      ),
      shiny::p(
        "For spread, the app uses the variation ratio proposed by Freeman (1965): 1 minus the share in the modal",
        "category. It is 0 when all observations are in the same category and grows as they spread evenly across",
        "categories, up to 1 − 1/k for k categories (0.80 with five). Agresti and Finlay and Morettin and Bussab do",
        "not cover it; both books describe nominal data through the frequency table and the mode."
      ),
      shiny::p(
        shiny::strong("Try it:"),
        "click \"Shuffle the codes\" a few times. The mode and the frequencies do not change, but the \"mean of",
        "the codes\" changes with every click, because it depends on the coding, which is arbitrary, not on the variable."
      )
    )
  )
}

cuidados_nominal <- function() {
  tr(
    c(
      "Média e desvio padrão" = paste(
        "exigem números com distância entre si. Códigos como 1 = Norte, 2 = Nordeste são só rótulos:",
        "trocar a codificação muda a \"média\", como mostra o botão ao lado."
      ),
      "Mediana e quartis" = "exigem ordem. Não existe uma categoria \"do meio\".",
      "Mínimo, máximo e amplitude" = "também exigem ordem. Não há categoria menor nem maior.",
      "Ordem das barras" = "é livre. Ordenar pela frequência costuma facilitar a leitura."
    ),
    c(
      "Mean and standard deviation" = paste(
        "require numbers with distances between them. Codes like 1 = North, 2 = Northeast are just labels:",
        "changing the coding changes the \"mean\", as the button in the sidebar shows."
      ),
      "Median and quartiles" = "require order. There is no \"middle\" category.",
      "Minimum, maximum and range" = "also require order. No category is smaller or larger.",
      "Bar order" = "is free. Sorting by frequency usually makes the chart easier to read."
    )
  )
}

mod_nominal_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      seletor_exemplo(ns("exemplo"), exemplos_nominal),
      slider_amostra(ns("n"), min = 20, max = 2000, value = 200),
      shiny::radioButtons(
        ns("ordem"), tr("Ordem das barras", "Bar order"),
        choices = stats::setNames(c("freq", "lista"), tr(c("Da mais à menos frequente", "Ordem da lista"), c("Most to least frequent", "List order")))
      ),
      shiny::radioButtons(
        ns("escala"), tr("Mostrar", "Show"),
        choices = stats::setNames(c("pct", "n"), tr(c("Porcentagem", "Contagem"), c("Percentage", "Count"))),
        inline = TRUE
      ),
      botao_sortear(ns("sortear")),
      shiny::tags$hr(),
      shiny::strong(tr("Teste da codificação", "Coding test")),
      shiny::helpText(tr(
        "Cada categoria recebe um código numérico, como num banco de dados.",
        "Each category gets a numeric code, as in a database."
      )),
      shiny::uiOutput(ns("codigos")),
      shiny::actionButton(ns("recodificar"), tr("Trocar os códigos", "Shuffle the codes"), icon = shiny::icon("shuffle"))
    ),
    cabecalho_tipo("nominal"),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(title = tr("Moda", "Mode"), value = shiny::textOutput(ns("moda")), shiny::textOutput(ns("moda_desc")), theme = "primary"),
      bslib::value_box(
        title = tr("Razão de variação", "Variation ratio"), value = shiny::textOutput(ns("rv")),
        tr("1 − proporção da moda; máximo 1 − 1/k", "1 − share in the modal category; maximum 1 − 1/k"), theme = "info"
      ),
      bslib::value_box(
        title = tr("Média dos códigos (não use!)", "Mean of the codes (don't use it!)"), value = shiny::textOutput(ns("media_codigos")),
        tr("Muda quando os códigos mudam", "Changes when the codes change"), theme = "danger"
      )
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico"), height = "400px"))),
      bslib::card(bslib::card_header(tr("Tabela de frequências", "Frequency table")), bslib::card_body(shiny::uiOutput(ns("tabela"))))
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(bslib::card_header(tr("Estatísticas aplicáveis", "Applicable statistics")), bslib::card_body(shiny::uiOutput(ns("estatisticas")))),
      cartao_cuidados(tr("O que não usar, e por quê", "What not to use, and why"), cuidados_nominal())
    ),
    bslib::card(bslib::card_header(rotulo_entenda()), bslib::card_body(texto_nominal()))
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
      shiny::p(class = "small", paste(exemplo()$categorias, "=", codigos(), collapse = " | "))
    })

    output$moda <- shiny::renderText(moda_tabela(freq()))
    output$moda_desc <- shiny::renderText(sprintf(tr("%s das observações", "%s of the observations"), fmt_pct(max(freq()$prop))))
    output$rv <- shiny::renderText(fmt_num(1 - max(freq()$prop), 3))
    output$media_codigos <- shiny::renderText(fmt_num(mean(codigos()[as.integer(dados())]), 2))

    output$tabela <- shiny::renderUI({
      f <- freq()
      tabela <- data.frame(
        c(f$categoria, "Total"),
        c(fmt_num(f$n, 0), fmt_num(sum(f$n), 0)),
        c(fmt_pct(f$prop), fmt_pct(1))
      )
      names(tabela) <- tr(c("Categoria", "Frequência (n)", "Frequência relativa"), c("Category", "Frequency (n)", "Relative frequency"))
      tabela_html(tabela, classes = c("", "num", "num"))
    })

    output$estatisticas <- shiny::renderUI({
      f <- freq()
      tabela <- data.frame(
        tr(
          c("Tamanho da amostra (n)", "Moda", "Proporção da moda", "Categorias observadas", "Razão de variação"),
          c("Sample size (n)", "Mode", "Share in the modal category", "Observed categories", "Variation ratio")
        ),
        c(
          fmt_num(sum(f$n), 0), moda_tabela(f), fmt_pct(max(f$prop)),
          sprintf(tr("%d de %d", "%d of %d"), sum(f$n > 0), nrow(f)), fmt_num(1 - max(f$prop), 3)
        ),
        tr(
          c(
            "Número de observações",
            "A categoria mais frequente",
            "Fração do total que está na moda",
            "Categorias com pelo menos uma observação",
            sprintf("0: tudo numa categoria; quanto maior, mais espalhada (máximo 1 − 1/k = %s)", fmt_num(1 - 1 / nrow(f), 2))
          ),
          c(
            "Number of observations",
            "The most frequent category",
            "Share of the total in the mode",
            "Categories with at least one observation",
            sprintf("0: all in one category; higher means more spread out (maximum 1 − 1/k = %s)", fmt_num(1 - 1 / nrow(f), 2))
          )
        )
      )
      names(tabela) <- nomes_estatisticas()
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
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.18)), labels = rotulos_eixo()) +
        ggplot2::labs(
          title = tr("Frequência de cada categoria", "Frequency of each category"),
          subtitle = exemplo()$rotulo,
          x = if (em_pct) tr("Frequência relativa (%)", "Relative frequency (%)") else tr("Frequência (n)", "Frequency (n)"),
          y = NULL,
          caption = sprintf(
            tr("Dados simulados (%s), n = %s | Em destaque: moda", "Simulated data (%s), n = %s | Highlighted: mode"),
            exemplo()$nota, fmt_num(input$n, 0)
          )
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
    })
  })
}
