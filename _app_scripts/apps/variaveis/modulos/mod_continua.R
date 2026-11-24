# =============================================================================
# Script:  mod_continua.R (módulo: variável quantitativa contínua)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-23
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/apps/variaveis/app.R)
# Saída:
#   - funções mod_continua_ui() e mod_continua_server()
# =============================================================================

texto_continua <- function() {
  tr(
    shiny::tagList(
      shiny::p(
        "Uma variável contínua vem de uma medição e pode assumir qualquer valor num intervalo: 168,3 cm,",
        "168,31 cm, e assim por diante, até onde o instrumento permitir. Valem as mesmas estatísticas da",
        "discreta. A diferença está na forma de olhar a distribuição: como quase nenhum valor se repete,",
        "os dados são agrupados em classes (histograma e tabela de frequências por classes)."
      ),
      shiny::p(
        "O coeficiente de assimetria resume a forma. Pela regra prática usada aqui, entre −0,5 e 0,5 a",
        "distribuição é tratada como aproximadamente simétrica, e média e mediana costumam ficar próximas. Acima",
        "de 0,5, há uma cauda longa à direita (comum em renda e tempos de espera), e a média costuma ficar acima",
        "da mediana, puxada pela cauda. No boxplot, essa assimetria costuma aparecer nas distâncias: a mediana",
        "fica mais longe do terceiro quartil do que do primeiro, e o bigode da direita é mais longo, muitas vezes",
        "com pontos exteriores desse lado. Morettin e Bussab avaliam a simetria justamente comparando essas distâncias."
      ),
      shiny::p(
        "Em dados com forma aproximada de sino, como a altura, cerca de 68% das observações ficam a até 1 desvio",
        "padrão da média, cerca de 95% a até 2 e quase todas a até 3 (a regra empírica). Em dados muito",
        "assimétricos ou com poucos valores distintos, essas porcentagens falham: na renda simulada, perto de 89%",
        "das observações ficam a até 1 desvio padrão. O que vale para qualquer conjunto de dados é bem mais fraco",
        "(desigualdade de Chebyshev, ver Casella e Berger, Statistical Inference, seção 3.6): pelo menos 75% das",
        "observações ficam a até 2 desvios padrão da média."
      ),
      shiny::p(
        shiny::strong("Experimente:"),
        "compare altura e renda. Na altura, média e mediana quase coincidem; na renda, a média fica bem acima,",
        "puxada por poucas rendas altas. Depois mude o número de classes e veja a forma do histograma mudar,",
        "sem que nenhum dado tenha mudado."
      )
    ),
    shiny::tagList(
      shiny::p(
        "A continuous variable comes from measuring and can take any value within a range: 168.3 cm,",
        "168.31 cm, and so on, as far as the instrument allows. The same statistics as for discrete variables",
        "apply. The difference is how you look at the distribution: since almost no value repeats, the data",
        "are grouped into bins (histogram and frequency table by class)."
      ),
      shiny::p(
        "The skewness coefficient summarizes the shape. By the rule of thumb used here, between −0.5 and 0.5 the",
        "distribution is treated as roughly symmetric, and the mean and median are usually close. Above 0.5,",
        "there is a long right tail (common for income and waiting times), and the mean usually sits above the",
        "median, pulled toward the tail. In the box plot, this skew usually shows up in the distances: the median",
        "is farther from the third quartile than from the first, and the right whisker is longer, often with",
        "potential outliers on that side."
      ),
      shiny::p(
        "If the data are approximately bell-shaped, as height is, about 68% of the observations fall within 1",
        "standard deviation of the mean, about 95% within 2, and all or nearly all within 3 (the Empirical Rule).",
        "For highly skewed data, or data with few distinct values, these percentages fail: in the simulated income,",
        "close to 89% of the observations fall within 1 standard deviation. What holds for any data set is much",
        "weaker (Chebyshev's inequality; see Casella and Berger, Statistical Inference, section 3.6): at least 75%",
        "of the observations fall within 2 standard deviations of the mean."
      ),
      shiny::p(
        shiny::strong("Try it:"),
        "compare height and income. For height, the mean and median almost coincide; for income, the mean is",
        "well above, pulled up by a few high incomes. Then change the number of bins and watch the histogram's shape",
        "change without any data changing."
      )
    )
  )
}

cuidados_continua <- function() {
  tr(
    c(
      "Moda" = paste(
        "quase nenhum valor contínuo se repete, então a moda dos valores brutos não informa nada.",
        "Use a classe modal (ou o seu ponto médio, como em Morettin e Bussab), que depende da escolha das classes."
      ),
      "Número de classes" = "muda a forma do histograma. Poucas classes escondem detalhes; muitas mostram ruído.",
      "Distribuições assimétricas" = paste(
        "afastam média e mediana. Para descrever o valor típico de renda, a mediana costuma ser melhor,",
        "porque não é puxada pelas poucas rendas muito altas."
      ),
      "Coeficiente de variação" = paste(
        "só faz sentido quando o zero da escala significa ausência (altura, renda, tempo). Não use com",
        "temperatura em °C, cujo zero é uma convenção."
      ),
      "Agrupar em faixas" = "transforma a variável em ordinal: a leitura fica mais fácil, mas o detalhe dentro de cada faixa se perde."
    ),
    c(
      "Mode" = paste(
        "almost no continuous value repeats, so the mode of the raw values tells you nothing.",
        "Use the modal class (or its midpoint), which depends on how the bins are chosen."
      ),
      "Number of bins" = "changes how the histogram looks. Few bins hide detail; many bins show noise.",
      "Skewed distributions" = paste(
        "pull the mean and median apart. To describe typical income, the median is usually better,",
        "because it is not pulled up by the few very high incomes."
      ),
      "Coefficient of variation" = paste(
        "only makes sense when zero on the scale means absence (height, income, time). Do not use it for",
        "temperature in °C, whose zero is a convention."
      ),
      "Grouping into ranges" = "turns the variable into an ordinal one: easier to read, but the detail within each range is lost."
    )
  )
}

# Limites das classes com números "redondos" (o número final é aproximado)
quebras_classes <- function(x, classes) {
  pretty(range(x), n = classes)
}

# Frequência por classe [a; b), com a última classe fechada à direita.
# Em inglês o separador é vírgula: [a, b).
tabela_classes <- function(x, quebras, digitos) {
  n <- as.vector(table(cut(x, quebras, right = FALSE, include.lowest = TRUE)))
  inicio <- utils::head(quebras, -1)
  fim <- utils::tail(quebras, -1)
  fecha <- ifelse(seq_along(n) == length(n), "]", ")")
  data.frame(
    classe = sprintf("[%s%s%s%s", fmt_num(inicio, digitos), tr("; ", ", "), fmt_num(fim, digitos), fecha),
    inicio = inicio, fim = fim, n = n,
    prop = n / length(x), acum = cumsum(n) / length(x)
  )
}

mod_continua_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      seletor_exemplo(ns("exemplo"), exemplos_continua),
      slider_amostra(ns("n"), min = 20, max = 5000, value = 300),
      shiny::sliderInput(ns("classes"), tr("Número aproximado de classes", "Approximate number of bins"), min = 3, max = 60, value = 15, step = 1),
      shiny::checkboxInput(ns("boxplot"), tr("Mostrar boxplot", "Show box plot"), value = TRUE),
      caixa_extremo(ns("extremo")),
      shiny::uiOutput(ns("extremo_valor")),
      botao_sortear(ns("sortear"))
    ),
    cabecalho_tipo("continua"),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(title = tr("Média", "Mean"), value = shiny::textOutput(ns("media")), shiny::textOutput(ns("media_desc")), theme = "primary"),
      bslib::value_box(title = tr("Mediana", "Median"), value = shiny::textOutput(ns("mediana")), shiny::textOutput(ns("mediana_desc")), theme = "primary"),
      bslib::value_box(title = tr("Desvio padrão", "Standard deviation"), value = shiny::textOutput(ns("dp")), shiny::textOutput(ns("dp_desc")), theme = "info"),
      bslib::value_box(title = tr("Assimetria", "Skewness"), value = shiny::textOutput(ns("assimetria")), shiny::textOutput(ns("assimetria_desc")), theme = "info")
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico"), height = "440px"))),
      bslib::card(
        bslib::card_header(tr("Tabela de frequências por classes", "Grouped frequency table")),
        max_height = "520px",
        bslib::card_body(shiny::uiOutput(ns("tabela")))
      )
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(bslib::card_header(tr("Estatísticas aplicáveis", "Applicable statistics")), bslib::card_body(shiny::uiOutput(ns("estatisticas")))),
      cartao_cuidados(tr("Cuidados", "Watch out"), cuidados_continua())
    ),
    bslib::card(bslib::card_header(rotulo_entenda()), bslib::card_body(texto_continua()))
  )
}

mod_continua_server <- function(id, modo = shiny::reactive("light")) {
  shiny::moduleServer(id, function(input, output, session) {

    exemplo <- shiny::reactive(exemplos_continua[[input$exemplo]])

    output$extremo_valor <- shiny::renderUI({
      shiny::req(input$extremo)
      entrada_extremo(session, exemplo()$extremo)
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
      tabela <- data.frame(cl$classe, fmt_num(cl$n, 0), fmt_pct(cl$prop), fmt_pct(cl$acum))
      names(tabela) <- nomes_freq(tr("Classe", "Class"))
      shiny::tagList(
        shiny::p(class = "small text-muted", sprintf(
          tr(
            paste(
              "Unidade: %s. Cada classe inclui o limite inferior e exclui o superior, exceto a última, que inclui",
              "os dois. Em Morettin e Bussab, a altura de cada retângulo do histograma é a densidade de frequência",
              "(frequência relativa dividida pela amplitude da classe), e a área total vale 1. Como aqui as classes têm",
              "a mesma amplitude, usar a frequência não muda a forma; com amplitudes diferentes, a densidade é obrigatória."
            ),
            paste(
              "Unit: %s. Each class includes its lower limit and excludes the upper one, except the last, which",
              "includes both. Because all classes have the same width, the histogram bars can show frequencies without",
              "changing the shape; with unequal widths, they would have to show density (relative frequency divided by",
              "width), so that the area of each bar, not its height, reflects the share of observations."
            )
          ),
          un()
        )),
        tabela_html(tabela, classes = c("", "num", "num", "num"))
      )
    })

    output$estatisticas <- shiny::renderUI({
      cl <- classes()
      r <- resumo()
      extras <- linha_estatistica(
        tr(c("Classe modal", "Coeficiente de assimetria"), c("Modal class", "Skewness coefficient")),
        c(juntar(cl$classe[cl$n == max(cl$n)]), fmt_num(r$assimetria, 2)),
        c(
          tr(
            "Classe com mais observações; o ponto médio dela pode servir de moda. Muda com o número de classes",
            "Class with the most observations; its midpoint can serve as the mode. Changes with the number of bins"
          ),
          paste0(
            tr("Distribuição ", "Distribution is "), leitura_assimetria(r$assimetria),
            tr(
              ". Média dos desvios ao cubo dividida pelo cubo do desvio padrão, ambos com divisor n; Morettin e Bussab avaliam a forma pelos quartis e por gráficos",
              ". Mean of the cubed deviations divided by the cubed standard deviation, both with divisor n"
            )
          )
        )
      )
      tabela_estatisticas_quant(r, un(), dig(), extras) |>
        tabela_html(classes = c("", "num", "leitura"))
    })

    output$grafico <- shiny::renderPlot({
      pal <- paleta_modo(modo())
      r <- resumo()
      cl <- classes()
      topo <- max(cl$n)

      grafico <- ggplot2::ggplot() +
        ggplot2::geom_rect(
          data = cl,
          ggplot2::aes(xmin = inicio, xmax = fim, ymin = 0, ymax = n),
          fill = cores_app$destaque, colour = pal$fundo, alpha = 0.85
        ) +
        ggplot2::geom_vline(xintercept = r$media, colour = cores_app$referencia, linetype = "dashed", linewidth = 1) +
        ggplot2::geom_vline(xintercept = r$mediana, colour = pal$linha, linewidth = 1) +
        ggplot2::annotate("label", x = r$media, y = topo * 1.06, label = tr("média", "mean"), colour = cores_app$referencia, fill = pal$fundo, size = 4) +
        ggplot2::annotate("label", x = r$mediana, y = topo * 1.16, label = tr("mediana", "median"), colour = pal$linha, fill = pal$fundo, size = 4)

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
        ggplot2::scale_x_continuous(labels = rotulos_eixo()) +
        ggplot2::scale_y_continuous(breaks = function(l) scales::breaks_pretty()(c(0, l[2]))) +
        ggplot2::labs(
          title = tr("Histograma: a distribuição agrupada em classes", "Histogram: the distribution grouped into bins"),
          subtitle = exemplo()$rotulo,
          x = un(), y = tr("Frequência (n)", "Frequency (n)"),
          caption = sprintf(
            tr(
              "Dados simulados, n = %s | Tracejada: média | Contínua: mediana%s",
              "Simulated data, n = %s | Dashed: mean | Solid: median%s"
            ),
            fmt_num(r$n, 0),
            if (isTRUE(input$boxplot)) {
              tr(
                "\nAbaixo: boxplot (pontos vermelhos: pontos exteriores, a mais de 1,5 × d_q da caixa; não são necessariamente erros)",
                "\nBelow: box plot (red dots: potential outliers, more than 1.5 × IQR beyond the box; not necessarily errors)"
              )
            } else ""
          )
        ) +
        tema_grafico(modo())
    })
  })
}
