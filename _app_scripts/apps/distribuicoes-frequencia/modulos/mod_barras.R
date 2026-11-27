# =============================================================================
# Script:  mod_barras.R (módulo: tabela de frequências e gráfico de barras)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/apps/distribuicoes-frequencia/app.R)
# Saída:
#   - funções mod_barras_ui() e mod_barras_server()
# =============================================================================

rotulo_tipo <- function(tipo) {
  switch(
    tipo,
    nominal = tr("qualitativa nominal", "nominal categorical"),
    ordinal = tr("qualitativa ordinal", "ordinal categorical"),
    discreta = tr("quantitativa discreta", "discrete quantitative")
  )
}

# Posições das fatias no gráfico de setores, em fração da volta (0 a 1, no
# sentido horário a partir do topo). Fatias abaixo de 12% recebem o rótulo do
# lado de fora, porque dentro delas o texto não cabe.
setores_posicoes <- function(tab, limite = 0.12) {
  tab$ymax <- cumsum(tab$f_i)
  tab$ymin <- tab$ymax - tab$f_i
  tab$meio <- (tab$ymin + tab$ymax) / 2
  tab$fora <- tab$f_i < limite
  tab$lado <- 0
  tab
}

# Giro do círculo (em radianos) que leva o centro do bloco de fatias pequenas
# para as 3 horas. Ali os rótulos de fora ficam um embaixo do outro, em vez de
# lado a lado no topo, onde se sobrepunham. Sem fatias pequenas, não gira.
giro_setores <- function(tab) {
  if (!any(tab$fora)) return(0)
  centro <- (min(tab$ymin[tab$fora]) + max(tab$ymax[tab$fora])) / 2
  2 * pi * (0.25 - centro)
}

texto_barras <- function() {
  tr(
    shiny::withMathJax(
      shiny::p(
        "A tabela de frequências é o primeiro resumo de qualquer variável. Para cada categoria \\(i\\), ela mostra",
        "a frequência absoluta \\(n_i\\), quantas observações caíram ali, e a frequência relativa",
        "\\(f_i = n_i / n\\), a proporção do total. As \\(f_i\\) somam 1, ou 100%. Quando a tabela mostra só",
        "porcentagens, o \\(n\\) total precisa aparecer em algum lugar, porque 50% de 4 pessoas não informa o mesmo",
        "que 50% de 4 mil."
      ),
      shiny::p(
        "A frequência acumulada \\(F_i\\) soma as categorias até a \\(i\\)-ésima. Ela só tem sentido quando as",
        "categorias têm ordem, como no grau de instrução ou no número de filhos: \"até o ensino médio\" quer dizer",
        "algo, \"até o interior\" não. Por isso a tabela esconde a acumulada nas variáveis nominais."
      ),
      shiny::p(
        "No gráfico de barras, cada categoria vira uma barra com altura proporcional à frequência. As barras ficam",
        "separadas, porque as categorias não se encostam como as classes de um histograma. Numa variável ordinal ou",
        "discreta, a ordem do eixo é a ordem natural. Numa nominal, ordenar pela frequência facilita a leitura.",
        "O gráfico de setores (pizza) mostra o mesmo, mas comparar ângulos é mais difícil do que comparar alturas,",
        "e, segundo Agresti e Finlay, as barras comparam com mais precisão categorias de frequências parecidas."
      ),
      shiny::p(
        shiny::strong("Experimente:"),
        "no meio de transporte, ordene as barras pela frequência e compare com a ordem original. Depois passe para",
        "o número de filhos e veja a categoria 4 aparecer com zero: numa variável discreta, o valor sem ninguém",
        "também é informação."
      )
    ),
    shiny::withMathJax(
      shiny::p(
        "The frequency table is the first summary of any variable. For each category \\(i\\), it shows the",
        "frequency \\(n_i\\), how many observations fell there, and the relative frequency \\(f_i = n_i / n\\), the",
        "proportion of the total. The \\(f_i\\) add up to 1, or 100%. When a table shows only percentages, the total",
        "\\(n\\) must appear somewhere, because 50% of 4 people does not say the same as 50% of 4,000."
      ),
      shiny::p(
        "The cumulative frequency \\(F_i\\) adds up the categories through the \\(i\\)th one. It only makes sense when",
        "the categories are ordered, as with education level or number of children: \"up to secondary school\" means",
        "something, \"up to the interior\" does not. That is why the table hides it for nominal variables."
      ),
      shiny::p(
        "In a bar graph, each category becomes a bar whose height is proportional to its frequency. The bars are",
        "separated, because categories do not touch each other the way histogram bins do. For an ordinal or discrete",
        "variable, the axis follows the natural order. For a nominal one, sorting by frequency makes it easier to read.",
        "A pie chart shows the same thing, but comparing angles is harder than comparing heights, and Agresti and",
        "Finlay note that a bar graph compares categories with similar frequencies more precisely."
      ),
      shiny::p(
        shiny::strong("Try it:"),
        "for means of transport, sort the bars by frequency and compare with the original order. Then switch to",
        "number of children and see category 4 show up with zero: for a discrete variable, a value nobody has is",
        "information too."
      )
    )
  )
}

mod_barras_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::selectInput(
        ns("exemplo"), tr("Variável", "Variable"),
        choices = stats::setNames(names(exemplos_categoricos), purrr::map_chr(exemplos_categoricos, "rotulo"))
      ),
      shiny::radioButtons(
        ns("escala"), tr("Altura das barras", "Bar height"),
        choices = stats::setNames(c("n", "pct"), tr(c("Frequência (nᵢ)", "Porcentagem (100 fᵢ)"), c("Frequency (nᵢ)", "Percentage (100 fᵢ)"))),
        selected = "pct"
      ),
      shiny::uiOutput(ns("controle_ordem")),
      shiny::checkboxInput(ns("setores"), tr("Comparar com o gráfico de setores", "Compare with a pie chart"), value = FALSE)
    ),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(title = tr("Tipo da variável", "Type of variable"), value = shiny::textOutput(ns("tipo")), shiny::textOutput(ns("fonte")), theme = "primary"),
      bslib::value_box(title = tr("Total de observações", "Total observations"), value = shiny::textOutput(ns("total")), tr("n = Σ nᵢ", "n = Σ nᵢ"), theme = "info"),
      bslib::value_box(title = tr("Moda", "Mode"), value = shiny::textOutput(ns("moda")), shiny::textOutput(ns("moda_desc")), theme = NULL)
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico"), height = "420px"))),
      bslib::card(
        bslib::card_header(tr("Tabela de frequências", "Frequency table")),
        bslib::card_body(
          shiny::uiOutput(ns("tabela")),
          shiny::p(class = "small text-muted mb-0", shiny::textOutput(ns("nota_tabela"), inline = TRUE))
        )
      )
    ),
    bslib::card(bslib::card_header(rotulo_entenda()), bslib::card_body(texto_barras()))
  )
}

mod_barras_server <- function(id, modo = shiny::reactive("light")) {
  shiny::moduleServer(id, function(input, output, session) {

    exemplo <- shiny::reactive(exemplos_categoricos[[input$exemplo]])
    tem_ordem <- shiny::reactive(exemplo()$tipo != "nominal")

    output$controle_ordem <- shiny::renderUI({
      if (tem_ordem()) {
        shiny::helpText(tr(
          "Categorias com ordem natural: o eixo segue essa ordem.",
          "Categories with a natural order: the axis follows it."
        ))
      } else {
        shiny::checkboxInput(session$ns("ordenar"), tr("Ordenar barras pela frequência", "Sort bars by frequency"), value = FALSE)
      }
    })

    tabela <- shiny::reactive({
      ex <- exemplo()
      tab <- tabela_frequencias(ex$categorias, ex$contagens)
      if (!tem_ordem() && isTRUE(input$ordenar)) tab <- tab[order(-tab$n_i), ]
      tab
    })

    output$tipo <- shiny::renderText(rotulo_tipo(exemplo()$tipo))
    output$fonte <- shiny::renderText(sprintf(tr("Fonte: %s", "Source: %s"), exemplo()$fonte))
    output$total <- shiny::renderText(fmt_num(sum(exemplo()$contagens), 0))

    output$moda <- shiny::renderText({
      ex <- exemplo()
      paste(ex$categorias[ex$contagens == max(ex$contagens)], collapse = tr(" e ", " and "))
    })
    output$moda_desc <- shiny::renderText({
      ex <- exemplo()
      sprintf(tr("Categoria mais frequente: %s de %s", "Most frequent category: %s of %s"),
              fmt_num(max(ex$contagens), 0), fmt_num(sum(ex$contagens), 0))
    })

    output$tabela <- shiny::renderUI({
      tab <- tabela()
      saida <- data.frame(
        tab$categoria, fmt_num(tab$n_i, 0), fmt_num(tab$f_i, 3), fmt_pct(tab$f_i)
      )
      nomes <- tr(c("Categoria", "nᵢ", "fᵢ", "100 fᵢ"), c("Category", "nᵢ", "fᵢ", "100 fᵢ"))
      classes <- c("", "num", "num", "num")
      if (tem_ordem()) {
        saida <- cbind(saida, fmt_num(tab$N_i, 0), fmt_pct(tab$F_i))
        nomes <- c(nomes, "Nᵢ", "100 Fᵢ")
        classes <- c(classes, "num", "num")
      }
      total <- c(tr("Total", "Total"), fmt_num(sum(tab$n_i), 0), fmt_num(1, 3), fmt_pct(1), rep("", length(nomes) - 4))
      saida <- rbind(stats::setNames(saida, nomes), stats::setNames(as.data.frame(as.list(total)), nomes))
      tabela_html(saida, classes = classes)
    })

    output$nota_tabela <- shiny::renderText({
      if (tem_ordem()) {
        tr(
          "Nᵢ e Fᵢ: frequências acumuladas até a categoria da linha. A última linha de Fᵢ chega a 100%.",
          "Nᵢ and Fᵢ: cumulative frequencies through the category in that row. The last Fᵢ reaches 100%."
        )
      } else {
        tr(
          "Variável nominal: sem frequência acumulada, porque as categorias não têm ordem.",
          "Nominal variable: no cumulative frequency, because the categories have no order."
        )
      }
    })

    output$grafico <- shiny::renderPlot({
      pal <- paleta_modo(modo())
      tab <- tabela()
      ex <- exemplo()
      tab$categoria <- factor(tab$categoria, levels = tab$categoria)
      tab$altura <- if (input$escala == "n") tab$n_i else 100 * tab$f_i
      tab$rotulo <- if (input$escala == "n") fmt_num(tab$n_i, 0) else fmt_pct(tab$f_i)
      rotulo_y <- if (input$escala == "n") tr("Frequência (nᵢ)", "Frequency (nᵢ)") else tr("Porcentagem", "Percentage")
      legenda <- sprintf(tr("n = %s | %s", "n = %s | %s"), fmt_num(sum(tab$n_i), 0), ex$fonte)

      if (isTRUE(input$setores)) {
        tab <- setores_posicoes(tab[tab$n_i > 0, ])
        grandes <- tab[!tab$fora, ]
        pequenas <- tab[tab$fora, ]
        return(
          ggplot2::ggplot(tab) +
            ggplot2::geom_rect(ggplot2::aes(xmin = 0, xmax = 1, ymin = ymin, ymax = ymax, fill = categoria), colour = pal$fundo) +
            ggplot2::geom_text(
              data = grandes, ggplot2::aes(x = 0.62, y = meio, label = paste0(categoria, "\n", fmt_pct(f_i))),
              colour = "white", size = 4
            ) +
            ggplot2::geom_text(
              data = pequenas, ggplot2::aes(x = 1.06, y = meio, label = paste0(categoria, " (", fmt_pct(f_i), ")"), hjust = lado),
              colour = pal$texto, size = 4
            ) +
            ggplot2::scale_x_continuous(limits = c(0, 1.3), expand = c(0, 0)) +
            ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
            ggplot2::coord_polar(theta = "y", start = giro_setores(tab), clip = "off") +
            ggplot2::scale_fill_manual(values = grDevices::colorRampPalette(c("#1f3a9e", "#7d93ef"))(nrow(tab))) +
            ggplot2::labs(
              # Em duas linhas: o círculo tem proporção fixa e estreita a área do título
              title = tr("Setores: o mesmo dado,\nmais difícil de comparar", "Pie chart: same data,\nharder to compare"),
              subtitle = ex$rotulo, x = NULL, y = NULL, caption = legenda
            ) +
            tema_grafico(modo()) +
            ggplot2::theme(axis.text = ggplot2::element_blank(), panel.grid.major = ggplot2::element_blank())
        )
      }

      ggplot2::ggplot(tab, ggplot2::aes(x = categoria, y = altura)) +
        ggplot2::geom_col(fill = cores_app$destaque, width = 0.65) +
        ggplot2::geom_text(ggplot2::aes(label = rotulo), vjust = -0.5, colour = pal$texto, size = 4.5) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12)), labels = rotulos_eixo()) +
        ggplot2::labs(
          title = tr("Gráfico de barras", "Bar graph"),
          subtitle = ex$rotulo, x = NULL, y = rotulo_y, caption = legenda
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
    }, bg = "transparent") # o círculo tem proporção fixa: fora dele, vale o fundo do card
  })
}
