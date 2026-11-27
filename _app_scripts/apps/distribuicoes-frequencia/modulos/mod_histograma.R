# =============================================================================
# Script:  mod_histograma.R (módulo: tabela por classes e histograma com densidade)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/apps/distribuicoes-frequencia/app.R)
# Saída:
#   - funções mod_histograma_ui() e mod_histograma_server()
# =============================================================================

# Classes desiguais: a metade de cima das classes iguais é juntada de duas em
# duas, como acontece quando a cauda tem poucos dados (Morettin e Bussab,
# Problema 2.10). A última classe pode sobrar com a largura original.
quebras_desiguais <- function(quebras) {
  k <- length(quebras)
  meio <- ceiling(k / 2)
  juntas <- quebras[seq(meio, k, by = 2)]
  unique(c(quebras[seq_len(meio)], juntas, quebras[k]))
}

texto_histograma <- function() {
  tr(
    shiny::withMathJax(
      shiny::p(
        "Como quase nenhum valor se repete numa variável contínua, a tabela agrupa os dados em classes",
        "\\([a; b)\\), que incluem o limite inferior e excluem o superior. As classes precisam cobrir todos os",
        "dados sem se sobrepor, e Morettin e Bussab sugerem, em geral, de 5 a 15 classes de mesma amplitude."
      ),
      shiny::p(
        "No histograma de Morettin e Bussab, a altura de cada retângulo é a densidade de frequência",
        "\\(f_i / \\Delta_i\\), a frequência relativa dividida pela amplitude da classe. Dessa forma, a área do",
        "retângulo, \\(\\Delta_i \\times f_i/\\Delta_i = f_i\\), é a proporção de observações na classe, e a área",
        "total vale 1. Com classes de mesma amplitude, usar a frequência no eixo vertical só muda a escala. Com",
        "amplitudes diferentes, no entanto, a densidade é obrigatória, uma vez que a classe larga junta mais",
        "observações só por ser larga, e a altura pela frequência faria a cauda parecer mais pesada do que é."
      ),
      shiny::p(
        "O desenho depende de duas escolhas que não estão nos dados: a amplitude das classes e o ponto onde a",
        "primeira começa (a origem). Poucas classes escondem a forma; muitas mostram ruído. Mudar só a origem, com",
        "a mesma amplitude, já pode criar ou apagar um pico."
      ),
      shiny::p(
        shiny::strong("Experimente:"),
        "nos salários, marque classes desiguais e alterne entre frequência e densidade: com frequência, a classe",
        "dupla da direita parece ter mais gente do que tem. Depois, no tempo de espera, use amplitude grande e veja",
        "os dois picos virarem um só."
      )
    ),
    shiny::withMathJax(
      shiny::p(
        "Since almost no value repeats in a continuous variable, the table groups the data into intervals",
        "\\([a, b)\\) that include the lower limit and exclude the upper one. The intervals must cover all the data",
        "without overlapping, and Morettin and Bussab suggest, in general, 5 to 15 intervals of the same width."
      ),
      shiny::p(
        "In Morettin and Bussab's histogram, the height of each bar is the density \\(f_i / \\Delta_i\\), the",
        "relative frequency divided by the width of the interval. The area of the bar,",
        "\\(\\Delta_i \\times f_i/\\Delta_i = f_i\\), is then the share of observations in the interval, and the total",
        "area is 1. With equal widths, putting frequency on the vertical axis only changes the scale. With unequal",
        "widths, however, density is required, since a wide interval holds more observations just because it is",
        "wide, and frequency on the vertical axis would make the tail look heavier than it is."
      ),
      shiny::p(
        "The picture depends on two choices that are not in the data: the width of the intervals and where the",
        "first one starts (the origin). Too few intervals hide the shape; too many show noise. Shifting only the",
        "origin, with the same width, can already create or erase a peak."
      ),
      shiny::p(
        shiny::strong("Try it:"),
        "for the salaries, tick unequal intervals and switch between frequency and density: with frequency, the",
        "double-width interval on the right looks more crowded than it is. Then, for the waiting time, use a large",
        "width and watch the two peaks merge into one."
      )
    )
  )
}

mod_histograma_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::selectInput(
        ns("exemplo"), tr("Variável", "Variable"),
        choices = stats::setNames(names(exemplos_continuos), purrr::map_chr(exemplos_continuos, "rotulo"))
      ),
      shiny::conditionalPanel(
        "input.exemplo != 'salarios'", ns = ns,
        shiny::sliderInput(ns("n"), tr("Tamanho da amostra (n)", "Sample size (n)"), min = 30, max = 2000, value = 200, step = 10, sep = sep_milhar())
      ),
      shiny::uiOutput(ns("controles_classes")),
      shiny::checkboxInput(ns("desiguais"), tr("Classes desiguais (juntar a metade de cima)", "Unequal widths (merge the upper half)"), value = FALSE),
      shiny::radioButtons(
        ns("altura"), tr("Altura dos retângulos", "Bar height"),
        choices = stats::setNames(c("densidade", "frequencia"), tr(c("Densidade (fᵢ / Δᵢ)", "Frequência relativa (fᵢ)"), c("Density (fᵢ / Δᵢ)", "Relative frequency (fᵢ)")))
      ),
      shiny::checkboxInput(ns("pontos"), tr("Mostrar os dados abaixo do eixo", "Show the data below the axis"), value = TRUE),
      shiny::conditionalPanel(
        "input.exemplo != 'salarios'", ns = ns,
        shiny::actionButton(ns("sortear"), tr("Sortear nova amostra", "Draw a new sample"), class = "btn-primary", icon = shiny::icon("dice"))
      )
    ),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(title = tr("Classes", "Intervals"), value = shiny::textOutput(ns("n_classes")), shiny::textOutput(ns("amplitudes")), theme = "primary"),
      bslib::value_box(title = tr("Área total do histograma", "Total area of the histogram"), value = shiny::textOutput(ns("area")), shiny::textOutput(ns("area_desc")), theme = "info"),
      bslib::value_box(title = tr("Classe modal", "Modal interval"), value = shiny::textOutput(ns("modal")), shiny::textOutput(ns("modal_desc")), theme = NULL)
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico"), height = "440px"))),
      bslib::card(
        bslib::card_header(tr("Tabela de frequências por classes", "Grouped frequency table")),
        max_height = "560px",
        bslib::card_body(
          shiny::uiOutput(ns("tabela")),
          shiny::p(class = "small text-muted mb-0", tr(
            "Δᵢ: amplitude da classe. fᵢ / Δᵢ: densidade de frequência, a altura do retângulo. Fᵢ: frequência acumulada.",
            "Δᵢ: interval width. fᵢ / Δᵢ: density, the height of the bar. Fᵢ: cumulative relative frequency."
          ))
        )
      )
    ),
    bslib::card(bslib::card_header(rotulo_entenda()), bslib::card_body(texto_histograma()))
  )
}

mod_histograma_server <- function(id, modo = shiny::reactive("light")) {
  shiny::moduleServer(id, function(input, output, session) {

    exemplo <- shiny::reactive(exemplos_continuos[[input$exemplo]])

    dados <- shiny::reactive({
      input$sortear
      exemplo()$gerar(input$n)
    })

    # Os sliders levam o nome da variável no id. Ao trocar de variável, o
    # slider antigo some e o novo ainda não mandou valor: classes() espera
    # (req) em vez de desenhar os dados novos com a amplitude da variável
    # anterior. freezeReactiveValue() não serve aqui, porque o valor novo pode
    # ser igual ao antigo (altura e espera têm amplitude 5) e o gráfico travaria.
    id_largura <- shiny::reactive(paste0("largura_", input$exemplo))
    id_deslocamento <- shiny::reactive(paste0("deslocamento_", input$exemplo))

    output$controles_classes <- shiny::renderUI({
      ex <- exemplo()
      ns <- session$ns
      shiny::tagList(
        shiny::sliderInput(
          ns(id_largura()), sprintf(tr("Amplitude das classes (%s)", "Interval width (%s)"), ex$unidade),
          min = ex$largura / 4, max = ex$largura * 4, value = ex$largura, step = ex$largura / 4
        ),
        shiny::sliderInput(
          ns(id_deslocamento()), tr("Deslocar a origem (fração da amplitude)", "Shift the origin (fraction of the width)"),
          min = 0, max = 0.9, value = 0, step = 0.1
        )
      )
    })

    classes <- shiny::reactive({
      largura <- input[[id_largura()]]
      deslocamento <- input[[id_deslocamento()]]
      shiny::req(largura, !is.null(deslocamento))
      x <- dados()
      ex <- exemplo()
      origem <- ex$origem + deslocamento * largura
      quebras <- quebras_largura(x, largura, origem)
      if (isTRUE(input$desiguais) && length(quebras) > 4) quebras <- quebras_desiguais(quebras)
      tabela_classes_densidade(x, quebras, ex$digitos)
    })

    output$n_classes <- shiny::renderText(nrow(classes()))
    output$amplitudes <- shiny::renderText({
      d <- unique(round(classes()$delta, 6))
      if (length(d) == 1) {
        sprintf(tr("Todas com amplitude %s", "All of width %s"), fmt_num(d, exemplo()$digitos))
      } else {
        sprintf(tr("Amplitudes diferentes: %s", "Different widths: %s"), paste(fmt_num(d, exemplo()$digitos), collapse = tr("; ", ", ")))
      }
    })

    output$area <- shiny::renderText({
      cl <- classes()
      if (input$altura == "densidade") fmt_num(sum(cl$densidade * cl$delta), 2) else fmt_num(sum(cl$f_i * cl$delta), 2)
    })
    output$area_desc <- shiny::renderText({
      if (input$altura == "densidade") {
        tr("Soma de Δᵢ × fᵢ/Δᵢ = Σ fᵢ = 1", "Sum of Δᵢ × fᵢ/Δᵢ = Σ fᵢ = 1")
      } else {
        tr("Com altura fᵢ, a área depende das amplitudes", "With height fᵢ, the area depends on the widths")
      }
    })

    output$modal <- shiny::renderText({
      cl <- classes()
      paste(cl$classe[cl$densidade == max(cl$densidade)], collapse = " ")
    })
    output$modal_desc <- shiny::renderText(tr(
      "Maior densidade (não necessariamente maior nᵢ)",
      "Highest density (not necessarily the largest nᵢ)"
    ))

    output$tabela <- shiny::renderUI({
      cl <- classes()
      dig <- exemplo()$digitos
      saida <- data.frame(
        cl$classe, fmt_num(cl$n_i, 0), fmt_num(cl$f_i, 3), fmt_num(cl$delta, dig),
        fmt_num(cl$densidade, 4), fmt_pct(cl$F_i)
      )
      names(saida) <- c(tr("Classe", "Interval"), "nᵢ", "fᵢ", "Δᵢ", "fᵢ / Δᵢ", "Fᵢ")
      tabela_html(saida, classes = c("", "num", "num", "num", "num", "num"))
    })

    output$grafico <- shiny::renderPlot({
      pal <- paleta_modo(modo())
      cl <- classes()
      ex <- exemplo()
      x <- dados()
      densidade <- input$altura == "densidade"
      cl$topo <- if (densidade) cl$densidade else cl$f_i
      teto <- max(cl$topo)

      grafico <- ggplot2::ggplot() +
        ggplot2::geom_rect(
          data = cl, ggplot2::aes(xmin = inicio, xmax = fim, ymin = 0, ymax = topo),
          fill = cores_app$destaque, colour = pal$fundo, alpha = 0.85
        )

      if (isTRUE(input$pontos)) {
        grafico <- grafico +
          ggplot2::geom_jitter(
            data = data.frame(x = x), ggplot2::aes(x = x, y = -teto * 0.07),
            width = 0, height = teto * 0.03, colour = pal$texto, alpha = if (length(x) > 500) 0.25 else 0.6, size = 1.4
          )
      }

      grafico +
        ggplot2::scale_x_continuous(breaks = cl$inicio[seq(1, nrow(cl), by = max(1, ceiling(nrow(cl) / 12)))], labels = rotulos_eixo()) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = if (densidade) tr("Histograma com densidade de frequência", "Density histogram") else tr("Histograma com frequência relativa", "Relative frequency histogram"),
          subtitle = ex$rotulo,
          x = ex$unidade,
          y = if (densidade) sprintf(tr("Densidade (por %s)", "Density (per %s)"), ex$unidade) else tr("Frequência relativa (fᵢ)", "Relative frequency (fᵢ)"),
          caption = sprintf(
            tr("n = %s%s", "n = %s%s"), fmt_num(length(x), 0),
            if (isTRUE(input$pontos)) tr(" | Pontos abaixo do eixo: cada observação", " | Dots below the axis: each observation") else ""
          )
        ) +
        tema_grafico(modo())
    })
  })
}
