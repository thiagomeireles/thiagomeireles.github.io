# =============================================================================
# Script:  mod_posicao.R (módulo: média, mediana e quantis nas convenções dos livros)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - objetos e funções de _app_scripts/apps/posicao-dispersao/modulos/dados.R
# Saída:
#   - funções mod_posicao_ui() e mod_posicao_server()
# =============================================================================

texto_posicao <- function() {
  tr(
    shiny::withMathJax(
      shiny::p(
        "Resumir a localização de uma variável num único número exige escolher o que se entende por \"centro\".",
        "A média, \\(\\bar{x} = \\frac{1}{n}\\sum_{i=1}^{n} x_i\\) (notação \\(\\bar{y}\\) em Agresti e Finlay), funciona",
        "como o centro de gravidade dos dados: os desvios positivos e negativos em torno dela se anulam exatamente,",
        "\\(\\sum (x_i - \\bar{x}) = 0\\). Por usar a distância de cada observação, um único salário muito alto desloca",
        "o ponto de equilíbrio para a direita, mesmo que todos os demais continuem no mesmo lugar."
      ),
      shiny::p(
        "A mediana (\\(md\\)) usa só a ordem. Ordenados os valores \\(x_{(1)} \\le x_{(2)} \\le \\dots \\le x_{(n)}\\),",
        "ela ocupa a posição central \\((n + 1)/2\\) quando \\(n\\) é ímpar, ou é a média das duas posições centrais",
        "quando \\(n\\) é par. Dessa forma, trocar o maior valor por dez vezes o seu tamanho não altera a mediana,",
        "motivo pelo qual ela descreve melhor o valor típico em distribuições assimétricas como a de renda."
      ),
      shiny::p(
        "Os quantis estendem essa ideia para qualquer corte \\(p \\in (0, 1)\\): os quartis dividem os dados ordenados",
        "em quatro partes (\\(p = 0{,}25;\\; 0{,}50;\\; 0{,}75\\)), os decis em dez e os percentis em cem. Em amostras",
        "finitas, no entanto, livros e programas interpolam as posições intermediárias de maneiras ligeiramente",
        "distintas. Morettin e Bussab (Problema 3.17) associam \\(x_{(i)}\\) à ordem \\(p_i = (i - 0{,}5)/n\\) e",
        "interpolam linearmente entre vizinhos (`quantile(..., type = 5)` no R); Agresti e Finlay definem \\(Q_1\\) e",
        "\\(Q_3\\) como as medianas das metades inferior e superior; e o padrão do R (`type = 7`) usa",
        "\\(p_i = (i - 1)/(n - 1)\\). Com \\(n\\) grande as três definições convergem, mas em amostras pequenas",
        "convém declarar qual foi adotada."
      ),
      shiny::p(
        shiny::strong("Experimente:"),
        "selecione a amostra pequena do Exemplo 3.5 (\\(n = 9\\)) e veja \\(q(0{,}25)\\) passar de 4,50 em Morettin e",
        "Bussab para 4,00 pelas metades estritas e 5,00 nas dobradiças de Tukey (`fivenum`) e no `type = 7`. Depois,",
        "nos salários ou na renda, inclua um valor extremo e compare o salto da média com a estabilidade da mediana."
      )
    ),
    shiny::withMathJax(
      shiny::p(
        "Summarizing the location of a variable in a single number requires choosing what \"center\" means.",
        "The mean, \\(\\bar{y} = \\frac{1}{n}\\sum_{i=1}^{n} y_i\\) (written \\(\\bar{x}\\) in Morettin and Bussab),",
        "acts as the center of gravity of the data: positive and negative deviations around it cancel out exactly,",
        "\\(\\sum (y_i - \\bar{y}) = 0\\). Because it uses the distance of every observation, a single very high",
        "salary pulls the balance point to the right even when everyone else stays put."
      ),
      shiny::p(
        "The median (\\(md\\)) uses only rank order. Once the values are sorted \\(y_{(1)} \\le y_{(2)} \\le \\dots \\le y_{(n)}\\),",
        "it sits at the middle position \\((n + 1)/2\\) when \\(n\\) is odd, or equals the average of the two middle",
        "positions when \\(n\\) is even. Multiplying the largest value by ten therefore leaves the median untouched,",
        "which is why it better describes a typical observation in skewed distributions such as income."
      ),
      shiny::p(
        "Quantiles extend this idea to any cut \\(p \\in (0, 1)\\): quartiles split the sorted data into four parts",
        "(\\(p = 0.25,\\; 0.50,\\; 0.75\\)), deciles into ten and percentiles into one hundred. In finite samples,",
        "however, textbooks and software interpolate between ranks in slightly different ways. Morettin and Bussab",
        "(Problem 3.17) attach \\(y_{(i)}\\) to order \\(p_i = (i - 0.5)/n\\) and interpolate linearly",
        "(`quantile(..., type = 5)` in R); Agresti and Finlay define \\(Q_1\\) and \\(Q_3\\) as the medians of the",
        "lower and upper halves; and R's default (`type = 7`) uses \\(p_i = (i - 1)/(n - 1)\\). For large \\(n\\) all",
        "three agree closely, whereas in small samples it is worth stating which definition was used."
      ),
      shiny::p(
        shiny::strong("Try it:"),
        "pick the small sample from Example 3.5 (\\(n = 9\\)) and watch \\(Q_1\\) change from 4.50 under Morettin and",
        "Bussab to 4.00 under strict halves and 5.00 under Tukey's hinges (`fivenum`) and `type = 7`. Then switch to",
        "salaries or income, add an extreme value and compare the jump in the mean with the stability of the median."
      )
    )
  )
}

cuidados_posicao <- function() {
  tr(
    c(
      "Média − mediana e forma" = paste(
        "numa distribuição unimodal assimétrica à direita, a cauda alta costuma puxar a média para cima da mediana.",
        "Ainda que frequente, essa relação não é uma lei matemática para qualquer distribuição multimodal ou discreta."
      ),
      "Quartis em programas diferentes" = paste(
        "R (`type = 7`), SPSS (`type = 6`), Morettin e Bussab (`type = 5`) e `stats::fivenum()` diferem na interpolação.",
        "Em exercícios de livro, a divergência vem da fórmula escolhida, não de erro de conta."
      ),
      "Empates em dados discretos" = paste(
        "quando um mesmo valor se repete muitas vezes, mais de 25% das observações podem cair em Q1 ou abaixo dele.",
        "A definição garante \"pelo menos p\" abaixo ou igual e \"pelo menos 1 − p\" acima ou igual."
      ),
      "Escala da variável" = paste(
        "média exige variável quantitativa (ou dicotômica 0/1, em que a média é a proporção); mediana e quantis",
        "valem também para qualitativas ordinais, sem interpolar códigos numéricos."
      )
    ),
    c(
      "Mean − median and shape" = paste(
        "in a unimodal right-skewed distribution, the upper tail usually pulls the mean above the median.",
        "Although common, this rule is not a mathematical theorem for arbitrary multimodal or discrete distributions."
      ),
      "Quartiles across software" = paste(
        "R (`type = 7`), SPSS (`type = 6`), Morettin and Bussab (`type = 5`) and `stats::fivenum()` interpolate",
        "differently. When checking textbook answers, small differences come from the definition, not arithmetic error."
      ),
      "Ties in discrete data" = paste(
        "when the same value repeats often, well over 25% of observations can sit at or below Q1.",
        "The definition guarantees at least p at or below and at least 1 − p at or above."
      ),
      "Measurement scale" = paste(
        "the mean requires a quantitative variable (or a 0/1 indicator, whose mean is the proportion); the median",
        "and quantiles also apply to ordinal variables without interpolating arbitrary numeric codes."
      )
    )
  )
}

mod_posicao_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::selectInput(
        ns("exemplo"), tr("Conjunto de dados", "Data set"),
        choices = stats::setNames(names(exemplos_dados), purrr::map_chr(exemplos_dados, "rotulo"))
      ),
      shiny::uiOutput(ns("controle_n")),
      shiny::radioButtons(
        ns("convencao"), tr("Convenção dos quartis no gráfico", "Quartile convention in the chart"),
        choices = stats::setNames(
          c("bm", "af", "fivenum", "r7"),
          tr(
            c("Morettin e Bussab: pᵢ = (i − 0,5)/n", "Agresti e Finlay: metades", "Tukey: stats::fivenum()", "R padrão: type = 7"),
            c("Morettin and Bussab: pᵢ = (i − 0.5)/n", "Agresti and Finlay: halves", "Tukey: stats::fivenum()", "R default: type = 7")
          )
        ),
        selected = "bm"
      ),
      shiny::sliderInput(
        ns("ordem_p"), tr("Consultar quantil q(p) de ordem p", "Inspect quantile q(p) of order p"),
        min = 0.05, max = 0.95, value = 0.10, step = 0.05
      ),
      shiny::checkboxInput(ns("extremo"), tr("Incluir um valor extremo", "Add an extreme value"), value = FALSE),
      shiny::uiOutput(ns("controle_extremo")),
      shiny::uiOutput(ns("aviso_extremo")),
      shiny::uiOutput(ns("botao_sortear"))
    ),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(
        title = tr("Média (centro de gravidade)", "Mean (center of gravity)"),
        value = shiny::textOutput(ns("media")),
        shiny::textOutput(ns("media_desc")),
        theme = "primary"
      ),
      bslib::value_box(
        title = tr("Mediana (posição central)", "Median (middle rank)"),
        value = shiny::textOutput(ns("mediana")),
        shiny::textOutput(ns("mediana_desc")),
        theme = "info"
      ),
      bslib::value_box(
        title = shiny::textOutput(ns("titulo_qp")),
        value = shiny::textOutput(ns("valor_qp")),
        shiny::textOutput(ns("desc_qp")),
        theme = NULL
      )
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico"), height = "420px"))),
      bslib::card(
        bslib::card_header(tr("Quartis nas diferentes convenções", "Quartiles across conventions")),
        bslib::card_body(
          shiny::uiOutput(ns("tabela_quartis")),
          shiny::p(class = "small text-muted mt-2 mb-0", tr(
            paste(
              "Com n par, Agresti e Finlay e stats::fivenum() coincidem. Com n ímpar, fivenum() inclui a mediana",
              "nas duas metades (dobradiças de Tukey), enquanto a divisão estrita a exclui."
            ),
            paste(
              "For even n, Agresti and Finlay and stats::fivenum() match. For odd n, fivenum() includes the median",
              "in both halves (Tukey's hinges), whereas strict halves exclude it."
            )
          ))
        )
      )
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
      bslib::card(bslib::card_header(rotulo_entenda()), bslib::card_body(texto_posicao())),
      cartao_cuidados(tr("Cuidados", "Watch out"), cuidados_posicao())
    )
  )
}

mod_posicao_server <- function(id, modo = shiny::reactive("light")) {
  shiny::moduleServer(id, function(input, output, session) {

    exemplo <- shiny::reactive(exemplos_dados[[input$exemplo]])

    output$controle_n <- shiny::renderUI({
      if (isTRUE(exemplo()$fixo)) return(NULL)
      shiny::sliderInput(
        session$ns("n"), tr("Tamanho da amostra (n)", "Sample size (n)"),
        min = 15, max = 500, value = 80, step = 5, sep = sep_milhar()
      )
    })

    output$botao_sortear <- shiny::renderUI({
      if (isTRUE(exemplo()$fixo)) return(shiny::helpText(exemplo()$fonte))
      shiny::actionButton(
        session$ns("sortear"), tr("Sortear nova amostra", "Draw a new sample"),
        class = "btn-primary", icon = shiny::icon("dice")
      )
    })

    output$controle_extremo <- shiny::renderUI({
      shiny::req(input$extremo)
      ex <- exemplo()
      shiny::numericInput(
        session$ns("valor_extremo"),
        sprintf(tr("Valor extremo (%s)", "Extreme value (%s)"), ex$un_curta),
        value = ex$extremo, min = ex$minimo, max = ex$maximo, step = 1
      )
    })

    shiny::observeEvent(input$exemplo, congelar_extremo(input), ignoreInit = TRUE)
    output$aviso_extremo <- shiny::renderUI(aviso_extremo(input$extremo, input$valor_extremo, exemplo()))

    base <- shiny::reactive({
      if (!isTRUE(exemplo()$fixo)) input$sortear
      n_val <- if (isTRUE(exemplo()$fixo)) 0L else (input$n %||% 80L)
      exemplo()$gerar(n_val)
    })

    tem_extremo <- shiny::reactive(extremo_aceito(input$extremo, input$valor_extremo, exemplo()))

    dados <- shiny::reactive({
      if (tem_extremo()) c(base(), input$valor_extremo) else base()
    })

    output$media <- shiny::renderText(fmt_un(mean(dados()), exemplo()$un_curta, exemplo()$digitos))
    output$media_desc <- shiny::renderText({
      if (tem_extremo()) {
        sprintf(tr("Sem o extremo: %s", "Without the extreme: %s"), fmt_un(mean(base()), exemplo()$un_curta, exemplo()$digitos))
      } else {
        sprintf(tr("n = %s | Σ(xᵢ − x̄) = 0", "n = %s | Σ(yᵢ − ȳ) = 0"), fmt_num(length(dados()), 0))
      }
    })

    output$mediana <- shiny::renderText(fmt_un(stats::median(dados()), exemplo()$un_curta, exemplo()$digitos))
    output$mediana_desc <- shiny::renderText({
      if (tem_extremo()) {
        sprintf(tr("Sem o extremo: %s", "Without the extreme: %s"), fmt_un(stats::median(base()), exemplo()$un_curta, exemplo()$digitos))
      } else {
        sprintf(tr("Posição central: (n + 1)/2 = %s", "Middle rank: (n + 1)/2 = %s"), fmt_num((length(dados()) + 1) / 2, 1))
      }
    })

    output$titulo_qp <- shiny::renderText({
      sprintf(tr("Quantil q(%s)", "Quantile q(%s)"), fmt_num(input$ordem_p, 2))
    })
    output$valor_qp <- shiny::renderText({
      metodo <- if (identical(input$convencao, "bm")) "bm" else "r7"
      fmt_un(quantil_ordem(dados(), input$ordem_p, metodo), exemplo()$un_curta, exemplo()$digitos)
    })
    output$desc_qp <- shiny::renderText({
      x <- dados()
      metodo <- if (identical(input$convencao, "bm")) "bm" else "r7"
      qp <- quantil_ordem(x, input$ordem_p, metodo)
      abaixo <- mean(x <= qp)
      # As metades de Agresti e Finlay e as dobradiças de Tukey só definem quartis;
      # para outras ordens p, o app usa o padrão do R
      definicao <- if (identical(metodo, "bm")) "type = 5" else "type = 7"
      sprintf(tr("%s das observações ≤ q(p) | %s", "%s of observations ≤ q(p) | %s"), fmt_pct(abaixo, 1), definicao)
    })

    output$tabela_quartis <- shiny::renderUI({
      x <- dados()
      dig <- exemplo()$digitos
      metodos <- c("bm", "af", "fivenum", "r7")
      nomes_m <- tr(
        c("Morettin e Bussab (type = 5)", "Agresti e Finlay (metades)", "Tukey (stats::fivenum)", "R padrão (type = 7)"),
        c("Morettin and Bussab (type = 5)", "Agresti and Finlay (halves)", "Tukey (stats::fivenum)", "R default (type = 7)")
      )
      qs <- purrr::map(metodos, \(m) quartis_convencao(x, m))
      tabela <- data.frame(
        nomes_m,
        purrr::map_chr(qs, \(q) fmt_num(q[1], dig)),
        purrr::map_chr(qs, \(q) fmt_num(q[2], dig)),
        purrr::map_chr(qs, \(q) fmt_num(q[3], dig)),
        purrr::map_chr(qs, \(q) fmt_num(q[3] - q[1], dig))
      )
      names(tabela) <- c(
        tr("Definição", "Definition"),
        tr("1º quartil", "1st quartile"),
        tr("Mediana", "Median"),
        tr("3º quartil", "3rd quartile"),
        tr("d_q (Q3 − Q1)", "IQR (Q3 − Q1)")
      )
      tabela_html(tabela, classes = c("", "num", "num", "num", "num"))
    })

    output$grafico <- shiny::renderPlot({
      pal <- paleta_modo(modo())
      ex <- exemplo()
      x <- dados()
      qs <- quartis_convencao(x, input$convencao)
      med <- mean(x)
      md <- qs[2]
      metodo_qp <- if (identical(input$convencao, "bm")) "bm" else "r7"
      qp <- quantil_ordem(x, input$ordem_p, metodo_qp)

      faixa_iqr <- data.frame(xmin = qs[1], xmax = qs[3])

      if (length(x) <= 40) {
        d <- data.frame(x = x, extremo = tem_extremo() & seq_along(x) == length(x))
        grafico <- ggplot2::ggplot(d, ggplot2::aes(x = x)) +
          ggplot2::geom_rect(
            data = faixa_iqr, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = cores_app$destaque, alpha = 0.12
          ) +
          ggplot2::geom_dotplot(
            binwidth = max(diff(range(x)) / 28, 1e-3),
            fill = cores_app$destaque, colour = pal$fundo, dotsize = 0.85
          )
        if (tem_extremo()) {
          grafico <- grafico +
            ggplot2::geom_point(
              data = d[d$extremo, , drop = FALSE], ggplot2::aes(x = x, y = 0.05),
              shape = 21, size = 5, stroke = 1.3, colour = cores_app$referencia, fill = NA
            )
        }
        rotulo_y <- tr("Densidade de pontos", "Dot density")
      } else {
        quebras <- pretty(range(x), n = 18)
        grafico <- ggplot2::ggplot(data.frame(x = x), ggplot2::aes(x = x)) +
          ggplot2::geom_rect(
            data = faixa_iqr, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = cores_app$destaque, alpha = 0.12
          ) +
          ggplot2::geom_histogram(
            ggplot2::aes(y = ggplot2::after_stat(density)),
            breaks = quebras, fill = cores_app$destaque, colour = pal$fundo, alpha = 0.8
          )
        rotulo_y <- tr("Densidade", "Density")
      }

      grafico +
        ggplot2::geom_vline(xintercept = qs[c(1, 3)], colour = cores_app$destaque, linetype = "dotted", linewidth = 0.9) +
        ggplot2::geom_vline(xintercept = md, colour = pal$linha, linewidth = 1.1) +
        ggplot2::geom_vline(xintercept = med, colour = cores_app$referencia, linetype = "dashed", linewidth = 1.1) +
        ggplot2::geom_vline(xintercept = qp, colour = "#2fb380", linetype = "dotdash", linewidth = 0.95) +
        ggplot2::scale_x_continuous(labels = rotulos_eixo()) +
        ggplot2::scale_y_continuous(labels = if (length(x) <= 40) NULL else rotulos_eixo()) +
        ggplot2::labs(
          title = tr("Centro e cortes da distribuição", "Center and cuts of the distribution"),
          subtitle = sprintf(
            tr("%s | média = %s | mediana = %s", "%s | mean = %s | median = %s"),
            ex$rotulo, fmt_num(med, ex$digitos), fmt_num(md, ex$digitos)
          ),
          x = sprintf("%s (%s)", tr("Valor", "Value"), ex$unidade),
          y = rotulo_y,
          caption = sprintf(
            tr(
              "Contínua: mediana | Tracejada vermelha: média | Faixa azul: Q1 a Q3 | Verde: q(%s)",
              "Solid: median | Red dashed: mean | Blue band: Q1 to Q3 | Green: q(%s)"
            ),
            fmt_num(input$ordem_p, 2)
          )
        ) +
        tema_grafico(modo())
    })
  })
}
