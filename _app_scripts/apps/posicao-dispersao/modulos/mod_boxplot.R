# =============================================================================
# Script:  mod_boxplot.R (módulo: cinco números, box plot nas convenções dos
#          livros e gráfico de simetria de Morettin e Bussab)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-25
#
# Entrada:
#   - objetos e funções de _app_scripts/apps/posicao-dispersao/modulos/dados.R
# Saída:
#   - funções mod_boxplot_ui() e mod_boxplot_server()
# =============================================================================

texto_boxplot <- function() {
  tr(
    shiny::withMathJax(
      shiny::p(
        "O box plot traduz o esquema dos cinco números — mínimo, primeiro quartil (\\(q_1\\)), mediana (\\(md\\)),",
        "terceiro quartil (\\(q_3\\)) e máximo — num desenho compacto de posição, dispersão, assimetria e caudas.",
        "A caixa cobre os 50% centrais dos dados, cuja largura é a distância interquartil",
        "\\(d_q = q_3 - q_1\\) (`IQR` em Agresti e Finlay). Como \\(q_1\\) e \\(q_3\\) ignoram os 25% mais baixos e os",
        "25% mais altos, a caixa permanece estável mesmo na presença de valores extremos."
      ),
      shiny::p(
        "Para decidir até onde vão as hastes (bigodes), calculam-se os limites internos — ou cercas —",
        "\\(LI = q_1 - 1{,}5\\,d_q\\) e \\(LS = q_3 + 1{,}5\\,d_q\\). Na distribuição normal padrão, esse intervalo",
        "vai de \\(-2{,}698\\) a \\(+2{,}698\\) e concentra 99,3% da probabilidade, razão da constante 1,5 proposta por",
        "Tukey (Morettin e Bussab, seção 3.4). Atenção a um detalhe frequente: cada haste termina na observação mais",
        "extrema que ainda está dentro das cercas, e não no próprio valor de \\(LI\\) ou \\(LS\\). Os valores além das",
        "cercas são marcados individualmente — chamados de \"pontos exteriores\" por Morettin e Bussab e de \"valores",
        "atípicos potenciais\" (*potential outliers*) por Agresti e Finlay."
      ),
      shiny::p(
        "A assimetria aparece no box plot quando \\(q_3 - md\\) difere bastante de \\(md - q_1\\) e uma haste é bem",
        "mais longa que a outra. Morettin e Bussab (seção 3.5) refinam essa leitura com o gráfico de simetria:",
        "pareando os dados ordenados das pontas para o centro, comparam-se as distâncias inferiores",
        "\\(u_i = md - x_{(i)}\\) com as superiores \\(v_i = x_{(n+1-i)} - md\\). Numa distribuição perfeitamente",
        "simétrica todos os pares caem sobre a bissetriz \\(v = u\\); numa distribuição assimétrica à direita, \\(v_i\\)",
        "supera \\(u_i\\) e a nuvem sobe acima da reta."
      ),
      shiny::p(
        shiny::strong("Experimente:"),
        "nos salários da Cia. MB e na renda simulada, veja como o gráfico de simetria se descola para cima da reta",
        "\\(v = u\\). Em seguida, alterne a convenção dos quartis na amostra pequena (\\(n = 9\\)) para notar a caixa",
        "mudar de largura conforme a definição escolhida."
      )
    ),
    shiny::withMathJax(
      shiny::p(
        "A box plot turns the five-number summary — minimum, first quartile (\\(Q_1\\)), median (\\(md\\)), third",
        "quartile (\\(Q_3\\)) and maximum — into a compact picture of center, spread, skewness and tails. The box spans",
        "the middle 50% of the data, whose width is the interquartile range \\(\\text{IQR} = Q_3 - Q_1\\) (\\(d_q\\)",
        "in Morettin and Bussab). Because \\(Q_1\\) and \\(Q_3\\) ignore the bottom 25% and top 25%, the box stays",
        "stable even when extreme values are present."
      ),
      shiny::p(
        "To set the whiskers, inner fences are placed at \\(Q_1 - 1.5\\,\\text{IQR}\\) and \\(Q_3 + 1.5\\,\\text{IQR}\\).",
        "Under a standard normal distribution, that interval runs from \\(-2.698\\) to \\(+2.698\\) and holds 99.3% of",
        "the probability, which motivates Tukey's 1.5 constant (Morettin and Bussab, section 3.4). A common pitfall",
        "is worth noting: each whisker stops at the most extreme observed value still inside the fences, not at the",
        "fence itself. Observations beyond the fences are plotted individually — called \"potential outliers\" by",
        "Agresti and Finlay and \"outside points\" (*pontos exteriores*) by Morettin and Bussab."
      ),
      shiny::p(
        "Skewness shows up in the box plot when \\(Q_3 - md\\) differs markedly from \\(md - Q_1\\) and one whisker",
        "stretches farther than the other. Morettin and Bussab (section 3.5) sharpen this check with the symmetry",
        "plot: pairing sorted values from the ends inward, they plot the lower distances \\(u_i = md - y_{(i)}\\)",
        "against the upper distances \\(v_i = y_{(n+1-i)} - md\\). Under perfect symmetry every pair sits on the",
        "bisector \\(v = u\\); under right skewness, \\(v_i > u_i\\) and the points climb above the line."
      ),
      shiny::p(
        shiny::strong("Try it:"),
        "on the MB Co. salaries and simulated income, watch the symmetry plot peel upward away from \\(v = u\\). Then",
        "switch the quartile convention on the small sample (\\(n = 9\\)) to see the box width change with the",
        "definition."
      )
    )
  )
}

cuidados_boxplot <- function() {
  tr(
    c(
      "Ponto exterior não é erro" = paste(
        "Morettin e Bussab distinguem ponto exterior (valor legítimo de uma cauda longa, como São Paulo entre os",
        "municípios brasileiros) de erro de medida ou digitação. Nunca exclua um ponto só porque passou de 1,5 × d_q."
      ),
      "Distribuições bimodais" = paste(
        "duas distribuições com um pico e com dois picos podem ter os mesmos cinco números e gerar o mesmo box plot.",
        "Sempre combine o box plot com o histograma ou com os pontos."
      ),
      "Padrão do geom_boxplot()" = paste(
        "no ggplot2, geom_boxplot() usa quantile(type = 7). Neste aplicativo a caixa é desenhada exatamente na",
        "convenção escolhida no painel lateral."
      )
    ),
    c(
      "Outlier does not mean error" = paste(
        "both textbooks distinguish genuine observations from a long tail (such as a megacity among municipalities)",
        "from recording errors. Never drop a point solely because it lies beyond 1.5 × IQR."
      ),
      "Bimodal distributions" = paste(
        "a unimodal and a bimodal distribution can share the exact same five-number summary and look identical in a",
        "box plot. Pair the box plot with a histogram or jittered points."
      ),
      "ggplot2's geom_boxplot() default" = paste(
        "by default, geom_boxplot() uses quantile(type = 7). Here the box is drawn directly from the convention",
        "selected in the sidebar."
      )
    )
  )
}

mod_boxplot_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::selectInput(
        ns("exemplo"), tr("Conjunto de dados", "Data set"),
        choices = stats::setNames(names(exemplos_dados), purrr::map_chr(exemplos_dados, "rotulo")),
        selected = "salarios"
      ),
      shiny::uiOutput(ns("controle_n")),
      shiny::radioButtons(
        ns("convencao"), tr("Convenção do box plot", "Box plot convention"),
        choices = stats::setNames(
          c("bm", "af", "r7"),
          tr(
            c("Morettin e Bussab (type = 5)", "Agresti e Finlay (metades)", "R / ggplot2 padrão (type = 7)"),
            c("Morettin and Bussab (type = 5)", "Agresti and Finlay (halves)", "R / ggplot2 default (type = 7)")
          )
        ),
        selected = "bm"
      ),
      shiny::checkboxInput(ns("cercas"), tr("Mostrar cercas teóricas (q₁ − 1,5 d_q e q₃ + 1,5 d_q)", "Show theoretical fences (Q₁ − 1.5 IQR and Q₃ + 1.5 IQR)"), value = TRUE),
      shiny::checkboxInput(ns("pontos"), tr("Mostrar observações individuais", "Show individual observations"), value = TRUE),
      shiny::checkboxInput(ns("extremo"), tr("Incluir um valor extremo", "Add an extreme value"), value = FALSE),
      shiny::uiOutput(ns("controle_extremo")),
      shiny::uiOutput(ns("botao_sortear"))
    ),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(
        title = tr("Cinco números (mín | q₁ | md | q₃ | máx)", "Five-number summary (min | Q₁ | md | Q₃ | max)"),
        value = shiny::textOutput(ns("cinco")),
        shiny::textOutput(ns("cinco_desc")),
        theme = "primary"
      ),
      bslib::value_box(
        title = tr("Distância interquartil (d_q = q₃ − q₁)", "Interquartile range (IQR = Q₃ − Q₁)"),
        value = shiny::textOutput(ns("dq")),
        shiny::textOutput(ns("cercas_txt")),
        theme = "info"
      ),
      bslib::value_box(
        title = tr("Pontos exteriores / valores atípicos", "Outside points / potential outliers"),
        value = shiny::textOutput(ns("n_ext")),
        shiny::textOutput(ns("simetria_txt")),
        theme = NULL
      )
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico_box"), height = "420px"))),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico_simetria"), height = "420px")))
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
      bslib::card(
        bslib::card_header(tr("Anatomia do box plot nesta amostra", "Anatomy of the box plot in this sample")),
        bslib::card_body(shiny::uiOutput(ns("tabela")))
      ),
      cartao_cuidados(tr("Cuidados", "Watch out"), cuidados_boxplot())
    ),
    bslib::card(bslib::card_header(rotulo_entenda()), bslib::card_body(texto_boxplot()))
  )
}

mod_boxplot_server <- function(id, modo = shiny::reactive("light")) {
  shiny::moduleServer(id, function(input, output, session) {

    exemplo <- shiny::reactive(exemplos_dados[[input$exemplo]])

    output$controle_n <- shiny::renderUI({
      if (isTRUE(exemplo()$fixo)) return(NULL)
      shiny::sliderInput(
        session$ns("n"), tr("Tamanho da amostra (n)", "Sample size (n)"),
        min = 15, max = 500, value = 100, step = 5, sep = sep_milhar()
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
        value = ex$extremo, step = 1
      )
    })

    base <- shiny::reactive({
      if (!isTRUE(exemplo()$fixo)) input$sortear
      n_val <- if (isTRUE(exemplo()$fixo)) 0L else (input$n %||% 100L)
      exemplo()$gerar(n_val)
    })

    dados <- shiny::reactive({
      if (isTRUE(input$extremo) && !is.null(input$valor_extremo) && is.finite(input$valor_extremo)) {
        c(base(), input$valor_extremo)
      } else {
        base()
      }
    })

    res <- shiny::reactive(resumo_boxplot(dados(), input$convencao))

    output$cinco <- shiny::renderText({
      r <- res()
      dig <- exemplo()$digitos
      paste(fmt_num(c(r$min, r$q1, r$md, r$q3, r$max), dig), collapse = " | ")
    })
    output$cinco_desc <- shiny::renderText(sprintf("%s | n = %s", exemplo()$unidade, fmt_num(res()$n, 0)))

    output$dq <- shiny::renderText(fmt_un(res()$dq, exemplo()$un_curta, exemplo()$digitos))
    output$cercas_txt <- shiny::renderText({
      r <- res()
      dig <- exemplo()$digitos
      sprintf(tr("Cercas: [%s; %s]", "Fences: [%s, %s]"), fmt_num(r$cerca_inf, dig), fmt_num(r$cerca_sup, dig))
    })

    output$n_ext <- shiny::renderText(fmt_num(length(res()$exteriores), 0))
    output$simetria_txt <- shiny::renderText({
      r <- res()
      dig <- exemplo()$digitos
      sprintf(
        tr("md − q₁ = %s | q₃ − md = %s", "md − Q₁ = %s | Q₃ − md = %s"),
        fmt_num(r$dist_inf, dig), fmt_num(r$dist_sup, dig)
      )
    })

    output$tabela <- shiny::renderUI({
      r <- res()
      ex <- exemplo()
      dig <- ex$digitos
      ext_str <- if (length(r$exteriores) == 0) {
        tr("nenhum", "none")
      } else {
        paste(fmt_num(sort(r$exteriores), dig), collapse = tr("; ", ", "))
      }
      linhas <- data.frame(
        tr(
          c(
            "1º quartil, mediana e 3º quartil",
            "Distâncias da mediana aos quartis (md − q₁ | q₃ − md)",
            "Cercas teóricas (q₁ − 1,5 d_q | q₃ + 1,5 d_q)",
            "Extremos das hastes (observados dentro das cercas)",
            "Valores além das cercas"
          ),
          c(
            "1st quartile, median and 3rd quartile",
            "Distances from median to quartiles (md − Q₁ | Q₃ − md)",
            "Theoretical fences (Q₁ − 1.5 IQR | Q₃ + 1.5 IQR)",
            "Whisker ends (most extreme values inside fences)",
            "Values beyond the fences"
          )
        ),
        c(
          paste(fmt_num(c(r$q1, r$md, r$q3), dig), collapse = " | "),
          sprintf("%s | %s", fmt_num(r$dist_inf, dig), fmt_num(r$dist_sup, dig)),
          sprintf("%s | %s", fmt_num(r$cerca_inf, dig), fmt_num(r$cerca_sup, dig)),
          sprintf("%s | %s", fmt_num(r$bigode_inf, dig), fmt_num(r$bigode_sup, dig)),
          ext_str
        )
      )
      names(linhas) <- tr(c("Componente", "Valor"), c("Component", "Value"))
      tabela_html(linhas, classes = c("", "num"))
    })

    output$grafico_box <- shiny::renderPlot({
      pal <- paleta_modo(modo())
      ex <- exemplo()
      r <- res()
      x <- dados()
      med <- mean(x)

      caixa_df <- data.frame(
        bigode_inf = r$bigode_inf, q1 = r$q1, md = r$md,
        q3 = r$q3, bigode_sup = r$bigode_sup
      )
      hastes_df <- data.frame(
        x = c(r$bigode_inf, r$q3, r$bigode_inf, r$bigode_sup),
        xend = c(r$q1, r$bigode_sup, r$bigode_inf, r$bigode_sup),
        y = c(0, 0, -0.09, -0.09),
        yend = c(0, 0, 0.09, 0.09)
      )

      grafico <- ggplot2::ggplot() +
        ggplot2::geom_segment(
          data = hastes_df,
          ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
          colour = pal$linha, linewidth = 0.85
        ) +
        ggplot2::geom_rect(
          data = caixa_df,
          ggplot2::aes(xmin = q1, xmax = q3, ymin = -0.18, ymax = 0.18),
          fill = pal$neutro, colour = pal$linha, linewidth = 0.85
        ) +
        ggplot2::geom_segment(
          data = caixa_df,
          ggplot2::aes(x = md, xend = md, y = -0.18, yend = 0.18),
          colour = pal$linha, linewidth = 1.35
        ) +
        ggplot2::geom_point(
          data = data.frame(x = med, y = 0), ggplot2::aes(x = x, y = y),
          shape = 23, size = 4.2, fill = cores_app$destaque, colour = pal$fundo
        )

      if (isTRUE(input$cercas)) {
        grafico <- grafico +
          ggplot2::geom_vline(
            xintercept = c(r$cerca_inf, r$cerca_sup),
            colour = cores_app$referencia, linetype = "dotted", linewidth = 0.9
          )
      }

      if (length(r$exteriores) > 0) {
        grafico <- grafico +
          ggplot2::geom_point(
            data = data.frame(x = r$exteriores, y = 0),
            ggplot2::aes(x = x, y = y),
            colour = cores_app$referencia, size = 3.6
          )
      }

      if (isTRUE(input$pontos)) {
        grafico <- grafico +
          ggplot2::geom_jitter(
            data = data.frame(x = x, y = -0.34), ggplot2::aes(x = x, y = y),
            width = 0, height = 0.06, colour = cores_app$destaque,
            alpha = if (length(x) > 150) 0.3 else 0.65, size = 1.8
          )
      }

      grafico +
        ggplot2::coord_cartesian(ylim = c(if (isTRUE(input$pontos)) -0.5 else -0.3, 0.35)) +
        ggplot2::scale_x_continuous(labels = rotulos_eixo()) +
        ggplot2::scale_y_continuous(breaks = NULL) +
        ggplot2::labs(
          title = tr("Box plot na convenção selecionada", "Box plot under the selected convention"),
          subtitle = ex$rotulo,
          x = sprintf("%s (%s)", tr("Valor", "Value"), ex$unidade), y = NULL,
          caption = tr(
            "Traço na caixa: mediana | Losango azul: média | Vermelhos: além das cercas (pontilhadas)",
            "Line in box: median | Blue diamond: mean | Red dots: beyond the fences (dotted)"
          )
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
    })

    output$grafico_simetria <- shiny::renderPlot({
      pal <- paleta_modo(modo())
      ps <- pares_simetria(dados())
      teto <- max(c(ps$u, ps$v, 1e-6)) * 1.05

      ggplot2::ggplot(ps, ggplot2::aes(x = u, y = v)) +
        ggplot2::geom_abline(intercept = 0, slope = 1, colour = cores_app$referencia, linetype = "dashed", linewidth = 1) +
        ggplot2::geom_point(colour = cores_app$destaque, size = 2.8, alpha = 0.85) +
        ggplot2::coord_equal(xlim = c(0, teto), ylim = c(0, teto)) +
        ggplot2::scale_x_continuous(labels = rotulos_eixo()) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = tr("Gráfico de simetria (vᵢ × uᵢ)", "Symmetry plot (vᵢ × uᵢ)"),
          subtitle = tr("uᵢ = md − x(i) contra vᵢ = x(n+1−i) − md", "uᵢ = md − y(i) versus vᵢ = y(n+1−i) − md"),
          x = tr("Distância abaixo da mediana (uᵢ)", "Distance below the median (uᵢ)"),
          y = tr("Distância acima da mediana (vᵢ)", "Distance above the median (vᵢ)"),
          caption = tr(
            "Tracejada: v = u (simetria) | Acima da reta: cauda direita mais longa",
            "Dashed: v = u (symmetry) | Above the line: longer right tail"
          )
        ) +
        tema_grafico(modo())
    }, bg = "transparent")
  })
}
