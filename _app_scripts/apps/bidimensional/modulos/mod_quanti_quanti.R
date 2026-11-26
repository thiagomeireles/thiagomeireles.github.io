# =============================================================================
# Script:  mod_quanti_quanti.R (módulo: duas variáveis quantitativas)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-19
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/apps/bidimensional/app.R)
# Saída:
#   - funções mod_quanti_quanti_ui() e mod_quanti_quanti_server()
# =============================================================================

texto_quanti_quanti <- function() {
  tr(
    shiny::withMathJax(
      shiny::p(
        "Com duas variáveis quantitativas, o primeiro passo é sempre o gráfico de dispersão, com um ponto por",
        "observação. Agresti e Finlay põem a variável resposta no eixo vertical e a explicativa no horizontal. É no",
        "gráfico que aparece a forma da relação (reta, curva ou nenhuma), antes de qualquer número."
      ),
      shiny::p(
        "As linhas das médias dividem o gráfico em quatro quadrantes. Nos quadrantes azuis, \\(x\\) e \\(y\\) estão",
        "do mesmo lado das suas médias, e o produto \\((x_i - \\bar{x})(y_i - \\bar{y})\\) é positivo; nos vermelhos,",
        "o produto é negativo. A covariância é a média desses produtos,",
        "\\(\\text{cov}(X, Y) = \\sum (x_i - \\bar{x})(y_i - \\bar{y}) / n\\), em Morettin e Bussab. Seu sinal diz se a",
        "relação é crescente ou decrescente, mas o valor depende das unidades. A correlação resolve isso dividindo",
        "pelos dois desvios padrão: \\(r = \\text{cov}(X, Y) / (dp(X)\\, dp(Y))\\), sempre entre −1 e 1."
      ),
      shiny::p(
        "A correlação mede só a parte linear da relação. Uma relação em U pode ser forte e, ainda assim, ter correlação",
        "perto de zero, uma vez que os pontos da esquerda e os da direita se compensam. A reta de regressão,",
        "\\(\\hat{y} = a + b x\\), resume a tendência linear e serve para prever \\(y\\) a partir de \\(x\\); sua inclinação",
        "é \\(b = r\\, dp(Y) / dp(X)\\)."
      ),
      shiny::p(
        shiny::strong("Experimente:"),
        "escolha temperatura e consumo de energia. A relação é clara no gráfico, mas a correlação fica perto de zero.",
        "Depois volte para altura e peso, inclua o ponto atípico com uma amostra pequena e veja quanto um único ponto",
        "muda a correlação e a reta."
      )
    ),
    shiny::withMathJax(
      shiny::p(
        "With two quantitative variables, the first step is always the scatterplot, with one point per observation.",
        "Agresti and Finlay put the response variable on the vertical axis and the explanatory one on the horizontal",
        "axis. The chart is where the form of the relationship (a line, a curve or nothing) shows up, before any number."
      ),
      shiny::p(
        "The lines at the two means split the chart into four quadrants. In the blue quadrants, \\(x\\) and \\(y\\) are",
        "on the same side of their means, and the product \\((x_i - \\bar{x})(y_i - \\bar{y})\\) is positive; in the red",
        "ones, the product is negative. The covariance is the mean of these products,",
        "\\(\\text{cov}(X, Y) = \\sum (x_i - \\bar{x})(y_i - \\bar{y}) / n\\) in Morettin and Bussab (Agresti and Finlay",
        "divide by \\(n - 1\\)). Its sign tells whether the relationship goes up or down, but its size depends on the",
        "units. The correlation fixes that by dividing by both standard deviations:",
        "\\(r = \\text{cov}(X, Y) / (\\text{sd}(X)\\, \\text{sd}(Y))\\), which yields the exact same number under divisor",
        "\\(n\\) or \\(n - 1\\) and always lies between −1 and 1."
      ),
      shiny::p(
        "Correlation only measures the linear part of a relationship. A U-shaped relationship can be strong and still",
        "have a correlation near zero, since the points on the left and on the right cancel each other out. The",
        "regression line, \\(\\hat{y} = a + b x\\), summarizes the linear trend and predicts \\(y\\) from \\(x\\); its slope",
        "is \\(b = r\\, \\text{sd}(Y) / \\text{sd}(X)\\)."
      ),
      shiny::p(
        shiny::strong("Try it:"),
        "choose temperature and energy use. The relationship is clear in the chart, but the correlation stays near zero.",
        "Then go back to height and weight, add the outlier with a small sample and see how much a single point moves",
        "the correlation and the line."
      )
    )
  )
}

cuidados_quanti_quanti <- function() {
  tr(
    c(
      "Correlação perto de zero" = "não quer dizer ausência de relação. A relação pode ser curva, como a do exemplo em U.",
      "Valores atípicos" = "um único ponto longe dos demais pode mudar bastante a correlação e a reta, sobretudo em amostras pequenas.",
      "Covariância" = paste(
        "depende das unidades: medir a altura em metros em vez de centímetros divide a covariância por 100.",
        "Para falar da força da relação, use a correlação."
      ),
      "Associação não é causa" = paste(
        "vender mais quando o preço cai é plausível, mas a correlação não prova o mecanismo. Uma terceira variável",
        "pode mover as duas."
      ),
      "Prever fora dos dados" = "a reta descreve a faixa observada de x. Estendê-la muito além dela é extrapolação."
    ),
    c(
      "Correlation near zero" = "does not mean no relationship. The relationship may be curved, as in the U-shaped example.",
      "Outliers" = "a single point far from the rest can change the correlation and the line a lot, especially in small samples.",
      "Covariance" = paste(
        "depends on the units: measuring height in meters instead of centimeters divides the covariance by 100.",
        "To talk about the strength of the relationship, use the correlation."
      ),
      "Association is not causation" = paste(
        "selling more when the price drops is plausible, but the correlation does not prove the mechanism. A third",
        "variable may drive both."
      ),
      "Predicting beyond the data" = "the line describes the observed range of x. Stretching it far beyond that range is extrapolation."
    )
  )
}

mod_quanti_quanti_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      seletor_exemplo(ns("exemplo"), exemplos_quanti_quanti),
      slider_amostra(ns("n"), min = 10, max = 2000, value = 100),
      shiny::sliderInput(ns("forca"), tr("Correlação na população (ρ)", "Correlation in the population (ρ)"), min = -1, max = 1, value = 0.7, step = 0.05),
      shiny::checkboxInput(ns("quadrantes"), tr("Mostrar médias e quadrantes", "Show means and quadrants"), value = TRUE),
      shiny::checkboxInput(ns("reta"), tr("Mostrar reta de regressão", "Show regression line"), value = FALSE),
      shiny::checkboxInput(ns("extremo"), tr("Incluir um ponto atípico", "Add an outlier"), value = FALSE),
      botao_sortear(ns("sortear"))
    ),
    cabecalho_par("quanti_quanti"),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(title = tr("Correlação (r)", "Correlation (r)"), value = shiny::textOutput(ns("r")), shiny::textOutput(ns("r_desc")), theme = "primary"),
      bslib::value_box(title = tr("Covariância", "Covariance"), value = shiny::textOutput(ns("cov")), shiny::textOutput(ns("cov_desc")), theme = "info"),
      bslib::value_box(title = tr("Reta de regressão", "Regression line"), value = shiny::textOutput(ns("reta_eq")), shiny::textOutput(ns("reta_desc")), theme = "info")
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico"), height = "460px"))),
      bslib::card(bslib::card_header(tr("Medidas", "Measures")), bslib::card_body(shiny::uiOutput(ns("medidas"))))
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(bslib::card_header(rotulo_entenda()), bslib::card_body(texto_quanti_quanti())),
      cartao_cuidados(tr("Cuidados", "Watch out"), cuidados_quanti_quanti())
    )
  )
}

mod_quanti_quanti_server <- function(id, modo = shiny::reactive("light")) {
  shiny::moduleServer(id, function(input, output, session) {

    exemplo <- shiny::reactive(exemplos_quanti_quanti[[input$exemplo]])

    # O controle de força muda de sentido conforme a forma da relação
    shiny::observeEvent(input$exemplo, {
      ex <- exemplo()
      if (identical(ex$forma, "u")) {
        shiny::updateSliderInput(session, "forca", label = tr("Força da relação em U", "Strength of the U-shaped relationship"), min = 0, max = 1, value = ex$forca)
      } else {
        shiny::updateSliderInput(session, "forca", label = tr("Correlação na população (ρ)", "Correlation in the population (ρ)"), min = -1, max = 1, value = ex$forca)
      }
    })

    # Com a forma em U, a força fica em [0, 1] mesmo antes do slider se atualizar
    forca <- shiny::reactive({
      if (identical(exemplo()$forma, "u")) min(max(input$forca, 0), 1) else input$forca
    })

    base <- shiny::reactive({
      input$sortear
      gerar_quanti_quanti(exemplo(), forca(), input$n)
    })

    dados <- shiny::reactive({
      if (isTRUE(input$extremo)) rbind(base(), ponto_atipico(exemplo(), forca())) else base()
    })

    resumo <- shiny::reactive(resumo_quanti_quanti(dados()$x, dados()$y))
    resumo_sem <- shiny::reactive(resumo_quanti_quanti(base()$x, base()$y))

    output$r <- shiny::renderText(fmt_num(resumo()$r, 3))
    output$r_desc <- shiny::renderText({
      if (isTRUE(input$extremo)) {
        paste(tr("Sem o ponto atípico:", "Without the outlier:"), fmt_num(resumo_sem()$r, 3))
      } else {
        r <- resumo()$r
        sentido <- if (abs(r) < 0.1) "" else if (r < 0) tr("e negativa", "and negative") else tr("e positiva", "and positive")
        trimws(paste(tr("Relação linear", "Linear relationship:"), leitura_forca(r), sentido))
      }
    })
    output$cov <- shiny::renderText(fmt_num(resumo()$cov, 2))
    output$cov_desc <- shiny::renderText(sprintf("%s × %s", exemplo()$un_x, exemplo()$un_y))
    output$reta_eq <- shiny::renderText({
      r <- resumo()
      sprintf("ŷ = %s %s %s x", fmt_num(r$a, 2), if (r$b < 0) "−" else "+", fmt_num(abs(r$b), 3))
    })
    output$reta_desc <- shiny::renderText(sprintf(
      tr("Cada 1 %s a mais em x: %s %s em y", "Each extra 1 %s in x: %s %s in y"),
      exemplo()$un_x, paste0(if (resumo()$b < 0) "−" else "+", fmt_num(abs(resumo()$b), 3)), exemplo()$un_y
    ))

    output$medidas <- shiny::renderUI({
      r <- resumo()
      ex <- exemplo()
      vx <- \(v) fmt_valor(v, ex$un_x, ex$dig_x)
      vy <- \(v) fmt_valor(v, ex$un_y, ex$dig_y)
      tabela <- linha_estatistica(
        tr(
          c(
            "Tamanho da amostra (n)", "Médias (x̄ e ȳ)", "Desvios padrão (dp(X) e dp(Y))",
            "Covariância, divisor n", "Covariância, divisor n − 1", "Correlação (r)", "r²", "Reta: intercepto (a) e inclinação (b)"
          ),
          c(
            "Sample size (n)", "Means (x̄ and ȳ)", "Standard deviations (s_X and s_Y)",
            "Covariance, divisor n", "Covariance, divisor n − 1", "Correlation (r)", "r²", "Line: intercept (a) and slope (b)"
          )
        ),
        c(
          fmt_num(r$n, 0), juntar_valores(vx(r$media_x), vy(r$media_y)), juntar_valores(vx(r$dp_x), vy(r$dp_y)),
          fmt_num(r$cov, 2), fmt_num(r$cov_n1, 2), fmt_num(r$r, 3), fmt_num(r$r^2, 3),
          juntar_valores(fmt_num(r$a, 2), fmt_num(r$b, 3))
        ),
        tr(
          c(
            "Número de pares (x, y)",
            sprintf("%s e %s", tolower(ex$nome_x), tolower(ex$nome_y)),
            "Divisor n, como no Cap. 4 de Morettin e Bussab",
            "Morettin e Bussab: média dos produtos (x − x̄)(y − ȳ); positiva quando os pontos ficam nos quadrantes azuis",
            "Agresti e Finlay dividem por n − 1; a diferença some com n grande",
            "Covariância dividida pelos dois desvios padrão; mesma conta com n ou n − 1",
            "Fração da variância de y que a reta reproduz",
            "b = cov / var(X), e a = ȳ − b x̄; a reta passa pelo ponto (x̄, ȳ)"
          ),
          c(
            "Number of (x, y) pairs",
            sprintf("%s and %s", tolower(ex$nome_x), tolower(ex$nome_y)),
            "Divisor n, as in Morettin and Bussab, ch. 4",
            "Morettin and Bussab: mean of the products (x − x̄)(y − ȳ); positive when points sit in the blue quadrants",
            "Agresti and Finlay divide by n − 1; the difference fades as n grows",
            "Covariance divided by both standard deviations; same result with n or n − 1",
            "Share of the variance of y reproduced by the line",
            "b = cov / var(X), and a = ȳ − b x̄; the line goes through the point (x̄, ȳ)"
          )
        )
      )
      names(tabela) <- nomes_estatisticas()
      tabela_html(tabela, classes = c("", "num", "leitura"))
    })

    output$grafico <- shiny::renderPlot({
      ex <- exemplo()
      pal <- paleta_modo(modo())
      r <- resumo()
      d <- dados()
      d$quadrante <- ifelse((d$x - r$media_x) * (d$y - r$media_y) >= 0, "positivo", "negativo")
      d$atipico <- isTRUE(input$extremo) & seq_len(nrow(d)) == nrow(d)
      transparencia <- if (nrow(d) > 500) 0.45 else 0.75

      grafico <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y))

      if (isTRUE(input$quadrantes)) {
        grafico <- grafico +
          ggplot2::geom_vline(xintercept = r$media_x, colour = pal$secundario, linetype = "dashed") +
          ggplot2::geom_hline(yintercept = r$media_y, colour = pal$secundario, linetype = "dashed") +
          ggplot2::geom_point(ggplot2::aes(colour = quadrante), size = 2.6, alpha = transparencia) +
          ggplot2::scale_colour_manual(values = c(positivo = cores_app$destaque, negativo = cores_app$referencia))
      } else {
        grafico <- grafico + ggplot2::geom_point(colour = cores_app$destaque, size = 2.6, alpha = transparencia)
      }

      if (isTRUE(input$extremo)) {
        grafico <- grafico +
          ggplot2::geom_point(data = d[d$atipico, ], shape = 21, size = 6, stroke = 1.3, colour = pal$linha, fill = NA)
      }

      if (isTRUE(input$reta)) {
        grafico <- grafico +
          ggplot2::geom_abline(intercept = r$a, slope = r$b, colour = pal$linha, linewidth = 1.1)
        if (isTRUE(input$extremo)) {
          s <- resumo_sem()
          grafico <- grafico +
            ggplot2::geom_abline(intercept = s$a, slope = s$b, colour = pal$secundario, linewidth = 0.9, linetype = "dotted")
        }
      }

      # Duas linhas no máximo: a terceira seria cortada na altura do cartão
      reta <- c(
        if (isTRUE(input$reta)) tr("Contínua: reta de regressão", "Solid: regression line"),
        if (isTRUE(input$reta) && isTRUE(input$extremo)) tr("pontilhada: sem o ponto atípico (circulado)", "dotted: without the outlier (circled)")
      )
      legenda <- c(
        if (isTRUE(input$quadrantes)) tr(
          "Tracejadas: médias | Azul: (x − x̄)(y − ȳ) positivo; vermelho: negativo",
          "Dashed: means | Blue: (x − x̄)(y − ȳ) positive; red: negative"
        ),
        if (length(reta) > 0) paste(reta, collapse = "; ")
      )

      grafico +
        ggplot2::scale_x_continuous(labels = rotulos_eixo()) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = tr("Gráfico de dispersão", "Scatterplot"),
          subtitle = sprintf("%s | r = %s", ex$rotulo, fmt_num(r$r, 3)),
          x = sprintf("%s (%s)", ex$nome_x, ex$un_x),
          y = sprintf("%s (%s)", ex$nome_y, ex$un_y),
          caption = paste(sprintf(tr("Dados simulados, n = %s", "Simulated data, n = %s"), fmt_num(r$n, 0)), paste(legenda, collapse = "\n"), sep = if (length(legenda) > 0) " | " else "")
        ) +
        tema_grafico(modo())
    })
  })
}

# Dois valores na mesma célula da tabela: "170,1 cm | 70,2 kg"
juntar_valores <- function(a, b) paste(a, b, sep = " | ")
