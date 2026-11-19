# =============================================================================
# Script:  mod_quali_quanti.R (módulo: variável qualitativa × quantitativa)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-19
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/apps/bidimensional/app.R)
# Saída:
#   - funções mod_quali_quanti_ui() e mod_quali_quanti_server()
# =============================================================================

texto_quali_quanti <- function() {
  tr(
    shiny::withMathJax(
      shiny::p(
        "Quando uma variável é qualitativa e a outra quantitativa, a análise compara a distribuição da quantitativa",
        "entre os grupos definidos pela qualitativa. Agresti e Finlay sugerem comparar médias e medianas e pôr os box",
        "plots lado a lado; Morettin e Bussab fazem o mesmo, com as medidas-resumo de cada grupo."
      ),
      shiny::p(
        "Para resumir a associação num número, Morettin e Bussab partem da variância. Se os grupos explicam boa parte",
        "da variação, a variância dentro de cada grupo é bem menor que a variância total. A variância média dos grupos,",
        "ponderada pelo tamanho de cada um, é \\(\\overline{var}(S) = \\sum n_i\\, var_i(S) / \\sum n_i\\), e o grau de",
        "associação é \\(R^2 = 1 - \\overline{var}(S) / var(S)\\). Ele vale 0 quando os grupos não reduzem nada da",
        "variância e se aproxima de 1 quando quase toda a variação está entre os grupos, e não dentro deles. Em outras",
        "palavras, \\(R^2\\) é a fração da variância total que se explica pela diferença entre os grupos."
      ),
      shiny::p(
        shiny::strong("Experimente:"),
        "com escolaridade e salário, leve a diferença entre os grupos de 0 ao máximo. As caixas se separam, as",
        "variâncias dentro dos grupos ficam abaixo da total e o \\(R^2\\) sobe. Compare com região e tempo de",
        "deslocamento, em que as diferenças são pequenas e o \\(R^2\\) fica perto de zero mesmo no máximo."
      )
    ),
    shiny::withMathJax(
      shiny::p(
        "When one variable is categorical and the other quantitative, the analysis compares the distribution of the",
        "quantitative variable across the groups defined by the categorical one. Agresti and Finlay suggest comparing",
        "means and medians and placing box plots side by side; Morettin and Bussab do the same, with summary measures",
        "for each group."
      ),
      shiny::p(
        "To sum up the association in one number, Morettin and Bussab start from the variance. If the groups account",
        "for much of the variation, the variance within each group is much smaller than the total variance. The mean",
        "within-group variance, weighted by group size, is \\(\\overline{var}(S) = \\sum n_i\\, var_i(S) / \\sum n_i\\), and",
        "the degree of association is \\(R^2 = 1 - \\overline{var}(S) / var(S)\\). It is 0 when the groups remove none of",
        "the variance and gets close to 1 when almost all the variation lies between groups rather than within them. In",
        "other words, \\(R^2\\) is the share of the total variance explained by differences between groups."
      ),
      shiny::p(
        shiny::strong("Try it:"),
        "with education and salary, move the difference between groups from 0 to the maximum. The boxes pull apart,",
        "the within-group variances fall below the total and \\(R^2\\) rises. Compare with region and commute time,",
        "where the differences are small and \\(R^2\\) stays near zero even at the maximum."
      )
    )
  )
}

cuidados_quali_quanti <- function() {
  tr(
    c(
      "Médias parecidas" = "não garantem distribuições parecidas. Olhe também a dispersão e a forma de cada caixa.",
      "Grupos pequenos" = "têm médias e variâncias instáveis. Confira o n de cada grupo antes de comparar.",
      "Distribuições assimétricas" = paste(
        "como a de salários, afastam média e mediana. Nesse caso, compare as medianas e leia o R² com cautela,",
        "porque poucos valores altos pesam muito na variância."
      ),
      "Divisor da variância" = paste(
        "esta aba divide por n, como Morettin e Bussab no Cap. 4. Com esse divisor, o R² é exatamente a fração",
        "da soma de quadrados entre grupos, a mesma da análise de variância (Cap. 15)."
      ),
      "Associação não é causa" = paste(
        "a diferença de salário entre níveis de escolaridade também carrega idade, região e origem familiar,",
        "entre outras variáveis."
      )
    ),
    c(
      "Similar means" = "do not guarantee similar distributions. Also look at the spread and shape of each box.",
      "Small groups" = "have unstable means and variances. Check the n of each group before comparing.",
      "Skewed distributions" = paste(
        "such as salaries, pull the mean and median apart. In that case, compare medians and read R² with care,",
        "because a few high values weigh heavily on the variance."
      ),
      "Variance divisor" = paste(
        "this tab divides by n, as Morettin and Bussab do in ch. 4. With that divisor, R² is exactly the share of",
        "the between-group sum of squares, the same as in analysis of variance (ch. 15)."
      ),
      "Association is not causation" = paste(
        "the salary gap between education levels also carries age, region and family background, among other",
        "variables."
      )
    )
  )
}

mod_quali_quanti_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      seletor_exemplo(ns("exemplo"), exemplos_quali_quanti),
      slider_amostra(ns("n"), min = 30, max = 3000, value = 300),
      shiny::sliderInput(ns("efeito"), tr("Diferença entre os grupos na população", "Difference between groups in the population"), min = 0, max = 1, value = 0.6, step = 0.05),
      shiny::helpText(tr("0: todos os grupos com a mesma distribuição.", "0: every group has the same distribution.")),
      shiny::checkboxInput(ns("pontos"), tr("Mostrar os dados (pontos)", "Show the data (points)"), value = FALSE),
      botao_sortear(ns("sortear"))
    ),
    cabecalho_par("quali_quanti"),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(title = tr("Variância total, var(S)", "Total variance, var(S)"), value = shiny::textOutput(ns("var")), shiny::textOutput(ns("var_desc")), theme = "info"),
      bslib::value_box(title = tr("Variância média dos grupos", "Mean within-group variance"), value = shiny::textOutput(ns("var_media")), shiny::textOutput(ns("var_media_desc")), theme = "info"),
      bslib::value_box(title = "R²", value = shiny::textOutput(ns("r2")), shiny::textOutput(ns("r2_desc")), theme = "primary")
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico"), height = "440px"))),
      bslib::card(bslib::card_header(tr("Medidas-resumo por grupo", "Summary measures by group")), bslib::card_body(shiny::uiOutput(ns("tabela"))))
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(bslib::card_header(rotulo_entenda()), bslib::card_body(texto_quali_quanti())),
      cartao_cuidados(tr("Cuidados", "Watch out"), cuidados_quali_quanti())
    )
  )
}

mod_quali_quanti_server <- function(id, modo = shiny::reactive("light")) {
  shiny::moduleServer(id, function(input, output, session) {

    exemplo <- shiny::reactive(exemplos_quali_quanti[[input$exemplo]])

    dados <- shiny::reactive({
      input$sortear
      gerar_quali_quanti(exemplo(), input$efeito, input$n)
    })

    resumo <- shiny::reactive(resumo_grupos(dados()))
    un2 <- shiny::reactive(if (identical(exemplo()$un_y, "R$")) "(R$)²" else paste0(exemplo()$un_y, "²"))

    output$var <- shiny::renderText(fmt_num(resumo()$var, 1))
    output$var_desc <- shiny::renderText(sprintf(tr("Todos juntos, em %s", "All together, in %s"), un2()))
    output$var_media <- shiny::renderText(fmt_num(resumo()$var_media, 1))
    output$var_media_desc <- shiny::renderText(sprintf(
      tr("%s da variância total", "%s of the total variance"), fmt_pct(resumo()$var_media / resumo()$var)
    ))
    output$r2 <- shiny::renderText(fmt_num(resumo()$r2, 3))
    output$r2_desc <- shiny::renderText(sprintf(
      tr("%s da variância está entre os grupos", "%s of the variance lies between groups"), fmt_pct(resumo()$r2)
    ))

    output$tabela <- shiny::renderUI({
      r <- resumo()
      ex <- exemplo()
      g <- r$por_grupo
      v <- \(x) fmt_num(x, ex$dig_y)
      tabela <- data.frame(
        c(g$grupo, "Total"),
        fmt_num(c(g$n, r$n), 0),
        v(c(g$media, r$media)),
        v(c(g$mediana, r$mediana)),
        fmt_num(c(g$var, r$var), 1)
      )
      names(tabela) <- c(ex$nome_grupo, "n", tr(c("Média", "Mediana", "Variância"), c("Mean", "Median", "Variance")))
      shiny::tagList(
        tabela_html(tabela, classes = c("", rep("num", 4))),
        shiny::p(class = "small text-muted mt-2 mb-0", sprintf(
          tr(
            paste(
              "%s em %s. Variância com divisor n. A variância média dos grupos (ponderada por n) é %s,",
              "e R² = 1 − %s / %s = %s."
            ),
            paste(
              "%s in %s. Variance with divisor n. The mean within-group variance (weighted by n) is %s,",
              "and R² = 1 − %s / %s = %s."
            )
          ),
          ex$nome_y, ex$un_y, fmt_num(r$var_media, 1), fmt_num(r$var_media, 1), fmt_num(r$var, 1), fmt_num(r$r2, 3)
        ))
      )
    })

    output$grafico <- shiny::renderPlot({
      ex <- exemplo()
      pal <- paleta_modo(modo())
      d <- dados()
      r <- resumo()
      medias <- data.frame(grupo = factor(r$por_grupo$grupo, levels = levels(d$grupo)), media = r$por_grupo$media)

      grafico <- ggplot2::ggplot(d, ggplot2::aes(x = y, y = grupo)) +
        ggplot2::geom_vline(xintercept = r$media, colour = pal$secundario, linetype = "dashed")

      if (isTRUE(input$pontos)) {
        grafico <- grafico +
          ggplot2::geom_jitter(height = 0.18, width = 0, colour = cores_app$destaque, alpha = if (nrow(d) > 800) 0.15 else 0.3, size = 1.5)
      }

      grafico +
        ggplot2::geom_boxplot(
          width = 0.55, fill = pal$neutro, colour = pal$texto, alpha = if (isTRUE(input$pontos)) 0.5 else 1,
          outlier.colour = cores_app$referencia, outlier.alpha = 0.6,
          outlier.shape = if (isTRUE(input$pontos)) NA else 19
        ) +
        ggplot2::geom_point(data = medias, ggplot2::aes(x = media, y = grupo), shape = 23, size = 4, fill = cores_app$referencia, colour = pal$fundo) +
        ggplot2::scale_y_discrete(limits = rev(levels(d$grupo))) +
        ggplot2::scale_x_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = sprintf(tr("%s por %s", "%s by %s"), ex$nome_y, tolower(ex$nome_grupo)),
          subtitle = sprintf("%s | R² = %s", ex$rotulo, fmt_num(r$r2, 3)),
          x = sprintf("%s (%s)", ex$nome_y, ex$un_y), y = NULL,
          caption = sprintf(
            tr(
              "Dados simulados, n = %s | Losango: média do grupo | Tracejada: média geral\nPontos vermelhos: valores atípicos, a mais de 1,5 × d_q da caixa",
              "Simulated data, n = %s | Diamond: group mean | Dashed: overall mean\nRed dots: potential outliers, more than 1.5 × IQR beyond the box"
            ),
            fmt_num(r$n, 0)
          )
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_text(size = 13, colour = pal$texto))
    })
  })
}
