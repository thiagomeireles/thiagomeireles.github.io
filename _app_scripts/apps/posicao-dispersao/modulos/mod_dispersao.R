# =============================================================================
# Script:  mod_dispersao.R (módulo: variância, desvio padrão, escore z, regra
#          empírica e desigualdade de Chebyshev)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-25
#
# Entrada:
#   - objetos e funções de _app_scripts/apps/posicao-dispersao/modulos/dados.R
# Saída:
#   - funções mod_dispersao_ui() e mod_dispersao_server()
# =============================================================================

texto_dispersao <- function() {
  tr(
    shiny::withMathJax(
      shiny::p(
        "Duas distribuições podem ter a mesma média e a mesma amplitude e, ainda assim, comportar-se de maneira",
        "oposta, como mostram as duas turmas do Exemplo 3.7 de Agresti e Finlay: ambas têm média 5 e vão de 0 a 10,",
        "mas uma concentra as notas no meio e a outra nas pontas. Como a soma dos desvios \\(\\sum (x_i - \\bar{x})\\)",
        "é sempre zero, medir o espalhamento em torno da média exige eliminar os sinais — seja pelo módulo, no desvio",
        "médio \\(\\text{dm} = \\frac{1}{n}\\sum |x_i - \\bar{x}|\\), seja pelo quadrado, na variância."
      ),
      shiny::p(
        "Os dois livros-base divergem no denominador da variância descritiva. Morettin e Bussab dividem por \\(n\\) no",
        "Cap. 3, \\(\\text{var}(X) = \\frac{1}{n}\\sum (x_i - \\bar{x})^2\\), reservando \\(n - 1\\) para o estimador",
        "não viesado \\(S^2\\) a partir do Cap. 10; Agresti e Finlay já usam \\(n - 1\\) desde o início,",
        "\\(s^2 = \\frac{1}{n-1}\\sum (y_i - \\bar{y})^2\\), que é o padrão de `var()` e `sd()` no R. Tirando a raiz",
        "quadrada, o desvio padrão devolve a medida para a unidade original dos dados."
      ),
      shiny::p(
        "O escore z padroniza cada observação pelo centro e pela dispersão: \\(z_i = (x_i - \\bar{x}) / s\\). O conjunto",
        "dos \\(z_i\\) passa a ter média 0, desvio padrão 1 e nenhuma unidade de medida, permitindo comparar posições",
        "relativas em escalas distintas e sinalizar valores muito afastados (Agresti e Finlay tratam \\(|z_i| > 3\\)",
        "como critério alternativo de valor atípico). Quando a distribuição tem forma aproximada de sino, a regra",
        "empírica prevê cerca de 68%, 95% e 99,7% das observações em até 1, 2 e 3 desvios padrão da média. Para",
        "distribuições muito assimétricas ou bimodais essa aproximação falha; o limite válido para qualquer conjunto de",
        "dados é a desigualdade de Chebyshev (Casella e Berger, seção 3.6), que garante ao menos \\(1 - 1/k^2\\) das",
        "observações a até \\(k\\) desvios padrão (75% para \\(k = 2\\) e 88,9% para \\(k = 3\\); na versão amostral,",
        "calculada exatamente com o mesmo divisor \\(n\\) ou \\(n - 1\\) adotado no desvio padrão)."
      ),
      shiny::p(
        shiny::strong("Experimente:"),
        "alterne entre as duas turmas do Exemplo 3.7 para ver o desvio padrão saltar de 3,35 para 5,14 sem mudar a",
        "média. Em seguida, compare altura (em sino) e renda (assimétrica) para \\(k = 1\\): na renda, a fração dentro",
        "de 1 desvio padrão passa bastante de 68%, porque a cauda direita infla o desvio padrão."
      )
    ),
    shiny::withMathJax(
      shiny::p(
        "Two distributions can share the same mean and the same range and yet behave in opposite ways, as the two",
        "classes in Agresti and Finlay's Example 3.7 illustrate: both have mean 5 and run from 0 to 10, but one",
        "clusters in the middle while the other sits at the extremes. Because the deviations \\(\\sum (y_i - \\bar{y})\\)",
        "always sum to zero, measuring spread around the mean requires removing signs — either with absolute values,",
        "in the mean absolute deviation \\(\\frac{1}{n}\\sum |y_i - \\bar{y}|\\), or by squaring, in the variance."
      ),
      shiny::p(
        "The two reference textbooks differ on the divisor of the descriptive variance. Agresti and Finlay divide by",
        "\\(n - 1\\) right away, \\(s^2 = \\frac{1}{n-1}\\sum (y_i - \\bar{y})^2\\), matching `var()` and `sd()` in R;",
        "Morettin and Bussab divide by \\(n\\) in ch. 3 and introduce \\(n - 1\\) only in ch. 10 for the unbiased",
        "estimator \\(S^2\\). Taking the square root gives the standard deviation, back in the original unit."
      ),
      shiny::p(
        "The z-score standardizes each observation by the mean and spread: \\(z_i = (y_i - \\bar{y}) / s\\). The",
        "standardized values always have mean 0, standard deviation 1 and no unit of measurement, making it possible",
        "to compare relative positions across scales and flag extreme observations (Agresti and Finlay treat",
        "\\(|z_i| > 3\\) as an alternative outlier rule). When the distribution is roughly bell-shaped, the Empirical",
        "Rule places about 68%, 95% and 99.7% of observations within 1, 2 and 3 standard deviations of the mean. For",
        "heavily skewed or bimodal data those percentages break down; what holds for any data set is Chebyshev's",
        "inequality (Casella and Berger, section 3.6), which guarantees at least \\(1 - 1/k^2\\) within \\(k\\)",
        "standard deviations (75% for \\(k = 2\\) and 88.9% for \\(k = 3\\))."
      ),
      shiny::p(
        shiny::strong("Try it:"),
        "switch between the two classes from Example 3.7 to watch the standard deviation jump from 3.35 to 5.14 while",
        "the mean stays at 5. Then compare height (bell-shaped) and income (skewed) for \\(k = 1\\): for income, the",
        "share within 1 standard deviation sits well above 68%, because the right tail inflates \\(s\\)."
      )
    )
  )
}

cuidados_dispersao <- function() {
  tr(
    c(
      "Unidade da variância" = "está elevada ao quadrado (reais², cm²). Para interpretar magnitudes na escala dos dados, use sempre o desvio padrão.",
      "Divisor n contra n − 1" = paste(
        "a razão entre os dois desvios padrão é √(n/(n − 1)). Com n = 6 a diferença é de 9,5%; com n = 100 cai para 0,5%.",
        "Sempre informe qual foi usado."
      ),
      "Escore z com valor extremo" = paste(
        "como o próprio extremo infla x̄ e s, em amostras muito pequenas o |z| nunca passa de (n − 1)/√n",
        "(com n = 6 e divisor n − 1, o teto matemático de |z| é 2,04, abaixo de 3!)."
      ),
      "Regra empírica fora do sino" = "em variáveis positivas muito assimétricas, x̄ − 2s costuma ser negativo, onde nem existem dados."
    ),
    c(
      "Variance units" = "are squared (dollars², cm²). Always use the standard deviation to interpret spread on the original scale.",
      "Divisor n versus n − 1" = paste(
        "the ratio of the two standard deviations is √(n/(n − 1)). At n = 6 they differ by 9.5%; at n = 100 by 0.5%.",
        "Always report which divisor was used."
      ),
      "z-score under an outlier" = paste(
        "because the outlier itself inflates ȳ and s, in very small samples |z| cannot exceed (n − 1)/√n",
        "(at n = 6 with divisor n − 1, the mathematical ceiling on |z| is 2.04, never reaching 3)."
      ),
      "Empirical Rule without bell shape" = "for nonnegative right-skewed variables, ȳ − 2s is often negative, where no observations can exist."
    )
  )
}

mod_dispersao_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::selectInput(
        ns("exemplo"), tr("Conjunto de dados", "Data set"),
        choices = stats::setNames(names(exemplos_dados), purrr::map_chr(exemplos_dados, "rotulo")),
        selected = "altura"
      ),
      shiny::uiOutput(ns("controle_n")),
      shiny::radioButtons(
        ns("divisor"), tr("Divisor da variância e do escore z", "Divisor for variance and z-score"),
        choices = stats::setNames(
          c("n1", "n"),
          tr(
            c("n − 1 (Agresti e Finlay / R)", "n (Morettin e Bussab, Cap. 3)"),
            c("n − 1 (Agresti and Finlay / R)", "n (Morettin and Bussab, ch. 3)")
          )
        ),
        selected = "n1"
      ),
      shiny::radioButtons(
        ns("k"), tr("Faixa em torno da média (x̄ ± k · dp)", "Band around the mean (ȳ ± k · sd)"),
        choices = c("1" = "1", "2" = "2", "3" = "3"),
        selected = "2", inline = TRUE
      ),
      shiny::uiOutput(ns("controle_obs")),
      shiny::uiOutput(ns("botao_sortear"))
    ),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(
        title = shiny::textOutput(ns("titulo_dp")),
        value = shiny::textOutput(ns("dp")),
        shiny::textOutput(ns("dp_outro")),
        theme = "primary"
      ),
      bslib::value_box(
        title = shiny::textOutput(ns("titulo_faixa")),
        value = shiny::textOutput(ns("pct_faixa")),
        shiny::textOutput(ns("ref_faixa")),
        theme = "info"
      ),
      bslib::value_box(
        title = tr("Escore z da observação destacada", "z-score of highlighted observation"),
        value = shiny::textOutput(ns("z_obs")),
        shiny::textOutput(ns("z_obs_desc")),
        theme = NULL
      )
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico_faixa"), height = "400px"))),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico_z"), height = "400px")))
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
      bslib::card(
        bslib::card_header(tr("Medidas de dispersão e cobertura por faixa", "Spread measures and band coverage")),
        bslib::card_body(shiny::uiOutput(ns("tabela")))
      ),
      cartao_cuidados(tr("Cuidados", "Watch out"), cuidados_dispersao())
    ),
    bslib::card(bslib::card_header(rotulo_entenda()), bslib::card_body(texto_dispersao()))
  )
}

mod_dispersao_server <- function(id, modo = shiny::reactive("light")) {
  shiny::moduleServer(id, function(input, output, session) {

    exemplo <- shiny::reactive(exemplos_dados[[input$exemplo]])

    output$controle_n <- shiny::renderUI({
      if (isTRUE(exemplo()$fixo)) return(NULL)
      shiny::sliderInput(
        session$ns("n"), tr("Tamanho da amostra (n)", "Sample size (n)"),
        min = 15, max = 500, value = 120, step = 5, sep = sep_milhar()
      )
    })

    output$botao_sortear <- shiny::renderUI({
      if (isTRUE(exemplo()$fixo)) return(shiny::helpText(exemplo()$fonte))
      shiny::actionButton(
        session$ns("sortear"), tr("Sortear nova amostra", "Draw a new sample"),
        class = "btn-primary", icon = shiny::icon("dice")
      )
    })

    dados <- shiny::reactive({
      if (!isTRUE(exemplo()$fixo)) input$sortear
      n_val <- if (isTRUE(exemplo()$fixo)) 0L else (input$n %||% 120L)
      sort(exemplo()$gerar(n_val))
    })

    output$controle_obs <- shiny::renderUI({
      n <- length(dados())
      shiny::sliderInput(
        session$ns("idx_obs"),
        tr("Destacar observação ordenada x(i)", "Highlight sorted observation y(i)"),
        min = 1, max = n, value = n, step = 1
      )
    })

    idx <- shiny::reactive({
      n <- length(dados())
      val <- input$idx_obs %||% n
      min(max(1L, as.integer(val)), n)
    })

    dp_atual <- shiny::reactive(dp_div(dados(), input$divisor))
    var_atual <- shiny::reactive(var_div(dados(), input$divisor))
    escores_z <- shiny::reactive((dados() - mean(dados())) / dp_atual())

    output$titulo_dp <- shiny::renderText({
      if (identical(input$divisor, "n1")) {
        tr("Desvio padrão s (divisor n − 1)", "Standard deviation s (divisor n − 1)")
      } else {
        tr("Desvio padrão dp (divisor n)", "Standard deviation sd (divisor n)")
      }
    })
    output$dp <- shiny::renderText(fmt_un(dp_atual(), exemplo()$un_curta, exemplo()$digitos))
    output$dp_outro <- shiny::renderText({
      outro <- if (identical(input$divisor, "n1")) "n" else "n1"
      rot <- if (identical(outro, "n")) tr("Com divisor n:", "With divisor n:") else tr("Com divisor n − 1:", "With divisor n − 1:")
      sprintf("%s %s | var = %s", rot, fmt_un(dp_div(dados(), outro), exemplo()$un_curta, exemplo()$digitos),
              fmt_un(var_atual(), exemplo()$un_curta, exemplo()$digitos, quadrado = TRUE))
    })

    output$titulo_faixa <- shiny::renderText({
      sprintf(tr("Observações em x̄ ± %s · dp", "Observations within ȳ ± %s · sd"), input$k)
    })
    output$pct_faixa <- shiny::renderText({
      k <- as.numeric(input$k)
      fmt_pct(mean(abs(escores_z()) <= k), 1)
    })
    output$ref_faixa <- shiny::renderText({
      k <- as.integer(input$k)
      emp <- c("68,3%", "95,4%", "99,7%")[k]
      emp_en <- c("68.3%", "95.4%", "99.7%")[k]
      cheb <- if (k == 1L) tr("0% (trivial)", "0% (trivial)") else fmt_pct(1 - 1 / k^2, 1)
      sprintf(tr("Regra empírica: ~%s | Chebyshev (div. n): ≥ %s", "Empirical Rule: ~%s | Chebyshev (div. n): ≥ %s"),
              tr(emp, emp_en), cheb)
    })

    output$z_obs <- shiny::renderText({
      z <- escores_z()[idx()]
      sinal <- if (z > 0) "+" else if (z < 0) "−" else ""
      paste0(sinal, fmt_num(abs(z), 2))
    })
    output$z_obs_desc <- shiny::renderText({
      x_i <- dados()[idx()]
      sprintf(
        tr("x(%d) = %s (%s)", "y(%d) = %s (%s)"),
        idx(), fmt_un(x_i, exemplo()$un_curta, exemplo()$digitos),
        if (abs(escores_z()[idx()]) > 3) tr("|z| > 3: valor atípico", "|z| > 3: potential outlier") else tr("|z| ≤ 3", "|z| ≤ 3")
      )
    })

    output$tabela <- shiny::renderUI({
      x <- dados()
      ex <- exemplo()
      dig <- ex$digitos
      zs_n <- (x - mean(x)) / dp_div(x, "n")
      zs_Atual <- escores_z()

      linhas <- data.frame(
        tr(
          c(
            "Amplitude (máx. − mín.)", "Desvio médio absoluto (dm)",
            "Variância com divisor n (Morettin e Bussab, Cap. 3)", "Variância com divisor n − 1 (Agresti e Finlay / Cap. 10)",
            "Desvio padrão (n contra n − 1)", "Cobertura em ±1 dp / ±2 dp / ±3 dp",
            "Referência: Regra empírica (sino) | Chebyshev (div. n)"
          ),
          c(
            "Range (max − min)", "Mean absolute deviation",
            "Variance with divisor n (Morettin and Bussab, ch. 3)", "Variance with divisor n − 1 (Agresti and Finlay / R)",
            "Standard deviation (n vs. n − 1)", "Share within ±1 sd / ±2 sd / ±3 sd",
            "Benchmarks: Empirical Rule (bell) | Chebyshev (div. n)"
          )
        ),
        c(
          fmt_un(max(x) - min(x), ex$un_curta, dig),
          fmt_un(dm_media(x), ex$un_curta, dig),
          fmt_un(var_div(x, "n"), ex$un_curta, dig, quadrado = TRUE),
          fmt_un(var_div(x, "n1"), ex$un_curta, dig, quadrado = TRUE),
          sprintf("%s | %s", fmt_un(dp_div(x, "n"), ex$un_curta, dig), fmt_un(dp_div(x, "n1"), ex$un_curta, dig)),
          paste(purrr::map_chr(1:3, \(kk) fmt_pct(mean(abs(zs_Atual) <= kk), 1)), collapse = " / "),
          tr("68% / 95% / 99,7% | ≥ 0% / ≥ 75,0% / ≥ 88,9%", "68% / 95% / 99.7% | ≥ 0% / ≥ 75.0% / ≥ 88.9%")
        )
      )
      names(linhas) <- tr(c("Medida / critério", "Valor na amostra"), c("Measure / benchmark", "Sample value"))
      tabela_html(linhas, classes = c("", "num"))
    })

    output$grafico_faixa <- shiny::renderPlot({
      pal <- paleta_modo(modo())
      ex <- exemplo()
      x <- dados()
      k <- as.numeric(input$k)
      med <- mean(x)
      dp <- dp_atual()
      inf <- med - k * dp
      sup <- med + k * dp
      x_sel <- x[idx()]

      faixa <- data.frame(xmin = inf, xmax = sup)

      if (length(x) <= 40) {
        grafico <- ggplot2::ggplot(data.frame(x = x), ggplot2::aes(x = x)) +
          ggplot2::geom_rect(
            data = faixa, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = cores_app$destaque, alpha = 0.14
          ) +
          ggplot2::geom_dotplot(
            binwidth = max(diff(range(c(x, inf, sup))) / 28, 1e-3),
            fill = cores_app$destaque, colour = pal$fundo, dotsize = 0.85
          )
        rotulo_y <- NULL
      } else {
        quebras <- pretty(range(x), n = 18)
        grafico <- ggplot2::ggplot(data.frame(x = x), ggplot2::aes(x = x)) +
          ggplot2::geom_rect(
            data = faixa, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = cores_app$destaque, alpha = 0.14
          ) +
          ggplot2::geom_histogram(
            ggplot2::aes(y = ggplot2::after_stat(density)),
            breaks = quebras, fill = cores_app$destaque, colour = pal$fundo, alpha = 0.8
          )
        rotulo_y <- tr("Densidade", "Density")
      }

      grafico +
        ggplot2::geom_vline(xintercept = med, colour = pal$linha, linewidth = 1.05) +
        ggplot2::geom_vline(xintercept = c(inf, sup), colour = cores_app$destaque, linetype = "dashed", linewidth = 0.9) +
        ggplot2::geom_vline(xintercept = x_sel, colour = cores_app$referencia, linewidth = 1) +
        ggplot2::scale_x_continuous(labels = rotulos_eixo()) +
        ggplot2::scale_y_continuous(labels = if (length(x) <= 40) NULL else rotulos_eixo()) +
        ggplot2::labs(
          title = tr("Escala original dos dados", "Original data scale"),
          subtitle = sprintf(
            tr("Faixa sombreada: [%s; %s]", "Shaded band: [%s, %s]"),
            fmt_num(inf, ex$digitos), fmt_num(sup, ex$digitos)
          ),
          x = sprintf("%s (%s)", tr("Valor", "Value"), ex$unidade), y = rotulo_y,
          caption = tr(
            "Preta: média | Tracejadas azuis: x̄ ± k·dp | Vermelha: observação destacada",
            "Black: mean | Blue dashed: ȳ ± k·sd | Red: highlighted observation"
          )
        ) +
        tema_grafico(modo())
    })

    output$grafico_z <- shiny::renderPlot({
      pal <- paleta_modo(modo())
      z <- escores_z()
      k <- as.numeric(input$k)
      d <- data.frame(
        ordem = seq_along(z),
        z = z,
        destaque = seq_along(z) == idx(),
        fora_3 = abs(z) > 3
      )
      lim_x <- max(3.4, max(abs(z)) * 1.08)

      ggplot2::ggplot(d, ggplot2::aes(x = z, y = ordem)) +
        ggplot2::annotate("rect", xmin = -k, xmax = k, ymin = -Inf, ymax = Inf, fill = cores_app$destaque, alpha = 0.12) +
        ggplot2::geom_vline(xintercept = 0, colour = pal$linha, linewidth = 0.9) +
        ggplot2::geom_vline(xintercept = c(-3, 3), colour = cores_app$referencia, linetype = "dotted", linewidth = 0.85) +
        ggplot2::geom_point(
          data = d[!d$destaque, , drop = FALSE],
          ggplot2::aes(colour = fora_3),
          size = if (nrow(d) > 120) 1.7 else 2.5, alpha = 0.8
        ) +
        ggplot2::geom_point(
          data = d[d$destaque, , drop = FALSE],
          colour = cores_app$referencia, size = 4.5
        ) +
        ggplot2::scale_colour_manual(values = c(`FALSE` = cores_app$destaque, `TRUE` = cores_app$referencia)) +
        ggplot2::scale_x_continuous(limits = c(-lim_x, lim_x), breaks = seq(-floor(lim_x), floor(lim_x), by = 1)) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = tr("Escala padronizada: escores zᵢ", "Standardized scale: zᵢ scores"),
          subtitle = tr("Média = 0, desvio padrão = 1 (sem unidade)", "Mean = 0, standard deviation = 1 (unitless)"),
          x = tr("Escore z = (xᵢ − x̄) / dp", "z-score = (yᵢ − ȳ) / sd"),
          y = tr("Posição na amostra ordenada (i)", "Rank in sorted sample (i)"),
          caption = tr(
            "Pontilhadas vermelhas: z = −3 e z = +3 | Ponto maior: observação destacada",
            "Red dotted: z = −3 and z = +3 | Larger dot: highlighted observation"
          )
        ) +
        tema_grafico(modo())
    })
  })
}
