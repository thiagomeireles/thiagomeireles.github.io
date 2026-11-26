# =============================================================================
# Script:  mod_robustez.R (módulo Para ir além: funções de perda, quantis
#          como otimização, ponto de ruptura, Huber e eficiência assintótica)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-25
#
# Entrada:
#   - objetos e funções de _app_scripts/apps/posicao-dispersao/modulos/dados.R
# Saída:
#   - funções mod_robustez_ui() e mod_robustez_server()
# =============================================================================

simular_robustez <- function(modelo, n, k_rep, delta) {
  m <- matrix(modelo$gerar(n * k_rep, delta = delta), nrow = k_rep)
  medias <- rowMeans(m)
  medianas <- purrr::map_dbl(seq_len(k_rep), \(i) stats::median(m[i, ]))
  aparadas <- purrr::map_dbl(seq_len(k_rep), \(i) mean(m[i, ], trim = 0.10))
  huber <- purrr::map_dbl(seq_len(k_rep), \(i) huber_locacao(m[i, ], k = 1.5))
  list(media = medias, aparada = aparadas, huber = huber, mediana = medianas)
}

texto_robustez <- function() {
  tr(
    shiny::withMathJax(
      shiny::p(
        "Média, mediana e quantis nascem da mesma pergunta — qual número \\(b\\) melhor resume \\(X\\)? — quando se",
        "troca a função de perda que penaliza o erro \\(X - b\\). Minimizar o erro quadrático esperado \\(E[(X - b)^2]\\)",
        "leva a \\(b = E(X)\\), a média (Casella e Berger, Teo. 2.2.1): como os erros são elevados ao quadrado, pontos",
        "distantes dominam a soma. Minimizar o erro absoluto \\(E[|X - b|]\\) leva à mediana, na qual importa apenas",
        "de que lado de \\(b\\) cada ponto está. De forma mais geral, penalizar erros positivos com peso \\(\\tau\\) e",
        "negativos com \\(1 - \\tau\\) pela função *check* \\(\\rho_\\tau(u) = u(\\tau - \\mathbf{1}(u < 0))\\) entrega",
        "o quantil de ordem \\(\\tau\\) — exatamente o princípio que estende os mínimos quadrados para a regressão",
        "quantílica condicional \\(Q_\\tau(Y \\mid X)\\) (Angrist e Pischke, seção 7.1)."
      ),
      shiny::p(
        "Essa diferença de perda determina o ponto de ruptura (*breakdown point*, Casella e Berger, seção 10.2.1): a",
        "maior fração da amostra que pode ser levada para \\(\\pm\\infty\\) sem arrastar o estimador junto. Na média e",
        "no desvio padrão o ponto de ruptura é \\(0\\%\\), uma vez que uma única observação contaminada basta para",
        "deslocá-los sem limite. A média aparada em \\(10\\%\\) (descarta os \\(10\\%\\) menores e maiores) resiste a",
        "até \\(10\\%\\) de contaminação; a mediana, a distância interquartil e o desvio absoluto mediano,",
        "\\(\\text{DAM} = \\text{mediana}(|X_i - md|)\\) (multiplicado por 1,4826 para estimar \\(\\sigma\\) na normal),",
        "atingem o máximo possível de \\(50\\%\\)."
      ),
      shiny::p(
        "Por que, então, não usar sempre a mediana? Porque resistência tem preço quando os dados são bem-comportados.",
        "Pela eficiência relativa assintótica (ARE, Casella e Berger, seção 10.1.4), na distribuição normal",
        "\\(\\text{Var}(md) \\approx \\frac{\\pi}{2}\\frac{\\sigma^2}{n} = 1{,}571\\,\\frac{\\sigma^2}{n}\\), o que dá",
        "\\(\\text{ARE}(md, \\bar{X}) = 2/\\pi \\approx 0{,}637\\): a mediana desperdiça cerca de 36% da amostra efetiva.",
        "Já sob caudas pesadas o quadro se inverte: na dupla exponencial (Laplace), \\(\\text{ARE}(md, \\bar{X}) = 2\\),",
        "e sob contaminação \\((1 - \\delta)N(0, 1) + \\delta N(0, 10^2)\\) bastam 5% de ruído pesado para a variância",
        "da média saltar de \\(1/n\\) para \\(5{,}95/n\\) (Tabela 10.2.1 de Casella e Berger). O M-estimador de Huber",
        "(seção 10.2.3) concilia os dois mundos usando perda quadrática no centro (\\(|u| \\le k\\)) e perda linear nas",
        "caudas (\\(|u| > k\\)): perde quase nada na normal e protege a estimativa quando aparecem extremos."
      ),
      shiny::p(
        shiny::strong("Experimente:"),
        "comece na normal padrão e confirme que a média tem a distribuição mais estreita. Depois mude para a normal",
        "contaminada com \\(\\delta = 0{,}05\\), para a dupla exponencial ou para a \\(t_3\\), e arraste o valor da",
        "observação contaminada no gráfico da direita para ver a curva de sensibilidade da média escapar enquanto",
        "mediana, média aparada e Huber permanecem limitados."
      )
    ),
    shiny::withMathJax(
      shiny::p(
        "The mean, median and quantiles all answer the same question — which number \\(b\\) best summarizes \\(Y\\)? —",
        "under different loss functions for the error \\(Y - b\\). Minimizing expected squared error \\(E[(Y - b)^2]\\)",
        "yields \\(b = E(Y)\\), the mean (Casella and Berger, Thm. 2.2.1): because errors are squared, distant points",
        "dominate. Minimizing expected absolute error \\(E[|Y - b|]\\) yields the median, where only the sign of",
        "\\(Y - b\\) matters. More generally, weighting positive errors by \\(\\tau\\) and negative ones by",
        "\\(1 - \\tau\\) via the check function \\(\\rho_\\tau(u) = u(\\tau - \\mathbf{1}(u < 0))\\) produces the",
        "\\(\\tau\\)-th quantile — the exact principle behind conditional quantile regression \\(Q_\\tau(Y \\mid X)\\)",
        "(Angrist and Pischke, section 7.1)."
      ),
      shiny::p(
        "That choice of loss governs the breakdown point (Casella and Berger, section 10.2.1): the largest fraction of",
        "the sample that can be sent to \\(\\pm\\infty\\) without carrying the estimator along. For the mean and",
        "standard deviation the breakdown point is \\(0\\%\\), since a single contaminated point moves them without",
        "bound. The \\(10\\%\\) trimmed mean (dropping the lowest and highest \\(10\\%\\)) withstands up to \\(10\\%\\)",
        "contamination; the median, the IQR and the median absolute deviation,",
        "\\(\\text{MAD} = \\text{median}(|Y_i - md|)\\) (scaled by 1.4826 to estimate \\(\\sigma\\) under normality),",
        "reach the \\(50\\%\\) ceiling."
      ),
      shiny::p(
        "Why not always use the median, then? Because resistance has a price when tails are light. By asymptotic",
        "relative efficiency (ARE, Casella and Berger, section 10.1.4), under normality",
        "\\(\\text{Var}(md) \\approx \\frac{\\pi}{2}\\frac{\\sigma^2}{n} = 1.571\\,\\frac{\\sigma^2}{n}\\), giving",
        "\\(\\text{ARE}(md, \\bar{Y}) = 2/\\pi \\approx 0.637\\): the median gives up about 36% of the effective sample.",
        "Under heavy tails the ranking flips: for the double exponential (Laplace), \\(\\text{ARE}(md, \\bar{Y}) = 2\\),",
        "and under \\((1 - \\delta)N(0, 1) + \\delta N(0, 10^2)\\) just 5% heavy contamination multiplies the variance",
        "of the mean nearly sixfold (Casella and Berger, Table 10.2.1). Huber's M-estimator (section 10.2.3) bridges",
        "both regimes with quadratic loss in the center (\\(|u| \\le k\\)) and linear loss in the tails (\\(|u| > k\\)),",
        "staying nearly optimal under normality while bounding the influence of outliers."
      ),
      shiny::p(
        shiny::strong("Try it:"),
        "start with the standard normal and verify that the mean has the tightest sampling distribution. Then switch to",
        "the contaminated normal with \\(\\delta = 0.05\\), Laplace or \\(t_3\\), and drag the contaminated point to",
        "watch the mean's sensitivity curve diverge while the median, trimmed mean and Huber level off."
      )
    )
  )
}

mod_robustez_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::selectInput(
        ns("modelo"), tr("População (centrada em μ = 0)", "Population (centered at μ = 0)"),
        choices = stats::setNames(names(modelos_robustez), purrr::map_chr(modelos_robustez, "rotulo")),
        selected = "contaminada"
      ),
      shiny::conditionalPanel(
        "input.modelo == 'contaminada'", ns = ns,
        shiny::sliderInput(
          ns("delta"), tr("Fração contaminada (δ)", "Contamination share (δ)"),
          min = 0, max = 0.25, value = 0.05, step = 0.01
        )
      ),
      shiny::sliderInput(ns("n"), tr("Tamanho de cada amostra (n)", "Sample size (n)"), min = 15, max = 200, value = 40, step = 5),
      shiny::sliderInput(ns("k_rep"), tr("Amostras na simulação", "Simulated samples"), min = 200, max = 2000, value = 800, step = 200, sep = sep_milhar()),
      shiny::hr(),
      shiny::sliderInput(
        ns("contaminacao_x"), tr("Valor de 1 ponto contaminado (curva de sensibilidade)", "Value of 1 contaminated point (sensitivity curve)"),
        min = 0, max = 30, value = 12, step = 1
      ),
      shiny::actionButton(ns("sortear"), tr("Simular novamente", "Simulate again"), class = "btn-primary", icon = shiny::icon("dice"))
    ),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(
        title = tr("EQM da média (x̄)", "MSE of the mean (ȳ)"),
        value = shiny::textOutput(ns("eqm_media")),
        tr("Ponto de ruptura: 0%", "Breakdown point: 0%"),
        theme = "primary"
      ),
      bslib::value_box(
        title = tr("EQM do M-estimador de Huber", "MSE of Huber's M-estimator"),
        value = shiny::textOutput(ns("eqm_huber")),
        tr("Quadrático no centro, linear nas caudas", "Quadratic in center, linear in tails"),
        theme = "info"
      ),
      bslib::value_box(
        title = tr("EQM da mediana (md)", "MSE of the median (md)"),
        value = shiny::textOutput(ns("eqm_mediana")),
        shiny::textOutput(ns("razao_eqm")),
        theme = NULL
      )
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico_eqm"), height = "420px"))),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico_ruptura"), height = "420px")))
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
      bslib::card(
        bslib::card_header(tr("Desempenho dos quatro estimadores de posição", "Performance of the four location estimators")),
        bslib::card_body(
          shiny::uiOutput(ns("tabela")),
          shiny::p(class = "small text-muted mt-2 mb-0", shiny::textOutput(ns("nota_are"), inline = TRUE))
        )
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(tr("Funções de perda ρ(u)", "Loss functions ρ(u)")),
        bslib::card_body(shiny::plotOutput(ns("grafico_perda"), height = "280px"))
      )
    ),
    bslib::card(bslib::card_header(rotulo_entenda()), bslib::card_body(texto_robustez()))
  )
}

mod_robustez_server <- function(id, modo = shiny::reactive("light")) {
  shiny::moduleServer(id, function(input, output, session) {

    modelo <- shiny::reactive(modelos_robustez[[input$modelo]])
    delta <- shiny::reactive(if (identical(input$modelo, "contaminada")) (input$delta %||% 0.05) else 0)

    sim <- shiny::reactive({
      input$sortear
      simular_robustez(modelo(), input$n, input$k_rep, delta())
    })

    # Amostra-base fixa por sorteio para traçar a curva de sensibilidade ao mover 1 ponto
    amostra_base <- shiny::reactive({
      input$sortear
      stats::rnorm(input$n - 1L)
    })

    eqm <- function(v) mean(v^2)

    output$eqm_media <- shiny::renderText(fmt_num(eqm(sim()$media), 4))
    output$eqm_huber <- shiny::renderText(fmt_num(eqm(sim()$huber), 4))
    output$eqm_mediana <- shiny::renderText(fmt_num(eqm(sim()$mediana), 4))
    output$razao_eqm <- shiny::renderText({
      s <- sim()
      sprintf(
        tr("EQM(x̄) / EQM(md) = %s | Ruptura: 50%%", "MSE(ȳ) / MSE(md) = %s | Breakdown: 50%%"),
        fmt_num(eqm(s$media) / eqm(s$mediana), 2)
      )
    })

    output$nota_are <- shiny::renderText(modelo()$are_texto)

    output$tabela <- shiny::renderUI({
      s <- sim()
      nomes <- tr(
        c("Média (x̄)", "Média aparada (10%)", "M-estimador de Huber (k = 1,5)", "Mediana (md)"),
        c("Mean (ȳ)", "Trimmed mean (10%)", "Huber M-estimator (k = 1.5)", "Median (md)")
      )
      ruptura <- c("0%", "10%", "~50%", "50%")
      lista <- list(s$media, s$aparada, s$huber, s$mediana)
      eqms <- purrr::map_dbl(lista, eqm)
      tabela <- data.frame(
        nomes,
        ruptura,
        purrr::map_chr(lista, \(v) fmt_num(mean(v), 4)),
        purrr::map_chr(lista, \(v) fmt_num(stats::sd(v), 4)),
        purrr::map_chr(eqms, \(e) fmt_num(e, 4)),
        purrr::map_chr(eqms, \(e) fmt_num(eqms[1] / e, 2))
      )
      names(tabela) <- tr(
        c("Estimador", "Ruptura", "Média", "EP", "EQM", "Eficiência vs. x̄"),
        c("Estimator", "Breakdown", "Mean", "SE", "MSE", "Efficiency vs. ȳ")
      )
      tabela_html(tabela, classes = c("", "num", "num", "num", "num", "num"))
    })

    output$grafico_eqm <- shiny::renderPlot({
      pal <- paleta_modo(modo())
      s <- sim()
      nomes <- tr(
        c("Média", "Aparada 10%", "Huber", "Mediana"),
        c("Mean", "Trimmed 10%", "Huber", "Median")
      )
      d <- data.frame(
        estimador = factor(rep(nomes, each = length(s$media)), levels = rev(nomes)),
        valor = c(s$media, s$aparada, s$huber, s$mediana)
      )
      lim <- stats::quantile(abs(d$valor), 0.99)

      ggplot2::ggplot(d, ggplot2::aes(x = valor, y = estimador)) +
        ggplot2::geom_vline(xintercept = 0, colour = cores_app$referencia, linetype = "dashed", linewidth = 0.95) +
        ggplot2::geom_boxplot(width = 0.5, fill = cores_app$destaque, colour = pal$texto, alpha = 0.75, outlier.alpha = 0.35) +
        ggplot2::coord_cartesian(xlim = c(-lim, lim)) +
        ggplot2::scale_x_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = tr("Distribuição amostral dos estimadores", "Sampling distribution of the estimators"),
          subtitle = sprintf(tr("%s amostras de tamanho n = %d (μ = 0)", "%s samples of size n = %d (μ = 0)"), fmt_num(input$k_rep, 0), input$n),
          x = tr("Estimativa", "Estimate"), y = NULL,
          caption = tr("Tracejada vermelha: parâmetro verdadeiro μ = 0", "Red dashed: true parameter μ = 0")
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_text(size = 12, colour = pal$texto))
    })

    output$grafico_ruptura <- shiny::renderPlot({
      pal <- paleta_modo(modo())
      base_x <- amostra_base()
      grade <- seq(0, 30, by = 0.75)
      nomes <- tr(
        c("Média (0%)", "Aparada 10%", "Huber", "Mediana (50%)"),
        c("Mean (0%)", "Trimmed 10%", "Huber", "Median (50%)")
      )
      curvas <- purrr::map(grade, \(c_val) {
        am <- c(base_x, c_val)
        c(mean(am), mean(am, trim = 0.10), huber_locacao(am, k = 1.5), stats::median(am))
      })
      d <- data.frame(
        c_val = rep(grade, each = 4),
        estimativa = unlist(curvas),
        estimador = factor(rep(nomes, times = length(grade)), levels = nomes)
      )

      ggplot2::ggplot(d, ggplot2::aes(x = c_val, y = estimativa, colour = estimador, linetype = estimador)) +
        ggplot2::geom_vline(xintercept = input$contaminacao_x, colour = pal$secundario, linetype = "dotted") +
        ggplot2::geom_line(linewidth = 1.1) +
        ggplot2::scale_colour_manual(values = c(cores_app$referencia, "#287bb5", "#2fb380", cores_app$destaque), name = NULL) +
        ggplot2::scale_linetype_manual(values = c("solid", "dashed", "dotdash", "solid"), name = NULL) +
        ggplot2::scale_x_continuous(labels = rotulos_eixo()) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::guides(colour = ggplot2::guide_legend(ncol = 2), linetype = ggplot2::guide_legend(ncol = 2)) +
        ggplot2::labs(
          title = tr("Curva de sensibilidade: 1 ponto contaminado", "Sensitivity curve: 1 contaminated point"),
          subtitle = sprintf(tr("n − 1 pontos fixos + 1 ponto levado até %d", "n − 1 fixed points + 1 point moved up to %d"), input$contaminacao_x),
          x = tr("Valor do ponto contaminado", "Value of the contaminated point"),
          y = tr("Valor do estimador", "Estimator value"),
          caption = tr("A média cresce com inclinação 1/n sem teto; os resistentes estacionam", "The mean grows with slope 1/n without bound; resistant estimators plateau")
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(legend.position = "top", legend.justification = "left")
    })

    output$grafico_perda <- shiny::renderPlot({
      pal <- paleta_modo(modo())
      u <- seq(-3, 3, length.out = 301)
      nomes <- tr(
        c("Quadrática: ½u² (média)", "Huber (k = 1,5)", "Absoluta: |u| (mediana)", "Check: τ = 0,75 (3º quartil)"),
        c("Quadratic: ½u² (mean)", "Huber (k = 1.5)", "Absolute: |u| (median)", "Check: τ = 0.75 (3rd quartile)")
      )
      d <- data.frame(
        u = rep(u, 4),
        perda = c(
          perda_posicao(u, "quadratica"),
          perda_posicao(u, "huber", k = 1.5),
          perda_posicao(u, "absoluta"),
          perda_posicao(u, "check", tau = 0.75)
        ),
        tipo = factor(rep(nomes, each = length(u)), levels = nomes)
      )

      ggplot2::ggplot(d, ggplot2::aes(x = u, y = perda, colour = tipo, linetype = tipo)) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::scale_colour_manual(values = c(cores_app$referencia, "#2fb380", cores_app$destaque, "#f4bd61"), name = NULL) +
        ggplot2::scale_linetype_manual(values = c("solid", "dotdash", "solid", "dashed"), name = NULL) +
        ggplot2::scale_x_continuous(labels = rotulos_eixo()) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::guides(colour = ggplot2::guide_legend(ncol = 2), linetype = ggplot2::guide_legend(ncol = 2)) +
        ggplot2::labs(x = "u = x − b", y = "ρ(u)") +
        tema_grafico(modo()) +
        ggplot2::theme(legend.position = "top", legend.justification = "left", legend.text = ggplot2::element_text(size = 9.5))
    })
  })
}
