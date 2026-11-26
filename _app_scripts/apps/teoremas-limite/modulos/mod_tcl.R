# =============================================================================
# Script:  mod_tcl.R (módulo: Teorema Central do Limite)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-25
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/apps/teoremas-limite/app.R)
# Saída:
#   - funções mod_tcl_ui() e mod_tcl_server()
# =============================================================================

# Populações com média e desvio padrão teóricos conhecidos
populacoes_tcl <- list(
  uniforme = list(
    rotulo = tr("Uniforme (0, 1)", "Uniform (0, 1)"),
    gerar  = \(n) stats::runif(n),
    media  = 0.5,
    dp     = sqrt(1 / 12)
  ),
  exponencial = list(
    rotulo = tr("Exponencial (média = 1), assimétrica", "Exponential (mean = 1), skewed"),
    gerar  = \(n) stats::rexp(n, rate = 1),
    media  = 1,
    dp     = 1
  ),
  bimodal = list(
    rotulo = tr("Bimodal (mistura de duas normais)", "Bimodal (mixture of two normals)"),
    gerar  = \(n) stats::rnorm(n, mean = sample(c(-2, 2), n, replace = TRUE), sd = 0.7),
    media  = 0,
    dp     = sqrt(0.7^2 + 4)
  ),
  lognormal = list(
    rotulo = tr("Log-normal, muito assimétrica", "Log-normal, highly skewed"),
    gerar  = \(n) stats::rlnorm(n, meanlog = 0, sdlog = 0.75),
    media  = exp(0.75^2 / 2),
    dp     = sqrt((exp(0.75^2) - 1) * exp(0.75^2))
  )
)

# k amostras de tamanho n de uma vez: cada linha da matriz é uma amostra
medias_amostrais <- function(pop, n, k) {
  matrix(pop$gerar(n * k), nrow = k) |> rowMeans()
}

texto_tcl <- function() {
  tr(
    shiny::withMathJax(
      shiny::p(
        "Enquanto a Lei dos Grandes Números garante que \\(\\bar{X}_n\\) se aproxima de \\(\\mu\\), o Teorema Central",
        "do Limite (Morettin e Bussab, seção 10.8; Casella e Berger, Teo. 5.5.15) descreve a distribuição das",
        "flutuações antes de esse limite chegar: para observações independentes e identicamente distribuídas com",
        "variância finita \\(0 < \\sigma^2 < \\infty\\), a média padronizada",
        "\\(Z_n = \\frac{\\bar{X}_n - \\mu}{\\sigma / \\sqrt{n}}\\) converge em distribuição para \\(N(0, 1)\\).",
        "Em termos práticos, \\(\\bar{X}_n\\) comporta-se aproximadamente como \\(N(\\mu, \\sigma^2/n)\\), em que",
        "\\(\\sigma/\\sqrt{n}\\) é o erro padrão da média."
      ),
      shiny::p(
        "O alcance do resultado vem do fato de a população não precisar ser normal: ela pode ser uniforme, bimodal",
        "ou assimétrica à direita, e ainda assim o histograma das médias amostrais ganha forma de sino quando \\(n\\)",
        "cresce. A velocidade dessa aproximação depende da simetria e do peso das caudas na população — simétrica",
        "como a uniforme já fica próxima da normal com \\(n = 5\\), ao passo que a log-normal ainda exibe cauda",
        "direita perceptível em amostras moderadas."
      ),
      shiny::p(
        shiny::strong("Experimente:"),
        "selecione a população log-normal com \\(n = 2\\) e suba \\(n\\) aos poucos até 80, acompanhando como o",
        "histograma da direita perde a assimetria e encosta na curva vermelha, enquanto a população da esquerda",
        "continua exatamente igual."
      )
    ),
    shiny::withMathJax(
      shiny::p(
        "Whereas the Law of Large Numbers says \\(\\bar{X}_n\\) settles at \\(\\mu\\), the Central Limit Theorem",
        "(Morettin and Bussab, section 10.8; Casella and Berger, Thm. 5.5.15) describes the shape of its fluctuations",
        "along the way: for i.i.d. observations with finite variance \\(0 < \\sigma^2 < \\infty\\), the standardized",
        "mean \\(Z_n = \\frac{\\bar{X}_n - \\mu}{\\sigma / \\sqrt{n}}\\) converges in distribution to \\(N(0, 1)\\).",
        "Equivalently, \\(\\bar{X}_n\\) is approximately \\(N(\\mu, \\sigma^2/n)\\), where \\(\\sigma/\\sqrt{n}\\) is",
        "the standard error of the mean."
      ),
      shiny::p(
        "What makes the theorem remarkable is that the parent population need not be normal: whether it is uniform,",
        "bimodal or right-skewed, the sampling distribution of the mean becomes bell-shaped as \\(n\\) increases.",
        "How fast the normal approximation kicks in depends on skewness and tail weight — a symmetric uniform already",
        "looks normal at \\(n = 5\\), whereas a skewed log-normal still shows a right tail at moderate sample sizes."
      ),
      shiny::p(
        shiny::strong("Try it:"),
        "pick the log-normal population at \\(n = 2\\) and raise \\(n\\) gradually toward 80, watching the right-hand",
        "histogram shed its skewness and match the red curve while the population on the left never changes."
      )
    )
  )
}

cuidados_tcl <- function() {
  tr(
    c(
      "A amostra individual não vira normal" = "aumentar n aproxima da normal a distribuição da média x̄ entre amostras, não o histograma dos dados brutos de uma única amostra (que converge para a própria população).",
      "Regra de bolso n ≥ 30" = "não é universal: em distribuições simétricas n = 10 já funciona bem, mas em rendas ou gastos muito concentrados n = 30 ainda pode manter assimetria relevante na média.",
      "Amostragem sem reposição" = "numa população finita de tamanho N, quando a fração amostrada n/N passa de ~5%, o erro padrão ganha o fator de correção √((N − n)/(N − 1)) (Morettin e Bussab, seção 10.7)."
    ),
    c(
      "Individual data do not turn normal" = "increasing n makes the distribution of the sample mean x̄ across samples look normal, not the histogram of a single sample (which mirrors the parent population).",
      "The n ≥ 30 rule of thumb" = "is not universal: for symmetric populations n = 10 is already close, whereas heavily skewed income data can retain noticeable skew in x̄ past n = 30.",
      "Sampling without replacement" = "from a finite population of size N, once n/N exceeds ~5%, the standard error shrinks by the finite-population factor √((N − n)/(N − 1)) (Morettin and Bussab, section 10.7)."
    )
  )
}

mod_tcl_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::selectInput(
        ns("populacao"), tr("População", "Population"),
        choices = stats::setNames(names(populacoes_tcl), purrr::map_chr(populacoes_tcl, "rotulo"))
      ),
      shiny::sliderInput(ns("n"), tr("Tamanho de cada amostra (n)", "Size of each sample (n)"), min = 1, max = 100, value = 5, step = 1),
      shiny::sliderInput(ns("k"), tr("Número de amostras", "Number of samples"), min = 100, max = 5000, value = 1000, step = 100, sep = sep_milhar()),
      shiny::actionButton(ns("sortear"), tr("Sortear novamente", "Draw again"), class = "btn-primary", icon = shiny::icon("dice"))
    ),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(
        title = tr("Média das médias amostrais", "Mean of the sample means"),
        value = shiny::textOutput(ns("media")),
        shiny::textOutput(ns("media_desc")),
        theme = "primary"
      ),
      bslib::value_box(
        title = tr("Erro padrão observado", "Observed standard error"),
        value = shiny::textOutput(ns("ep")),
        shiny::textOutput(ns("ep_desc")),
        theme = "info"
      )
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(5, 7)),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico_pop"), height = "380px"))),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico_medias"), height = "380px")))
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
      bslib::card(
        bslib::card_header(rotulo_entenda()),
        bslib::card_body(texto_tcl())
      ),
      cartao_cuidados_tl(tr("Cuidados", "Watch out"), cuidados_tcl())
    )
  )
}

mod_tcl_server <- function(id, modo = shiny::reactive("light")) {
  shiny::moduleServer(id, function(input, output, session) {

    sorteio <- shiny::reactive({
      input$sortear
      pop <- populacoes_tcl[[input$populacao]]
      list(
        pop     = pop,
        n       = input$n,
        k       = input$k,
        medias  = medias_amostrais(pop, input$n, input$k),
        valores = pop$gerar(20000)
      )
    })

    output$media      <- shiny::renderText(fmt_num(mean(sorteio()$medias), 3))
    output$media_desc <- shiny::renderText({
      sprintf(tr("Média da população (μ): %s", "Population mean (μ): %s"), fmt_num(sorteio()$pop$media, 3))
    })

    output$ep      <- shiny::renderText(fmt_num(stats::sd(sorteio()$medias), 3))
    output$ep_desc <- shiny::renderText({
      s <- sorteio()
      sprintf(tr("Previsto pelo TCL (σ/√n): %s", "Predicted by the CLT (σ/√n): %s"), fmt_num(s$pop$dp / sqrt(s$n), 3))
    })

    output$grafico_pop <- shiny::renderPlot({
      s <- sorteio()
      pal <- paleta_modo(modo())
      limites <- stats::quantile(s$valores, c(0.001, 0.995))

      ggplot2::ggplot(data.frame(x = s$valores), ggplot2::aes(x = x)) +
        ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), bins = 60, fill = cores_app$neutro, colour = pal$fundo) +
        ggplot2::geom_vline(xintercept = s$pop$media, linetype = "dashed", colour = pal$linha) +
        ggplot2::coord_cartesian(xlim = limites) +
        ggplot2::scale_x_continuous(labels = rotulos_eixo()) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title    = tr("População", "Population"),
          subtitle = sprintf("μ = %s | σ = %s", fmt_num(s$pop$media, 2), fmt_num(s$pop$dp, 2)),
          x = tr("Valor", "Value"), y = tr("Densidade", "Density"),
          caption  = tr("Tracejado: média da população", "Dashed: population mean")
        ) +
        tema_grafico(modo())
    })

    output$grafico_medias <- shiny::renderPlot({
      s <- sorteio()
      pal <- paleta_modo(modo())
      ep <- s$pop$dp / sqrt(s$n)
      grade <- seq(min(s$medias), max(s$medias), length.out = 400)
      curva <- data.frame(x = grade, y = stats::dnorm(grade, mean = s$pop$media, sd = ep))

      ggplot2::ggplot(data.frame(x = s$medias), ggplot2::aes(x = x)) +
        ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), bins = 40, fill = cores_app$destaque, colour = pal$fundo, alpha = 0.85) +
        ggplot2::geom_line(data = curva, ggplot2::aes(x = x, y = y), linewidth = 1.1, colour = cores_app$referencia) +
        ggplot2::scale_x_continuous(labels = rotulos_eixo()) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title    = tr("Distribuição das médias amostrais", "Distribution of the sample means"),
          subtitle = sprintf(tr("%s amostras de tamanho n = %d", "%s samples of size n = %d"), fmt_num(s$k, 0), s$n),
          x = tr("Média da amostra", "Sample mean"), y = tr("Densidade", "Density"),
          caption  = tr("Curva vermelha: normal prevista pelo TCL", "Red curve: normal predicted by the CLT")
        ) +
        tema_grafico(modo())
    })
  })
}
