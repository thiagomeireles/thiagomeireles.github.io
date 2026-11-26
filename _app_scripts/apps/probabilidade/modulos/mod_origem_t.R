# =============================================================================
# Script:  mod_origem_t.R (módulo Para ir além: construção formal T = Z / √(U/ν),
#          independência entre X̄ e S² na normal e cobertura em amostra pequena)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-25
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/apps/probabilidade/app.R)
# Saída:
#   - funções mod_origem_t_ui() e mod_origem_t_server()
# =============================================================================

simular_pivote_t <- function(n, k_rep, pop = "normal") {
  if (identical(pop, "exponencial")) {
    # Exponencial(1) centrada em μ = 1 e σ = 1: assimétrica, quebra independência entre X̄ e S²
    m <- matrix(stats::rexp(n * k_rep, rate = 1), nrow = k_rep)
    mu <- 1
    sigma <- 1
  } else {
    m <- matrix(stats::rnorm(n * k_rep, mean = 0, sd = 1), nrow = k_rep)
    mu <- 0
    sigma <- 1
  }
  medias <- rowMeans(m)
  s2 <- rowSums((m - medias)^2) / (n - 1)
  s <- sqrt(s2)
  z <- (medias - mu) / (sigma / sqrt(n))
  escala_s <- s / sigma
  t_stat <- z / escala_s
  list(z = z, s_sobre_sigma = escala_s, u = (n - 1) * s2 / sigma^2, t = t_stat)
}

texto_origem_t <- function() {
  tr(
    shiny::withMathJax(
      shiny::p(
        "Por que substituir \\(\\sigma\\) por \\(S\\) troca a normal pela \\(t\\) de Student, e não por outra curva",
        "qualquer? Em amostras \\(X_1, \\dots, X_n\\) independentes de uma população \\(N(\\mu, \\sigma^2)\\), o",
        "Teorema 5.3.1 de Casella e Berger estabelece três resultados exatos: (1) a média padronizada",
        "\\(Z = \\frac{\\bar{X} - \\mu}{\\sigma/\\sqrt{n}}\\) segue \\(N(0, 1)\\); (2) a soma de quadrados reescalada",
        "\\(U = \\frac{(n - 1)S^2}{\\sigma^2}\\) segue uma qui-quadrado com \\(\\nu = n - 1\\) graus de liberdade;",
        "e (3) \\(\\bar{X}\\) e \\(S^2\\) são variáveis aleatórias independentes — uma propriedade exclusiva da",
        "distribuição normal entre todas as famílias contínuas."
      ),
      shiny::p(
        "Dividindo o numerador e o denominador da estatística \\(t\\) por \\(\\sigma\\), o parâmetro desconhecido",
        "desaparece e sobra a razão de duas peças independentes (Definição 5.3.2):",
        "\\(T = \\frac{\\bar{X} - \\mu}{S/\\sqrt{n}} = \\frac{Z}{\\sqrt{U / \\nu}}\\). O gráfico de dispersão mostra o",
        "mecanismo concreto das caudas pesadas: mesmo quando \\(|Z|\\) é moderado (por exemplo, 1,2), uma amostra",
        "pequena pode sortear observações casualmente próximas entre si, produzindo \\(S/\\sigma\\) bem abaixo de 1",
        "e arremessando \\(|T|\\) para 4 ou 5. Se usarmos o corte \\(\\pm 1{,}96\\) da normal ignorando essa oscilação",
        "de \\(S\\), o intervalo de 95% cobre menos do que promete em amostras pequenas."
      ),
      shiny::p(
        "Essa construção também explica as propriedades analíticas da \\(t_\\nu\\) (Casella e Berger, cap. 5): sua",
        "densidade decai como potência \\((1 + t^2/\\nu)^{-(\\nu+1)/2}\\) em vez da queda exponencial \\(e^{-z^2/2}\\),",
        "logo \\(E(|T|^r) < \\infty\\) somente para \\(r < \\nu\\) e a função geradora de momentos não existe para",
        "nenhum \\(\\nu\\) finito. E se a população não for normal, como na exponencial? Em amostras pequenas,",
        "\\(\\bar{X}\\) e \\(S^2\\) passam a ser positivamente correlacionados (amostras na cauda direita têm média",
        "alta e desvio padrão alto ao mesmo tempo), distorcendo a simetria de \\(T\\) até que o Teorema Central do",
        "Limite e o Teorema de Slutsky recuperem a aproximação para \\(n\\) grande."
      ),
      shiny::p(
        shiny::strong("Experimente:"),
        "com população normal e \\(n = 4\\) (\\(\\nu = 3\\)), observe a independência no gráfico da direita",
        "(\\(\\text{corr}(Z, S/\\sigma) \\approx 0\\)) e veja a cobertura com \\(\\pm 1{,}96\\) cair perto de 86%,",
        "contra 95% usando \\(t_{3;\\,0{,}975} = 3{,}182\\). Depois troque para a população exponencial e veja",
        "\\(Z\\) e \\(S/\\sigma\\) caminharem juntos."
      )
    ),
    shiny::withMathJax(
      shiny::p(
        "Why does replacing \\(\\sigma\\) with \\(S\\) turn the normal into Student's \\(t\\) rather than some other",
        "distribution? For an i.i.d. sample \\(X_1, \\dots, X_n\\) from \\(N(\\mu, \\sigma^2)\\), Casella and Berger's",
        "Theorem 5.3.1 establishes three exact facts: (1) the standardized mean",
        "\\(Z = \\frac{\\bar{X} - \\mu}{\\sigma/\\sqrt{n}}\\) follows \\(N(0, 1)\\); (2) the rescaled sum of squares",
        "\\(U = \\frac{(n - 1)S^2}{\\sigma^2}\\) follows a chi-squared distribution with \\(\\nu = n - 1\\) degrees",
        "of freedom; and (3) \\(\\bar{X}\\) and \\(S^2\\) are independent random variables — a property unique to",
        "the normal family."
      ),
      shiny::p(
        "Dividing numerator and denominator of the \\(t\\)-statistic by \\(\\sigma\\) cancels the unknown parameter",
        "and leaves the ratio of two independent pieces (Definition 5.3.2):",
        "\\(T = \\frac{\\bar{X} - \\mu}{S/\\sqrt{n}} = \\frac{Z}{\\sqrt{U / \\nu}}\\). The scatterplot reveals the",
        "concrete mechanism behind heavy tails: even when \\(|Z|\\) is modest (say, 1.2), a small sample can happen",
        "to cluster tightly by chance, yielding \\(S/\\sigma\\) well below 1 and sending \\(|T|\\) past 4 or 5.",
        "Using the normal cutoff \\(\\pm 1.96\\) while ignoring that variability in \\(S\\) makes a nominal 95%",
        "interval undercover in small samples."
      ),
      shiny::p(
        "This construction also explains the analytical traits of \\(t_\\nu\\) (Casella and Berger, ch. 5): its",
        "density decays polynomially as \\((1 + t^2/\\nu)^{-(\\nu+1)/2}\\) rather than exponentially like \\(e^{-z^2/2}\\),",
        "so \\(E(|T|^r) < \\infty\\) only for \\(r < \\nu\\) and the moment generating function does not exist for",
        "any finite \\(\\nu\\). What if the population is skewed, such as an exponential? In small samples,",
        "\\(\\bar{X}\\) and \\(S^2\\) become positively correlated (draws in the right tail raise both the mean and",
        "the spread at once), skewing \\(T\\) until the Central Limit Theorem and Slutsky's theorem take over for",
        "large \\(n\\)."
      ),
      shiny::p(
        shiny::strong("Try it:"),
        "with a normal population and \\(n = 4\\) (\\(\\nu = 3\\)), note the independence in the right-hand plot",
        "(\\(\\text{corr}(Z, S/\\sigma) \\approx 0\\)) and see coverage under \\(\\pm 1.96\\) drop near 86%, versus",
        "95% with \\(t_{3,\\,0.975} = 3.182\\). Then switch to the exponential population to see \\(Z\\) and",
        "\\(S/\\sigma\\) move together."
      )
    )
  )
}

mod_origem_t_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::radioButtons(
        ns("pop"), tr("População de origem", "Parent population"),
        choices = stats::setNames(
          c("normal", "exponencial"),
          tr(
            c("Normal N(0, 1): hipótese exata da t", "Exponencial(1): assimétrica (X̄ e S² dependentes)"),
            c("Normal N(0, 1): exact t assumption", "Exponential(1): skewed (X̄ and S² dependent)")
          )
        )
      ),
      shiny::sliderInput(ns("n"), tr("Tamanho da amostra (n = ν + 1)", "Sample size (n = ν + 1)"), min = 3, max = 50, value = 5, step = 1),
      shiny::sliderInput(ns("k_rep"), tr("Amostras simuladas", "Simulated samples"), min = 500, max = 5000, value = 2000, step = 500, sep = sep_milhar()),
      shiny::actionButton(ns("sortear"), tr("Simular novamente", "Simulate again"), class = "btn-primary", icon = shiny::icon("dice"))
    ),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(
        title = tr("Cobertura usando ± 1,96 (Z)", "Coverage using ± 1.96 (Z)"),
        value = shiny::textOutput(ns("cob_z")),
        tr("Nominal desejado: 95,0%", "Target nominal level: 95.0%"),
        theme = "info"
      ),
      bslib::value_box(
        title = shiny::textOutput(ns("titulo_cob_t")),
        value = shiny::textOutput(ns("cob_t")),
        shiny::textOutput(ns("corte_t_txt")),
        theme = "primary"
      ),
      bslib::value_box(
        title = tr("Correlação entre Z e S/σ", "Correlation between Z and S/σ"),
        value = shiny::textOutput(ns("corr_zs")),
        shiny::textOutput(ns("corr_desc")),
        theme = NULL
      )
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico_hist"), height = "420px"))),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico_disp"), height = "420px")))
    ),
    bslib::card(bslib::card_header(rotulo_entenda()), bslib::card_body(texto_origem_t()))
  )
}

mod_origem_t_server <- function(id, modo = shiny::reactive("light")) {
  shiny::moduleServer(id, function(input, output, session) {

    nu <- shiny::reactive(input$n - 1L)
    corte_t <- shiny::reactive(stats::qt(0.975, df = nu()))

    sim <- shiny::reactive({
      input$sortear
      simular_pivote_t(input$n, input$k_rep, input$pop)
    })

    output$cob_z <- shiny::renderText(fmt_pct(mean(abs(sim()$t) <= stats::qnorm(0.975)), 1))
    output$titulo_cob_t <- shiny::renderText(sprintf(tr("Cobertura usando ± t(%d)", "Coverage using ± t(%d)"), nu()))
    output$cob_t <- shiny::renderText(fmt_pct(mean(abs(sim()$t) <= corte_t()), 1))
    output$corte_t_txt <- shiny::renderText(sprintf(tr("Corte t(0,975; %d gl) = ± %s", "Cutoff t(0.975, %d df) = ± %s"), nu(), fmt_num(corte_t(), 3)))

    output$corr_zs <- shiny::renderText(fmt_num(stats::cor(sim()$z, sim()$s_sobre_sigma), 3))
    output$corr_desc <- shiny::renderText({
      if (identical(input$pop, "normal")) {
        tr("Na normal: X̄ e S² são independentes (Teo. 5.3.1)", "Under normality: X̄ and S² are independent (Thm. 5.3.1)")
      } else {
        tr("Fora da normal: média e DP correlacionados", "Non-normal: sample mean and SD are correlated")
      }
    })

    output$grafico_hist <- shiny::renderPlot({
      pal <- paleta_modo(modo())
      t_vals <- sim()$t
      t_recorte <- t_vals[abs(t_vals) <= 6]
      xs <- seq(-5, 5, length.out = 500)
      nomes_c <- tr(
        c(sprintf("t teórica (%d gl)", nu()), "Normal padrão Z"),
        c(sprintf("Theoretical t (%d df)", nu()), "Standard normal Z")
      )
      curvas <- rbind(
        data.frame(x = xs, y = stats::dt(xs, df = nu()), curva = nomes_c[1]),
        data.frame(x = xs, y = stats::dnorm(xs), curva = nomes_c[2])
      )
      curvas$curva <- factor(curvas$curva, levels = nomes_c)

      ggplot2::ggplot() +
        ggplot2::geom_histogram(
          data = data.frame(t = t_recorte),
          ggplot2::aes(x = t, y = ggplot2::after_stat(density)),
          bins = 45, fill = cores_app$destaque, colour = pal$fundo, alpha = 0.72
        ) +
        ggplot2::geom_line(
          data = curvas, ggplot2::aes(x = x, y = y, colour = curva, linetype = curva),
          linewidth = 1.1
        ) +
        ggplot2::scale_colour_manual(values = c(cores_app$referencia, pal$linha), name = NULL) +
        ggplot2::scale_linetype_manual(values = c("solid", "dashed"), name = NULL) +
        ggplot2::coord_cartesian(xlim = c(-5, 5)) +
        ggplot2::scale_x_continuous(breaks = -5:5) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = tr("Distribuição simulada de T = (X̄ − μ) / (S/√n)", "Simulated distribution of T = (X̄ − μ) / (S/√n)"),
          subtitle = sprintf(tr("n = %d (ν = %d gl) | %s amostras", "n = %d (ν = %d df) | %s samples"), input$n, nu(), fmt_num(input$k_rep, 0)),
          x = "T", y = tr("Densidade", "Density"),
          caption = tr("Barras: valores simulados em [−6; 6]", "Bars: simulated draws in [−6, 6]")
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(legend.position = "top", legend.justification = "left")
    })

    output$grafico_disp <- shiny::renderPlot({
      pal <- paleta_modo(modo())
      s <- sim()
      d <- data.frame(
        z = s$z,
        escala = s$s_sobre_sigma,
        fora_z = abs(s$t) > 1.96
      )
      if (nrow(d) > 1200) d <- d[seq_len(1200), ]

      ggplot2::ggplot(d, ggplot2::aes(x = z, y = escala, colour = fora_z)) +
        ggplot2::geom_hline(yintercept = 1, colour = pal$secundario, linetype = "dashed") +
        ggplot2::geom_abline(intercept = 0, slope = 1 / 1.96, colour = cores_app$referencia, linetype = "dotted") +
        ggplot2::geom_abline(intercept = 0, slope = -1 / 1.96, colour = cores_app$referencia, linetype = "dotted") +
        ggplot2::geom_point(alpha = 0.55, size = 1.9) +
        ggplot2::scale_colour_manual(
          values = c(`FALSE` = cores_app$destaque, `TRUE` = cores_app$referencia),
          labels = tr(c("|T| ≤ 1,96", "|T| > 1,96"), c("|T| ≤ 1.96", "|T| > 1.96")),
          name = NULL
        ) +
        ggplot2::coord_cartesian(xlim = c(-4, 4), ylim = c(0, max(2.5, stats::quantile(d$escala, 0.99)))) +
        ggplot2::scale_x_continuous(labels = rotulos_eixo()) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = tr("Numerador Z contra denominador S/σ", "Numerator Z versus denominator S/σ"),
          subtitle = tr("Pontos abaixo do V pontilhado têm |T| = |Z| / (S/σ) > 1,96", "Points below the dotted V have |T| = |Z| / (S/σ) > 1.96"),
          x = "Z = (X̄ − μ) / (σ/√n)", y = "S / σ = √(U / ν)",
          caption = tr("Quando S/σ sai pequeno por acaso, |T| fica grande mesmo com |Z| < 1,96", "When S/σ happens to be small, |T| grows large even for |Z| < 1.96")
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(legend.position = "top", legend.justification = "left")
    })
  })
}
