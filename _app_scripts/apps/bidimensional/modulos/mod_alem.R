# =============================================================================
# Script:  mod_alem.R (módulo Para ir além: esperança condicional E(Y | X),
#          lei da variância total, aproximação linear e independência)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-25
#
# Entrada:
#   - funções de _app_scripts/apps/bidimensional/modulos/estatisticas.R
# Saída:
#   - funções mod_alem_ui() e mod_alem_server()
# =============================================================================

cenarios_cef <- list(
  bivariada = list(
    rotulo = tr("Normal bivariada (CEF linear: ρ = 0 equivale a independência)", "Bivariate normal (linear CEF: ρ = 0 iff independent)"),
    gerar = function(n, sigma_e) {
      x <- stats::rnorm(n, 0, 1)
      cef <- 0.8 * x
      y <- cef + stats::rnorm(n, 0, sigma_e)
      data.frame(x = x, y = y, cef = cef)
    },
    curva = function(xs) 0.8 * xs,
    var_cef_teo = function(sigma_e) 0.64,
    var_erro_teo = function(sigma_e) sigma_e^2
  ),
  parabola = list(
    rotulo = tr("Parábola simétrica Y = X² − 1 + ε (Casella e Berger, Ex. 4.5.9: Cov = 0, mas dependentes)", "Symmetric parabola Y = X² − 1 + ε (Casella and Berger, Ex. 4.5.9: Cov = 0, yet dependent)"),
    gerar = function(n, sigma_e) {
      x <- stats::rnorm(n, 0, 1)
      cef <- x^2 - 1
      y <- cef + stats::rnorm(n, 0, sigma_e)
      data.frame(x = x, y = y, cef = cef)
    },
    curva = function(xs) xs^2 - 1,
    var_cef_teo = function(sigma_e) 2,
    var_erro_teo = function(sigma_e) sigma_e^2
  ),
  monotona_curva = list(
    rotulo = tr("Curva crescente Y = log(X) + ε em X > 0 (Angrist e Pischke, Teo. 3.1.6: melhor aproximação linear)", "Increasing curve Y = log(X) + ε on X > 0 (Angrist and Pischke, Thm. 3.1.6: best linear approximation)"),
    gerar = function(n, sigma_e) {
      x <- stats::runif(n, 0.4, 4.4)
      cef <- log(x)
      y <- cef + stats::rnorm(n, 0, sigma_e)
      data.frame(x = x, y = y, cef = cef)
    },
    curva = function(xs) log(pmax(xs, 0.1)),
    var_cef_teo = function(sigma_e) {
      xs <- seq(0.4, 4.4, length.out = 2000)
      mean((log(xs) - mean(log(xs)))^2)
    },
    var_erro_teo = function(sigma_e) sigma_e^2
  ),
  heterocedastica = list(
    rotulo = tr("CEF linear com variância em leque Var(Y | X) crescente (Wooldridge, Cap. 8)", "Linear CEF with fan-shaped Var(Y | X) (Wooldridge, ch. 8)"),
    gerar = function(n, sigma_e) {
      x <- stats::runif(n, 0, 3)
      cef <- 0.9 * x
      y <- cef + stats::rnorm(n, 0, sigma_e * (0.3 + 0.8 * x))
      data.frame(x = x, y = y, cef = cef)
    },
    curva = function(xs) 0.9 * xs,
    var_cef_teo = function(sigma_e) 0.81 * (9 / 12),
    var_erro_teo = function(sigma_e) {
      xs <- seq(0, 3, length.out = 2000)
      mean((sigma_e * (0.3 + 0.8 * xs))^2)
    }
  )
)

texto_alem_bidim <- function() {
  tr(
    shiny::withMathJax(
      shiny::p(
        "As abas anteriores usaram duas ferramentas que parecem separadas — comparar médias de grupos em",
        "\\(\\text{quali} \\times \\text{quanti}\\) e ajustar uma reta em \\(\\text{quanti} \\times \\text{quanti}\\).",
        "Na teoria de probabilidade, ambas são casos da mesma peça: a função de esperança condicional (CEF),",
        "\\(m(x) = E(Y \\mid X = x)\\), que dá a média populacional de \\(Y\\) em cada valor fixado de \\(X\\)",
        "(Casella e Berger, seção 4.2; Wooldridge, apêndice B.4). Pela lei das esperanças iteradas,",
        "\\(E(Y) = E[E(Y \\mid X)]\\), e o erro \\(\\varepsilon = Y - E(Y \\mid X)\\) tem média condicional zero",
        "em qualquer \\(x\\)."
      ),
      shiny::p(
        "Essa decomposição sustenta dois resultados centrais. Primeiro, a lei da variância total (Casella e Berger,",
        "Teo. 4.4.7; Angrist e Pischke, Teo. 3.1.3): \\(\\text{Var}(Y) = \\text{Var}(E[Y \\mid X]) + E[\\text{Var}(Y \\mid X)]\\).",
        "Em outras palavras, a variância total soma a variação das médias condicionais (\"entre valores de \\(X\\)\")",
        "e a média das variâncias condicionais (\"dentro de cada \\(X\\)\") — a versão exata na população do",
        "\\(R^2 = 1 - \\overline{\\text{var}}(S)/\\text{var}(S)\\) de Morettin e Bussab. Segundo, pelo teorema da",
        "aproximação linear (Angrist e Pischke, Teo. 3.1.6), mesmo quando \\(E(Y \\mid X)\\) é curva, a reta",
        "populacional \\(\\alpha + \\beta X\\) com \\(\\beta = \\text{Cov}(X, Y)/\\text{Var}(X)\\) continua sendo a",
        "melhor aproximação linear em erro quadrático médio para a própria CEF."
      ),
      shiny::p(
        "Por fim, a desigualdade de Cauchy-Schwarz (Casella e Berger, Teo. 4.5.7) garante \\(|\\text{Cov}(X, Y)| \\le \\sigma_X \\sigma_Y\\),",
        "de modo que \\(|\\rho| \\le 1\\), atingindo \\(\\pm 1\\) se e só se \\(Y = a + bX\\) com probabilidade 1.",
        "Se \\(X\\) e \\(Y\\) são independentes, \\(E(Y \\mid X) = E(Y)\\) é constante e \\(\\text{Cov}(X, Y) = 0\\).",
        "A volta, porém, é falsa em geral: na parábola simétrica \\(Y = X^2 - 1 + \\varepsilon\\) com \\(X \\sim N(0, 1)\\)",
        "(Ex. 4.5.9), \\(E(XY) = E(X^3) - E(X) = 0\\) implica \\(\\rho = 0\\), apesar de \\(X\\) explicar grande",
        "parte da variância de \\(Y\\) via \\(\\eta^2 = \\text{Var}(E[Y \\mid X])/\\text{Var}(Y)\\). Correlação nula",
        "só equivale a independência sob normalidade conjunta."
      ),
      shiny::p(
        shiny::strong("Experimente:"),
        "compare a parábola simétrica (em que \\(r^2 \\approx 0\\) enquanto a fração explicada pela CEF \\(\\eta^2\\) é",
        "alta) com a curva crescente \\(\\log(X)\\), na qual a reta linear acompanha de perto a CEF. Ressalva essencial:",
        "mesmo a CEF verdadeira \\(E(Y \\mid X)\\) descreve apenas como a média de \\(Y\\) muda entre subpopulações com",
        "diferentes \\(X\\), não o efeito causal de intervir sobre \\(X\\) sem hipótese de identificação."
      )
    ),
    shiny::withMathJax(
      shiny::p(
        "The previous tabs used two seemingly separate tools — comparing group means in categorical × quantitative",
        "data and fitting a line in quantitative × quantitative data. In probability theory, both stem from the same",
        "object: the conditional expectation function (CEF), \\(m(x) = E(Y \\mid X = x)\\), which gives the population",
        "mean of \\(Y\\) at each fixed value of \\(X\\) (Casella and Berger, section 4.2; Wooldridge, appendix B.4).",
        "By the law of iterated expectations, \\(E(Y) = E[E(Y \\mid X)]\\), and the residual",
        "\\(\\varepsilon = Y - E(Y \\mid X)\\) has mean zero given any \\(x\\)."
      ),
      shiny::p(
        "Two key results follow. First, the law of total variance (Casella and Berger, Thm. 4.4.7; Angrist and Pischke,",
        "Thm. 3.1.3): \\(\\text{Var}(Y) = \\text{Var}(E[Y \\mid X]) + E[\\text{Var}(Y \\mid X)]\\). Put differently,",
        "total variance adds the variance of the conditional means (\"between \\(X\\) values\") and the average",
        "conditional variance (\"within each \\(X\\)\") — the exact population counterpart of Morettin and Bussab's",
        "\\(R^2 = 1 - \\overline{\\text{var}}(S)/\\text{var}(S)\\). Second, by the Linear CEF-Approximation Theorem",
        "(Angrist and Pischke, Thm. 3.1.6), even when \\(E(Y \\mid X)\\) is nonlinear, the population line",
        "\\(\\alpha + \\beta X\\) with \\(\\beta = \\text{Cov}(X, Y)/\\text{Var}(X)\\) remains the minimum mean-squared",
        "error linear approximation to the CEF itself."
      ),
      shiny::p(
        "Finally, the Cauchy-Schwarz inequality (Casella and Berger, Thm. 4.5.7) ensures",
        "\\(|\\text{Cov}(X, Y)| \\le \\sigma_X \\sigma_Y\\), so \\(|\\rho| \\le 1\\), hitting \\(\\pm 1\\) if and only if",
        "\\(Y = a + bX\\) with probability 1. If \\(X\\) and \\(Y\\) are independent, \\(E(Y \\mid X) = E(Y)\\) is flat",
        "and \\(\\text{Cov}(X, Y) = 0\\). The converse fails in general: on the symmetric parabola",
        "\\(Y = X^2 - 1 + \\varepsilon\\) with \\(X \\sim N(0, 1)\\) (Ex. 4.5.9), \\(E(XY) = E(X^3) - E(X) = 0\\) yields",
        "\\(\\rho = 0\\), even though \\(X\\) accounts for a large share of \\(\\text{Var}(Y)\\) via",
        "\\(\\eta^2 = \\text{Var}(E[Y \\mid X])/\\text{Var}(Y)\\). Zero correlation implies independence under joint normality."
      ),
      shiny::p(
        shiny::strong("Try it:"),
        "contrast the symmetric parabola (where \\(r^2 \\approx 0\\) while the CEF share \\(\\eta^2\\) is large) with the",
        "monotone curve \\(\\log(X)\\), where the best linear fit tracks the CEF closely. One caveat remains: even the",
        "exact CEF \\(E(Y \\mid X)\\) describes how mean \\(Y\\) varies across subpopulations with different \\(X\\),",
        "not the causal effect of changing \\(X\\) without an identification assumption."
      )
    )
  )
}

mod_alem_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::selectInput(
        ns("cenario"), tr("Distribuição conjunta de (X, Y)", "Joint distribution of (X, Y)"),
        choices = stats::setNames(names(cenarios_cef), purrr::map_chr(cenarios_cef, "rotulo")),
        selected = "parabola"
      ),
      shiny::sliderInput(ns("n"), tr("Tamanho da amostra (n)", "Sample size (n)"), min = 50, max = 2000, value = 400, step = 50, sep = sep_milhar()),
      shiny::sliderInput(ns("sigma_e"), tr("Dispersão do ruído condicional (σ_ε)", "Conditional noise spread (σ_ε)"), min = 0.15, max = 2.0, value = 0.75, step = 0.05),
      shiny::actionButton(ns("sortear"), tr("Sortear nova amostra", "Draw a new sample"), class = "btn-primary", icon = shiny::icon("dice"))
    ),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(
        title = tr("Fração explicada pela CEF (η²)", "Share explained by the CEF (η²)"),
        value = shiny::textOutput(ns("eta2")),
        shiny::textOutput(ns("eta2_teo")),
        theme = "primary"
      ),
      bslib::value_box(
        title = tr("Fração explicada pela reta (r²)", "Share explained by the line (r²)"),
        value = shiny::textOutput(ns("r2_lin")),
        shiny::textOutput(ns("r_sinal")),
        theme = "info"
      ),
      bslib::value_box(
        title = tr("Lei da variância total", "Law of total variance"),
        value = shiny::textOutput(ns("var_total")),
        shiny::textOutput(ns("decomp_txt")),
        theme = NULL
      )
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico"), height = "440px"))),
      bslib::card(
        bslib::card_header(tr("Decomposição de Var(Y): população × amostra", "Decomposition of Var(Y): population × sample")),
        bslib::card_body(shiny::uiOutput(ns("tabela")))
      )
    ),
    bslib::card(bslib::card_header(rotulo_entenda()), bslib::card_body(texto_alem_bidim()))
  )
}

mod_alem_server <- function(id, modo = shiny::reactive("light")) {
  shiny::moduleServer(id, function(input, output, session) {

    cen <- shiny::reactive(cenarios_cef[[input$cenario]])

    amostra <- shiny::reactive({
      input$sortear
      cen()$gerar(input$n, input$sigma_e)
    })

    metricas <- shiny::reactive({
      d <- amostra()
      c_obj <- cen()
      v_cef_teo <- c_obj$var_cef_teo(input$sigma_e)
      v_err_teo <- c_obj$var_erro_teo(input$sigma_e)
      v_tot_teo <- v_cef_teo + v_err_teo

      v_y <- var_n(d$y)
      v_cef_am <- var_n(d$cef)
      v_res_cef <- var_n(d$y - d$cef)
      r <- stats::cor(d$x, d$y)
      b <- cov_n(d$x, d$y) / var_n(d$x)
      a <- mean(d$y) - b * mean(d$x)

      list(
        v_cef_teo = v_cef_teo, v_err_teo = v_err_teo, v_tot_teo = v_tot_teo,
        eta2_teo = v_cef_teo / v_tot_teo,
        v_y = v_y, v_cef_am = v_cef_am, v_res_cef = v_res_cef,
        eta2_am = v_cef_am / v_y,
        r = r, r2 = r^2, a = a, b = b
      )
    })

    output$eta2 <- shiny::renderText(fmt_pct(metricas()$eta2_am, 1))
    output$eta2_teo <- shiny::renderText(sprintf(tr("Na população: %s", "In the population: %s"), fmt_pct(metricas()$eta2_teo, 1)))

    output$r2_lin <- shiny::renderText(fmt_pct(metricas()$r2, 1))
    output$r_sinal <- shiny::renderText(sprintf("r = %s", fmt_num(metricas()$r, 3)))

    output$var_total <- shiny::renderText(fmt_num(metricas()$v_tot_teo, 2))
    output$decomp_txt <- shiny::renderText({
      m <- metricas()
      sprintf(
        tr("Var(E[Y|X]) = %s + E[Var(Y|X)] = %s", "Var(E[Y|X]) = %s + E[Var(Y|X)] = %s"),
        fmt_num(m$v_cef_teo, 2), fmt_num(m$v_err_teo, 2)
      )
    })

    output$tabela <- shiny::renderUI({
      m <- metricas()
      tabela <- data.frame(
        tr(
          c(
            "Var(E[Y | X]) — variação entre valores de X",
            "E[Var(Y | X)] — variação média dentro de cada X",
            "Var(Y) = Var(E[Y | X]) + E[Var(Y | X)]",
            "η² = Var(E[Y | X]) / Var(Y) (qualquer forma)",
            "r² linear (só tendência em linha reta)"
          ),
          c(
            "Var(E[Y | X]) — variation across X values",
            "E[Var(Y | X)] — mean variation within each X",
            "Var(Y) = Var(E[Y | X]) + E[Var(Y | X)]",
            "η² = Var(E[Y | X]) / Var(Y) (any shape)",
            "Linear r² (straight-line trend only)"
          )
        ),
        c(
          fmt_num(m$v_cef_teo, 3), fmt_num(m$v_err_teo, 3), fmt_num(m$v_tot_teo, 3),
          fmt_pct(m$eta2_teo, 1), "—"
        ),
        c(
          fmt_num(m$v_cef_am, 3), fmt_num(m$v_res_cef, 3), fmt_num(m$v_y, 3),
          fmt_pct(m$eta2_am, 1), fmt_pct(m$r2, 1)
        )
      )
      names(tabela) <- tr(c("Componente", "População", "Amostra"), c("Component", "Population", "Sample"))
      shiny::tagList(
        tabela_html(tabela, classes = c("", "num", "num")),
        shiny::p(class = "small text-muted mt-2 mb-0", tr(
          paste(
            "Na amostra finita, Cov(E[Y|X], ε) oscila perto de zero, então as duas parcelas somam aproximadamente",
            "Var(Y); na população a igualdade é exata."
          ),
          paste(
            "In a finite sample, Cov(E[Y|X], ε) fluctuates near zero, so the two parts add up approximately to",
            "Var(Y); in the population the identity is exact."
          )
        ))
      )
    })

    output$grafico <- shiny::renderPlot({
      pal <- paleta_modo(modo())
      d <- amostra()
      m <- metricas()
      xs <- seq(min(d$x), max(d$x), length.out = 300)
      nomes_c <- tr(
        c("CEF verdadeira E(Y | X)", "Melhor aproximação linear (MQO)"),
        c("True CEF E(Y | X)", "Best linear approximation (OLS)")
      )
      curvas <- rbind(
        data.frame(x = xs, y = cen()$curva(xs), tipo = nomes_c[1]),
        data.frame(x = xs, y = m$a + m$b * xs, tipo = nomes_c[2])
      )
      curvas$tipo <- factor(curvas$tipo, levels = nomes_c)

      ggplot2::ggplot(d, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point(colour = cores_app$destaque, alpha = if (nrow(d) > 600) 0.3 else 0.55, size = 2) +
        ggplot2::geom_line(
          data = curvas, ggplot2::aes(x = x, y = y, colour = tipo, linetype = tipo),
          linewidth = 1.15
        ) +
        ggplot2::scale_colour_manual(values = c(cores_app$referencia, pal$linha), name = NULL) +
        ggplot2::scale_linetype_manual(values = c("solid", "dashed"), name = NULL) +
        ggplot2::scale_x_continuous(labels = rotulos_eixo()) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = tr("Esperança condicional E(Y | X) contra reta linear", "Conditional expectation E(Y | X) versus linear fit"),
          subtitle = sprintf(tr("η² (CEF) = %s | r² (reta) = %s", "η² (CEF) = %s | r² (line) = %s"), fmt_pct(m$eta2_am, 1), fmt_pct(m$r2, 1)),
          x = "X", y = "Y",
          caption = sprintf(tr("n = %s | Mesmo com CEF curva, a reta tracejada minimiza E[(E[Y|X] − a − bX)²]", "n = %s | Even with a curved CEF, the dashed line minimizes E[(E[Y|X] − a − bX)²]"), fmt_num(nrow(d), 0))
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(legend.position = "top", legend.justification = "left")
    })
  })
}
