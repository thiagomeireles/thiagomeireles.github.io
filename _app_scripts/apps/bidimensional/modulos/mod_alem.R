# =============================================================================
# Script:  mod_alem.R (módulo Para ir além: esperança condicional E(Y | X),
#          lei da variância total, aproximação linear e independência)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - funções de _app_scripts/apps/bidimensional/modulos/estatisticas.R
# Saída:
#   - funções mod_alem_ui() e mod_alem_server()
# =============================================================================

# Cada cenário traz a CEF verdadeira e as duas parcelas teóricas de Var(Y).
# Na parábola, X é uniforme em [−√3, √3] (média 0, variância 1, Var(X²) = 0,8):
# com X normal, a correlação amostral entre X e X² oscila muito (X³ tem caudas
# pesadas) e esconde o "r perto de zero", como no exemplo em U de estatisticas.R.
cenarios_cef <- list(
  bivariada = list(
    rotulo = tr("Normal bivariada (CEF linear)", "Bivariate normal (linear CEF)"),
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
    rotulo = tr("Parábola simétrica Y = X² − 1 + ε (Cov = 0, mas dependentes)", "Symmetric parabola Y = X² − 1 + ε (Cov = 0, yet dependent)"),
    gerar = function(n, sigma_e) {
      x <- stats::runif(n, -sqrt(3), sqrt(3))
      cef <- x^2 - 1
      y <- cef + stats::rnorm(n, 0, sigma_e)
      data.frame(x = x, y = y, cef = cef)
    },
    curva = function(xs) xs^2 - 1,
    var_cef_teo = function(sigma_e) 0.8,
    var_erro_teo = function(sigma_e) sigma_e^2
  ),
  monotona_curva = list(
    rotulo = tr("Curva crescente Y = log(X) + ε, com X > 0", "Increasing curve Y = log(X) + ε, with X > 0"),
    gerar = function(n, sigma_e) {
      x <- stats::runif(n, 0.4, 4.4)
      cef <- log(x)
      y <- cef + stats::rnorm(n, 0, sigma_e)
      data.frame(x = x, y = y, cef = cef)
    },
    curva = function(xs) log(pmax(xs, 0.1)),
    # Var(log X) com X uniforme em [0,4; 4,4], pela média numa grade fina
    var_cef_teo = function(sigma_e) {
      xs <- seq(0.4, 4.4, length.out = 2000)
      mean((log(xs) - mean(log(xs)))^2)
    },
    var_erro_teo = function(sigma_e) sigma_e^2
  ),
  heterocedastica = list(
    rotulo = tr("Reta com variância em leque: Var(Y | X) cresce com X", "Line with fan-shaped spread: Var(Y | X) grows with X"),
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
        "As abas anteriores usaram duas ferramentas que parecem separadas: comparar as médias dos grupos, na",
        "quali × quanti, e ajustar uma reta, na quanti × quanti. Na teoria de probabilidade, as duas partem do",
        "mesmo objeto, a função de esperança condicional (CEF, na sigla em inglês), \\(m(x) = E(Y \\mid X = x)\\),",
        "que dá a média de \\(Y\\) na população em cada valor de \\(X\\) (Casella e Berger, seção 4.2; Wooldridge,",
        "apêndice B.4). Pela lei das esperanças iteradas, \\(E(Y) = E[E(Y \\mid X)]\\) (Casella e Berger, Teo. 4.4.3;",
        "Gelman et al., seção 1.8), e o erro \\(\\varepsilon = Y - E(Y \\mid X)\\) tem média zero em cada \\(x\\)."
      ),
      shiny::p(
        "Daí saem dois resultados. O primeiro é a lei da variância total (Casella e Berger, Teo. 4.4.7; Angrist e",
        "Pischke, Teo. 3.1.3; Gelman et al., eq. 1.9): \\(\\text{Var}(Y) = \\text{Var}(E[Y \\mid X]) + E[\\text{Var}(Y \\mid X)]\\).",
        "A variância total soma a variação das médias condicionais, entre os valores de \\(X\\), e a média das",
        "variâncias condicionais, dentro de cada \\(X\\). É a versão na população do",
        "\\(R^2 = 1 - \\overline{\\text{var}}(S)/\\text{var}(S)\\) de Morettin e Bussab, e a fração",
        "\\(\\eta^2 = \\text{Var}(E[Y \\mid X])/\\text{Var}(Y)\\) faz o papel desse \\(R^2\\) para qualquer forma da CEF.",
        "O segundo é o teorema da regressão e da CEF (Angrist e Pischke, Teo. 3.1.6): mesmo quando \\(E(Y \\mid X)\\)",
        "é curva, a reta da população \\(\\alpha + \\beta X\\), com \\(\\beta = \\text{Cov}(X, Y)/\\text{Var}(X)\\), é a",
        "melhor aproximação linear da CEF em erro quadrático médio."
      ),
      shiny::p(
        "A correlação fica entre −1 e 1 e só chega a ±1 quando \\(Y = a + bX\\) com probabilidade 1, sendo",
        "\\(b \\neq 0\\) (Casella e Berger, Teo. 4.5.7; a desigualdade de Cauchy-Schwarz, Teo. 4.7.3, leva ao mesmo",
        "resultado por um caminho mais curto). Se \\(X\\) e \\(Y\\) são independentes, \\(E(Y \\mid X) = E(Y)\\) é",
        "constante e \\(\\text{Cov}(X, Y) = 0\\). A recíproca não vale em geral. Na parábola \\(Y = X^2 - 1 + \\varepsilon\\),",
        "com \\(X\\) uniforme em \\((-\\sqrt{3}, \\sqrt{3})\\), \\(E(XY) = E(X^3) - E(X) = 0\\) e, portanto,",
        "\\(\\rho = 0\\), embora \\(X\\) explique boa parte da variância de \\(Y\\). O exemplo adapta o Ex. 4.5.9 de",
        "Casella e Berger, que usa \\(X\\) uniforme em \\((-1, 1)\\). Sob normalidade conjunta, a CEF é linear",
        "(Angrist e Pischke, Teo. 3.1.4) e correlação nula equivale a independência."
      ),
      shiny::p(
        "O cenário da variância em leque mostra outra coisa que a CEF não conta. A média condicional é uma reta, e a",
        "reta de mínimos quadrados a acompanha bem, mas \\(\\text{Var}(Y \\mid X)\\) cresce com \\(X\\). Wooldridge",
        "chama essa situação de heterocedasticidade (Cap. 8)."
      ),
      shiny::p(
        shiny::strong("Experimente:"),
        "compare a parábola, com \\(r^2\\) perto de zero e \\(\\eta^2\\) alto, com a curva crescente \\(\\log(X)\\), em",
        "que a reta fica perto da CEF. Depois diminua \\(\\sigma_\\varepsilon\\) e veja o \\(\\eta^2\\) subir enquanto o",
        "\\(r^2\\) da parábola continua perto de zero."
      )
    ),
    shiny::withMathJax(
      shiny::p(
        "The previous tabs used two tools that look separate: comparing group means, in categorical × quantitative,",
        "and fitting a line, in quantitative × quantitative. In probability theory, both start from the same object,",
        "the conditional expectation function (CEF), \\(m(x) = E(Y \\mid X = x)\\), which gives the population mean of",
        "\\(Y\\) at each value of \\(X\\) (Casella and Berger, section 4.2; Wooldridge, appendix B.4). By the law of",
        "iterated expectations, \\(E(Y) = E[E(Y \\mid X)]\\) (Casella and Berger, Thm. 4.4.3; Gelman et al., section",
        "1.8), and the error \\(\\varepsilon = Y - E(Y \\mid X)\\) has mean zero at each \\(x\\)."
      ),
      shiny::p(
        "Two results follow. The first is the law of total variance (Casella and Berger, Thm. 4.4.7; Angrist and",
        "Pischke, Thm. 3.1.3; Gelman et al., eq. 1.9): \\(\\text{Var}(Y) = \\text{Var}(E[Y \\mid X]) + E[\\text{Var}(Y \\mid X)]\\).",
        "Total variance adds the variation of the conditional means, across values of \\(X\\), and the mean of the",
        "conditional variances, within each \\(X\\). It is the population version of Morettin and Bussab's",
        "\\(R^2 = 1 - \\overline{\\text{var}}(S)/\\text{var}(S)\\), and the share",
        "\\(\\eta^2 = \\text{Var}(E[Y \\mid X])/\\text{Var}(Y)\\) plays the role of that \\(R^2\\) for a CEF of any shape.",
        "The second is the Regression-CEF Theorem (Angrist and Pischke, Thm. 3.1.6): even when \\(E(Y \\mid X)\\) is",
        "curved, the population line \\(\\alpha + \\beta X\\), with \\(\\beta = \\text{Cov}(X, Y)/\\text{Var}(X)\\), is the",
        "best linear approximation to the CEF in mean squared error."
      ),
      shiny::p(
        "The correlation lies between −1 and 1 and only reaches ±1 when \\(Y = a + bX\\) with probability 1, where",
        "\\(b \\neq 0\\) (Casella and Berger, Thm. 4.5.7; the Cauchy-Schwarz inequality, Thm. 4.7.3, gets there by a",
        "shorter route). If \\(X\\) and \\(Y\\) are independent, \\(E(Y \\mid X) = E(Y)\\) is constant and",
        "\\(\\text{Cov}(X, Y) = 0\\). The converse fails in general. On the parabola \\(Y = X^2 - 1 + \\varepsilon\\),",
        "with \\(X\\) uniform on \\((-\\sqrt{3}, \\sqrt{3})\\), \\(E(XY) = E(X^3) - E(X) = 0\\), so \\(\\rho = 0\\), even",
        "though \\(X\\) accounts for a large share of the variance of \\(Y\\). The example adapts Casella and Berger's",
        "Ex. 4.5.9, which uses \\(X\\) uniform on \\((-1, 1)\\). Under joint normality, the CEF is linear (Angrist and",
        "Pischke, Thm. 3.1.4) and zero correlation is the same as independence."
      ),
      shiny::p(
        "The fan-shaped scenario shows something else the CEF leaves out. The conditional mean is a line, and the",
        "least squares line follows it closely, but \\(\\text{Var}(Y \\mid X)\\) grows with \\(X\\). Wooldridge calls",
        "this heteroskedasticity (ch. 8)."
      ),
      shiny::p(
        shiny::strong("Try it:"),
        "compare the parabola, with \\(r^2\\) near zero and a large \\(\\eta^2\\), with the increasing curve",
        "\\(\\log(X)\\), where the line stays close to the CEF. Then lower \\(\\sigma_\\varepsilon\\) and watch",
        "\\(\\eta^2\\) rise while the parabola's \\(r^2\\) stays near zero."
      )
    )
  )
}

cuidados_alem <- function() {
  tr(
    c(
      "CEF conhecida" = paste(
        "os dados são simulados, e por isso o app sabe qual é E(Y | X). Com dados reais, a CEF precisa ser",
        "estimada, e o η² da amostra depende dessa estimativa."
      ),
      "Correlação nula" = paste(
        "não é independência. Sob normalidade conjunta, as duas se equivalem, mas a parábola mostra que, sem essa",
        "hipótese, X e Y podem ter correlação nula e ainda assim ser dependentes."
      ),
      "Variância em leque" = paste(
        "a reta continua descrevendo a média, mas a dispersão em torno dela muda com X. Os erros padrão usuais da",
        "regressão supõem variância constante (Wooldridge, Cap. 8)."
      ),
      "CEF não é efeito causal" = paste(
        "E(Y | X) compara subpopulações com valores diferentes de X. Dizer o que acontece quando se muda X exige",
        "uma hipótese de identificação."
      )
    ),
    c(
      "Known CEF" = paste(
        "the data are simulated, so the app knows E(Y | X). With real data, the CEF has to be estimated, and the",
        "sample η² depends on that estimate."
      ),
      "Zero correlation" = paste(
        "is not independence. Under joint normality the two are equivalent, but the parabola shows that, without that",
        "assumption, X and Y can have zero correlation and still be dependent."
      ),
      "Fan-shaped spread" = paste(
        "the line still describes the mean, but the spread around it changes with X. The usual regression standard",
        "errors assume constant variance (Wooldridge, ch. 8)."
      ),
      "The CEF is not a causal effect" = paste(
        "E(Y | X) compares subpopulations with different values of X. Saying what happens when X is changed takes",
        "an identification assumption."
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
      slider_amostra(ns("n"), min = 50, max = 2000, value = 400, step = 50),
      shiny::sliderInput(ns("sigma_e"), tr("Dispersão do ruído condicional (σ_ε)", "Conditional noise spread (σ_ε)"), min = 0.15, max = 2.0, value = 0.5, step = 0.05),
      botao_sortear(ns("sortear"))
    ),
    shiny::div(
      class = "mb-2",
      shiny::tags$h4(class = "mb-1", tr("Esperança condicional, variância total e independência", "Conditional expectation, total variance and independence")),
      shiny::p(
        class = "mb-0",
        tr(
          "O que as médias dos grupos e a reta de regressão têm em comum, e o que a correlação deixa de fora?",
          "What do group means and the regression line have in common, and what does the correlation leave out?"
        ),
        " ", shiny::span(class = "badge text-bg-primary ms-1", tr("E(Y | X), η² e r²", "E(Y | X), η² and r²"))
      )
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
        title = tr("Var(Y) na população", "Var(Y) in the population"),
        value = shiny::textOutput(ns("var_total")),
        shiny::textOutput(ns("decomp_txt")),
        theme = "info"
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
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
      bslib::card(bslib::card_header(rotulo_entenda()), bslib::card_body(texto_alem_bidim())),
      cartao_cuidados(tr("Cuidados", "Watch out"), cuidados_alem())
    )
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
      ajuste <- resumo_quanti_quanti(d$x, d$y)

      list(
        v_cef_teo = v_cef_teo, v_err_teo = v_err_teo, v_tot_teo = v_tot_teo,
        eta2_teo = v_cef_teo / v_tot_teo,
        v_y = var_n(d$y), v_cef_am = var_n(d$cef), v_res_cef = var_n(d$y - d$cef),
        eta2_am = var_n(d$cef) / var_n(d$y),
        r = ajuste$r, r2 = ajuste$r^2, a = ajuste$a, b = ajuste$b
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
        tr("%s entre os valores de X + %s dentro de cada X", "%s across values of X + %s within each X"),
        fmt_num(m$v_cef_teo, 2), fmt_num(m$v_err_teo, 2)
      )
    })

    output$tabela <- shiny::renderUI({
      m <- metricas()
      tabela <- data.frame(
        tr(
          c(
            "Var(E[Y | X]): variação entre os valores de X",
            "E[Var(Y | X)]: variação média dentro de cada X",
            "Var(Y) = Var(E[Y | X]) + E[Var(Y | X)]",
            "η² = Var(E[Y | X]) / Var(Y), qualquer forma",
            "r², só a tendência em linha reta"
          ),
          c(
            "Var(E[Y | X]): variation across values of X",
            "E[Var(Y | X)]: mean variation within each X",
            "Var(Y) = Var(E[Y | X]) + E[Var(Y | X)]",
            "η² = Var(E[Y | X]) / Var(Y), any shape",
            "r², straight-line trend only"
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
            "Variâncias com divisor n. Na amostra, a covariância entre E(Y | X) e o erro oscila perto de zero, e as",
            "duas parcelas somam aproximadamente Var(Y); na população, a igualdade é exata. A tabela não traz o r² da",
            "população, que teria de ser calculado à parte em cada cenário."
          ),
          paste(
            "Variances with divisor n. In the sample, the covariance between E(Y | X) and the error hovers near zero,",
            "and the two parts add up to roughly Var(Y); in the population, the identity is exact. The table leaves",
            "out the population r², which would have to be worked out separately for each scenario."
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
        c("CEF verdadeira E(Y | X)", "Reta de mínimos quadrados da amostra"),
        c("True CEF E(Y | X)", "Sample least squares line")
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
          title = tr("Esperança condicional E(Y | X) e reta de regressão", "Conditional expectation E(Y | X) and regression line"),
          subtitle = sprintf(tr("η² (CEF) = %s | r² (reta) = %s", "η² (CEF) = %s | r² (line) = %s"), fmt_pct(m$eta2_am, 1), fmt_pct(m$r2, 1)),
          x = "X", y = "Y",
          caption = sprintf(
            tr(
              "Dados simulados, n = %s | Tracejada: estimativa da melhor aproximação linear da CEF",
              "Simulated data, n = %s | Dashed: estimate of the best linear approximation to the CEF"
            ),
            fmt_num(nrow(d), 0)
          )
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(legend.position = "top", legend.justification = "left")
    })
  })
}
