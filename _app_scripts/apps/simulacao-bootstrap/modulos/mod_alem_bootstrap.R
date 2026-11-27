# =============================================================================
# Script:  mod_alem_bootstrap.R (Menu "Para ir além": Método Delta vs.
#          bootstrap, bootstrap paramétrico e falha na fronteira de U(0, θ))
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - objetos e funções de _app_scripts/apps/simulacao-bootstrap/modulos/dados.R
# Saída:
#   - funções mod_alem_bootstrap_ui() e mod_alem_bootstrap_server()
# =============================================================================

texto_alem_bootstrap <- function() {
  tr(
    shiny::withMathJax(
      shiny::p(
        "A utilidade prática do ", shiny::em("bootstrap"), " aparece com mais nitidez quando aproximações analíticas",
        "de primeira ordem degeneram ou dependem de suposições fortes de forma distribucional (Casella e Berger,",
        "seções 10.1.3 e 10.1.4). Para estimar a variância \\(\\tau(p) = p(1 - p)\\) de uma Bernoulli pelo estimador",
        "de máxima verossimilhança \\(\\hat{p}(1 - \\hat{p})\\), o Método Delta de primeira ordem (eq. 10.1.7) entrega",
        "\\(\\widehat{\\text{Var}}_{\\text{Delta}} = \\hat{p}(1 - \\hat{p})(1 - 2\\hat{p})^2 / n\\) (Ex. 10.1.15). Como",
        "\\(\\tau'(p) = 1 - 2p\\) muda de sinal e se anula em \\(p = 1/2\\), essa aproximação colapsa exatamente para",
        "\\(0\\) quando \\(\\hat{p} = 1/2\\), exigindo uma expansão de Taylor de segunda ordem; o ",
        shiny::em("bootstrap"), " não paramétrico (Exs. 10.1.20 e 10.1.21), por outro lado, reavalia diretamente a",
        "função curvada em cada reamostra e recupera a variância verdadeira sem cálculo adicional."
      ),
      shiny::p(
        "Quando a família paramétrica \\(f(x \\mid \\theta)\\) é confiável, o ", shiny::em("bootstrap"),
        " paramétrico substitui \\(\\hat{F}_n\\) pela distribuição ", shiny::em("plug-in"),
        " \\(f(x \\mid \\hat{\\theta})\\) (Casella e Berger, Ex. 10.1.22): nos \\(n = 9\\) dados normais do livro",
        "(\\(\\bar{x} = 2{,}71\\), \\(s^2 = 4{,}82\\), vindos de \\(\\sigma^2 = 4\\)), simular de",
        "\\(N(\\bar{x}, s^2)\\) aproxima melhor \\(\\text{Var}(S^2) = 4{,}00\\) do que aplicar a fórmula normal com",
        "\\(s^2\\) no lugar de \\(\\sigma^2\\) (\\(2 s^4 / 8 = 5{,}81\\)), ainda que subestime fortemente a",
        "variância se a população real tiver caudas pesadas como \\(t_5\\). Por fim, nem o ", shiny::em("bootstrap"),
        " é universal (Miscellanea 10.6.2 e 10.6.3): em \\(X_1, \\dots, X_n \\sim U(0, \\theta)\\), o suporte depende",
        "de \\(\\theta\\) e o MLE \\(\\hat{\\theta} = X_{(n)}\\) fica na fronteira; qualquer reamostra não paramétrica",
        "satisfaz \\(X_{(n)}^* \\le X_{(n)}\\) e repete exatamente \\(X_{(n)}\\) com probabilidade",
        "\\(1 - (1 - 1/n)^n \\approx 63{,}2\\%\\), gerando cobertura nula para o intervalo percentil."
      ),
      shiny::p(
        shiny::strong("Experimente:"),
        "no caso Bernoulli, mova o número de sucessos \\(y\\) até \\(n / 2\\) para ver o Método Delta tocar zero",
        "enquanto o ", shiny::em("bootstrap"), " acompanha a curva exata; em seguida, passe ao caso \\(U(0, \\theta)\\)",
        "para observar o pico discreto de \\(63{,}2\\%\\) travado em \\(X_{(n)}\\)."
      )
    ),
    shiny::withMathJax(
      shiny::p(
        "The practical value of the bootstrap stands out sharpest when first-order analytical approximations",
        "degenerate or lean heavily on distributional shape assumptions (Casella and Berger, sections 10.1.3 and",
        "10.1.4). When estimating the Bernoulli variance \\(\\tau(p) = p(1 - p)\\) via its MLE",
        "\\(\\hat{p}(1 - \\hat{p})\\), the first-order Delta Method (eq. 10.1.7) gives",
        "\\(\\widehat{\\text{Var}}_{\\text{Delta}} = \\hat{p}(1 - \\hat{p})(1 - 2\\hat{p})^2 / n\\) (Ex. 10.1.15).",
        "Because \\(\\tau'(p) = 1 - 2p\\) changes sign and vanishes at \\(p = 1/2\\), that approximation collapses to",
        "\\(0\\) whenever \\(\\hat{p} = 1/2\\), forcing a second-order Taylor expansion; the nonparametric bootstrap",
        "(Exs. 10.1.20 and 10.1.21), on the other hand, evaluates the curved function directly across resamples and",
        "recovers the true variance automatically."
      ),
      shiny::p(
        "When a parametric family \\(f(x \\mid \\theta)\\) is credible, the parametric bootstrap replaces",
        "\\(\\hat{F}_n\\) with the plug-in model \\(f(x \\mid \\hat{\\theta})\\) (Casella and Berger, Ex. 10.1.22):",
        "on the textbook's \\(n = 9\\) normal observations (\\(\\bar{x} = 2.71\\), \\(s^2 = 4.82\\), drawn with",
        "\\(\\sigma^2 = 4\\)), drawing from \\(N(\\bar{x}, s^2)\\) gets closer to the true",
        "\\(\\text{Var}(S^2) = 4.00\\) than plugging \\(s^2\\) into \\(2\\sigma^4 / 8 = 5.81\\), although it",
        "severely understates \\(\\text{Var}(S^2)\\) if the true population has heavy tails such as \\(t_5\\).",
        "Finally, the bootstrap itself is not universal (Miscellanea 10.6.2 and 10.6.3): under",
        "\\(X_1, \\dots, X_n \\sim U(0, \\theta)\\), the support depends on \\(\\theta\\) and the MLE",
        "\\(\\hat{\\theta} = X_{(n)}\\) lies on the boundary; every nonparametric resample satisfies",
        "\\(X_{(n)}^* \\le X_{(n)}\\) and hits \\(X_{(n)}\\) with probability \\(1 - (1 - 1/n)^n \\approx 63.2\\%\\),",
        "giving the percentile interval zero coverage of \\(\\theta\\)."
      ),
      shiny::p(
        shiny::strong("Try it:"),
        "in the Bernoulli study, slide the number of successes \\(y\\) to \\(n / 2\\) to watch first-order Delta",
        "drop to zero while the bootstrap tracks the exact variance curve; then switch to \\(U(0, \\theta)\\) to see",
        "the \\(63.2\\%\\) discrete atom locked at \\(X_{(n)}\\)."
      )
    )
  )
}

cuidados_alem_bootstrap <- function() {
  tr(
    c(
      "Errata numérica na Tabela 10.1.1 de Casella e Berger" = paste(
        "com n = 24, a variância binomial exata de p̂(1 − p̂) em p = 1/4, 1/2 e 2/3 vale 0,00191, 0,00021 e 0,00111",
        "(e o bootstrap dá ~0,0019, ~0,0002 e ~0,0011); os valores ~0,005 impressos na Tabela 10.1.1 do livro decorrem",
        "de errata conhecida na simulação original, mas a conclusão qualitativa sobre a queda do Delta em p̂ = 1/2 permanece."
      ),
      "Paramétrico ganha suavidade ao preço do modelo" = paste(
        "em amostras pequenas como n = 9, amostrar de N(x̄, s²) evita os saltos discretos de F̂_n, porém trava a",
        "curtose em zero: sob caudas pesadas (t₅), subestima Var(S²) tanto quanto a fórmula normal."
      ),
      "Extremos e fronteiras quebram o bootstrap ingênuo" = paste(
        "estatísticas dominadas pelo máximo ou mínimo amostral, bem como dados com dependência temporal ou em",
        "conglomerados reamostrados como se fossem iid, invalidam a consistência do bootstrap padrão (Casella e Berger, seção 10.6.3)."
      )
    ),
    c(
      "Numerical erratum in Casella and Berger's Table 10.1.1" = paste(
        "at n = 24, the exact binomial variance of p̂(1 − p̂) for p = 1/4, 1/2 and 2/3 is 0.00191, 0.00021 and 0.00111",
        "(with bootstrap near 0.0019, 0.0002 and 0.0011); the ~0.005 figures printed in Table 10.1.1 stem from a known",
        "erratum in the book's simulation script, though the qualitative lesson at p̂ = 1/2 holds."
      ),
      "Parametric bootstrap trades model risk for smoothness" = paste(
        "for small n such as n = 9, drawing from N(x̄, s²) avoids the discrete jumps of F̂_n, yet hard-codes zero",
        "excess kurtosis: under heavy tails (t₅), it understates Var(S²) just like the normal formula."
      ),
      "Extremes and boundaries break naive bootstrap" = paste(
        "statistics governed by sample maxima/minima, as well as time-dependent or clustered data resampled as if iid,",
        "invalidate standard nonparametric bootstrap consistency (Casella and Berger, section 10.6.3)."
      )
    )
  )
}

mod_alem_bootstrap_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::selectInput(
        ns("estudo"), tr("Estudo de caso (Casella e Berger, cap. 10)", "Case study (Casella and Berger, ch. 10)"),
        choices = stats::setNames(
          c("delta", "parametrico", "uniforme"),
          c(
            tr("1. Bootstrap vs. Método Delta em p̂(1 − p̂)", "1. Bootstrap vs. Delta Method for p̂(1 − p̂)"),
            tr("2. Paramétrico vs. não paramétrico para S²", "2. Parametric vs. nonparametric for S²"),
            tr("3. Falha na fronteira: X_(n) em U(0, θ)", "3. Boundary failure: X_(n) in U(0, θ)")
          )
        )
      ),
      shiny::uiOutput(ns("controles_estudo")),
      shiny::hr(),
      shiny::sliderInput(
        ns("b_rep"), tr("Réplicas bootstrap (B)", "Bootstrap replications (B)"),
        min = 300, max = 3000, value = 1000, step = 100, sep = sep_milhar()
      ),
      shiny::actionButton(
        ns("simular"), tr("Executar simulação", "Run simulation"),
        class = "btn-primary", icon = shiny::icon("bolt")
      )
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico_alem"), height = "410px"))),
      bslib::card(
        bslib::card_header(tr("Síntese numérica do estudo", "Numerical summary of the study")),
        bslib::card_body(
          shiny::uiOutput(ns("tabela_alem")),
          shiny::hr(),
          shiny::uiOutput(ns("leitura_alem"))
        )
      )
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
      bslib::card(bslib::card_header(rotulo_entenda()), bslib::card_body(texto_alem_bootstrap())),
      cartao_cuidados(tr("Cuidados", "Watch out"), cuidados_alem_bootstrap())
    )
  )
}

mod_alem_bootstrap_server <- function(id, modo = shiny::reactive("light")) {
  shiny::moduleServer(id, function(input, output, session) {

    output$controles_estudo <- shiny::renderUI({
      shiny::req(input$estudo)
      if (identical(input$estudo, "delta")) {
        n_atu <- shiny::isolate(input$n_bern) %||% 24L
        y_atu <- min(shiny::isolate(input$y_bern) %||% 12L, n_atu)
        shiny::tagList(
          shiny::sliderInput(
            session$ns("n_bern"), tr("Tamanho da amostra Bernoulli (n)", "Bernoulli sample size (n)"),
            min = 12, max = 60, value = n_atu, step = 4
          ),
          shiny::sliderInput(
            session$ns("y_bern"), tr("Sucessos observados (y, logo p̂ = y/n)", "Observed successes (y, so p̂ = y/n)"),
            min = 0, max = n_atu, value = y_atu, step = 1
          )
        )
      } else if (identical(input$estudo, "parametrico")) {
        shiny::tagList(
          shiny::selectInput(
            session$ns("fonte_s2"), tr("Origem da amostra (σ² verdadeiro = 4)", "Sample source (true σ² = 4)"),
            choices = stats::setNames(
              c("cb1022", "normal", "t5"),
              c(
                tr("Dados fixos do Ex. 10.1.22 (n = 9, Normal)", "Fixed Ex. 10.1.22 data (n = 9, Normal)"),
                tr("Nova amostra Normal N(3, 4)", "Fresh Normal sample N(3, 4)"),
                tr("Nova amostra de cauda pesada t₅ (Var = 4)", "Fresh heavy-tailed t₅ sample (Var = 4)")
              )
            ),
            selected = shiny::isolate(input$fonte_s2) %||% "cb1022"
          ),
          shiny::sliderInput(
            session$ns("n_s2"), tr("Tamanho n (para amostras sorteadas)", "Sample size n (for fresh draws)"),
            min = 9, max = 50, value = shiny::isolate(input$n_s2) %||% 15, step = 1
          )
        )
      } else {
        shiny::tagList(
          shiny::sliderInput(
            session$ns("theta_unif"), tr("Limite superior verdadeiro θ de U(0, θ)", "True upper bound θ of U(0, θ)"),
            min = 5, max = 20, value = shiny::isolate(input$theta_unif) %||% 10, step = 1
          ),
          shiny::sliderInput(
            session$ns("n_unif"), tr("Tamanho da amostra (n)", "Sample size (n)"),
            min = 8, max = 60, value = shiny::isolate(input$n_unif) %||% 25, step = 1
          )
        )
      }
    })

    shiny::observeEvent(input$n_bern, {
      shiny::req(is.finite(input$n_bern))
      n_b <- as.integer(input$n_bern)
      y_b <- if (!is.null(input$y_bern) && is.finite(input$y_bern)) min(as.integer(input$y_bern), n_b) else n_b %/% 2L
      shiny::updateSliderInput(session, "y_bern", max = n_b, value = y_b)
    }, ignoreInit = TRUE)

    res_alem <- shiny::reactive({
      input$simular
      shiny::req(input$estudo, is.finite(input$b_rep))
      b_v <- as.integer(input$b_rep)

      if (identical(input$estudo, "delta")) {
        n_v <- if (!is.null(input$n_bern) && is.finite(input$n_bern)) as.integer(input$n_bern) else 24L
        y_v <- if (!is.null(input$y_bern) && is.finite(input$y_bern)) min(as.integer(input$y_bern), n_v) else 12L
        list(tipo = "delta", dados = simular_delta_boot_bernoulli(n = n_v, p_obs_sucessos = y_v, b_rep = b_v))
      } else if (identical(input$estudo, "parametrico")) {
        f_v <- input$fonte_s2 %||% "cb1022"
        n_v <- if (!is.null(input$n_s2) && is.finite(input$n_s2)) as.integer(input$n_s2) else 15L
        list(tipo = "parametrico", dados = simular_boot_parametrico_s2(fonte = f_v, n = n_v, b_rep = b_v))
      } else {
        th_v <- if (!is.null(input$theta_unif) && is.finite(input$theta_unif)) input$theta_unif else 10
        n_v <- if (!is.null(input$n_unif) && is.finite(input$n_unif)) as.integer(input$n_unif) else 25L
        list(tipo = "uniforme", dados = simular_falha_uniforme(n = n_v, theta = th_v, b_rep = b_v))
      }
    })

    output$grafico_alem <- shiny::renderPlot({
      r <- res_alem()
      d <- r$dados
      p <- paleta_modo(modo())

      if (identical(r$tipo, "delta")) {
        cv <- d$curva
        rot_v <- tr("Verdadeira Var_p", "True Var_p")
        rot_b <- tr("E[Var* bootstrap]", "E[Bootstrap Var*]")
        rot_d <- tr("Delta 1ª ordem", "1st-order Delta")

        df_lin <- data.frame(
          pr = rep(cv$p, 3L),
          var = c(cv$verdadeira, cv$boot_esperada, cv$delta1),
          metodo = factor(
            c(rep(rot_v, nrow(cv)), rep(rot_b, nrow(cv)), rep(rot_d, nrow(cv))),
            levels = c(rot_v, rot_b, rot_d)
          )
        )
        df_pts <- data.frame(
          pr = c(d$phat, d$phat),
          var = c(d$var_boot, d$var_delta1),
          metodo = factor(c(rot_b, rot_d), levels = c(rot_v, rot_b, rot_d))
        )

        ggplot2::ggplot(df_lin, ggplot2::aes(x = pr, y = var, colour = metodo, linetype = metodo)) +
          ggplot2::geom_line(linewidth = 1.05) +
          ggplot2::geom_vline(xintercept = d$phat, colour = p$secundario, linetype = "dotted") +
          ggplot2::geom_point(data = df_pts, size = 3.2) +
          ggplot2::scale_colour_manual(values = c(p$linha, cores_app$destaque, cores_app$referencia)) +
          ggplot2::scale_linetype_manual(values = c("solid", "dashed", "twodash")) +
          ggplot2::scale_x_continuous(labels = rotulos_eixo()) +
          ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
          tema_grafico(modo()) +
          ggplot2::theme(legend.position = "top") +
          ggplot2::labs(
            title = sprintf(tr("Variância de p̂(1 − p̂) ao longo de p (n = %d)", "Variance of p̂(1 − p̂) across p (n = %d)"), d$n),
            subtitle = sprintf(
              tr("Pontos marcados em p̂ = %s: Delta 1ª ordem zera em 0,50", "Marked points at p̂ = %s: 1st-order Delta hits zero at 0.50"),
              fmt_num(d$phat, 2)
            ),
            x = "p", y = "Var[p̂(1 − p̂)]", colour = NULL, linetype = NULL,
            caption = tr("Casella e Berger, Exs. 10.1.15, 10.1.20 e 10.1.21", "Casella and Berger, Exs. 10.1.15, 10.1.20 and 10.1.21")
          )
      } else if (identical(r$tipo, "parametrico")) {
        rot_par <- tr("Paramétrico N(x̄, s²)", "Parametric N(x̄, s²)")
        rot_np  <- tr("Não paramétrico F̂_n", "Nonparametric F̂_n")
        df_s2 <- data.frame(
          s2 = c(d$s2_param, d$s2_np),
          metodo = factor(
            c(rep(rot_par, length(d$s2_param)), rep(rot_np, length(d$s2_np))),
            levels = c(rot_par, rot_np)
          )
        )
        ggplot2::ggplot(df_s2, ggplot2::aes(x = s2, fill = metodo, colour = metodo)) +
          ggplot2::geom_density(alpha = 0.28, linewidth = 0.95) +
          ggplot2::geom_vline(xintercept = d$s2, colour = p$linha, linewidth = 0.95) +
          ggplot2::geom_vline(xintercept = 4, colour = cores_app$referencia, linetype = "dashed", linewidth = 0.95) +
          ggplot2::scale_fill_manual(values = c(cores_app$destaque, p$neutro)) +
          ggplot2::scale_colour_manual(values = c(cores_app$destaque, p$secundario)) +
          ggplot2::scale_x_continuous(labels = rotulos_eixo()) +
          ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
          tema_grafico(modo()) +
          ggplot2::theme(legend.position = "top") +
          ggplot2::labs(
            title = sprintf(tr("Distribuições bootstrap de S²* (n = %d)", "Bootstrap distributions of S²* (n = %d)"), d$n),
            subtitle = tr("Traço cheio: s² da amostra | Tracejado vermelho: σ² = 4 verdadeiro", "Solid line: sample s² | Dashed red: true σ² = 4"),
            x = "S²*", y = tr("Densidade", "Density"), fill = NULL, colour = NULL,
            caption = tr("Casella e Berger, Ex. 10.1.22", "Casella and Berger, Ex. 10.1.22")
          )
      } else {
        rot_np   <- tr("Bootstrap não paramétrico X*_(n)", "Nonparametric bootstrap X*_(n)")
        rot_real <- tr("População real X_(n) ~ U(0, θ)", "True population X_(n) ~ U(0, θ)")
        df_u <- data.frame(
          maximo = c(d$max_np, d$max_real),
          origem = factor(
            c(rep(rot_np, length(d$max_np)), rep(rot_real, length(d$max_real))),
            levels = c(rot_np, rot_real)
          )
        )
        ggplot2::ggplot(df_u, ggplot2::aes(x = maximo, fill = origem)) +
          ggplot2::geom_histogram(
            ggplot2::aes(y = ggplot2::after_stat(density)),
            position = "identity", bins = 35, alpha = 0.45, colour = p$fundo
          ) +
          ggplot2::geom_vline(xintercept = d$x_max, colour = cores_app$destaque, linewidth = 1.05) +
          ggplot2::geom_vline(xintercept = d$theta, colour = cores_app$referencia, linetype = "dashed", linewidth = 1.05) +
          ggplot2::scale_fill_manual(values = c(cores_app$destaque, cores_app$referencia)) +
          ggplot2::scale_x_continuous(labels = rotulos_eixo()) +
          ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
          tema_grafico(modo()) +
          ggplot2::theme(legend.position = "top") +
          ggplot2::labs(
            title = tr("Pico discreto de X*_(n) contra a distribuição real de X_(n)", "Discrete spike of X*_(n) vs. true distribution of X_(n)"),
            subtitle = sprintf(
              tr("Átomo em X_(n) = %s concentra %s das reamostras; nenhuma alcança θ = %s", "Atom at X_(n) = %s holds %s of resamples; none reaches θ = %s"),
              fmt_num(d$x_max, 2), fmt_pct(d$massa_atomo_emp, 1), fmt_num(d$theta, 1)
            ),
            x = tr("Máximo amostral", "Sample maximum"),
            y = tr("Densidade", "Density"), fill = NULL,
            caption = tr("Casella e Berger, seções 10.1.4, 10.6.2 e 10.6.3", "Casella and Berger, sections 10.1.4, 10.6.2 and 10.6.3")
          )
      }
    })

    output$tabela_alem <- shiny::renderUI({
      r <- res_alem()
      d <- r$dados

      if (identical(r$tipo, "delta")) {
        df <- data.frame(
          Metodo = c(
            tr("Variância verdadeira em p = p̂", "True variance at p = p̂"),
            tr("Bootstrap não paramétrico Var*_B", "Nonparametric bootstrap Var*_B"),
            tr("Método Delta de 1ª ordem", "1st-order Delta Method"),
            tr("Termo quadrático de 2ª ordem", "2nd-order quadratic term")
          ),
          Var = c(
            fmt_num(d$var_verdadeira, 5),
            fmt_num(d$var_boot, 5),
            fmt_num(d$var_delta1, 5),
            fmt_num(d$var_delta2, 5)
          ),
          EP = c(
            fmt_num(sqrt(d$var_verdadeira), 4),
            fmt_num(sqrt(d$var_boot), 4),
            fmt_num(sqrt(d$var_delta1), 4),
            fmt_num(sqrt(d$var_delta2), 4)
          ),
          stringsAsFactors = FALSE
        )
        names(df) <- c(tr("Avaliação", "Evaluation"), tr("Variância", "Variance"), tr("Erro padrão", "Std. error"))
        return(tabela_html(df, c("", "num", "num")))
      }

      if (identical(r$tipo, "parametrico")) {
        df <- data.frame(
          Metodo = c(
            tr("Verdadeira Var(S²) na população", "True population Var(S²)"),
            tr("Bootstrap paramétrico N(x̄, s²)", "Parametric bootstrap N(x̄, s²)"),
            tr("Bootstrap não paramétrico F̂_n", "Nonparametric bootstrap F̂_n"),
            tr("Fórmula normal plug-in 2(s²)²/(n−1)", "Normal plug-in 2(s²)²/(n−1)")
          ),
          Valor = c(
            fmt_num(d$var_real_s2, 2),
            fmt_num(d$var_param, 2),
            fmt_num(d$var_np, 2),
            fmt_num(d$var_formula_normal, 2)
          ),
          stringsAsFactors = FALSE
        )
        names(df) <- c(tr("Estimador de Var(S²)", "Estimator of Var(S²)"), tr("Valor", "Value"))
        return(tabela_html(df, c("", "num")))
      }

      df <- data.frame(
        Item = c(
          tr("Parâmetro verdadeiro θ", "True parameter θ"),
          tr("Máximo observado X_(n)", "Observed maximum X_(n)"),
          tr("Massa em X*_(n) = X_(n) (emp. / teór.)", "Mass at X*_(n) = X_(n) (emp. / theor.)"),
          tr("IC 95% percentil não paramétrico", "Nonparametric 95% percentile CI"),
          tr("EP real vs. EP*_NP vs. EP*_Param", "True SE vs. NP SE* vs. Param SE*")
        ),
        Valor = c(
          fmt_num(d$theta, 2),
          fmt_num(d$x_max, 3),
          sprintf("%s / %s", fmt_pct(d$massa_atomo_emp, 1), fmt_pct(d$massa_atomo_teo, 1)),
          sprintf("[%s; %s]", fmt_num(d$ic_perc_np[1], 2), fmt_num(d$ic_perc_np[2], 2)),
          sprintf("%s | %s | %s", fmt_num(d$ep_real, 3), fmt_num(d$ep_np, 3), fmt_num(d$ep_param, 3))
        ),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Quantidade", "Quantity"), tr("Resultado", "Result"))
      tabela_html(df, c("", "num"))
    })

    output$leitura_alem <- shiny::renderUI({
      r <- res_alem()
      d <- r$dados
      if (identical(r$tipo, "delta")) {
        if (abs(d$phat - 0.5) < 1e-9) {
          return(shiny::p(class = "mb-0", sprintf(
            tr(
              "Exatamente em p̂ = 0,50, a derivada 1 − 2p̂ se anula e o Método Delta de 1ª ordem entrega variância zero, enquanto o bootstrap estima %s (verdadeira = %s).",
              "Right at p̂ = 0.50, the derivative 1 − 2p̂ vanishes and 1st-order Delta returns zero variance, whereas the bootstrap estimates %s (true = %s)."
            ),
            fmt_num(d$var_boot, 5), fmt_num(d$var_verdadeira, 5)
          )))
        }
        return(shiny::p(class = "mb-0", sprintf(
          tr(
            "Em p̂ = %s (τ̂ = %s), o bootstrap entrega Var*_B = %s contra %s do Método Delta de 1ª ordem.",
            "At p̂ = %s (τ̂ = %s), the bootstrap yields Var*_B = %s versus %s from 1st-order Delta."
          ),
          fmt_num(d$phat, 2), fmt_num(d$tau_hat, 3), fmt_num(d$var_boot, 5), fmt_num(d$var_delta1, 5)
        )))
      }
      if (identical(r$tipo, "parametrico")) {
        if (identical(d$fonte, "t5")) {
          return(shiny::p(class = "mb-0", tr(
            "Sob caudas pesadas t₅, Var(S²) verdadeira explode por causa do quarto momento central, mas tanto o bootstrap paramétrico normal quanto 2(s²)²/(n−1) assumem curtose normal e subestimam o risco.",
            "Under heavy-tailed t₅ draws, true Var(S²) is inflated by the fourth central moment, yet both normal parametric bootstrap and 2(s²)²/(n−1) assume zero excess kurtosis and understate variance."
          )))
        }
        return(shiny::p(class = "mb-0", sprintf(
          tr(
            "Na amostra atual (x̄ = %s, s² = %s), o bootstrap paramétrico N(x̄, s²) estima Var*_B(S²) = %s frente à variância verdadeira %s.",
            "In the current sample (x̄ = %s, s² = %s), normal parametric bootstrap estimates Var*_B(S²) = %s against true variance %s."
          ),
          fmt_num(d$xbar, 2), fmt_num(d$s2, 2), fmt_num(d$var_param, 2), fmt_num(d$var_real_s2, 2)
        )))
      }
      shiny::p(class = "mb-0", sprintf(
        tr(
          "Como X_(n) = %s é estritamente menor que θ = %s, o intervalo percentil [%s; %s] termina em X_(n) e falha em cobrir θ em 100%% das amostras contínuas.",
          "Because X_(n) = %s is strictly below θ = %s, the percentile interval [%s, %s] stops at X_(n) and misses θ in 100%% of continuous samples."
        ),
        fmt_num(d$x_max, 3), fmt_num(d$theta, 2), fmt_num(d$ic_perc_np[1], 2), fmt_num(d$ic_perc_np[2], 2)
      ))
    })
  })
}
