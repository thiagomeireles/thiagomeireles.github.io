# =============================================================================
# Script:  mod_alem_limites.R (módulo Para ir além: modos de convergência e
#          quebra da LGN e do TCL sob caudas sem momentos finitos)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-25
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/apps/teoremas-limite/app.R)
# Saída:
#   - funções mod_alem_limites_ui() e mod_alem_limites_server()
# =============================================================================

regimes_cauda <- list(
  normal = list(
    rotulo = tr("Normal padrão N(0, 1) — E|X| < ∞ e Var(X) = 1", "Standard normal N(0, 1) — E|X| < ∞ and Var(X) = 1"),
    lgn = tr("Vale (X̄ₙ → 0)", "Holds (X̄ₙ → 0)"),
    tcl = tr("Vale (√n X̄ₙ → N(0, 1))", "Holds (√n X̄ₙ → N(0, 1))"),
    gerar = function(n) stats::rnorm(n)
  ),
  t2 = list(
    rotulo = tr("t de Student com 2 gl — E(X) = 0, mas Var(X) = ∞", "Student's t with 2 df — E(X) = 0, but Var(X) = ∞"),
    lgn = tr("Vale (X̄ₙ → 0, mais devagar)", "Holds (X̄ₙ → 0, more slowly)"),
    tcl = tr("Falha na escala √n (caudas pesadas)", "Fails at √n scale (heavy tails)"),
    gerar = function(n) stats::rt(n, df = 2)
  ),
  cauchy = list(
    rotulo = tr("Cauchy padrão (t com 1 gl) — sem esperança nem variância", "Standard Cauchy (t with 1 df) — neither mean nor variance"),
    lgn = tr("Falha: X̄ₙ tem a mesma distribuição de X₁!", "Fails: X̄ₙ has the exact same distribution as X₁!"),
    tcl = tr("Falha: √n X̄ₙ explode", "Fails: √n X̄ₙ blows up"),
    gerar = function(n) stats::rcauchy(n)
  )
)

quadro_convergencias <- function() {
  tabela <- data.frame(
    tr(
      c("Quase certa (Xₙ → q.c. X)", "Em probabilidade (Xₙ → p X / plim)", "Em distribuição (Xₙ → d X)"),
      c("Almost sure (Xₙ → a.s. X)", "In probability (Xₙ → p X / plim)", "In distribution (Xₙ → d X)")
    ),
    tr(
      c("P(lim Xₙ = X) = 1", "Para todo ε > 0, P(|Xₙ − X| ≥ ε) → 0", "F_Xₙ(x) → F_X(x) onde F_X é contínua"),
      c("P(lim Xₙ = X) = 1", "For every ε > 0, P(|Xₙ − X| ≥ ε) → 0", "F_Xₙ(x) → F_X(x) where F_X is continuous")
    ),
    tr(
      c(
        "Lei Forte dos Grandes Números (Teo. 5.5.14): cada trajetória inteira se fixa em μ",
        "Lei Fraca (Teo. 5.5.9) e consistência de estimadores (Wooldridge, ap. C.3)",
        "Teorema Central do Limite (Teo. 5.5.15): converge a curva de probabilidades, não os valores para uma constante"
      ),
      c(
        "Strong Law of Large Numbers (Thm. 5.5.14): entire sample paths settle at μ",
        "Weak Law (Thm. 5.5.9) and consistency of estimators (Wooldridge, app. C.3)",
        "Central Limit Theorem (Thm. 5.5.15): the CDFs converge, not the random variables to a constant"
      )
    )
  )
  names(tabela) <- tr(
    c("Modo de convergência", "Definição formal", "Onde aparece"),
    c("Mode of convergence", "Formal definition", "Where it shows up")
  )
  tabela_html(tabela)
}

texto_alem_limites <- function() {
  tr(
    shiny::withMathJax(
      shiny::p(
        "Dizer que uma sequência de estatísticas \"converge quando \\(n \\to \\infty\\)\" esconde três conceitos",
        "distintos (Casella e Berger, seção 5.5; Wooldridge, apêndice C). Na Lei dos Grandes Números,",
        "\\(\\bar{X}_n\\) aproxima-se de um número fixo \\(\\mu\\) — tanto em probabilidade (Lei Fraca, \\(P(|\\bar{X}_n - \\mu| \\ge \\varepsilon) \\to 0\\))",
        "quanto quase certamente (Lei Forte, \\(P(\\lim \\bar{X}_n = \\mu) = 1\\)). Já no Teorema Central do Limite,",
        "\\(Z_n = \\sqrt{n}(\\bar{X}_n - \\mu)/\\sigma\\) não estaciona em número algum: continua variando de amostra",
        "para amostra, e o que converge é apenas a sua função de distribuição acumulada \\(F_{Z_n}(z) \\to \\Phi(z)\\)",
        "(convergência em distribuição). Vale sempre a hierarquia: quase certa \\(\\Rightarrow\\) em probabilidade",
        "\\(\\Rightarrow\\) em distribuição; a volta da última para a segunda só vale quando o limite é uma constante",
        "(Teo. 5.5.4)."
      ),
      shiny::p(
        "Ambos os teoremas apoiam-se em hipóteses sobre os momentos da população, e não são automáticos. Na",
        "distribuição de Cauchy padrão (igual à \\(t\\) com 1 grau de liberdade, densidade \\(\\frac{1}{\\pi(1 + x^2)}\\)),",
        "as caudas decaem tão devagar que \\(E|X| = \\infty\\). Pela função característica, a média de \\(n\\)",
        "observações independentes de Cauchy tem exatamente a mesma distribuição Cauchy padrão de uma única",
        "observação: \\(\\bar{X}_n \\sim \\text{Cauchy}(0, 1)\\) para todo \\(n\\)! Em outras palavras, acumular",
        "mil observações não traz ganho algum de precisão para a média amostral — vez após vez surge um salto extremo",
        "da ordem de \\(n\\) que desfaz toda a estabilização anterior (enquanto a mediana amostral, por depender só",
        "das estatísticas de ordem centrais, converge normalmente para 0)."
      ),
      shiny::p(
        "Num caso intermediário, a \\(t\\) com 2 graus de liberdade tem \\(E(X) = 0\\) finito, mas \\(\\text{Var}(X) = \\infty\\):",
        "a Lei dos Grandes Números continua valendo (\\(\\bar{X}_n \\xrightarrow{p} 0\\)), porém o reescalonamento",
        "clássico por \\(\\sqrt{n}\\) do TCL falha e mantém caudas pesadas."
      ),
      shiny::p(
        shiny::strong("Experimente:"),
        "selecione a Cauchy padrão: na trajetória acumulada da esquerda, veja os saltos abruptos mesmo perto de",
        "\\(n = 2.000\\), enquanto a mediana acumulada (linha azul-esverdeada) crava em 0; à direita, confirme que",
        "a distância interquartil de \\(\\bar{X}_n\\) fica perto de 2,0 tanto em \\(n = 5\\) quanto em \\(n = 200\\)."
      )
    ),
    shiny::withMathJax(
      shiny::p(
        "Saying that a sequence of statistics \"converges as \\(n \\to \\infty\\)\" covers three distinct notions",
        "(Casella and Berger, section 5.5; Wooldridge, appendix C). In the Law of Large Numbers, \\(\\bar{X}_n\\)",
        "approaches a fixed constant \\(\\mu\\) — both in probability (Weak Law,",
        "\\(P(|\\bar{X}_n - \\mu| \\ge \\varepsilon) \\to 0\\)) and almost surely (Strong Law,",
        "\\(P(\\lim \\bar{X}_n = \\mu) = 1\\)). In the Central Limit Theorem, by contrast,",
        "\\(Z_n = \\sqrt{n}(\\bar{X}_n - \\mu)/\\sigma\\) never settles at a number: it keeps fluctuating from sample",
        "to sample, and what converges is its cumulative distribution function \\(F_{Z_n}(z) \\to \\Phi(z)\\)",
        "(convergence in distribution). The hierarchy always runs: almost sure \\(\\Rightarrow\\) in probability",
        "\\(\\Rightarrow\\) in distribution, with the last arrow reversing only when the limit is a constant (Thm. 5.5.4)."
      ),
      shiny::p(
        "Both theorems rest on moment assumptions rather than holding unconditionally. Under the standard Cauchy",
        "distribution (Student's \\(t\\) with 1 df, density \\(\\frac{1}{\\pi(1 + x^2)}\\)), the tails decay so",
        "slowly that \\(E|X| = \\infty\\). Via characteristic functions, the sample mean of \\(n\\) independent",
        "Cauchy draws has the exact same standard Cauchy distribution as a single observation:",
        "\\(\\bar{X}_n \\sim \\text{Cauchy}(0, 1)\\) for every \\(n\\)! Put differently, averaging a thousand",
        "observations buys zero precision for the sample mean — occasional spikes of order \\(n\\) abruptly reset the",
        "running average (whereas the sample median, relying only on middle order statistics, converges smoothly to 0)."
      ),
      shiny::p(
        "As an intermediate case, Student's \\(t\\) with 2 df has finite \\(E(X) = 0\\) but \\(\\text{Var}(X) = \\infty\\):",
        "the Law of Large Numbers still holds (\\(\\bar{X}_n \\xrightarrow{p} 0\\)), yet the classical \\(\\sqrt{n}\\)",
        "scaling of the CLT fails and leaves heavy tails."
      ),
      shiny::p(
        shiny::strong("Try it:"),
        "choose the standard Cauchy: on the left, watch the running mean jump abruptly even near \\(n = 2{,}000\\)",
        "while the running median (teal curve) locks onto 0; on the right, confirm that the IQR of \\(\\bar{X}_n\\)",
        "stays near 2.0 whether \\(n = 5\\) or \\(n = 200\\)."
      )
    )
  )
}

mod_alem_limites_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::selectInput(
        ns("regime"), tr("População simétrica em torno de 0", "Population symmetric around 0"),
        choices = stats::setNames(names(regimes_cauda), purrr::map_chr(regimes_cauda, "rotulo")),
        selected = "cauchy"
      ),
      shiny::sliderInput(ns("n_max"), tr("Comprimento da trajetória (n)", "Path length (n)"), min = 200, max = 3000, value = 1500, step = 100, sep = sep_milhar()),
      shiny::sliderInput(ns("n_comp"), tr("Tamanho da amostra na distribuição de X̄ₙ", "Sample size n for the distribution of X̄ₙ"), min = 5, max = 200, value = 50, step = 5),
      shiny::actionButton(ns("sortear"), tr("Simular novamente", "Simulate again"), class = "btn-primary", icon = shiny::icon("dice"))
    ),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(
        title = tr("Lei dos Grandes Números", "Law of Large Numbers"),
        value = shiny::textOutput(ns("status_lgn")),
        shiny::textOutput(ns("final_media_md")),
        theme = "primary"
      ),
      bslib::value_box(
        title = tr("Teorema Central do Limite", "Central Limit Theorem"),
        value = shiny::textOutput(ns("status_tcl")),
        shiny::textOutput(ns("iqr_comparacao")),
        theme = "info"
      )
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico_traj"), height = "410px"))),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico_dist"), height = "410px")))
    ),
    bslib::card(
      bslib::card_header(tr("Os três modos de convergência estocástica", "The three modes of stochastic convergence")),
      bslib::card_body(quadro_convergencias())
    ),
    bslib::card(bslib::card_header(rotulo_entenda()), bslib::card_body(texto_alem_limites()))
  )
}

mod_alem_limites_server <- function(id, modo = shiny::reactive("light")) {
  shiny::moduleServer(id, function(input, output, session) {

    reg <- shiny::reactive(regimes_cauda[[input$regime]])

    dados_sim <- shiny::reactive({
      input$sortear
      r <- reg()
      x <- r$gerar(input$n_max)
      pts <- unique(round(seq(1, input$n_max, length.out = 300)))
      media_acum <- (cumsum(x) / seq_along(x))[pts]
      mediana_acum <- purrr::map_dbl(pts, \(m) stats::median(x[seq_len(m)]))

      k <- 1000L
      medias_n5 <- rowMeans(matrix(r$gerar(5L * k), nrow = k))
      medias_n <- rowMeans(matrix(r$gerar(input$n_comp * k), nrow = k))

      list(
        pts = pts, media_acum = media_acum, mediana_acum = mediana_acum,
        medias_n5 = medias_n5, medias_n = medias_n
      )
    })

    output$status_lgn <- shiny::renderText(reg()$lgn)
    output$final_media_md <- shiny::renderText({
      d <- dados_sim()
      sprintf(
        tr("Em n = %s: x̄ = %s | mediana = %s", "At n = %s: x̄ = %s | median = %s"),
        fmt_num(input$n_max, 0),
        fmt_num(utils::tail(d$media_acum, 1), 2),
        fmt_num(utils::tail(d$mediana_acum, 1), 2)
      )
    })

    output$status_tcl <- shiny::renderText(reg()$tcl)
    output$iqr_comparacao <- shiny::renderText({
      d <- dados_sim()
      iqr5 <- diff(stats::quantile(d$medias_n5, c(0.25, 0.75), names = FALSE))
      iqrn <- diff(stats::quantile(d$medias_n, c(0.25, 0.75), names = FALSE))
      sprintf(
        tr("d_q de X̄: %s (n = 5) → %s (n = %d)", "IQR of X̄: %s (n = 5) → %s (n = %d)"),
        fmt_num(iqr5, 2), fmt_num(iqrn, 2), input$n_comp
      )
    })

    output$grafico_traj <- shiny::renderPlot({
      pal <- paleta_modo(modo())
      d <- dados_sim()
      nomes <- tr(c("Média acumulada (X̄ₙ)", "Mediana acumulada"), c("Running mean (X̄ₙ)", "Running median"))
      df <- data.frame(
        n = rep(d$pts, 2),
        valor = c(d$media_acum, d$mediana_acum),
        serie = factor(rep(nomes, each = length(d$pts)), levels = nomes)
      )

      ggplot2::ggplot(df, ggplot2::aes(x = n, y = valor, colour = serie)) +
        ggplot2::geom_hline(yintercept = 0, colour = pal$linha, linetype = "dashed") +
        ggplot2::geom_line(linewidth = 0.95) +
        ggplot2::scale_colour_manual(values = c(cores_app$referencia, "#2fb380"), name = NULL) +
        ggplot2::scale_x_continuous(labels = rotulos_eixo()) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = tr("Trajetória da média contra a da mediana", "Running mean versus running median"),
          subtitle = reg()$rotulo,
          x = "n", y = tr("Valor acumulado", "Running value"),
          caption = tr("Na Cauchy, um único sorteio extremo faz X̄ₙ saltar em qualquer n", "Under Cauchy, a single extreme draw makes X̄ₙ jump at any n")
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(legend.position = "top", legend.justification = "left")
    })

    output$grafico_dist <- shiny::renderPlot({
      pal <- paleta_modo(modo())
      d <- dados_sim()
      rot5 <- "n = 5"
      rotn <- sprintf("n = %d", input$n_comp)
      df <- rbind(
        data.frame(x = d$medias_n5, grupo = rot5),
        data.frame(x = d$medias_n, grupo = rotn)
      )
      df$grupo <- factor(df$grupo, levels = c(rot5, rotn))
      lim <- max(1.5, min(8, stats::quantile(abs(df$x), 0.94)))
      df_vis <- df[abs(df$x) <= lim, ]

      ggplot2::ggplot(df_vis, ggplot2::aes(x = x, fill = grupo, colour = grupo)) +
        ggplot2::geom_density(alpha = 0.35, linewidth = 1) +
        ggplot2::scale_fill_manual(values = c(pal$neutro, cores_app$destaque), name = NULL) +
        ggplot2::scale_colour_manual(values = c(pal$secundario, cores_app$destaque), name = NULL) +
        ggplot2::coord_cartesian(xlim = c(-lim, lim)) +
        ggplot2::scale_x_continuous(labels = rotulos_eixo()) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = tr("A distribuição de X̄ₙ estreita com n?", "Does the distribution of X̄ₙ narrow with n?"),
          subtitle = tr("Comparação entre n = 5 e o n escolhido (1.000 amostras)", "Comparison between n = 5 and the chosen n (1,000 samples)"),
          x = tr("Média amostral (X̄ₙ)", "Sample mean (X̄ₙ)"), y = tr("Densidade empírica", "Empirical density"),
          caption = tr("Na normal e na t(2) a curva afunila em 0; na Cauchy ela não sai do lugar", "Under normal and t(2) the curve concentrates at 0; under Cauchy it stays put")
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(legend.position = "top", legend.justification = "left")
    })
  })
}
