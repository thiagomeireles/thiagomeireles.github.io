# =============================================================================
# Script:  mod_bootstrap.R (Aba 3: reamostragem bootstrap não paramétrica,
#          erro padrão e intervalo percentil)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - objetos e funções de _app_scripts/apps/simulacao-bootstrap/modulos/dados.R
# Saída:
#   - funções mod_bootstrap_ui() e mod_bootstrap_server()
# =============================================================================

texto_bootstrap <- function() {
  tr(
    shiny::withMathJax(
      shiny::p(
        "Na prática empírica, não podemos voltar à população \\(F\\) para sortear centenas de novas amostras e medir",
        "a variabilidade de um estimador \\(\\hat{\\theta}\\). A ideia central do ", shiny::em("bootstrap"),
        " não paramétrico (Efron, 1979; Morettin e Bussab, seção 11.7; Casella e Berger, seção 10.1.4) consiste em",
        "tratar a distribuição empírica \\(\\hat{F}_n\\) — que atribui peso \\(1/n\\) a cada observação",
        "\\(x_1, \\dots, x_n\\) — como substituta da população: cada reamostra \\(x_1^*, \\dots, x_n^*\\) sorteia",
        "\\(n\\) valores ", shiny::strong("com reposição"), " dos próprios dados observados, repetindo alguns pontos",
        "e deixando em média \\((1 - 1/n)^n \\approx e^{-1} \\approx 36{,}8\\%\\) deles de fora."
      ),
      shiny::p(
        "Em amostras minúsculas como \\(\\{2, 4, 9, 12\\}\\) (Casella e Berger, Exs. 1.2.20 e 10.1.19), dá para",
        "listar todas as \\(4^4 = 256\\) reamostras ordenadas e obter a variância ", shiny::em("bootstrap"),
        " exata \\(\\text{Var}^*(\\bar{X}) = 3{,}94\\) (eq. 10.1.9). No entanto, como \\(n^n\\) explode rapidamente",
        "— já em \\(n = 36\\) há \\(36^{36} \\approx 1{,}06 \\times 10^{56}\\) sequências —, sorteiam-se \\(B\\)",
        "reamostras de Monte Carlo para aproximar o erro padrão por",
        "\\(\\text{EP}_B^*(\\hat{\\theta}) = \\sqrt{(B - 1)^{-1} \\sum_{b=1}^{B} (\\hat{\\theta}_b^* - \\bar{\\theta}^*)^2}\\)",
        "(eq. 10.1.11) e construir o intervalo pelo método percentil entre os quantis \\(\\alpha/2\\) e",
        "\\(1 - \\alpha/2\\) das estimativas reamostradas (Agresti e Finlay, seção 5.5), valendo para mediana,",
        "desvio padrão ou coeficiente de variação sem exigir derivação analítica."
      ),
      shiny::p(
        shiny::strong("Experimente:"),
        "escolha a renda simulada assimétrica, compare os botões \"Nova amostra da população\" e \"Novas B reamostras\"",
        "e suba \\(B\\) de \\(100\\) para \\(3.000\\). Aumentar \\(B\\) estabiliza a forma do histograma ao redor da",
        "estimativa \\(\\hat{\\theta}\\) da amostra atual, mas só um \\(n\\) maior aproxima \\(\\hat{\\theta}\\) do",
        "parâmetro populacional verdadeiro."
      )
    ),
    shiny::withMathJax(
      shiny::p(
        "In empirical work, we cannot go back to the population \\(F\\) to draw hundreds of fresh samples and",
        "observe how much an estimator \\(\\hat{\\theta}\\) varies. The core idea of the nonparametric bootstrap",
        "(Efron, 1979; Morettin and Bussab, section 11.7; Casella and Berger, section 10.1.4) is to treat the",
        "empirical distribution \\(\\hat{F}_n\\) — which places weight \\(1/n\\) on each observed point",
        "\\(x_1, \\dots, x_n\\) — as a stand-in for the population: each bootstrap resample \\(x_1^*, \\dots, x_n^*\\)",
        "draws \\(n\\) values ", shiny::strong("with replacement"), " from the observed data, duplicating some points",
        "while leaving out an average of \\((1 - 1/n)^n \\approx e^{-1} \\approx 36.8\\%\\) of them."
      ),
      shiny::p(
        "For a tiny dataset such as \\(\\{2, 4, 9, 12\\}\\) (Casella and Berger, Exs. 1.2.20 and 10.1.19), one can",
        "enumerate all \\(4^4 = 256\\) ordered resamples and compute the exact bootstrap variance",
        "\\(\\text{Var}^*(\\bar{X}) = 3.94\\) (eq. 10.1.9). Because \\(n^n\\) grows explosively — already",
        "\\(36^{36} \\approx 1.06 \\times 10^{56}\\) at \\(n = 36\\) — we instead draw \\(B\\) Monte Carlo resamples",
        "to approximate the standard error via",
        "\\(\\text{SE}_B^*(\\hat{\\theta}) = \\sqrt{(B - 1)^{-1} \\sum_{b=1}^{B} (\\hat{\\theta}_b^* - \\bar{\\theta}^*)^2}\\)",
        "(eq. 10.1.11) and form the percentile confidence interval from the \\(\\alpha/2\\) and \\(1 - \\alpha/2\\)",
        "quantiles of \\(\\hat{\\theta}_b^*\\) (Agresti and Finlay, section 5.5), applying the exact same recipe to",
        "medians, standard deviations or coefficients of variation."
      ),
      shiny::p(
        shiny::strong("Try it:"),
        "select simulated right-skewed income, contrast \"Draw new sample from population\" with \"Draw fresh B resamples\"",
        "and raise \\(B\\) from \\(100\\) to \\(3,000\\). Increasing \\(B\\) stabilizes the histogram around the",
        "current sample's \\(\\hat{\\theta}\\), whereas only a larger \\(n\\) pulls \\(\\hat{\\theta}\\) closer to",
        "the true population parameter."
      )
    )
  )
}

cuidados_bootstrap <- function() {
  tr(
    c(
      "A distribuição bootstrap centra em θ̂, não em θ" = paste(
        "reamostrar os mesmos n dados não acrescenta informação nova sobre a população nem corrige viés de seleção:",
        "se a amostra original for atípica ou enviesada, a distribuição bootstrap se concentrará no lugar errado."
      ),
      "Duas camadas de aproximação (B → ∞ vs. n → ∞)" = paste(
        "como destacam Casella e Berger (seção 10.1.4), EP*_B converge a EP* quando B → ∞ dentro da amostra fixa",
        "(aproximador de Monte Carlo), mas EP* só converge ao verdadeiro EP(θ̂) quando n → ∞ em amostragem iid."
      ),
      "Sem reposição não há bootstrap" = paste(
        "sortear n observações sem reposição de uma amostra de tamanho n apenas embaralha os mesmos valores e devolve",
        "exatamente o mesmo θ̂ em todas as réplicas, produzindo erro padrão zero."
      )
    ),
    c(
      "The bootstrap distribution centers at θ̂, not θ" = paste(
        "resampling the same n observations adds no new information about the population and cannot fix sampling",
        "bias: if the original sample is atypical or biased, the bootstrap distribution centers at the wrong value."
      ),
      "Two distinct limits (B → ∞ vs. n → ∞)" = paste(
        "as Casella and Berger (section 10.1.4) emphasize, SE*_B converges to SE* as B → ∞ given the fixed sample",
        "(Monte Carlo approximator), whereas SE* converges to the true SE(θ̂) only as n → ∞ under iid sampling."
      ),
      "Without replacement there is no bootstrap" = paste(
        "drawing n items without replacement from a sample of size n merely permutes the exact same numbers and",
        "returns the identical θ̂ every time, yielding a standard error of zero."
      )
    )
  )
}

rotulo_estat_boot <- function(estat) {
  switch(estat,
    media   = tr("Média (x̄)", "Mean (x̄)"),
    mediana = tr("Mediana (md)", "Median (md)"),
    dp      = tr("Desvio padrão (s)", "Standard deviation (s)"),
    cv      = tr("Coeficiente de variação (s / x̄)", "Coefficient of variation (s / x̄)")
  )
}

mod_bootstrap_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::selectInput(
        ns("conjunto"), tr("Amostra observada", "Observed sample"),
        choices = stats::setNames(names(conjuntos_bootstrap), purrr::map_chr(conjuntos_bootstrap, "rotulo")),
        selected = "livros"
      ),
      shiny::uiOutput(ns("controles_amostra")),
      shiny::selectInput(
        ns("estat"), tr("Estatística de interesse θ̂", "Target statistic θ̂"),
        choices = stats::setNames(
          c("mediana", "media", "dp", "cv"),
          c(
            tr("Mediana (md)", "Median (md)"),
            tr("Média (x̄)", "Mean (x̄)"),
            tr("Desvio padrão amostral (s)", "Sample standard deviation (s)"),
            tr("Coeficiente de variação (s / x̄)", "Coefficient of variation (s / x̄)")
          )
        )
      ),
      shiny::hr(),
      shiny::sliderInput(
        ns("b_rep"), tr("Número de reamostras bootstrap (B)", "Number of bootstrap resamples (B)"),
        min = 100, max = 4000, value = 1000, step = 100, sep = sep_milhar()
      ),
      shiny::selectInput(
        ns("gama"), tr("Nível de confiança (γ)", "Confidence level (γ)"),
        choices = c("90%" = "0.90", "95%" = "0.95", "99%" = "0.99"),
        selected = "0.95"
      ),
      shiny::sliderInput(
        ns("idx_ver"), tr("Inspecionar reamostra individual nº", "Inspect individual resample #"),
        min = 1, max = 20, value = 1, step = 1
      ),
      shiny::actionButton(
        ns("reamostrar"), tr("Novas B reamostras (mesma amostra)", "Draw fresh B resamples (same sample)"),
        class = "btn-primary", icon = shiny::icon("rotate")
      )
    ),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(
        title = tr("Estimativa na amostra original (θ̂)", "Estimate in original sample (θ̂)"),
        value = shiny::textOutput(ns("vb_theta")),
        shiny::textOutput(ns("vb_theta_sub")),
        theme = "primary"
      ),
      bslib::value_box(
        title = tr("Erro padrão bootstrap EP*_B(θ̂)", "Bootstrap standard error SE*_B(θ̂)"),
        value = shiny::textOutput(ns("vb_ep")),
        shiny::textOutput(ns("vb_ep_sub")),
        theme = "info"
      ),
      bslib::value_box(
        title = tr("IC bootstrap percentil", "Percentile bootstrap CI"),
        value = shiny::textOutput(ns("vb_ic")),
        shiny::textOutput(ns("vb_ic_sub")),
        theme = NULL
      )
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(5, 7)),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico_reamostra"), height = "390px"))),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico_dist_boot"), height = "390px")))
    ),
    bslib::card(
      bslib::card_header(tr("Comparação de intervalos e erros padrão na amostra atual", "Comparison of intervals and standard errors on the current sample")),
      bslib::card_body(shiny::uiOutput(ns("tabela_comp")))
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
      bslib::card(bslib::card_header(rotulo_entenda()), bslib::card_body(texto_bootstrap())),
      cartao_cuidados(tr("Cuidados", "Watch out"), cuidados_bootstrap())
    )
  )
}

mod_bootstrap_server <- function(id, modo = shiny::reactive("light")) {
  shiny::moduleServer(id, function(input, output, session) {

    cfg_conj <- shiny::reactive({
      shiny::req(input$conjunto)
      conjuntos_bootstrap[[input$conjunto]]
    })

    output$controles_amostra <- shiny::renderUI({
      cfg <- cfg_conj()
      if (isTRUE(cfg$fixo)) {
        return(shiny::helpText(cfg$fonte))
      }
      shiny::tagList(
        shiny::sliderInput(
          session$ns("n_sim"), tr("Tamanho da amostra original (n)", "Original sample size (n)"),
          min = 12, max = 120, value = shiny::isolate(input$n_sim) %||% 35, step = 4
        ),
        shiny::actionButton(
          session$ns("nova_amostra"),
          tr("Nova amostra da população", "Draw new sample from population"),
          class = "btn-sm btn-outline-primary mb-2"
        )
      )
    })

    amostra_atual <- shiny::reactive({
      cfg <- cfg_conj()
      if (isTRUE(cfg$fixo)) return(cfg$gerar(NULL))
      input$nova_amostra
      n_val <- if (!is.null(input$n_sim) && is.finite(input$n_sim)) as.integer(input$n_sim) else 35L
      cfg$gerar(n_val)
    })

    boot_res <- shiny::reactive({
      input$reamostrar
      shiny::req(input$estat, is.finite(input$b_rep), input$gama)
      x <- amostra_atual()
      executar_bootstrap_np(
        x = x,
        estat = input$estat,
        b_rep = as.integer(input$b_rep),
        gama = as.numeric(input$gama)
      )
    })

    exato_cb <- shiny::reactive({
      if (!identical(input$conjunto, "cb1019")) return(NULL)
      enumerar_exato_cb1019(input$estat)
    })

    param_real <- shiny::reactive({
      cfg <- cfg_conj()
      if (is.null(cfg$param_verdadeiro)) return(NULL)
      cfg$param_verdadeiro(input$estat)
    })

    output$vb_theta <- shiny::renderText({
      r <- boot_res()
      fmt_num(r$theta_hat, 3)
    })

    output$vb_theta_sub <- shiny::renderText({
      r <- boot_res()
      pr <- param_real()
      if (!is.null(pr)) {
        sprintf(tr("Parâmetro populacional θ = %s (n = %d)", "True parameter θ = %s (n = %d)"), fmt_num(pr, 3), r$n)
      } else {
        sprintf("n = %d | %s", r$n, rotulo_estat_boot(r$estat))
      }
    })

    output$vb_ep <- shiny::renderText({
      r <- boot_res()
      fmt_num(r$ep_boot, 3)
    })

    output$vb_ep_sub <- shiny::renderText({
      r <- boot_res()
      ex <- exato_cb()
      if (!is.null(ex)) {
        sprintf(tr("Exato (256 reamostras): EP* = %s | Var* = %s", "Exact (256 resamples): SE* = %s | Var* = %s"),
                fmt_num(ex$ep_exato, 3), fmt_num(ex$var_exata, 2))
      } else {
        sprintf(tr("Var*_B = %s | Viés* = %s", "Var*_B = %s | Bias* = %s"),
                fmt_num(r$var_boot, 3), fmt_num(r$vies_boot, 3))
      }
    })

    output$vb_ic <- shiny::renderText({
      r <- boot_res()
      sprintf("[%s; %s]", fmt_num(r$ic_perc[1], 2), fmt_num(r$ic_perc[2], 2))
    })

    output$vb_ic_sub <- shiny::renderText({
      r <- boot_res()
      sprintf(tr("Nível %s (%s reamostras)", "%s level (%s resamples)"),
              fmt_pct(r$gama, 0), formatar_inteiro(r$b_rep))
    })

    output$grafico_reamostra <- shiny::renderPlot({
      r <- boot_res()
      p <- paleta_modo(modo())
      k <- min(max(1L, as.integer(input$idx_ver %||% 1L)), nrow(r$idx_guardados))
      idx_k <- r$idx_guardados[k, ]

      ord_pos <- order(r$x)
      x_ord <- r$x[ord_pos]
      mult <- tabulate(match(idx_k, ord_pos), nbins = r$n)
      prop_fora_emp <- mean(mult == 0L)

      grupo <- ifelse(mult == 0L, "fora", ifelse(mult == 1L, "uma", "rep"))
      df_pts <- data.frame(
        ordem = seq_len(r$n),
        valor = x_ord,
        mult = mult,
        grupo = factor(grupo, levels = c("fora", "uma", "rep"))
      )

      rotulos_leg <- stats::setNames(
        c(
          tr("Fora (0×)", "Omitted (0×)"),
          tr("Sorteada 1×", "Drawn 1×"),
          tr("Repetida (2× ou +)", "Repeated (2×+)")
        ),
        c("fora", "uma", "rep")
      )

      ggplot2::ggplot(df_pts, ggplot2::aes(x = ordem, y = valor)) +
        ggplot2::geom_segment(
          ggplot2::aes(xend = ordem, y = min(valor), yend = valor),
          colour = p$grade, linewidth = 0.45
        ) +
        ggplot2::geom_point(
          ggplot2::aes(colour = grupo, size = pmax(mult, 0.7))
        ) +
        ggplot2::scale_colour_manual(
          values = c(fora = p$neutro, uma = cores_app$destaque, rep = cores_app$referencia),
          labels = rotulos_leg,
          drop = FALSE
        ) +
        ggplot2::scale_size_continuous(range = c(2.0, 5.2), guide = "none") +
        ggplot2::scale_x_continuous(labels = rotulos_eixo()) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        tema_grafico(modo()) +
        ggplot2::theme(legend.position = "top") +
        ggplot2::labs(
          title = sprintf(tr("Reamostra individual nº %d", "Individual resample #%d"), k),
          subtitle = sprintf(
            tr("Fora desta reamostra: %s (teórico (1 − 1/n)ⁿ = %s)", "Left out of this resample: %s (theoretical (1 − 1/n)ⁿ = %s)"),
            fmt_pct(prop_fora_emp, 1), fmt_pct(r$prop_fora_teo, 1)
          ),
          x = tr("Índice na amostra ordenada (1 … n)", "Rank in ordered sample (1 … n)"),
          y = cfg_conj()$unidade,
          colour = NULL,
          caption = cfg_conj()$fonte
        )
    })

    output$grafico_dist_boot <- shiny::renderPlot({
      r <- boot_res()
      p <- paleta_modo(modo())
      pr <- param_real()
      df_b <- data.frame(est = r$est_boot)

      g <- ggplot2::ggplot(df_b, ggplot2::aes(x = est)) +
        ggplot2::annotate(
          "rect",
          xmin = r$ic_perc[1], xmax = r$ic_perc[2],
          ymin = -Inf, ymax = Inf,
          fill = cores_app$destaque, alpha = 0.12
        ) +
        ggplot2::geom_histogram(
          ggplot2::aes(y = ggplot2::after_stat(density)),
          bins = if (r$n <= 6L) 18 else 30,
          fill = cores_app$destaque, alpha = 0.42, colour = p$fundo
        ) +
        ggplot2::geom_vline(xintercept = r$ic_perc, colour = cores_app$destaque, linetype = "dotted", linewidth = 0.9) +
        ggplot2::geom_vline(xintercept = r$theta_hat, colour = p$linha, linewidth = 1.05)

      if (!is.null(pr)) {
        g <- g + ggplot2::geom_vline(xintercept = pr, colour = cores_app$referencia, linetype = "dashed", linewidth = 1.0)
      }

      sub_txt <- if (!is.null(pr)) {
        tr("Traço cheio: θ̂ da amostra | Tracejado vermelho: θ verdadeiro | Faixa: IC percentil", "Solid line: sample θ̂ | Dashed red: true θ | Band: percentile CI")
      } else {
        tr("Traço cheio: θ̂ da amostra | Pontilhado e faixa azul: limites do IC percentil", "Solid line: sample θ̂ | Dotted lines and blue band: percentile CI limits")
      }

      g +
        ggplot2::scale_x_continuous(labels = rotulos_eixo()) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        tema_grafico(modo()) +
        ggplot2::labs(
          title = tr("Distribuição das B estimativas θ̂*_b", "Distribution of the B bootstrap estimates θ̂*_b"),
          subtitle = sub_txt,
          x = rotulo_estat_boot(r$estat),
          y = tr("Densidade", "Density"),
          caption = tr("Efron (1979); Morettin e Bussab, seção 11.7; Agresti e Finlay, seção 5.5", "Efron (1979); Morettin and Bussab, section 11.7; Agresti and Finlay, section 5.5")
        )
    })

    output$tabela_comp <- shiny::renderUI({
      r <- boot_res()
      ex <- exato_cb()

      metodos <- c(
        tr("Bootstrap percentil (quantis de θ̂*)", "Percentile bootstrap (quantiles of θ̂*)"),
        tr("Bootstrap normal (θ̂ ± z · EP*_B)", "Normal bootstrap (θ̂ ± z · SE*_B)")
      )
      eps <- c(fmt_num(r$ep_boot, 3), fmt_num(r$ep_boot, 3))
      infs <- c(fmt_num(r$ic_perc[1], 2), fmt_num(r$ic_norm[1], 2))
      sups <- c(fmt_num(r$ic_perc[2], 2), fmt_num(r$ic_norm[2], 2))
      amps <- c(fmt_num(diff(r$ic_perc), 2), fmt_num(diff(r$ic_norm), 2))

      if (!is.null(r$ic_classico)) {
        cl <- r$ic_classico
        metodos <- c(metodos, cl$metodo)
        eps <- c(eps, if (is.na(cl$ep)) "—" else fmt_num(cl$ep, 3))
        infs <- c(infs, fmt_num(cl$inf, 2))
        sups <- c(sups, fmt_num(cl$sup, 2))
        amps <- c(amps, fmt_num(cl$sup - cl$inf, 2))
      }

      if (!is.null(ex)) {
        alfa <- 1 - r$gama
        q_ex <- stats::quantile(ex$valores, probs = c(alfa / 2, 1 - alfa / 2), names = FALSE)
        metodos <- c(metodos, tr("Bootstrap exato (todas as 256 reamostras)", "Exact bootstrap (all 256 resamples)"))
        eps <- c(eps, fmt_num(ex$ep_exato, 3))
        infs <- c(infs, fmt_num(q_ex[1], 2))
        sups <- c(sups, fmt_num(q_ex[2], 2))
        amps <- c(amps, fmt_num(diff(q_ex), 2))
      }

      df <- data.frame(Metodo = metodos, EP = eps, Inf_ic = infs, Sup_ic = sups, Amp = amps, stringsAsFactors = FALSE)
      names(df) <- c(
        tr("Procedimento", "Procedure"),
        tr("Erro padrão", "Standard error"),
        tr("Limite inferior", "Lower bound"),
        tr("Limite superior", "Upper bound"),
        tr("Largura", "Width")
      )
      tabela_html(df, c("", "num", "num", "num", "num"))
    })
  })
}
