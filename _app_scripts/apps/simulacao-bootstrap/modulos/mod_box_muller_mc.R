# =============================================================================
# Script:  mod_box_muller_mc.R (Aba 2: transformação de Box-Müller e integração
#          / verificação por Monte Carlo)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - objetos e funções de _app_scripts/apps/simulacao-bootstrap/modulos/dados.R
# Saída:
#   - funções mod_box_muller_mc_ui() e mod_box_muller_mc_server()
# =============================================================================

texto_box_muller_mc <- function() {
  tr(
    shiny::withMathJax(
      shiny::p(
        "Nem todo modelo contínuo permite isolar \\(F_X^{-1}(u)\\) numa fórmula elementar. A distribuição normal",
        "é o caso mais importante: como \\(\\Phi(z) = \\int_{-\\infty}^{z} (2\\pi)^{-1/2} e^{-t^2/2}\\,dt\\) não tem",
        "primitiva algébrica fechada, Box e Muller (1958) contornaram a inversão unidimensional transformando pares",
        "independentes \\((U_1, U_2)\\) em \\((0, 1)^2\\) via coordenadas polares (Morettin e Bussab, seção 9.3).",
        "Com raio \\(R = \\sqrt{-2 \\ln U_1}\\) e ângulo \\(\\Theta = 2\\pi U_2\\), os valores",
        "\\(Z_1 = R \\cos \\Theta\\) e \\(Z_2 = R \\sin \\Theta\\) são normais padrão exatas e independentes, e",
        "\\(X = \\mu + \\sigma Z_1\\) reproduz qualquer \\(N(\\mu, \\sigma^2)\\)."
      ),
      shiny::p(
        "Gerar amostras no computador abre caminho para o método de Monte Carlo (Morettin e Bussab, seção 9.4):",
        "qualquer probabilidade \\(p = P(X \\in A)\\) ou esperança \\(\\theta = E[g(X)]\\) é estimada pela média",
        "empírica \\(\\hat{\\theta}_M = M^{-1} \\sum_{m=1}^{M} g(X_m)\\) ao longo de \\(M\\) réplicas independentes.",
        "Pela Lei dos Grandes Números e pelo Teorema Central do Limite (Casella e Berger, seção 5.5),",
        "\\(\\hat{\\theta}_M\\) oscila ao redor de \\(\\theta\\) com erro padrão de Monte Carlo",
        "\\(\\text{EP}_{\\text{MC}} = s_g / \\sqrt{M}\\). Posto de outra forma, reduzir o ruído da simulação a um",
        "décimo exige multiplicar \\(M\\) por \\(100\\) — mas isso nos permite medir propriedades sem solução",
        "analítica simples, como a cobertura real do intervalo \\(t\\) sob assimetria forte."
      ),
      shiny::p(
        shiny::strong("Experimente:"),
        "selecione a cobertura do intervalo \\(t\\) sob \\(\\text{Exp}(\\beta)\\) com \\(n = 12\\) e compare \\(M = 200\\)",
        "a \\(M = 4.000\\). As trajetórias estabilizam perto de \\(90{,}2\\%\\), e não em \\(95\\%\\): aumentar \\(M\\)",
        "mede com precisão a subcobertura verdadeira de \\(n = 12\\), sem corrigi-la."
      )
    ),
    shiny::withMathJax(
      shiny::p(
        "Not every continuous model admits a closed-form elementary expression for \\(F_X^{-1}(u)\\). The normal",
        "distribution is the prime example: because \\(\\Phi(z) = \\int_{-\\infty}^{z} (2\\pi)^{-1/2} e^{-t^2/2}\\,dt\\)",
        "cannot be inverted algebraically, Box and Muller (1958) bypassed one-dimensional inversion by mapping",
        "independent pairs \\((U_1, U_2)\\) on \\((0, 1)^2\\) into polar coordinates (Morettin and Bussab, section 9.3).",
        "Setting \\(R = \\sqrt{-2 \\ln U_1}\\) and \\(\\Theta = 2\\pi U_2\\) produces two exact, independent standard",
        "normals \\(Z_1 = R \\cos \\Theta\\) and \\(Z_2 = R \\sin \\Theta\\), from which \\(X = \\mu + \\sigma Z_1\\)",
        "generates any \\(N(\\mu, \\sigma^2)\\)."
      ),
      shiny::p(
        "Once we can draw from a model, Monte Carlo integration (Morettin and Bussab, section 9.4) approximates any",
        "probability \\(p = P(X \\in A)\\) or expectation \\(\\theta = E[g(X)]\\) by the sample average",
        "\\(\\hat{\\theta}_M = M^{-1} \\sum_{m=1}^{M} g(X_m)\\) across \\(M\\) independent replications. By the Law of",
        "Large Numbers and the Central Limit Theorem (Casella and Berger, section 5.5), \\(\\hat{\\theta}_M\\)",
        "fluctuates around \\(\\theta\\) with Monte Carlo standard error \\(\\text{SE}_{\\text{MC}} = s_g / \\sqrt{M}\\).",
        "Put differently, cutting simulation noise by a factor of \\(10\\) requires \\(100\\) times as many",
        "replications — yet it lets us evaluate finite-sample properties that lack simple closed-form formulas, such",
        "as the actual coverage of Student's \\(t\\) interval under strong skewness."
      ),
      shiny::p(
        shiny::strong("Try it:"),
        "pick the \\(t\\)-interval coverage under \\(\\text{Exp}(\\beta)\\) with \\(n = 12\\) and compare \\(M = 200\\)",
        "against \\(M = 4,000\\). The paths settle near \\(90.2\\%\\) rather than \\(95\\%\\): raising \\(M\\)",
        "pins down the true finite-sample undercoverage at \\(n = 12\\) without fixing it."
      )
    )
  )
}

cuidados_box_muller_mc <- function() {
  tr(
    c(
      "Box-Müller é exato, não aproximação assintótica" = paste(
        "diferente de somar 12 uniformes pelo Teorema Central do Limite, Z₁ e Z₂ seguem N(0, 1) exatamente caso",
        "U₁ e U₂ sejam uniformes independentes em (0, 1) (Box e Muller, 1958; Morettin e Bussab, seção 9.3)."
      ),
      "Número de réplicas M ≠ tamanho da amostra n" = paste(
        "M reduz apenas a incerteza numérica do simulador (EP_MC = s_g / √M); no estudo do intervalo t com n = 12,",
        "subir M para 5.000 não muda n = 12 nem aproxima a cobertura de 95%."
      ),
      "Sempre reporte o erro de Monte Carlo" = paste(
        "com M = 100 e uma probabilidade perto de 0,5, 1,96 · EP_MC vale quase ±10 pontos percentuais; conclusões",
        "de simulação sem EP_MC confundem flutuação numérica com propriedade do estimador."
      )
    ),
    c(
      "Box-Muller is exact, not an asymptotic approximation" = paste(
        "unlike summing 12 uniforms via the Central Limit Theorem, Z₁ and Z₂ follow N(0, 1) exactly whenever",
        "U₁ and U₂ are independent uniforms on (0, 1) (Box and Muller, 1958; Morettin and Bussab, section 9.3)."
      ),
      "Monte Carlo replications M ≠ sample size n" = paste(
        "M only shrinks simulation noise (SE_MC = s_g / √M); in the n = 12 t-interval study, raising M to 5,000",
        "does not change n = 12 or push coverage toward 95%."
      ),
      "Always report the Monte Carlo standard error" = paste(
        "with M = 100 and a probability near 0.5, 1.96 · SE_MC is nearly ±10 percentage points; simulation claims",
        "without SE_MC mistake numerical noise for genuine statistical behavior."
      )
    )
  )
}

mod_box_muller_mc_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 315,
      shiny::selectInput(
        ns("cenario"), tr("Problema de Monte Carlo", "Monte Carlo problem"),
        choices = stats::setNames(
          c("box_muller", "pi_circulo", "cobertura_t"),
          c(
            tr("Box-Müller: cauda P(X > c) em N(μ, σ²)", "Box-Muller: tail P(X > c) in N(μ, σ²)"),
            tr("Integração geométrica: estimar π no círculo", "Geometric integration: estimate π in circle"),
            tr("Cobertura real do IC t sob Exp(β) (n = 12)", "True t-CI coverage under Exp(β) (n = 12)")
          )
        )
      ),
      shiny::uiOutput(ns("controles_cenario")),
      shiny::hr(),
      shiny::sliderInput(
        ns("m_rep"), tr("Réplicas de Monte Carlo (M)", "Monte Carlo replications (M)"),
        min = 100, max = 5000, value = 1000, step = 100, sep = sep_milhar()
      ),
      shiny::sliderInput(
        ns("n_traj"), tr("Trajetórias simultâneas", "Simultaneous paths"),
        min = 1, max = 8, value = 5, step = 1
      ),
      shiny::actionButton(
        ns("rodar"), tr("Sortear novas réplicas", "Draw fresh replications"),
        class = "btn-primary", icon = shiny::icon("play")
      )
    ),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(
        title = tr("Valor de referência θ", "Reference value θ"),
        value = shiny::textOutput(ns("vb_alvo")),
        shiny::textOutput(ns("vb_alvo_sub")),
        theme = "primary"
      ),
      bslib::value_box(
        title = tr("Estimativa de Monte Carlo θ̂_M (1ª traj.)", "Monte Carlo estimate θ̂_M (1st path)"),
        value = shiny::textOutput(ns("vb_est")),
        shiny::textOutput(ns("vb_est_sub")),
        theme = "info"
      ),
      bslib::value_box(
        title = tr("Erro padrão de Monte Carlo (EP_MC)", "Monte Carlo standard error (SE_MC)"),
        value = shiny::textOutput(ns("vb_ep")),
        shiny::textOutput(ns("vb_ep_sub")),
        theme = NULL
      )
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(5, 7)),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico_mecanismo"), height = "400px"))),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico_convergencia"), height = "400px")))
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
      bslib::card(bslib::card_header(rotulo_entenda()), bslib::card_body(texto_box_muller_mc())),
      cartao_cuidados(tr("Cuidados", "Watch out"), cuidados_box_muller_mc())
    )
  )
}

mod_box_muller_mc_server <- function(id, modo = shiny::reactive("light")) {
  shiny::moduleServer(id, function(input, output, session) {

    output$controles_cenario <- shiny::renderUI({
      shiny::req(input$cenario)
      if (identical(input$cenario, "box_muller")) {
        shiny::tagList(
          shiny::sliderInput(
            session$ns("mu"), tr("Média μ", "Mean μ"),
            min = -5, max = 5, value = shiny::isolate(input$mu) %||% 0, step = 0.5
          ),
          shiny::sliderInput(
            session$ns("sigma"), tr("Desvio padrão σ", "Standard deviation σ"),
            min = 0.5, max = 4, value = shiny::isolate(input$sigma) %||% 1, step = 0.25
          ),
          shiny::sliderInput(
            session$ns("corte"), tr("Ponto de corte c para P(X > c)", "Cutoff c for P(X > c)"),
            min = -3, max = 6, value = shiny::isolate(input$corte) %||% 1.5, step = 0.25
          )
        )
      } else if (identical(input$cenario, "pi_circulo")) {
        shiny::helpText(tr(
          "Sorteia (U₁, U₂) no quadrado [0, 1]²; a fração com U₁² + U₂² ≤ 1 tem esperança π/4, então 4·p̂ estima π.",
          "Draws (U₁, U₂) on [0, 1]²; the fraction with U₁² + U₂² ≤ 1 has expectation π/4, so 4·p̂ estimates π."
        ))
      } else {
        shiny::tagList(
          shiny::sliderInput(
            session$ns("beta_exp"), tr("Escala / média β de Exp(β)", "Scale / mean β of Exp(β)"),
            min = 0.5, max = 5, value = shiny::isolate(input$beta_exp) %||% 2, step = 0.5
          ),
          shiny::helpText(tr(
            "Em cada réplica sorteiam-se n = 12 valores de Exp(β) e verifica-se se x̄ ± t₀,₉₇₅;₁₁ · s/√12 cobre β (nominal = 95%).",
            "Each replication draws n = 12 values from Exp(β) and checks whether x̄ ± t₀.₉₇₅,₁₁ · s/√12 covers β (nominal = 95%)."
          ))
        )
      }
    })

    sim <- shiny::reactive({
      input$rodar
      shiny::req(input$cenario, is.finite(input$m_rep), is.finite(input$n_traj))
      mu_v <- if (!is.null(input$mu) && is.finite(input$mu)) input$mu else 0
      sg_v <- if (!is.null(input$sigma) && is.finite(input$sigma)) input$sigma else 1
      ct_v <- if (!is.null(input$corte) && is.finite(input$corte)) input$corte else 1.5
      bt_v <- if (!is.null(input$beta_exp) && is.finite(input$beta_exp)) input$beta_exp else 2
      simular_monte_carlo(
        cenario = input$cenario,
        m_rep = as.integer(input$m_rep),
        n_traj = as.integer(input$n_traj),
        mu = mu_v, sigma = sg_v, corte = ct_v, beta_exp = bt_v, n_t = 12L
      )
    })

    output$vb_alvo <- shiny::renderText({
      s <- sim()
      if (identical(s$cenario, "pi_circulo")) fmt_num(s$alvo, 4) else paste0(fmt_num(s$alvo, 4), " (", fmt_pct(s$alvo, 1), ")")
    })

    output$vb_alvo_sub <- shiny::renderText({
      s <- sim()
      if (identical(s$cenario, "cobertura_t")) {
        tr("Nominal da fórmula normal: 95,0%", "Normal-theory nominal level: 95.0%")
      } else if (identical(s$cenario, "pi_circulo")) {
        tr("4 × área do quarto de círculo (π/4)", "4 × quarter-circle area (π/4)")
      } else {
        tr("Valor exato por 1 − Φ((c − μ)/σ)", "Exact value from 1 − Φ((c − μ)/σ)")
      }
    })

    output$vb_est <- shiny::renderText({
      s <- sim()
      if (identical(s$cenario, "pi_circulo")) fmt_num(s$est1, 4) else paste0(fmt_num(s$est1, 4), " (", fmt_pct(s$est1, 1), ")")
    })

    output$vb_est_sub <- shiny::renderText({
      s <- sim()
      sprintf(tr("Erro |θ̂_M − θ| = %s", "Error |θ̂_M − θ| = %s"), fmt_num(abs(s$est1 - s$alvo), 4))
    })

    output$vb_ep <- shiny::renderText({
      s <- sim()
      fmt_num(s$ep_mc, 4)
    })

    output$vb_ep_sub <- shiny::renderText({
      s <- sim()
      ic <- s$est1 + c(-1.96, 1.96) * s$ep_mc
      sprintf(tr("Faixa MC 95%%: [%s; %s]", "95%% MC band: [%s, %s]"), fmt_num(ic[1], 4), fmt_num(ic[2], 4))
    })

    output$grafico_mecanismo <- shiny::renderPlot({
      s <- sim()
      p <- paleta_modo(modo())
      d <- s$detalhe

      if (identical(s$cenario, "box_muller")) {
        n_pts <- min(1200L, length(d$x))
        df_bm <- data.frame(
          x = d$x[seq_len(n_pts)],
          y = (d$mu + d$sigma * d$z2)[seq_len(n_pts)],
          cauda = ifelse(d$x[seq_len(n_pts)] > d$corte, "sim", "nao")
        )
        ggplot2::ggplot(df_bm, ggplot2::aes(x = x, y = y, colour = cauda)) +
          ggplot2::geom_point(alpha = 0.55, size = 1.6) +
          ggplot2::geom_vline(xintercept = d$corte, colour = cores_app$referencia, linetype = "dashed", linewidth = 0.95) +
          ggplot2::scale_colour_manual(values = c(sim = cores_app$referencia, nao = cores_app$destaque)) +
          ggplot2::scale_x_continuous(labels = rotulos_eixo()) +
          ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
          ggplot2::coord_fixed(ratio = 1) +
          tema_grafico(modo()) +
          ggplot2::labs(
            title = tr("Nuvem bivariada gerada por Box-Müller", "Bivariate cloud from Box-Muller"),
            subtitle = sprintf(
              tr("Vermelho: pontos com X₁ > %s (%s)", "Red: points with X₁ > %s (%s)"),
              fmt_num(d$corte, 2), fmt_pct(s$est1, 1)
            ),
            x = "X₁ = μ + σZ₁",
            y = "X₂ = μ + σZ₂",
            caption = tr("Box e Muller (1958); Morettin e Bussab, seção 9.3", "Box and Muller (1958); Morettin and Bussab, section 9.3")
          )
      } else if (identical(s$cenario, "pi_circulo")) {
        n_pts <- min(1500L, length(d$u1))
        df_pi <- data.frame(
          u1 = d$u1[seq_len(n_pts)],
          u2 = d$u2[seq_len(n_pts)],
          dentro = ifelse(d$dentro[seq_len(n_pts)], "sim", "nao")
        )
        ang <- seq(0, pi / 2, length.out = 200L)
        arco <- data.frame(u1 = cos(ang), u2 = sin(ang))

        ggplot2::ggplot() +
          ggplot2::geom_point(
            data = df_pi,
            ggplot2::aes(x = u1, y = u2, colour = dentro),
            alpha = 0.55, size = 1.6
          ) +
          ggplot2::geom_path(data = arco, ggplot2::aes(x = u1, y = u2), colour = p$linha, linewidth = 1.0) +
          ggplot2::scale_colour_manual(values = c(sim = cores_app$destaque, nao = p$neutro)) +
          ggplot2::scale_x_continuous(limits = c(0, 1), labels = rotulos_eixo()) +
          ggplot2::scale_y_continuous(limits = c(0, 1), labels = rotulos_eixo()) +
          ggplot2::coord_fixed(ratio = 1) +
          tema_grafico(modo()) +
          ggplot2::labs(
            title = tr("Sorteio uniforme no quarto de círculo", "Uniform dart-throwing in the quarter circle"),
            subtitle = sprintf(
              tr("Em azul: U₁² + U₂² ≤ 1 | 4 · p̂ = %s", "Blue: U₁² + U₂² ≤ 1 | 4 · p̂ = %s"),
              fmt_num(s$est1, 4)
            ),
            x = "U₁", y = "U₂",
            caption = tr("Morettin e Bussab, seção 9.4", "Morettin and Bussab, section 9.4")
          )
      } else {
        t_vals <- d$estat_t[d$estat_t >= -6 & d$estat_t <= 5]
        grade_t <- seq(-5.5, 4.5, length.out = 300L)
        curva_t <- data.frame(t = grade_t, dens = stats::dt(grade_t, df = d$n_t - 1L))
        ggplot2::ggplot(data.frame(t = t_vals), ggplot2::aes(x = t)) +
          ggplot2::geom_histogram(
            ggplot2::aes(y = ggplot2::after_stat(density)),
            bins = 30, fill = cores_app$destaque, alpha = 0.35, colour = p$fundo
          ) +
          ggplot2::geom_line(data = curva_t, ggplot2::aes(x = t, y = dens), colour = cores_app$referencia, linewidth = 1.05) +
          ggplot2::geom_vline(xintercept = c(-d$t_crit, d$t_crit), colour = p$linha, linetype = "dashed") +
          ggplot2::scale_x_continuous(labels = rotulos_eixo()) +
          ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
          tema_grafico(modo()) +
          ggplot2::labs(
            title = tr("Estatística T = (X̄ − β)/(S/√12) sob Exp(β)", "Statistic T = (X̄ − β)/(S/√12) under Exp(β)"),
            subtitle = tr(
              "Assimetria à esquerda faz T escapar de ±t₀,₉₇₅ mais de 5% das vezes",
              "Left skewness pushes T beyond ±t₀.₉₇₅ more than 5% of the time"
            ),
            x = "T", y = tr("Densidade", "Density"),
            caption = tr("Agresti e Finlay, seção 5.3; Casella e Berger, seção 10.4.2", "Agresti and Finlay, section 5.3; Casella and Berger, section 10.4.2")
          )
      }
    })

    output$grafico_convergencia <- shiny::renderPlot({
      s <- sim()
      p <- paleta_modo(modo())
      trj <- s$trajetorias
      env <- s$envelope
      trj_1 <- trj[trj$trajetoria == "1", ]
      trj_f <- trj[trj$trajetoria != "1", ]

      # Evita explosão visual do funil nos primeiros 10 passos
      env_vis <- env[env$passo >= 15L, ]

      g <- ggplot2::ggplot() +
        ggplot2::geom_ribbon(
          data = env_vis,
          ggplot2::aes(x = passo, ymin = inf, ymax = sup),
          fill = cores_app$destaque, alpha = 0.13
        )

      if (nrow(trj_f) > 0L) {
        g <- g + ggplot2::geom_line(
          data = trj_f,
          ggplot2::aes(x = passo, y = estimativa, group = trajetoria),
          colour = p$secundario, alpha = 0.42, linewidth = 0.6
        )
      }

      if (identical(s$cenario, "cobertura_t")) {
        g <- g + ggplot2::geom_hline(yintercept = 0.95, colour = p$linha, linetype = "dotted", linewidth = 0.8)
      }

      g +
        ggplot2::geom_hline(yintercept = s$alvo, colour = cores_app$referencia, linetype = "dashed", linewidth = 0.95) +
        ggplot2::geom_line(
          data = trj_1,
          ggplot2::aes(x = passo, y = estimativa),
          colour = cores_app$destaque, linewidth = 1.05
        ) +
        ggplot2::scale_x_continuous(labels = rotulos_eixo()) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        tema_grafico(modo()) +
        ggplot2::labs(
          title = tr("Convergência na escala 1/√m", "Convergence at the 1/√m rate"),
          subtitle = tr(
            "Faixa sombreada: envelope teórico de 95% (±1,96 · σ_g / √m) ao redor de θ",
            "Shaded band: theoretical 95% funnel (±1.96 · σ_g / √m) around θ"
          ),
          x = tr("Réplicas acumuladas (m)", "Cumulative replications (m)"),
          y = tr("Estimativa acumulada θ̂_m", "Running estimate θ̂_m"),
          caption = tr("Morettin e Bussab, seção 9.4; Casella e Berger, seção 5.5", "Morettin and Bussab, section 9.4; Casella and Berger, section 5.5")
        )
    })
  })
}
