# =============================================================================
# Script:  mod_prng_inversa.R (Aba 1: gerador congruencial linear e método da
#          transformação inversa)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - objetos e funções de _app_scripts/apps/simulacao-bootstrap/modulos/dados.R
# Saída:
#   - funções mod_prng_inversa_ui() e mod_prng_inversa_server()
# =============================================================================

texto_prng_inversa <- function() {
  tr(
    shiny::withMathJax(
      shiny::p(
        "Computadores executam operações determinísticas e, por isso, não produzem aleatoriedade física sem hardware",
        "externo. Em simulação estatística, recorre-se a sequências pseudoaleatórias em \\((0, 1)\\) que imitam",
        "sorteios independentes de \\(U(0, 1)\\), mantendo a reprodutibilidade exata quando a semente \\(x_0\\) é",
        "fixada (Morettin e Bussab, seção 9.1). No gerador congruencial linear, cada inteiro sai de",
        "\\(x_{k+1} = (a x_k + c) \\bmod m\\) e é reescalado por \\(u_k = x_k / m\\); no entanto, a qualidade da",
        "sequência depende de \\((a, c, m)\\), uma vez que constantes mal escolhidas geram ciclos curtos ou prendem",
        "os pares consecutivos \\((u_k, u_{k+1})\\) em retas paralelas mesmo quando o histograma marginal parece uniforme."
      ),
      shiny::p(
        "Uma vez obtida uma sequência uniforme confiável, o Teorema da Transformação Integral de Probabilidade",
        "(Casella e Berger, Teo. 2.1.10; Morettin e Bussab, seção 9.2) converte \\(U \\sim U(0, 1)\\) em qualquer",
        "distribuição-alvo \\(F_X\\). De forma mais específica, basta aplicar a inversa generalizada",
        "\\(X = F_X^{-1}(U) = \\inf\\{x : F_X(x) \\ge U\\}\\) (Casella e Berger, eq. 2.1.13): na exponencial de",
        "média \\(\\beta\\), \\(F_X(x) = 1 - e^{-x/\\beta}\\) leva diretamente a \\(X = -\\beta \\ln(1 - U)\\);",
        "já em variáveis discretas como Poisson e binomial, os degraus horizontais de \\(F_X\\) acumulam todos os",
        "valores de \\(U\\) que caem em \\((F_X(x_{i-1}), F_X(x_i)]\\) exatamente sobre o ponto \\(x_i\\), com",
        "probabilidade igual à altura do salto \\(P(X = x_i)\\)."
      ),
      shiny::p(
        shiny::strong("Experimente:"),
        "mantenha a exponencial com \\(\\beta = 2\\) e troque o gerador de período longo pelo de retas paralelas",
        "e depois pelo de ciclo curto. Observe como o gráfico de pares \\((u_k, u_{k+1})\\) denuncia imediatamente",
        "a estrutura determinística e, no ciclo curto, esburaca o histograma da variável transformada."
      )
    ),
    shiny::withMathJax(
      shiny::p(
        "Computers execute deterministic arithmetic and therefore do not produce physical randomness without external",
        "hardware. Statistical simulation relies instead on pseudorandom sequences in \\((0, 1)\\) that mimic",
        "independent \\(U(0, 1)\\) draws while guaranteeing exact reproducibility whenever the seed \\(x_0\\) is fixed",
        "(Morettin and Bussab, section 9.1). In a linear congruential generator, each integer follows",
        "\\(x_{k+1} = (a x_k + c) \\bmod m\\) and is scaled to \\(u_k = x_k / m\\); however, sequence quality",
        "hinges on \\((a, c, m)\\), since poorly chosen constants produce short cycles or trap consecutive pairs",
        "\\((u_k, u_{k+1})\\) on parallel lines even when the one-dimensional histogram looks uniform."
      ),
      shiny::p(
        "Once a reliable uniform stream is available, the Probability Integral Transformation (Casella and Berger,",
        "Thm. 2.1.10; Morettin and Bussab, section 9.2) maps \\(U \\sim U(0, 1)\\) onto any target distribution",
        "\\(F_X\\). More specifically, applying the generalized inverse",
        "\\(X = F_X^{-1}(U) = \\inf\\{x : F_X(x) \\ge U\\}\\) (Casella and Berger, eq. 2.1.13) yields",
        "\\(X = -\\beta \\ln(1 - U)\\) for an exponential with mean \\(\\beta\\), whereas for discrete models such",
        "as the Poisson and binomial, the flat steps of \\(F_X\\) map every \\(U\\) in \\((F_X(x_{i-1}), F_X(x_i)]\\)",
        "onto \\(x_i\\) with probability equal to the jump height \\(P(X = x_i)\\)."
      ),
      shiny::p(
        shiny::strong("Try it:"),
        "keep the exponential with \\(\\beta = 2\\) and switch from the long-period generator to parallel lines and",
        "then to the short cycle. Watch how the \\((u_k, u_{k+1})\\) pair plot immediately exposes the lattice",
        "structure and, under the short cycle, punches gaps into the transformed histogram."
      )
    )
  )
}

cuidados_prng_inversa <- function() {
  tr(
    c(
      "Uniformidade em 1D não garante independência" = paste(
        "o gerador a = 65, c = 1, m = 2.048 percorre todas as 2.048 frações k/m uma vez por ciclo, dando média",
        "próxima de 0,5, mas falha em dimensões maiores porque u_{k+1} depende fortemente de u_k."
      ),
      "Parametrização da exponencial nos livros e no R" = paste(
        "Morettin e Bussab (seção 9.2) e Casella e Berger (Ex. 2.2.2) escrevem Exp(β) com média β e variância β²;",
        "no R, stats::rexp() recebe a taxa rate = 1 / β."
      ),
      "Semente fixa reproduz, não melhora a amostra" = paste(
        "trocar a semente x₀ apenas desloca o ponto de partida no ciclo do gerador; para reduzir a variabilidade",
        "entre os momentos simulados e os teóricos, é preciso aumentar n (Morettin e Bussab, seção 9.4)."
      )
    ),
    c(
      "1D uniformity does not imply independence" = paste(
        "the generator a = 65, c = 1, m = 2,048 visits all 2,048 fractions k/m once per period, giving a mean near",
        "0.5, yet fails in two dimensions because u_{k+1} is tightly tied to u_k."
      ),
      "Exponential parameterization in textbooks vs. R" = paste(
        "Morettin and Bussab (section 9.2) and Casella and Berger (Ex. 2.2.2) write Exp(β) with mean β and variance",
        "β², whereas R's stats::rexp() takes rate = 1 / β."
      ),
      "Fixing the seed ensures reproducibility, not accuracy" = paste(
        "changing the seed x₀ only shifts the starting point along the generator's cycle; bringing simulated",
        "moments closer to theoretical ones requires increasing n (Morettin and Bussab, section 9.4)."
      )
    )
  )
}

mod_prng_inversa_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 315,
      shiny::selectInput(
        ns("preset"), tr("Gerador congruencial linear", "Linear congruential generator"),
        choices = stats::setNames(names(presets_lcg), purrr::map_chr(presets_lcg, "rotulo"))
      ),
      shiny::uiOutput(ns("nota_lcg")),
      shiny::sliderInput(
        ns("semente"), tr("Semente inteira (x₀)", "Integer seed (x₀)"),
        min = 1, max = 500, value = 123, step = 1
      ),
      shiny::sliderInput(
        ns("n_sim"), tr("Tamanho da sequência gerada (n)", "Generated sequence length (n)"),
        min = 50, max = 1500, value = 400, step = 50, sep = sep_milhar()
      ),
      shiny::hr(),
      shiny::selectInput(
        ns("modelo"), tr("Modelo-alvo pela inversa F⁻¹(U)", "Target model via inverse F⁻¹(U)"),
        choices = stats::setNames(names(modelos_inversa), purrr::map_chr(modelos_inversa, "rotulo"))
      ),
      shiny::uiOutput(ns("controle_param"))
    ),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(
        title = tr("Valores distintos de uₖ na amostra", "Distinct uₖ values in sample"),
        value = shiny::textOutput(ns("vb_distintos")),
        shiny::textOutput(ns("vb_distintos_sub")),
        theme = "primary"
      ),
      bslib::value_box(
        title = tr("Média simulada x̄ vs. E(X)", "Simulated mean x̄ vs. E(X)"),
        value = shiny::textOutput(ns("vb_media")),
        shiny::textOutput(ns("vb_media_sub")),
        theme = "info"
      ),
      bslib::value_box(
        title = tr("Fórmula de inversão usada", "Inversion formula applied"),
        value = shiny::textOutput(ns("vb_formula")),
        shiny::textOutput(ns("vb_fonte_curta")),
        theme = NULL
      )
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(5, 7)),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico_pares"), height = "370px"))),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico_fda"), height = "370px")))
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico_amostra"), height = "360px"))),
      bslib::card(
        bslib::card_header(tr("Momentos teóricos vs. simulados", "Theoretical vs. simulated moments")),
        bslib::card_body(
          shiny::uiOutput(ns("tabela_momentos")),
          shiny::hr(),
          shiny::uiOutput(ns("leitura_momentos"))
        )
      )
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
      bslib::card(bslib::card_header(rotulo_entenda()), bslib::card_body(texto_prng_inversa())),
      cartao_cuidados(tr("Cuidados", "Watch out"), cuidados_prng_inversa())
    )
  )
}

mod_prng_inversa_server <- function(id, modo = shiny::reactive("light")) {
  shiny::moduleServer(id, function(input, output, session) {

    lcg_cfg <- shiny::reactive({
      shiny::req(input$preset)
      presets_lcg[[input$preset]]
    })

    mod_cfg <- shiny::reactive({
      shiny::req(input$modelo)
      modelos_inversa[[input$modelo]]
    })

    output$nota_lcg <- shiny::renderUI({
      shiny::p(class = "small text-muted", lcg_cfg()$nota)
    })

    output$controle_param <- shiny::renderUI({
      m <- mod_cfg()
      id_sl <- paste0("param_", input$modelo)
      val_ant <- shiny::isolate(input[[id_sl]])
      val_ini <- if (!is.null(val_ant) && is.finite(val_ant)) val_ant else m$padrao
      shiny::sliderInput(
        session$ns(id_sl), m$param_rotulo,
        min = m$min, max = m$max, value = val_ini, step = m$passo
      )
    })

    param_atual <- shiny::reactive({
      m <- mod_cfg()
      val <- input[[paste0("param_", input$modelo)]]
      if (!is.null(val) && is.finite(val)) val else m$padrao
    })

    seq_u <- shiny::reactive({
      shiny::req(is.finite(input$semente), is.finite(input$n_sim))
      cfg <- lcg_cfg()
      gerar_congruencial(input$n_sim, semente = input$semente, a = cfg$a, c_inc = cfg$c_inc, m = cfg$m)
    })

    sim_inv <- shiny::reactive({
      u <- seq_u()$u
      transformar_inversa(u, chave = input$modelo, param = param_atual())
    })

    output$vb_distintos <- shiny::renderText({
      s <- seq_u()
      sprintf("%s / %s", formatar_inteiro(s$distintos), formatar_inteiro(length(s$u)))
    })

    output$vb_distintos_sub <- shiny::renderText({
      cfg <- lcg_cfg()
      sprintf("m = %s", formatar_inteiro(cfg$m))
    })

    output$vb_media <- shiny::renderText({
      inv <- sim_inv()
      sprintf("%s vs. %s", fmt_num(mean(inv$x), 3), fmt_num(inv$media_teo, 3))
    })

    output$vb_media_sub <- shiny::renderText({
      inv <- sim_inv()
      sprintf(tr("|x̄ − E(X)| = %s", "|x̄ − E(X)| = %s"), fmt_num(abs(mean(inv$x) - inv$media_teo), 3))
    })

    output$vb_formula <- shiny::renderText(mod_cfg()$formula)
    output$vb_fonte_curta <- shiny::renderText(mod_cfg()$fonte)

    output$grafico_pares <- shiny::renderPlot({
      u <- seq_u()$u
      p <- paleta_modo(modo())
      df_pares <- data.frame(uk = u[-length(u)], uk1 = u[-1])

      ggplot2::ggplot(df_pares, ggplot2::aes(x = uk, y = uk1)) +
        ggplot2::geom_point(colour = cores_app$destaque, alpha = 0.65, size = 1.8) +
        ggplot2::scale_x_continuous(limits = c(0, 1), labels = rotulos_eixo()) +
        ggplot2::scale_y_continuous(limits = c(0, 1), labels = rotulos_eixo()) +
        ggplot2::coord_fixed(ratio = 1) +
        tema_grafico(modo()) +
        ggplot2::labs(
          title = tr("Pares consecutivos (uₖ, uₖ₊₁)", "Consecutive pairs (uₖ, uₖ₊₁)"),
          subtitle = tr(
            "Em [0, 1)², um bom gerador espalha pontos sem faixas ou grade",
            "On [0, 1)², a good generator scatters points without stripes or grids"
          ),
          x = "uₖ",
          y = "uₖ₊₁",
          caption = tr("Morettin e Bussab, seção 9.1", "Morettin and Bussab, section 9.1")
        )
    })

    output$grafico_fda <- shiny::renderPlot({
      u <- seq_u()$u
      inv <- sim_inv()
      p <- paleta_modo(modo())
      n_setas <- min(14L, length(u))
      df_proj <- data.frame(u = u[seq_len(n_setas)], x = inv$x[seq_len(n_setas)])

      g <- ggplot2::ggplot() +
        ggplot2::geom_segment(
          data = df_proj,
          ggplot2::aes(x = 0, xend = x, y = u, yend = u),
          colour = p$secundario, linetype = "dotted", linewidth = 0.55
        ) +
        ggplot2::geom_segment(
          data = df_proj,
          ggplot2::aes(x = x, xend = x, y = u, yend = 0),
          colour = cores_app$referencia, alpha = 0.55, linewidth = 0.6
        )

      if (inv$discreta) {
        cf <- inv$curva_fda
        cf_deg <- data.frame(
          x_ini = cf$x[-nrow(cf)],
          x_fim = cf$x[-1],
          fda = cf$fda[-nrow(cf)]
        )
        g <- g +
          ggplot2::geom_segment(
            data = cf_deg,
            ggplot2::aes(x = x_ini, xend = x_fim, y = fda, yend = fda),
            colour = cores_app$destaque, linewidth = 1.15
          ) +
          ggplot2::geom_point(
            data = cf,
            ggplot2::aes(x = x, y = fda),
            colour = cores_app$destaque, size = 2.4
          )
      } else {
        g <- g +
          ggplot2::geom_line(
            data = inv$curva_fda,
            ggplot2::aes(x = x, y = fda),
            colour = cores_app$destaque, linewidth = 1.15
          )
      }

      g +
        ggplot2::geom_point(
          data = df_proj,
          ggplot2::aes(x = x, y = 0),
          colour = cores_app$referencia, size = 2.3
        ) +
        ggplot2::scale_x_continuous(labels = rotulos_eixo()) +
        ggplot2::scale_y_continuous(limits = c(0, 1), labels = rotulos_eixo()) +
        tema_grafico(modo()) +
        ggplot2::labs(
          title = tr("Transformação inversa x = F⁻¹(u)", "Inverse transform x = F⁻¹(u)"),
          subtitle = sprintf(
            tr("Azul: f.d.a. F(x) | Vermelho: projeção dos primeiros %d valores de uₖ", "Blue: CDF F(x) | Red: projection of the first %d values of uₖ"),
            n_setas
          ),
          x = "x",
          y = "u = F(x)",
          caption = mod_cfg()$fonte
        )
    })

    output$grafico_amostra <- shiny::renderPlot({
      inv <- sim_inv()
      p <- paleta_modo(modo())

      if (inv$discreta) {
        ks <- inv$curva_mod$x
        tab <- tabulate(inv$x + 1L, nbins = max(ks) + 1L)
        freq_emp <- tab[ks + 1L] / length(inv$x)
        df_bar <- data.frame(
          x = rep(ks, 2L),
          prob = c(freq_emp, inv$curva_mod$y),
          tipo = factor(
            c(rep(tr("Simulada", "Simulated"), length(ks)), rep(tr("Teórica", "Theoretical"), length(ks))),
            levels = c(tr("Simulada", "Simulated"), tr("Teórica", "Theoretical"))
          )
        )
        ggplot2::ggplot(df_bar, ggplot2::aes(x = factor(x), y = prob, fill = tipo)) +
          ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.72), width = 0.65) +
          ggplot2::scale_fill_manual(values = c(cores_app$destaque, cores_app$referencia)) +
          ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
          tema_grafico(modo()) +
          ggplot2::theme(legend.position = "top") +
          ggplot2::labs(
            title = tr("Frequências simuladas vs. função de probabilidade", "Simulated frequencies vs. probability mass function"),
            subtitle = sprintf("n = %s", formatar_inteiro(length(inv$x))),
            x = "x", y = tr("Probabilidade / frequência relativa", "Probability / relative frequency"),
            fill = NULL,
            caption = tr("Morettin e Bussab, seções 9.3 e 9.4", "Morettin and Bussab, sections 9.3 and 9.4")
          )
      } else {
        df_x <- data.frame(x = inv$x)
        ggplot2::ggplot(df_x, ggplot2::aes(x = x)) +
          ggplot2::geom_histogram(
            ggplot2::aes(y = ggplot2::after_stat(density)),
            bins = 24, fill = cores_app$destaque, alpha = 0.35, colour = p$fundo
          ) +
          ggplot2::geom_line(
            data = inv$curva_mod,
            ggplot2::aes(x = x, y = y),
            colour = cores_app$referencia, linewidth = 1.1
          ) +
          ggplot2::scale_x_continuous(labels = rotulos_eixo()) +
          ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
          tema_grafico(modo()) +
          ggplot2::labs(
            title = tr("Densidade simulada vs. curva teórica f(x)", "Simulated density vs. theoretical curve f(x)"),
            subtitle = sprintf(
              tr("Histograma de n = %s valores transformados | Linha vermelha: densidade exata", "Histogram of n = %s transformed values | Red line: exact density"),
              formatar_inteiro(length(inv$x))
            ),
            x = "x", y = tr("Densidade", "Density"),
            caption = tr("Morettin e Bussab, seções 9.2 e 9.4", "Morettin and Bussab, sections 9.2 and 9.4")
          )
      }
    })

    output$tabela_momentos <- shiny::renderUI({
      u <- seq_u()$u
      inv <- sim_inv()
      x <- inv$x
      df <- data.frame(
        Medida = c(
          tr("Uniforme: média de uₖ", "Uniform: mean of uₖ"),
          tr("Uniforme: variância s²(uₖ)", "Uniform: variance s²(uₖ)"),
          tr("Alvo X: média", "Target X: mean"),
          tr("Alvo X: variância (s² com n − 1)", "Target X: variance (s² with n − 1)"),
          tr("Alvo X: desvio padrão", "Target X: standard deviation")
        ),
        Teorico = c(
          fmt_num(0.5, 4),
          fmt_num(1 / 12, 4),
          fmt_num(inv$media_teo, 4),
          fmt_num(inv$var_teo, 4),
          fmt_num(sqrt(inv$var_teo), 4)
        ),
        Simulado = c(
          fmt_num(mean(u), 4),
          fmt_num(stats::var(u), 4),
          fmt_num(mean(x), 4),
          fmt_num(stats::var(x), 4),
          fmt_num(stats::sd(x), 4)
        ),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Quantidade", "Quantity"), tr("Teórico", "Theoretical"), tr("Simulado", "Simulated"))
      tabela_html(df, c("", "num", "num"))
    })

    output$leitura_momentos <- shiny::renderUI({
      s <- seq_u()
      inv <- sim_inv()
      if (s$distintos < min(30L, length(s$u))) {
        return(shiny::p(class = "mb-0", tr(
          "Como o período m do gerador é minúsculo, os mesmos valores de uₖ se repetem dezenas de vezes: aumentar n apenas repete o ciclo sem aproximar a distribuição teórica.",
          "Because the generator modulus m is tiny, the exact same uₖ values repeat over and over: raising n merely replays the cycle without approaching the theoretical distribution."
        )))
      }
      shiny::p(class = "mb-0", sprintf(
        tr(
          "Com %s pontos gerados, a média simulada difere de E(X) por %s e o desvio padrão difere de √Var(X) por %s.",
          "With %s generated points, the simulated mean differs from E(X) by %s and the standard deviation differs from √Var(X) by %s."
        ),
        formatar_inteiro(length(inv$x)),
        fmt_num(abs(mean(inv$x) - inv$media_teo), 3),
        fmt_num(abs(stats::sd(inv$x) - sqrt(inv$var_teo)), 3)
      ))
    })
  })
}
