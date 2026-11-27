# =============================================================================
# Script:  mod_binomial.R (Aba 2: Bernoulli e Binomial b(n, p))
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Exemplo do livro ou parâmetros n, p e intervalo de contagens [k_inf, k_sup]
# Saída:
#   - Gráfico de barras b(n, p) com destaque do intervalo, simulação e tabela
# =============================================================================

mod_binomial_ui <- function(id) {
  ns <- shiny::NS(id)
  exemplos <- exemplos_binomial()

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::selectInput(
        ns("exemplo"),
        tr("Exemplo de Morettin e Bussab", "Example from Morettin and Bussab"),
        choices = c(stats::setNames("livre", tr("Livre (ajuste os controles)", "Free (set the controls)")), stats::setNames(names(exemplos), purrr::map_chr(exemplos, "rotulo")))
      ),
      shiny::sliderInput(ns("n_bin"), tr("Número de ensaios n", "Number of trials n"), min = 1, max = 40, value = 12, step = 1),
      shiny::sliderInput(ns("p_bin"), tr("Probabilidade de sucesso p", "Success probability p"), min = 0.02, max = 0.98, value = 0.35, step = 0.01),
      shiny::sliderInput(ns("faixa_k"), tr("Intervalo k₁ ≤ X ≤ k₂", "Interval k₁ ≤ X ≤ k₂"), min = 0, max = 12, value = c(3, 6), step = 1),
      shiny::actionButton(ns("simular"), tr("Simular 1.000 repetições", "Simulate 1,000 repetitions"), class = "btn-sm btn-primary mt-2")
    ),
    bslib::layout_columns(
      col_widths = c(8, 4),
      bslib::card(
        bslib::card_header(tr("Distribuição binomial b(n, p)", "Binomial distribution b(n, p)")),
        bslib::card_body(shiny::plotOutput(ns("grafico_bin"), height = "350px"))
      ),
      bslib::card(
        bslib::card_header(tr("Resumo do intervalo e momentos", "Interval summary and moments")),
        bslib::card_body(
          shiny::uiOutput(ns("tabela_bin")),
          shiny::hr(),
          shiny::p(class = "mb-0", tr(
            "Para P(X ≥ k), use 1 − P(X ≤ k − 1). Em R: 1 - pbinom(k - 1, n, p).",
            "For P(X ≥ k), use 1 − P(X ≤ k − 1). In R: 1 - pbinom(k - 1, n, p)."
          ))
        )
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header(rotulo_entenda()),
        bslib::card_body(
          shiny::p(tr(
            "Um experimento binomial tem n ensaios de Bernoulli independentes, cada um com a mesma probabilidade de sucesso p. X conta os sucessos, e P(X = k) = C(n, k) pᵏ (1 − p)ⁿ⁻ᵏ, porque toda sequência com k sucessos tem probabilidade pᵏ (1 − p)ⁿ⁻ᵏ e há C(n, k) delas (Morettin e Bussab, seção 6.6.3, fórmula 6.15). E(X) = np e Var(X) = np(1 − p). Com n = 1, a binomial é a Bernoulli (Gelman et al., apêndice A).",
            "A binomial experiment has n independent Bernoulli trials, each with the same success probability p. X counts the successes, and P(X = k) = C(n, k) pᵏ (1 − p)ⁿ⁻ᵏ, because every sequence with k successes has probability pᵏ (1 − p)ⁿ⁻ᵏ and there are C(n, k) of them (Morettin and Bussab, section 6.6.3, formula 6.15). E(X) = np and Var(X) = np(1 − p). With n = 1, the binomial is the Bernoulli (Gelman et al., appendix A)."
          )),
          shiny::p(
            shiny::strong(tr("Experimente:", "Try this:")),
            tr(
              "escolha o Ex. 6.12 e confira P(X = 2) = 3/8. Depois troque p por 0,9 e veja a massa ir para a direita. Por fim, clique em simular: os pontos vermelhos são as proporções de 1.000 repetições e ficam perto das barras.",
              "pick Ex. 6.12 and check P(X = 2) = 3/8. Then change p to 0.9 and watch the mass move right. Finally, click simulate: the red dots are the proportions from 1,000 repetitions and land close to the bars."
            )
          )
        )
      ),
      cartao_cuidados(
        tr("Cuidados", "Watch out"),
        stats::setNames(
          list(
            tr(
              "a binomial exige ensaios independentes e p constante. Sortear sem reposição de um lote pequeno muda p a cada retirada, e aí o modelo é a hipergeométrica (ver \"Para ir além\").",
              "the binomial needs independent trials and a constant p. Drawing without replacement from a small lot changes p at each draw, and then the model is the hypergeometric (see \"Going further\")."
            ),
            tr(
              "em variáveis discretas, P(X ≥ k) e P(X > k) diferem por P(X = k). P(X ≥ k) = 1 − P(X ≤ k − 1) inclui o próprio k.",
              "for discrete variables, P(X ≥ k) and P(X > k) differ by P(X = k). P(X ≥ k) = 1 − P(X ≤ k − 1) includes k itself."
            ),
            tr(
              "no Ex. 6.13, P(X = 10) = 10⁻¹⁰ é pequena, mas não é zero. A tabela mostra o valor em notação científica.",
              "in Ex. 6.13, P(X = 10) = 10⁻¹⁰ is tiny, but it is not zero. The table shows it in scientific notation."
            )
          ),
          c(
            tr("Independência", "Independence"),
            tr("≥ não é >", "≥ is not >"),
            tr("Pequeno não é zero", "Tiny is not zero")
          )
        )
      )
    )
  )
}

mod_binomial_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$exemplo, {
      ex <- exemplos_binomial()[[input$exemplo]]
      shiny::req(ex)
      shiny::updateSliderInput(session, "n_bin", value = ex$n)
      shiny::updateSliderInput(session, "p_bin", value = ex$p)
      shiny::updateSliderInput(session, "faixa_k", max = ex$n, value = ex$k)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$n_bin, {
      shiny::req(is.finite(input$n_bin))
      fx <- if (length(input$faixa_k) == 2) input$faixa_k else c(0, input$n_bin)
      novo <- c(min(fx[1], input$n_bin), min(fx[2], input$n_bin))
      shiny::updateSliderInput(session, "faixa_k", max = input$n_bin, value = novo)
    })

    sim_bin <- shiny::reactiveVal(NULL)
    shiny::observeEvent(list(input$n_bin, input$p_bin), sim_bin(NULL))
    shiny::observeEvent(input$simular, {
      shiny::req(is.finite(input$n_bin), is.finite(input$p_bin))
      draws <- stats::rbinom(1000, size = input$n_bin, prob = input$p_bin)
      tab <- table(factor(draws, levels = 0:input$n_bin))
      sim_bin(as.numeric(tab) / 1000)
    })

    res <- shiny::reactive({
      shiny::req(is.finite(input$n_bin), is.finite(input$p_bin), length(input$faixa_k) == 2)
      dist_binomial(input$n_bin, input$p_bin, min(input$faixa_k[1], input$n_bin), min(input$faixa_k[2], input$n_bin))
    })

    output$grafico_bin <- shiny::renderPlot({
      r <- res()
      df <- r$df
      p <- paleta_modo(modo())
      sim <- sim_bin()

      g <- ggplot2::ggplot(df, ggplot2::aes(x = k, y = pmf, fill = destaque)) +
        ggplot2::geom_col(width = 0.7) +
        ggplot2::scale_fill_manual(values = c("FALSE" = p$neutro, "TRUE" = cores_app$destaque))

      if (!is.null(sim) && length(sim) == nrow(df)) {
        df_s <- data.frame(k = df$k, sim = sim)
        g <- g + ggplot2::geom_point(
          data = df_s,
          ggplot2::aes(x = k, y = sim),
          inherit.aes = FALSE,
          colour = cores_app$referencia, size = 2.2
        )
      }

      legenda <- tr("Barras azuis: intervalo selecionado", "Blue bars: selected interval")
      if (!is.null(sim)) {
        legenda <- paste(legenda, tr("| Pontos vermelhos: proporção em 1.000 repetições", "| Red dots: proportion in 1,000 repetitions"))
      }

      g +
        ggplot2::scale_x_continuous(breaks = function(lim) unique(round(pretty(lim)))) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = sprintf("b(n = %d, p = %s)", as.integer(input$n_bin), fmt_num(input$p_bin, 2)),
          subtitle = sprintf("P(%d ≤ X ≤ %d) = %s", as.integer(r$a), as.integer(r$b), fmt_prob(r$prob_interv)),
          x = tr("Número de sucessos k", "Number of successes k"),
          y = "P(X = k)",
          caption = legenda
        ) +
        tema_grafico(modo())
    })

    output$tabela_bin <- shiny::renderUI({
      r <- res()
      df <- data.frame(
        Medida = c(
          sprintf("P(%d ≤ X ≤ %d)", as.integer(r$a), as.integer(r$b)),
          "E(X) = n·p",
          "Var(X) = n·p·(1 − p)",
          "DP(X)"
        ),
        Valor = c(
          fmt_prob(r$prob_interv),
          fmt_num(r$media, 3),
          fmt_num(r$variancia, 3),
          fmt_num(r$dp, 3)
        ),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Quantidade", "Quantity"), tr("Valor", "Value"))
      tabela_html(df, c("", "num"))
    })
  })
}
