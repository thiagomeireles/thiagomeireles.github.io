# =============================================================================
# Script:  mod_poisson.R (Aba 3: Poisson, aproximação binomial e processo)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - n e p da aproximação (λ = np); taxa λ, horizonte t e contagem k do
#     processo de Poisson
# Saída:
#   - Gráfico Binomial(n, p) vs. Poisson(λ = np) com tabela de erro máximo e
#     gráfico de Po(λt) com P(N_t ≤ k)
# =============================================================================

mod_poisson_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::strong(tr("Aproximação da binomial", "Binomial approximation")),
      shiny::sliderInput(ns("n_pois"), tr("Número de ensaios n", "Number of trials n"), min = 10, max = 200, value = 80, step = 5),
      shiny::sliderInput(ns("p_pois"), tr("Probabilidade p", "Probability p"), min = 0.005, max = 0.400, value = 0.030, step = 0.005),
      shiny::hr(),
      shiny::strong(tr("Processo de Poisson", "Poisson process")),
      shiny::sliderInput(ns("taxa"), tr("Taxa λ (eventos por unidade de tempo)", "Rate λ (events per unit of time)"), min = 0.5, max = 10, value = 5, step = 0.5),
      shiny::sliderInput(ns("t_proc"), tr("Horizonte t (unidades de tempo)", "Horizon t (units of time)"), min = 1, max = 10, value = 1, step = 1),
      shiny::sliderInput(ns("k_proc"), tr("Contagem k em P(N ≤ k)", "Count k in P(N ≤ k)"), min = 0, max = 30, value = 0, step = 1)
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header(tr("Aproximação Binomial(n, p) → Poisson(λ = n·p)", "Binomial(n, p) → Poisson(λ = n·p) approximation")),
        bslib::card_body(shiny::plotOutput(ns("grafico_pois"), height = "330px"))
      ),
      bslib::card(
        bslib::card_header(tr("Comparação", "Comparison")),
        bslib::card_body(
          shiny::uiOutput(ns("tabela_pois")),
          shiny::uiOutput(ns("leitura_regra"))
        )
      )
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header(tr("Processo de Poisson: N ~ Po(λ·t)", "Poisson process: N ~ Po(λ·t)")),
        bslib::card_body(shiny::plotOutput(ns("grafico_proc"), height = "300px"))
      ),
      bslib::card(
        bslib::card_header(tr("Contagem no intervalo", "Count in the interval")),
        bslib::card_body(shiny::uiOutput(ns("texto_processo")))
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header(rotulo_entenda()),
        bslib::card_body(
          shiny::p(tr(
            "Com n grande e p pequeno, b(k; n, p) ≈ e^(−np) (np)ᵏ / k!. Morettin e Bussab (seção 6.6.5) dizem que a aproximação é boa quando np ≤ 7. A Poisson conta eventos num intervalo de tempo, área ou volume, e E(N) = Var(N) = λ.",
            "With n large and p small, b(k; n, p) ≈ e^(−np) (np)ᵏ / k!. Morettin and Bussab (section 6.6.5) say the approximation works well when np ≤ 7. The Poisson counts events in an interval of time, area or volume, and E(N) = Var(N) = λ."
          )),
          shiny::p(tr(
            "As duas metades da aba são a mesma ideia. No processo de Poisson (seção 6.7), divida [0, t) em n pedaços: cada um tem um evento com probabilidade λt/n, de modo que a contagem é binomial com np = λt e vira Po(λt) quando n cresce. Somas de Poisson independentes também são Poisson (Gelman et al., apêndice A); por isso, t unidades com taxa λ cada dão Po(λt).",
            "Both halves of the tab are the same idea. In the Poisson process (section 6.7), split [0, t) into n pieces: each holds an event with probability λt/n, so the count is binomial with np = λt and becomes Po(λt) as n grows. Sums of independent Poissons are also Poisson (Gelman et al., appendix A); that is why t units with rate λ each give Po(λt)."
          )),
          shiny::p(
            shiny::strong(tr("Experimente:", "Try this:")),
            tr(
              "o processo abre no Ex. 6.18, um telefone que recebe, em média, cinco chamadas por minuto: P(N = 0) = 0,0067 em um minuto. Ponha t = 4 e k = 2 para ver P(N ≤ 2) = 221e⁻²⁰, quase zero. Na aproximação, suba p até np passar de 7 e acompanhe o erro máximo.",
              "the process opens on Ex. 6.18, a phone that gets, on average, five calls per minute: P(N = 0) = 0.0067 in one minute. Set t = 4 and k = 2 to see P(N ≤ 2) = 221e⁻²⁰, almost zero. In the approximation, raise p until np goes past 7 and follow the maximum error."
            )
          )
        )
      ),
      cartao_cuidados(
        tr("Cuidados", "Watch out"),
        stats::setNames(
          list(
            tr(
              "o que importa é p pequeno, não só n grande. Com n = 200 e p = 0,4, a binomial tem variância 48 e a Poisson, 80.",
              "what matters is a small p, not just a large n. With n = 200 and p = 0.4, the binomial has variance 48 and the Poisson has 80."
            ),
            tr(
              "λ é a média por unidade de tempo. Em t unidades, a média passa a ser λt. No Ex. 6.18, usar λ = 5 para quatro minutos dá a resposta de um minuto só.",
              "λ is the mean per unit of time. Over t units, the mean becomes λt. In Ex. 6.18, using λ = 5 for four minutes gives the answer for a single minute."
            ),
            tr(
              "o processo supõe incrementos independentes e estacionários e no máximo um evento num intervalo muito curto (suposições S1 a S5). Chegadas em rajadas ou com horário de pico violam o modelo.",
              "the process assumes independent and stationary increments and at most one event in a very short interval (assumptions S1 to S5). Arrivals in bursts or with rush hours break the model."
            )
          ),
          c(
            tr("p pequeno", "Small p"),
            tr("Ajuste λ ao intervalo", "Scale λ to the interval"),
            tr("Suposições", "Assumptions")
          )
        )
      )
    )
  )
}

mod_poisson_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    res <- shiny::reactive({
      shiny::req(is.finite(input$n_pois), is.finite(input$p_pois))
      dist_poisson_vs_binomial(input$n_pois, input$p_pois)
    })

    proc <- shiny::reactive({
      shiny::req(is.finite(input$taxa), is.finite(input$t_proc), is.finite(input$k_proc))
      processo_poisson(input$taxa, input$t_proc, input$k_proc)
    })

    output$grafico_pois <- shiny::renderPlot({
      r <- res()
      df <- r$df
      p <- paleta_modo(modo())

      ggplot2::ggplot(df, ggplot2::aes(x = k)) +
        ggplot2::geom_col(ggplot2::aes(y = binomial), fill = p$neutro, width = 0.65) +
        ggplot2::geom_line(ggplot2::aes(y = poisson), colour = cores_app$referencia, linewidth = 0.9) +
        ggplot2::geom_point(ggplot2::aes(y = poisson), colour = cores_app$referencia, size = 2.5) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = sprintf("Binomial(%d, %s) vs. Poisson(λ = %s)", as.integer(input$n_pois), fmt_num(input$p_pois, 3), fmt_num(r$lambda, 2)),
          subtitle = sprintf(tr("Erro máximo |P_Bin − P_Pois| = %s", "Max error |P_Bin − P_Pois| = %s"), fmt_num(r$erro_max, 5)),
          x = "k", y = "P(X = k)",
          caption = tr("Barras: Binomial | Linha/pontos vermelhos: Poisson", "Bars: Binomial | Red line/dots: Poisson")
        ) +
        tema_grafico(modo())
    })

    output$tabela_pois <- shiny::renderUI({
      r <- res()
      df <- data.frame(
        Medida = c("λ = n·p", "Var(Binomial) = n·p·(1−p)", "Var(Poisson) = λ", tr("Diferença máxima |Δ|", "Max difference |Δ|")),
        Valor = c(fmt_num(r$lambda, 3), fmt_num(r$var_bin, 4), fmt_num(r$var_pois, 4), fmt_num(r$erro_max, 5)),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Quantidade", "Quantity"), tr("Valor", "Value"))
      tabela_html(df, c("", "num"))
    })

    output$leitura_regra <- shiny::renderUI({
      r <- res()
      texto <- if (r$regra_ok) {
        tr(
          "np = %s ≤ 7: dentro da faixa em que Morettin e Bussab consideram a aproximação boa.",
          "np = %s ≤ 7: inside the range where Morettin and Bussab consider the approximation good."
        )
      } else {
        tr(
          "np = %s > 7: fora da faixa recomendada. Compare as variâncias: λ na Poisson e λ(1 − p) na binomial.",
          "np = %s > 7: outside the recommended range. Compare the variances: λ for the Poisson and λ(1 − p) for the binomial."
        )
      }
      shiny::p(class = "small text-muted mb-0", sprintf(texto, fmt_num(r$lambda, 2)))
    })

    output$grafico_proc <- shiny::renderPlot({
      pr <- proc()
      p <- paleta_modo(modo())
      ggplot2::ggplot(pr$df, ggplot2::aes(x = k, y = prob, fill = destaque)) +
        ggplot2::geom_col(width = 0.7) +
        ggplot2::scale_fill_manual(values = c("FALSE" = p$neutro, "TRUE" = cores_app$destaque)) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = sprintf("Po(λ·t = %s)", fmt_num(pr$media, 1)),
          subtitle = sprintf("P(N ≤ %d) = %s", as.integer(input$k_proc), fmt_prob(pr$prob_ate_k)),
          x = tr("Número de eventos k", "Number of events k"), y = "P(N = k)",
          caption = tr("Barras azuis: N ≤ k", "Blue bars: N ≤ k")
        ) +
        tema_grafico(modo())
    })

    output$texto_processo <- shiny::renderUI({
      pr <- proc()
      shiny::tagList(
        shiny::p(sprintf(
          tr(
            "Com taxa λ = %s eventos por unidade, em t = %d unidades a contagem segue Po(λ·t = %s): média e variância iguais a %s.",
            "With rate λ = %s events per unit, over t = %d units the count follows Po(λ·t = %s): mean and variance both equal %s."
          ),
          fmt_num(input$taxa, 1), as.integer(input$t_proc), fmt_num(pr$media, 1), fmt_num(pr$media, 1)
        )),
        shiny::p(class = "mb-0", sprintf(
          tr("P(N = 0) = %s e P(N ≤ %d) = %s.", "P(N = 0) = %s and P(N ≤ %d) = %s."),
          fmt_prob(pr$prob_zero), as.integer(input$k_proc), fmt_prob(pr$prob_ate_k)
        ))
      )
    })
  })
}
