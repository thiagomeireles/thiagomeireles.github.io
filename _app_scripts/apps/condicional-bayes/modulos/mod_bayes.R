# =============================================================================
# Script:  mod_bayes.R (Aba 3: Probabilidade total e Teorema de Bayes)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Exemplos (teste diagnóstico, queda da bolsa de Morettin e Bussab, Código
#     Morse de Casella e Berger) e sliders
# Saída:
#   - Tabela completa de Bayes, chances a posteriori (Gelman et al., eq. 1.5),
#     gráfico priori vs. posteriori e grade de 1.000 frequências naturais
# =============================================================================

# Valores de partida e significado de C₁, C₂ e A em cada exemplo
exemplos_bayes <- function() {
  list(
    diagnostico = list(
      valores = c(priori1 = 0.010, verossim1 = 0.95, verossim2 = 0.05),
      hipoteses = c(tr("C₁: Doença presente", "C₁: Disease present"), tr("C₂: Doença ausente", "C₂: Disease absent")),
      descricao = tr(
        "A = teste positivo. A doença atinge 1% das pessoas; o teste dá positivo em 95% dos doentes e em 5% dos sadios.",
        "A = positive test. The disease affects 1% of people; the test is positive in 95% of the sick and 5% of the healthy."
      )
    ),
    bolsa = list(
      valores = c(priori1 = 0.100, verossim1 = 0.20, verossim2 = 0.05),
      hipoteses = c(tr("C₁: Bolsa cai (E)", "C₁: Market falls (E)"), tr("C₂: Bolsa sobe (Eᶜ)", "C₂: Market rises (Eᶜ)")),
      descricao = tr(
        "A = notícia de alta do dólar. A previsão inicial de queda da bolsa é 0,10; a notícia precedeu 20% das quedas e 5% das altas (Morettin e Bussab, Ex. 5.16).",
        "A = news of a rising dollar. The initial forecast of a market fall is 0.10; the news came before 20% of falls and 5% of rises (Morettin and Bussab, Ex. 5.16)."
      )
    ),
    morse = list(
      valores = c(priori1 = 0.429, verossim1 = 0.875, verossim2 = 0.125),
      hipoteses = c(tr("C₁: Ponto enviado", "C₁: Dot sent"), tr("C₂: Traço enviado", "C₂: Dash sent")),
      descricao = tr(
        "A = ponto recebido. Pontos e traços são enviados na proporção 3:4, então P(C₁) = 3/7 ≈ 0,429, e 1/8 dos símbolos é trocado na transmissão (Casella e Berger, Ex. 1.3.6).",
        "A = dot received. Dots and dashes are sent in a 3:4 ratio, so P(C₁) = 3/7 ≈ 0.429, and 1/8 of the symbols are switched in transmission (Casella and Berger, Ex. 1.3.6)."
      )
    )
  )
}

mod_bayes_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 330,
      shiny::selectInput(
        ns("preset"),
        tr("Exemplo", "Example"),
        choices = stats::setNames(
          c("diagnostico", "bolsa", "morse"),
          c(
            tr("Teste diagnóstico (taxa base)", "Diagnostic test (base rate)"),
            tr("Queda da bolsa (Morettin e Bussab, Ex. 5.16)", "Market fall (Morettin and Bussab, Ex. 5.16)"),
            tr("Código Morse (Casella e Berger, Ex. 1.3.6)", "Morse code (Casella and Berger, Ex. 1.3.6)")
          )
        )
      ),
      shiny::uiOutput(ns("descricao")),
      shiny::sliderInput(
        ns("priori1"),
        tr("Priori da hipótese 1, P(C₁)", "Prior of hypothesis 1, P(C₁)"),
        min = 0.001, max = 0.999, value = 0.010, step = 0.001
      ),
      shiny::sliderInput(
        ns("verossim1"),
        tr("Verossimilhança P(A | C₁)", "Likelihood P(A | C₁)"),
        min = 0.005, max = 0.995, value = 0.95, step = 0.005
      ),
      shiny::sliderInput(
        ns("verossim2"),
        tr("Verossimilhança P(A | C₂)", "Likelihood P(A | C₂)"),
        min = 0.005, max = 0.995, value = 0.05, step = 0.005
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header(tr("Priori → Posteriori", "Prior → Posterior")),
        bslib::card_body(
          shiny::plotOutput(ns("grafico_bayes"), height = "310px")
        )
      ),
      bslib::card(
        bslib::card_header(tr("Frequências naturais em 1.000 casos", "Natural frequencies in 1,000 cases")),
        bslib::card_body(
          shiny::plotOutput(ns("grafico_grade"), height = "310px")
        )
      )
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header(tr("Tabela do Teorema de Bayes", "Bayes' theorem table")),
        bslib::card_body(
          shiny::uiOutput(ns("tabela_bayes")),
          shiny::uiOutput(ns("texto_total"))
        )
      ),
      bslib::card(
        bslib::card_header(tr("Chances a posteriori (Gelman et al., eq. 1.5)", "Posterior odds (Gelman et al., eq. 1.5)")),
        bslib::card_body(
          shiny::uiOutput(ns("texto_odds"))
        )
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header(rotulo_entenda()),
        bslib::card_body(
          shiny::p(tr(
            "Quando C₁, …, Cₙ formam uma partição de Ω, A se divide em pedaços disjuntos, e P(A) = Σ P(Cⱼ) P(A | Cⱼ) é a probabilidade total, uma média das verossimilhanças ponderada pelas prioris. O Teorema de Bayes divide cada pedaço por esse total: P(Cᵢ | A) = P(Cᵢ) P(A | Cᵢ) / P(A) (Morettin e Bussab, seção 5.4, Teorema 5.1; Casella e Berger, Teo. 1.3.5).",
            "When C₁, …, Cₙ form a partition of Ω, A splits into disjoint pieces, and P(A) = Σ P(Cⱼ) P(A | Cⱼ) is the total probability, an average of the likelihoods weighted by the priors. Bayes' theorem divides each piece by that total: P(Cᵢ | A) = P(Cᵢ) P(A | Cᵢ) / P(A) (Morettin and Bussab, section 5.4, Theorem 5.1; Casella and Berger, Thm. 1.3.5)."
          )),
          shiny::p(tr(
            "Na forma de chances, P(A) some: chances a posteriori = chances a priori × razão de verossimilhanças (Gelman et al., eq. 1.5). A grade de 1.000 casos mostra a mesma conta em contagens: a posteriori de C₁ é a fração de pontos em C₁ entre todos os pontos de A.",
            "In odds form, P(A) drops out: posterior odds = prior odds × likelihood ratio (Gelman et al., eq. 1.5). The 1,000-case grid shows the same computation in counts: the posterior of C₁ is the share of C₁ dots among all the dots in A."
          )),
          shiny::p(
            shiny::strong(tr("Experimente:", "Try this:")),
            tr(
              "no teste diagnóstico, um positivo leva a probabilidade de doença de 1% para só 16%. Suba a priori para 10% e veja a posteriori passar de 60%. Na queda da bolsa, use a posteriori 0,308 como nova priori e as verossimilhanças 0,10 e 0,60 da segunda notícia (queda dos juros): a probabilidade de queda cai para 0,07, como no livro.",
              "in the diagnostic test, a positive result takes the probability of disease from 1% to only 16%. Raise the prior to 10% and watch the posterior pass 60%. In the market example, use the posterior 0.308 as the new prior and the likelihoods 0.10 and 0.60 of the second news item (an interest rate cut): the probability of a fall drops to 0.07, as in the book."
            )
          )
        )
      ),
      cartao_cuidados(
        tr("Cuidados", "Watch out"),
        stats::setNames(
          list(
            tr(
              "com priori baixa, até um teste bom gera mais falsos positivos que verdadeiros. Ignorar P(C₁) é o erro da taxa base.",
              "with a low prior, even a good test produces more false positives than true ones. Ignoring P(C₁) is the base-rate fallacy."
            ),
            tr(
              "P(A | C₁) e P(A | C₂) não precisam somar 1, porque cada uma vem de uma hipótese diferente. Quem soma 1 são as prioris e as posterioris.",
              "P(A | C₁) and P(A | C₂) need not sum to 1, because each comes from a different hypothesis. The priors and the posteriors are what sum to 1."
            ),
            tr(
              "a grade arredonda cada célula para um número inteiro de casos. Com prioris muito pequenas, a fração de pontos pode diferir da posteriori exata da tabela.",
              "the grid rounds each cell to a whole number of cases. With very small priors, the share of dots may differ from the exact posterior in the table."
            )
          ),
          c(
            tr("Taxa base", "Base rate"),
            tr("Verossimilhanças não são uma distribuição", "Likelihoods are not a distribution"),
            tr("Arredondamento da grade", "Grid rounding")
          )
        )
      )
    )
  )
}

mod_bayes_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    exemplos <- exemplos_bayes()

    shiny::observeEvent(input$preset, {
      v <- exemplos[[input$preset]]$valores
      shiny::updateSliderInput(session, "priori1", value = v[["priori1"]])
      shiny::updateSliderInput(session, "verossim1", value = v[["verossim1"]])
      shiny::updateSliderInput(session, "verossim2", value = v[["verossim2"]])
    }, ignoreInit = TRUE)

    exemplo <- shiny::reactive({
      shiny::req(input$preset %in% names(exemplos))
      exemplos[[input$preset]]
    })

    output$descricao <- shiny::renderUI({
      shiny::p(class = "small text-muted", exemplo()$descricao)
    })

    res <- shiny::reactive({
      shiny::req(
        is.finite(input$priori1),
        is.finite(input$verossim1),
        is.finite(input$verossim2)
      )
      calcular_bayes_particao(
        nomes = exemplo()$hipoteses,
        priori = c(input$priori1, 1 - input$priori1),
        verossim = c(input$verossim1, input$verossim2)
      )
    })

    output$grafico_bayes <- shiny::renderPlot({
      r <- res()
      tb <- r$tabela
      p <- paleta_modo(modo())

      df_plot <- data.frame(
        hipotese = rep(tb$hipotese, 2),
        etapa = factor(
          c(rep(tr("Priori P(Cᵢ)", "Prior P(Cᵢ)"), 2),
            rep(tr("Posteriori P(Cᵢ | A)", "Posterior P(Cᵢ | A)"), 2)),
          levels = c(tr("Priori P(Cᵢ)", "Prior P(Cᵢ)"), tr("Posteriori P(Cᵢ | A)", "Posterior P(Cᵢ | A)"))
        ),
        valor = c(tb$priori, tb$posteriori),
        stringsAsFactors = FALSE
      )

      ggplot2::ggplot(df_plot, ggplot2::aes(x = hipotese, y = valor, fill = etapa)) +
        ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.68), width = 0.6) +
        ggplot2::geom_text(
          ggplot2::aes(label = fmt_pct(valor, 1)),
          position = ggplot2::position_dodge(width = 0.68),
          vjust = -0.35, size = 3.8, colour = p$texto
        ) +
        ggplot2::scale_fill_manual(values = c(p$neutro, cores_app$destaque)) +
        ggplot2::scale_y_continuous(limits = c(0, 1.12), labels = rotulos_eixo()) +
        ggplot2::labs(
          title = tr("Atualização da crença após observar A", "Belief update after observing A"),
          x = NULL,
          y = tr("Probabilidade", "Probability"),
          fill = NULL
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(legend.position = "top")
    })

    output$grafico_grade <- shiny::renderPlot({
      r <- res()
      p <- paleta_modo(modo())
      n_tot <- 1000L
      cont <- contagens_naturais(r$tabela, n_tot)
      niveis <- names(cont)

      grade <- data.frame(
        x = rep(1:40, each = 25),
        y = rep(25:1, times = 40),
        cat = factor(rep(niveis, times = cont), levels = niveis),
        stringsAsFactors = FALSE
      )

      nomes_cat <- c(
        C1_A = tr("C₁ e A", "C₁ and A"),
        C2_A = tr("C₂ e A", "C₂ and A"),
        C1_nA = tr("C₁ sem A", "C₁ without A"),
        C2_nA = tr("C₂ sem A", "C₂ without A")
      )

      ggplot2::ggplot(grade, ggplot2::aes(x = x, y = y, colour = cat)) +
        ggplot2::geom_point(size = 2.0) +
        ggplot2::scale_colour_manual(
          values = c(C1_A = cores_app$destaque, C2_A = cores_app$referencia, C1_nA = "#f59f00", C2_nA = p$grade),
          labels = nomes_cat,
          drop = FALSE
        ) +
        ggplot2::labs(
          title = tr("Onde estão os casos com A?", "Where are the cases with A?"),
          subtitle = sprintf(
            tr("%d em C₁ e A contra %d em C₂ e A (total de A = %d)", "%d in C₁ and A vs. %d in C₂ and A (total A = %d)"),
            cont[["C1_A"]], cont[["C2_A"]], cont[["C1_A"]] + cont[["C2_A"]]
          ),
          x = NULL, y = NULL, colour = NULL
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(
          axis.text = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          legend.position = "bottom"
        )
    })

    output$tabela_bayes <- shiny::renderUI({
      r <- res()
      tb <- r$tabela
      df <- data.frame(
        Hipotese = tb$hipotese,
        Priori = fmt_num(tb$priori, 4),
        Verossim = fmt_num(tb$verossimilhanca, 4),
        Conjunta = fmt_num(tb$conjunta, 4),
        Posteriori = fmt_num(tb$posteriori, 4),
        stringsAsFactors = FALSE
      )
      names(df) <- c(
        tr("Hipótese", "Hypothesis"),
        tr("Priori P(Cᵢ)", "Prior P(Cᵢ)"),
        tr("Verossim. P(A | Cᵢ)", "Likelih. P(A | Cᵢ)"),
        tr("Conjunta P(Cᵢ ∩ A)", "Joint P(Cᵢ ∩ A)"),
        tr("Posteriori P(Cᵢ | A)", "Posterior P(Cᵢ | A)")
      )
      tabela_html(df, c("", "num", "num", "num", "num"))
    })

    output$texto_total <- shiny::renderUI({
      r <- res()
      shiny::p(class = "mt-3 mb-0", sprintf(
        tr(
          "Probabilidade total P(A) = Σ P(Cⱼ) P(A | Cⱼ) = %s. A posteriori divide cada conjunta por P(A).",
          "Total probability P(A) = Σ P(Cⱼ) P(A | Cⱼ) = %s. The posterior divides each joint term by P(A)."
        ),
        fmt_num(r$prob_total, 4)
      ))
    })

    output$texto_odds <- shiny::renderUI({
      r <- res()
      if (is.null(r$odds)) return(NULL)
      o <- r$odds
      shiny::tagList(
        shiny::p(sprintf(
          tr("Chances a priori P(C₁)/P(C₂) = %s", "Prior odds P(C₁)/P(C₂) = %s"),
          fmt_num(o$odds_priori, 4)
        )),
        shiny::p(sprintf(
          tr("Razão de verossimilhanças P(A | C₁)/P(A | C₂) = %s", "Likelihood ratio P(A | C₁)/P(A | C₂) = %s"),
          fmt_num(o$razao_verossim, 4)
        )),
        shiny::p(shiny::strong(sprintf(
          tr("Chances a posteriori = %s × %s = %s", "Posterior odds = %s × %s = %s"),
          fmt_num(o$odds_priori, 4),
          fmt_num(o$razao_verossim, 4),
          fmt_num(o$odds_posteriori, 4)
        ))),
        shiny::p(class = "text-muted mb-0", tr(
          "A probabilidade total P(A) aparece no numerador e no denominador das chances e se cancela. Por isso a conta de atualizações sucessivas fica mais simples.",
          "The total probability P(A) appears in both the numerator and the denominator of the odds and cancels out. That is why successive updates are simpler to compute."
        ))
      )
    })
  })
}
