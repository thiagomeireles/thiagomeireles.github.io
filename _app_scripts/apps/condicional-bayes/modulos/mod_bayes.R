# =============================================================================
# Script:  mod_bayes.R (Aba 3: Probabilidade total e Teorema de Bayes)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Exemplos (teste diagnóstico, queda da bolsa, cinco urnas e seleção de
#     funcionários de Morettin e Bussab, Código Morse de Casella e Berger),
#     número de hipóteses (2 ou 3) e sliders
# Saída:
#   - Tabela completa de Bayes, chances a posteriori por par de hipóteses
#     (Gelman et al., eq. 1.5), gráfico priori vs. posteriori, grade de 1.000
#     frequências naturais e histórico de atualizações sucessivas
# =============================================================================

# Valores de partida e significado das hipóteses e de A em cada exemplo
exemplos_bayes <- function() {
  list(
    diagnostico = list(
      k = 2,
      valores = c(priori1 = 0.010, priori2 = 0.990, verossim1 = 0.95, verossim2 = 0.05, verossim3 = 0.50),
      hipoteses = c(tr("C₁: Doença presente", "C₁: Disease present"), tr("C₂: Doença ausente", "C₂: Disease absent")),
      descricao = tr(
        "A = teste positivo. A doença atinge 1% das pessoas; o teste dá positivo em 95% dos doentes e em 5% dos sadios.",
        "A = positive test. The disease affects 1% of people; the test is positive in 95% of the sick and 5% of the healthy."
      )
    ),
    bolsa = list(
      k = 2,
      valores = c(priori1 = 0.100, priori2 = 0.900, verossim1 = 0.20, verossim2 = 0.05, verossim3 = 0.50),
      hipoteses = c(tr("C₁: Bolsa cai (E)", "C₁: Market falls (E)"), tr("C₂: Bolsa sobe (Eᶜ)", "C₂: Market rises (Eᶜ)")),
      descricao = tr(
        "A = notícia de alta do dólar. A previsão inicial de queda da bolsa é 0,10; a notícia precedeu 20% das quedas e 5% das altas (Morettin e Bussab, Ex. 5.16).",
        "A = news of a rising dollar. The initial forecast of a market fall is 0.10; the news came before 20% of falls and 5% of rises (Morettin and Bussab, Ex. 5.16)."
      )
    ),
    morse = list(
      k = 2,
      valores = c(priori1 = 0.429, priori2 = 0.571, verossim1 = 0.875, verossim2 = 0.125, verossim3 = 0.50),
      hipoteses = c(tr("C₁: Ponto enviado", "C₁: Dot sent"), tr("C₂: Traço enviado", "C₂: Dash sent")),
      descricao = tr(
        "A = ponto recebido. Pontos e traços são enviados na proporção 3:4, então P(C₁) = 3/7 ≈ 0,429, e 1/8 dos símbolos é trocado na transmissão (Casella e Berger, Ex. 1.3.6).",
        "A = dot received. Dots and dashes are sent in a 3:4 ratio, so P(C₁) = 3/7 ≈ 0.429, and 1/8 of the symbols are switched in transmission (Casella and Berger, Ex. 1.3.6)."
      )
    ),
    urnas = list(
      k = 3,
      valores = c(priori1 = 0.400, priori2 = 0.400, verossim1 = 0.500, verossim2 = 0.333, verossim3 = 1.000),
      hipoteses = c(
        tr("C₁: Urna tipo 1", "C₁: Type 1 urn"),
        tr("C₂: Urna tipo 2", "C₂: Type 2 urn"),
        tr("C₃: Urna tipo 3", "C₃: Type 3 urn")
      ),
      descricao = tr(
        "A = sair bola branca. São cinco urnas, cada uma com seis bolas: duas do tipo 1 (3 brancas), duas do tipo 2 (2 brancas) e uma do tipo 3 (6 brancas). O slider aproxima P(A | C₂) = 1/3 por 0,333, e o livro chega a P(C₃ | A) = 3/8 = 0,375 (Morettin e Bussab, Ex. 5.14).",
        "A = drawing a white ball. There are five urns with six balls each: two of type 1 (3 white), two of type 2 (2 white) and one of type 3 (6 white). The slider approximates P(A | C₂) = 1/3 by 0.333, and the book arrives at P(C₃ | A) = 3/8 = 0.375 (Morettin and Bussab, Ex. 5.14)."
      )
    ),
    funcionarios = list(
      k = 3,
      valores = c(priori1 = 0.250, priori2 = 0.500, verossim1 = 0.80, verossim2 = 0.50, verossim3 = 0.20),
      hipoteses = c(
        tr("C₁: Bom no curso", "C₁: Good in training"),
        tr("C₂: Médio no curso", "C₂: Average in training"),
        tr("C₃: Fraco no curso", "C₃: Weak in training")
      ),
      descricao = tr(
        "A = aprovado no teste. No curso de treinamento, 25% saem bons, 50% médios e 25% fracos; o teste aprova 80% dos bons, 50% dos médios e 20% dos fracos. O livro chega a P(C₃ | A) = 0,10 (Morettin e Bussab, Ex. 5.15).",
        "A = passing the test. In the training course, 25% of trainees turn out good, 50% average and 25% weak; the test passes 80% of the good ones, 50% of the average ones and 20% of the weak ones. The book arrives at P(C₃ | A) = 0.10 (Morettin and Bussab, Ex. 5.15)."
      )
    )
  )
}

# Rótulos genéricos quando o número de hipóteses escolhido difere do exemplo
hipoteses_genericas <- function(k) {
  purrr::map_chr(seq_len(k), \(i) paste0("C", c("₁", "₂", "₃")[i]))
}

# Vetor formatado "0,100 / 0,900" para o histórico de atualizações
fmt_vetor <- function(x, digitos = 3) paste(fmt_num(x, digitos), collapse = " / ")

mod_bayes_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 330,
      shiny::selectInput(
        ns("preset"),
        tr("Exemplo", "Example"),
        choices = stats::setNames(
          c("diagnostico", "bolsa", "morse", "urnas", "funcionarios"),
          c(
            tr("Teste diagnóstico (taxa base)", "Diagnostic test (base rate)"),
            tr("Queda da bolsa (Morettin e Bussab, Ex. 5.16)", "Market fall (Morettin and Bussab, Ex. 5.16)"),
            tr("Código Morse (Casella e Berger, Ex. 1.3.6)", "Morse code (Casella and Berger, Ex. 1.3.6)"),
            tr("Cinco urnas (Morettin e Bussab, Ex. 5.14)", "Five urns (Morettin and Bussab, Ex. 5.14)"),
            tr("Seleção de funcionários (Morettin e Bussab, Ex. 5.15)", "Employee selection (Morettin and Bussab, Ex. 5.15)")
          )
        )
      ),
      shiny::uiOutput(ns("descricao")),
      shiny::radioButtons(
        ns("k"),
        tr("Número de hipóteses", "Number of hypotheses"),
        choices = c("2" = "2", "3" = "3"),
        inline = TRUE
      ),
      shiny::sliderInput(
        ns("priori1"),
        tr("Priori da hipótese 1, P(C₁)", "Prior of hypothesis 1, P(C₁)"),
        min = 0, max = 1, value = 0.010, step = passo_bayes
      ),
      shiny::conditionalPanel(
        condition = "input.k == '3'",
        ns = ns,
        shiny::sliderInput(
          ns("priori2"),
          tr("Priori da hipótese 2, P(C₂)", "Prior of hypothesis 2, P(C₂)"),
          min = 0, max = 1, value = 0.400, step = passo_bayes
        )
      ),
      shiny::uiOutput(ns("priori_resto")),
      shiny::sliderInput(
        ns("verossim1"),
        tr("Verossimilhança P(A | C₁)", "Likelihood P(A | C₁)"),
        min = 0, max = 1, value = 0.95, step = passo_bayes
      ),
      shiny::sliderInput(
        ns("verossim2"),
        tr("Verossimilhança P(A | C₂)", "Likelihood P(A | C₂)"),
        min = 0, max = 1, value = 0.05, step = passo_bayes
      ),
      shiny::conditionalPanel(
        condition = "input.k == '3'",
        ns = ns,
        shiny::sliderInput(
          ns("verossim3"),
          tr("Verossimilhança P(A | C₃)", "Likelihood P(A | C₃)"),
          min = 0, max = 1, value = 0.50, step = passo_bayes
        )
      ),
      shiny::actionButton(
        ns("atualizar"),
        tr("Usar a posteriori como nova priori", "Use the posterior as the new prior"),
        class = "btn-sm btn-primary"
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
    bslib::card(
      bslib::card_header(tr("Atualizações sucessivas", "Successive updates")),
      bslib::card_body(
        shiny::p(tr(
          "Cada clique em \"Usar a posteriori como nova priori\" guarda uma linha e leva a posteriori para os sliders de priori. Depois, ajuste as verossimilhanças da nova informação. A última linha é a conta que está na tela agora.",
          "Each click on \"Use the posterior as the new prior\" stores a row and moves the posterior to the prior sliders. Then set the likelihoods of the new information. The last row is the computation currently on screen."
        )),
        shiny::uiOutput(ns("tabela_historico"))
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
            "Na forma de chances, P(A) some: chances a posteriori = chances a priori × razão de verossimilhanças (Gelman et al., eq. 1.5). Com três hipóteses, as chances comparam as hipóteses duas a duas. A grade de 1.000 casos mostra o Teorema de Bayes em contagens: a posteriori de Cᵢ é a fração de pontos de Cᵢ entre todos os pontos de A.",
            "In odds form, P(A) drops out: posterior odds = prior odds × likelihood ratio (Gelman et al., eq. 1.5). With three hypotheses, the odds compare the hypotheses two at a time. The 1,000-case grid shows Bayes' theorem in counts: the posterior of Cᵢ is the share of Cᵢ dots among all the dots in A."
          )),
          shiny::p(
            shiny::strong(tr("Experimente:", "Try this:")),
            tr(
              "no teste diagnóstico, um resultado positivo leva a probabilidade de doença de 1% para só 16%. Clique em \"Usar a posteriori como nova priori\" para simular um segundo teste positivo e veja a posteriori passar de 78%. Na queda da bolsa, faça o mesmo e ajuste as verossimilhanças para as da segunda notícia (queda dos juros), 0,10 e 0,60: a probabilidade de queda vai de 0,10 a 0,31 e depois a 0,07, como no livro.",
              "in the diagnostic test, a positive result takes the probability of disease from 1% to only 16%. Click \"Use the posterior as the new prior\" to simulate a second positive test and watch the posterior pass 78%. In the market fall example, do the same and set the likelihoods to those of the second news item (an interest rate cut), 0.10 and 0.60: the probability of a fall goes from 0.10 to 0.31 and then to 0.07, as in the book."
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
              "as verossimilhanças P(A | Cᵢ) não precisam somar 1, porque cada uma é a probabilidade de A sob uma hipótese diferente. As prioris somam 1, e as posterioris também.",
              "the likelihoods P(A | Cᵢ) need not sum to 1, because each one is the probability of A under a different hypothesis. The priors sum to 1, and so do the posteriors."
            ),
            tr(
              "usar a posteriori como nova priori supõe que a nova informação é independente da anterior, dada a hipótese, ou que as novas verossimilhanças já são condicionais à informação anterior. É o caso do Ex. 5.16: as verossimilhanças da segunda notícia são P(B₂ | E, B₁), e a conta vale do mesmo jeito.",
              "using the posterior as the new prior assumes the new information is independent of the previous one, given the hypothesis, or that the new likelihoods are already conditional on the previous information. That is the case in Ex. 5.16: the likelihoods of the second news item are P(B₂ | E, B₁), and the computation holds just the same."
            ),
            tr(
              "a grade arredonda cada célula para um número inteiro de casos. Com prioris muito pequenas, a fração de pontos pode diferir da posteriori exata da tabela.",
              "the grid rounds each cell to a whole number of cases. With very small priors, the share of dots may differ from the exact posterior in the table."
            )
          ),
          c(
            tr("Taxa base", "Base rate"),
            tr("Verossimilhanças não são uma distribuição", "Likelihoods are not a distribution"),
            tr("Atualização sequencial", "Sequential updating"),
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
    historico <- shiny::reactiveVal(list())

    shiny::observeEvent(input$preset, {
      ex <- exemplos[[input$preset]]
      v <- ex$valores
      historico(list())
      shiny::updateRadioButtons(session, "k", selected = as.character(ex$k))
      shiny::updateSliderInput(session, "priori1", value = v[["priori1"]])
      shiny::updateSliderInput(session, "priori2", value = v[["priori2"]], max = 1 - v[["priori1"]])
      shiny::updateSliderInput(session, "verossim1", value = v[["verossim1"]])
      shiny::updateSliderInput(session, "verossim2", value = v[["verossim2"]])
      shiny::updateSliderInput(session, "verossim3", value = v[["verossim3"]])
    }, ignoreInit = TRUE)

    # Com 3 hipóteses, P(C₂) não pode passar de 1 − P(C₁). Só o máximo muda:
    # o próprio slider recorta o valor, sem sobrescrever o que o exemplo mandou.
    shiny::observeEvent(input$priori1, {
      shiny::req(is.finite(input$priori1))
      shiny::updateSliderInput(session, "priori2", max = arredondar_bayes(1 - input$priori1))
    })

    shiny::observeEvent(input$k, historico(list()), ignoreInit = TRUE)

    k <- shiny::reactive(if (identical(input$k, "3")) 3L else 2L)

    exemplo <- shiny::reactive({
      shiny::req(input$preset %in% names(exemplos))
      exemplos[[input$preset]]
    })

    output$descricao <- shiny::renderUI({
      shiny::p(class = "small text-muted", exemplo()$descricao)
    })

    prioris <- shiny::reactive({
      shiny::req(is.finite(input$priori1))
      if (k() == 2L) {
        c(input$priori1, 1 - input$priori1)
      } else {
        shiny::req(is.finite(input$priori2))
        c(input$priori1, input$priori2, 1 - input$priori1 - input$priori2)
      }
    })

    output$priori_resto <- shiny::renderUI({
      pr <- prioris()
      ultima <- length(pr)
      shiny::p(class = "small text-muted", sprintf(
        tr("Restante: P(C%s) = 1 − %s = %s", "Remainder: P(C%s) = 1 − %s = %s"),
        c("₁", "₂", "₃")[ultima],
        fmt_num(sum(pr[-ultima]), 3),
        fmt_num(pr[ultima], 3)
      ))
    })

    res <- shiny::reactive({
      pr <- prioris()
      shiny::validate(shiny::need(
        pr[length(pr)] >= -1e-9,
        tr("P(C₁) + P(C₂) passa de 1. Diminua uma das prioris.", "P(C₁) + P(C₂) exceeds 1. Lower one of the priors.")
      ))
      ver <- c(input$verossim1, input$verossim2, input$verossim3)[seq_len(k())]
      shiny::req(all(is.finite(ver)))
      nomes <- if (length(exemplo()$hipoteses) == k()) exemplo()$hipoteses else hipoteses_genericas(k())
      r <- calcular_bayes_particao(nomes = nomes, priori = pmax(pr, 0), verossim = ver)
      shiny::validate(shiny::need(
        r$prob_total > 0,
        tr("Com essas verossimilhanças, P(A) = 0 e a posteriori não está definida.", "With these likelihoods, P(A) = 0 and the posterior is undefined.")
      ))
      r
    })

    shiny::observeEvent(input$atualizar, {
      r <- res()
      tb <- r$tabela
      historico(c(historico(), list(tb)))
      nova <- arredondar_bayes(tb$posteriori)
      shiny::updateSliderInput(session, "priori1", value = nova[1])
      if (k() == 3L) {
        shiny::updateSliderInput(session, "priori2", max = arredondar_bayes(1 - nova[1]), value = min(nova[2], arredondar_bayes(1 - nova[1])))
      }
    })

    output$grafico_bayes <- shiny::renderPlot({
      r <- res()
      tb <- r$tabela
      p <- paleta_modo(modo())
      n_h <- nrow(tb)

      df_plot <- data.frame(
        hipotese = factor(rep(tb$hipotese, 2), levels = tb$hipotese),
        etapa = factor(
          c(rep(tr("Priori P(Cᵢ)", "Prior P(Cᵢ)"), n_h),
            rep(tr("Posteriori P(Cᵢ | A)", "Posterior P(Cᵢ | A)"), n_h)),
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
          vjust = -0.35, size = if (n_h == 3) 3.3 else 3.8, colour = p$texto
        ) +
        ggplot2::scale_fill_manual(values = c(p$neutro, cores_app$destaque)) +
        ggplot2::scale_x_discrete(labels = \(x) sub(": ", ":\n", x)) +
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
      n_h <- nrow(r$tabela)
      subs <- c("₁", "₂", "₃")[seq_len(n_h)]

      grade <- data.frame(
        x = rep(1:40, each = 25),
        y = rep(25:1, times = 40),
        cat = factor(rep(niveis, times = cont), levels = niveis),
        stringsAsFactors = FALSE
      )

      nomes_cat <- stats::setNames(
        c(paste0("C", subs, tr(" e A", " and A")), tr("sem A", "without A")),
        niveis
      )
      cores_cat <- stats::setNames(
        c(c(cores_app$destaque, cores_app$referencia, "#f59f00")[seq_len(n_h)], p$grade),
        niveis
      )
      em_a <- cont[seq_len(n_h)]

      ggplot2::ggplot(grade, ggplot2::aes(x = x, y = y, colour = cat)) +
        ggplot2::geom_point(size = 2.0) +
        ggplot2::scale_colour_manual(values = cores_cat, labels = nomes_cat, drop = FALSE) +
        ggplot2::labs(
          title = tr("Onde estão os casos com A?", "Where are the cases with A?"),
          subtitle = sprintf(
            tr("%d casos com A: %s", "%d cases with A: %s"),
            sum(em_a),
            paste(paste0(em_a, tr(" em C", " in C"), subs), collapse = ", ")
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
      o <- r$odds
      subs <- c("₁", "₂", "₃")
      fmt_od <- function(x) ifelse(is.na(x), tr("indefinida", "undefined"), fmt_num(x, 3))
      df <- data.frame(
        Par = paste0("C", subs[o$i], " : C", subs[o$j]),
        Priori = fmt_od(o$odds_priori),
        Razao = fmt_od(o$razao_verossim),
        Posteriori = fmt_od(o$odds_posteriori),
        stringsAsFactors = FALSE
      )
      names(df) <- c(
        tr("Par", "Pair"),
        tr("Chances a priori", "Prior odds"),
        tr("Razão de verossim.", "Likelihood ratio"),
        tr("Chances a posteriori", "Posterior odds")
      )
      shiny::tagList(
        shiny::p(tr(
          "Chances a posteriori = chances a priori × razão de verossimilhanças, para cada par de hipóteses.",
          "Posterior odds = prior odds × likelihood ratio, for each pair of hypotheses."
        )),
        tabela_html(df, c("", "num", "num", "num")),
        shiny::p(class = "text-muted mb-0", tr(
          "A probabilidade total P(A) aparece no numerador e no denominador das chances e se cancela. Por isso a conta de atualizações sucessivas fica mais simples.",
          "The total probability P(A) appears in both the numerator and the denominator of the odds and cancels out. That is why successive updates are simpler to compute."
        ))
      )
    })

    output$tabela_historico <- shiny::renderUI({
      atual <- res()$tabela
      linhas <- c(historico(), list(atual))
      n <- length(linhas)
      df <- data.frame(
        Etapa = c(as.character(seq_len(n - 1)), tr("agora", "now")),
        Priori = purrr::map_chr(linhas, \(t) fmt_vetor(t$priori)),
        Verossim = purrr::map_chr(linhas, \(t) fmt_vetor(t$verossimilhanca)),
        Posteriori = purrr::map_chr(linhas, \(t) fmt_vetor(t$posteriori)),
        stringsAsFactors = FALSE
      )
      names(df) <- c(
        tr("Etapa", "Step"),
        tr("Priori P(C₁) / …", "Prior P(C₁) / …"),
        tr("Verossim. P(A | C₁) / …", "Likelih. P(A | C₁) / …"),
        tr("Posteriori P(C₁ | A) / …", "Posterior P(C₁ | A) / …")
      )
      tabela_html(df, c("", "num", "num", "num"))
    })
  })
}
