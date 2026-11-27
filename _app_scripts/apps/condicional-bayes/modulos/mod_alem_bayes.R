# =============================================================================
# Script:  mod_alem_bayes.R (Para ir além: independência mútua, Três Prisioneiros
#          e atualização sequencial / crítica de modelo)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Seletores dos exemplos de Casella e Berger (seção 1.3), simulação dos Três
#     Prisioneiros, histórico de filhos (hemofilia, Gelman et al., seção 1.4) e
#     contexto de 'radom'
# Saída:
#   - Verificador de independência mútua, simulador dos Três Prisioneiros,
#     trajetória sequencial + preditiva a posteriori e ajuste de contexto
# =============================================================================

# Descrição dos eventos e leitura de cada exemplo de independência
textos_indep_mutua <- function(caso) {
  switch(caso,
    ex1310 = list(
      eventos = tr(
        "Dois dados honestos, 36 resultados equiprováveis. A = sair uma dupla; B = soma entre 7 e 10; C = soma 2, 7 ou 8.",
        "Two fair dice, 36 equally likely outcomes. A = doubles; B = sum between 7 and 10; C = sum is 2, 7 or 8."
      ),
      leitura = tr(
        "O produto triplo vale (1/36 dos dois lados), mas A e B, e B e C, são dependentes. O produto triplo não garante a independência dois a dois.",
        "The triple product holds (1/36 on both sides), but A and B, and B and C, are dependent. The triple product does not guarantee pairwise independence."
      )
    ),
    ex1311 = list(
      eventos = tr(
        "Nove sequências equiprováveis de letras: aaa, bbb, ccc e as seis permutações de abc. A, B e C = a letra a ocupa a 1ª, a 2ª e a 3ª posição.",
        "Nine equally likely letter strings: aaa, bbb, ccc and the six permutations of abc. A, B and C = the letter a is in the 1st, 2nd and 3rd position."
      ),
      leitura = tr(
        "Os três pares são independentes, mas o produto triplo falha: P(A ∩ B ∩ C) = 1/9, e não 1/27. A independência dois a dois, sozinha, não basta.",
        "All three pairs are independent, but the triple product fails: P(A ∩ B ∩ C) = 1/9, not 1/27. Pairwise independence alone is not enough."
      )
    ),
    tr(
      list(
        eventos = "Três lançamentos de uma moeda honesta, 8 resultados equiprováveis. A, B e C = cara no 1º, no 2º e no 3º lançamento.",
        leitura = "Todos os pares e o produto triplo batem: os três eventos são mutuamente independentes (Def. 1.3.12)."
      ),
      list(
        eventos = "Three tosses of a fair coin, 8 equally likely outcomes. A, B and C = heads on the 1st, 2nd and 3rd toss.",
        leitura = "Every pair and the triple product match: the three events are mutually independent (Def. 1.3.12)."
      )
    )
  )
}

mod_alem_bayes_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header(tr("Independência dois a dois vs. mútua (Casella e Berger, seção 1.3)", "Pairwise vs. mutual independence (Casella and Berger, section 1.3)")),
        bslib::card_body(
          shiny::selectInput(
            ns("caso_indep"),
            tr("Exemplo do livro", "Book example"),
            choices = stats::setNames(
              c("ex1311", "ex1310", "ex1313"),
              c(
                tr("Ex. 1.3.11 (letras): dois a dois vale, o triplo falha", "Ex. 1.3.11 (letters): pairwise holds, triple fails"),
                tr("Ex. 1.3.10 (dois dados): o triplo vale, dois a dois falha", "Ex. 1.3.10 (two dice): triple holds, pairwise fails"),
                tr("Ex. 1.3.13 (três moedas): independência mútua", "Ex. 1.3.13 (three coins): mutual independence")
              )
            )
          ),
          shiny::uiOutput(ns("tabela_indep_mutua"))
        )
      ),
      bslib::card(
        bslib::card_header(tr("Os Três Prisioneiros (Casella e Berger, Ex. 1.3.4)", "The Three Prisoners (Casella and Berger, Ex. 1.3.4)")),
        bslib::card_body(
          shiny::p(tr(
            "A, B e C estão condenados, e um deles, sorteado, será perdoado. A pede ao guarda que diga o nome de um dos outros dois que será executado, e ouve \"B\" (evento W). A probabilidade de A continua 1/3, porque W não é o mesmo que Bᶜ: se o perdoado for A, o guarda pode dizer B ou C.",
            "A, B and C are sentenced to death, and one of them, drawn at random, will be pardoned. A asks the warden to name one of the other two who will be executed, and hears \"B\" (event W). A's probability stays at 1/3, because W is not the same as Bᶜ: if A is the one pardoned, the warden may say B or C."
          )),
          shiny::div(
            class = "d-flex flex-wrap align-items-center gap-3 mb-3",
            shiny::actionButton(ns("sim_prisioneiros"), tr("Simular 2.000 sorteios", "Simulate 2,000 trials"), class = "btn-sm btn-primary"),
            shiny::textOutput(ns("resumo_prisioneiros"), inline = TRUE)
          ),
          shiny::uiOutput(ns("tabela_prisioneiros"))
        )
      )
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header(tr("Atualização sequencial e preditiva (Gelman et al., seções 1.3 e 1.4)", "Sequential updating and prediction (Gelman et al., sections 1.3 and 1.4)")),
        bslib::card_body(
          shiny::p(tr(
            "Caso genético da hemofilia: a mulher tem priori P(θ = 1) = 0,50 de ser portadora. Cada filho observado atualiza a posteriori, que vira a priori da etapa seguinte e define a preditiva P(ỹ = 1 | y) = 0,5 · P(θ = 1 | y) de o próximo filho ser afetado. Com dois filhos não afetados, a posteriori é 0,20; com o terceiro, 0,111, como no livro.",
            "Hemophilia genetics case: the woman has prior P(θ = 1) = 0.50 of being a carrier. Each observed son updates the posterior, which becomes the prior for the next stage and sets the posterior predictive P(ỹ = 1 | y) = 0.5 · P(θ = 1 | y) that the next son is affected. With two unaffected sons, the posterior is 0.20; with the third, 0.111, as in the book."
          )),
          shiny::div(
            class = "d-flex flex-wrap gap-2 mb-3",
            shiny::actionButton(ns("add_filho_0"), tr("+ Filho não afetado (0)", "+ Unaffected son (0)"), class = "btn-sm btn-outline-primary"),
            shiny::actionButton(ns("add_filho_1"), tr("+ Filho afetado (1)", "+ Affected son (1)"), class = "btn-sm btn-outline-danger"),
            shiny::actionButton(ns("reset_filhos"), tr("Restaurar 0, 0, 0", "Reset to 0, 0, 0"), class = "btn-sm btn-outline-secondary")
          ),
          shiny::plotOutput(ns("grafico_hemofilia"), height = "250px"),
          shiny::uiOutput(ns("tabela_hemofilia"))
        )
      ),
      bslib::card(
        bslib::card_header(tr("Crítica de modelo: 'radom' (Gelman et al., passo 3)", "Model checking: 'radom' (Gelman et al., step 3)")),
        bslib::card_body(
          shiny::p(tr(
            "Alguém digita 'radom'. Sem contexto, a palavra rara 'radom' ganha (posteriori de 67,3%), porque digitar 'radom' querendo 'random' é um erro pouco frequente. Num texto de estatística, porém, 'random' é bem mais comum: a priori deveria ser p(θ | x), condicionada ao contexto x. O controle abaixo multiplica a priori de 'random' por um peso que representa esse contexto.",
            "Someone types 'radom'. Without context, the rare word 'radom' wins (posterior of 67.3%), because typing 'radom' when meaning 'random' is an uncommon mistake. In a statistics text, though, 'random' is far more common: the prior should be p(θ | x), conditioned on the context x. The control below multiplies the prior of 'random' by a weight that stands for that context."
          )),
          shiny::sliderInput(
            ns("mult_radom"),
            tr("Peso contextual para 'random' em estatística", "Contextual weight for 'random' in statistics"),
            min = 1, max = 50, value = 1, step = 1
          ),
          shiny::uiOutput(ns("tabela_radom"))
        )
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header(rotulo_entenda()),
        bslib::card_body(
          shiny::p(tr(
            "Para três ou mais eventos, a independência mútua exige o produto em toda subcoleção: em cada par e no trio (Casella e Berger, Def. 1.3.12). Os Exemplos 1.3.10 e 1.3.11 mostram que nenhuma das duas condições basta sozinha.",
            "For three or more events, mutual independence requires the product rule for every subcollection: each pair and the triple (Casella and Berger, Def. 1.3.12). Examples 1.3.10 and 1.3.11 show that neither condition is enough on its own."
          )),
          shiny::p(tr(
            "Na hemofilia, atualizar filho por filho dá o mesmo resultado que usar todos os filhos de uma vez, em qualquer ordem. McElreath (seção 2.2.2) chama isso de atualização bayesiana: cada posteriori é o ponto de partida da próxima inferência.",
            "In the hemophilia case, updating son by son gives the same result as using all the sons at once, in any order. McElreath (section 2.2.2) calls this Bayesian updating: each posterior is the starting point of the next inference."
          )),
          shiny::p(
            shiny::strong(tr("Experimente:", "Try this:")),
            tr(
              "adicione filhos não afetados e veja a posteriori cair cada vez menos a cada filho. Um único filho afetado leva a posteriori a 1, e nenhum filho não afetado depois disso a tira de lá. No 'radom', descubra o menor peso que faz 'random' passar à frente.",
              "add unaffected sons and watch the posterior drop by less with each son. A single affected son takes the posterior to 1, and no unaffected son after that moves it. In 'radom', find the smallest weight that puts 'random' ahead."
            )
          )
        )
      ),
      cartao_cuidados(
        tr("Cuidados", "Watch out"),
        stats::setNames(
          list(
            tr(
              "nos Três Prisioneiros, o erro de A é tratar W como Bᶜ. Com Bᶜ, a conta daria P(A | Bᶜ) = 1/2; com W, dá 1/3. A conta depende de como o guarda escolhe o nome que diz.",
              "in the Three Prisoners, A's mistake is to treat W as Bᶜ. With Bᶜ, the computation would give P(A | Bᶜ) = 1/2; with W, it gives 1/3. The answer depends on how the warden picks which name to say."
            ),
            tr(
              "a conta da hemofilia supõe que os filhos são independentes dado θ. Gêmeos idênticos, por exemplo, não trariam duas informações.",
              "the hemophilia computation assumes the sons are independent given θ. Identical twins, for instance, would not bring two pieces of information."
            ),
            tr(
              "a tabela mostra a priori reescalada entre as três candidatas, para somar 100%. A posteriori é a mesma com ou sem esse reescalonamento.",
              "the table shows the prior rescaled among the three candidates, so that it sums to 100%. The posterior is the same with or without that rescaling."
            )
          ),
          c(
            tr("O evento observado", "The observed event"),
            tr("Independência condicional", "Conditional independence"),
            tr("Priori do 'radom'", "Prior in 'radom'")
          )
        )
      )
    )
  )
}

mod_alem_bayes_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    output$tabela_indep_mutua <- shiny::renderUI({
      shiny::req(input$caso_indep %in% c("ex1310", "ex1311", "ex1313"))
      r <- contraexemplo_independencia(input$caso_indep)
      txt <- textos_indep_mutua(input$caso_indep)
      n <- r$n
      k <- r$k
      fr <- function(kk, nn = n) fmt_frac(kk, nn)
      marca <- function(ok) if (ok) "=" else "≠"

      df <- data.frame(
        Verificacao = c(
          "P(A), P(B), P(C)",
          "P(A ∩ B) vs. P(A)P(B)",
          "P(A ∩ C) vs. P(A)P(C)",
          "P(B ∩ C) vs. P(B)P(C)",
          "P(A ∩ B ∩ C) vs. P(A)P(B)P(C)"
        ),
        Resultado = c(
          paste(fr(k[["A"]]), fr(k[["B"]]), fr(k[["C"]]), sep = ", "),
          paste(fr(r$k_ab), marca(r$par_ok[["ab"]]), fr(k[["A"]] * k[["B"]], n^2)),
          paste(fr(r$k_ac), marca(r$par_ok[["ac"]]), fr(k[["A"]] * k[["C"]], n^2)),
          paste(fr(r$k_bc), marca(r$par_ok[["bc"]]), fr(k[["B"]] * k[["C"]], n^2)),
          paste(fr(r$k_abc), marca(r$produto_triplo), fr(k[["A"]] * k[["B"]] * k[["C"]], n^3))
        ),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Condição", "Condition"), tr("Observado vs. produto", "Observed vs. product"))
      veredicto <- if (r$mutua) {
        tr("Independência mútua satisfeita.", "Mutual independence holds.")
      } else {
        tr("Não há independência mútua.", "Mutual independence fails.")
      }
      shiny::tagList(
        shiny::p(class = "small text-muted", txt$eventos),
        tabela_html(df, c("", "num")),
        shiny::p(class = "mt-2 mb-1 fw-semibold", veredicto),
        shiny::p(class = "mb-0", txt$leitura)
      )
    })

    sim_p <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$sim_prisioneiros, {
      sim_p(simular_tres_prisioneiros(2000))
    })

    output$resumo_prisioneiros <- shiny::renderText({
      s <- sim_p()
      if (is.null(s)) return("")
      sprintf(
        tr("Entre %d casos com W: A = %s, C = %s", "Among %d cases with W: A = %s, C = %s"),
        s$n_w_b, fmt_pct(s$prop_a_dado_w, 1), fmt_pct(s$prop_c_dado_w, 1)
      )
    })

    output$tabela_prisioneiros <- shiny::renderUI({
      s <- sim_p()
      df <- data.frame(
        Prisioneiro = c("A", "B", "C"),
        Priori = c("1/3", "1/3", "1/3"),
        Verossim = c("1/2", "0", "1"),
        Posteriori = c("1/3", "0", "2/3"),
        Simulada = if (is.null(s)) c("—", "—", "—") else c(fmt_num(s$prop_a_dado_w, 3), fmt_num(0, 3), fmt_num(s$prop_c_dado_w, 3)),
        stringsAsFactors = FALSE
      )
      names(df) <- c(
        tr("Perdoado", "Pardoned"),
        tr("Priori", "Prior"),
        tr("P(W | perdoado)", "P(W | pardoned)"),
        tr("Posteriori P(· | W)", "Posterior P(· | W)"),
        tr("Simulação", "Simulation")
      )
      tabela_html(df, c("", "num", "num", "num", "num"))
    })

    filhos_seq <- shiny::reactiveVal(c(0L, 0L, 0L))
    shiny::observeEvent(input$add_filho_0, {
      filhos_seq(c(filhos_seq(), 0L))
    })
    shiny::observeEvent(input$add_filho_1, {
      filhos_seq(c(filhos_seq(), 1L))
    })
    shiny::observeEvent(input$reset_filhos, {
      filhos_seq(c(0L, 0L, 0L))
    })

    traj <- shiny::reactive({
      atualizar_hemofilia_seq(0.5, filhos_seq())
    })

    output$grafico_hemofilia <- shiny::renderPlot({
      df <- traj()
      p <- paleta_modo(modo())
      ggplot2::ggplot(df, ggplot2::aes(x = etapa, y = prob_portadora)) +
        ggplot2::geom_line(colour = cores_app$destaque, linewidth = 1.1) +
        ggplot2::geom_point(colour = cores_app$destaque, size = 3) +
        ggplot2::geom_text(
          ggplot2::aes(label = fmt_num(prob_portadora, 3)),
          vjust = -0.7, size = 3.6, colour = p$texto
        ) +
        ggplot2::scale_x_continuous(breaks = df$etapa) +
        ggplot2::scale_y_continuous(limits = c(0, 1.12), labels = rotulos_eixo()) +
        ggplot2::labs(
          title = tr("Trajetória de P(θ = 1 | dados)", "Trajectory of P(θ = 1 | data)"),
          x = tr("Filhos observados", "Sons observed"),
          y = tr("P(portadora)", "P(carrier)")
        ) +
        tema_grafico(modo())
    })

    output$tabela_hemofilia <- shiny::renderUI({
      df <- traj()
      rot_ev <- purrr::map_chr(df$evento, \(e) {
        if (e == -1L) tr("Priori inicial", "Initial prior")
        else if (e == 0L) tr("Filho não afetado (0)", "Unaffected son (0)")
        else tr("Filho afetado (1)", "Affected son (1)")
      })
      out <- data.frame(
        Etapa = df$etapa,
        Evento = rot_ev,
        Portadora = fmt_num(df$prob_portadora, 4),
        Preditiva = fmt_num(df$pred_afetado, 4),
        stringsAsFactors = FALSE
      )
      names(out) <- c(
        tr("Filhos", "Sons"),
        tr("Informação", "Information"),
        "P(θ = 1 | y)",
        "P(ỹ = 1 | y)"
      )
      tabela_html(out, c("num", "", "num", "num"))
    })

    output$tabela_radom <- shiny::renderUI({
      shiny::req(is.finite(input$mult_radom))
      r <- calcular_radom(input$mult_radom)
      df <- data.frame(
        Palavra = r$palavra,
        Priori = fmt_pct(r$priori, 2),
        Verossim = fmt_num(r$verossimilhanca, 5),
        Posteriori = fmt_pct(r$posteriori, 1),
        stringsAsFactors = FALSE
      )
      names(df) <- c(
        tr("Candidata θ", "Candidate θ"),
        tr("Priori", "Prior"),
        "p('radom' | θ)",
        tr("Posteriori", "Posterior")
      )
      tabela_html(df, c("", "num", "num", "num"))
    })
  })
}
