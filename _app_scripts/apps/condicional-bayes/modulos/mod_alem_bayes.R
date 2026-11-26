# =============================================================================
# Script:  mod_alem_bayes.R (Para ir além: independência mútua, Três Prisioneiros
#          e atualização sequencial / crítica de modelo)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-26
#
# Entrada:
#   - Seletores de contraexemplos, simulação dos Três Prisioneiros, histórico
#     de filhos (hemofilia, Gelman Seção 1.4) e contexto de 'radom'
# Saída:
#   - Verificador de independência mútua, simulador dos Três Prisioneiros,
#     trajetória sequencial + preditiva a posteriori e ajuste de contexto
# =============================================================================

mod_alem_bayes_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header(tr("Independência dois a dois vs. mútua (Casella & Berger, 1.3)", "Pairwise vs. mutual independence (Casella & Berger, 1.3)")),
        bslib::card_body(
          shiny::selectInput(
            ns("caso_indep"),
            tr("Contraexemplo ou referência", "Counterexample or reference"),
            choices = stats::setNames(
              c("ex1311", "ex1310", "mutua"),
              c(
                tr("Ex. 1.3.11: dois a dois ok, falha no triplo", "Ex. 1.3.11: pairwise holds, triple fails"),
                tr("Ex. 1.3.10: triplo ok, falha dois a dois", "Ex. 1.3.10: triple holds, pairwise fails"),
                tr("3 moedas honestas: independência mútua", "3 fair coins: mutual independence")
              )
            )
          ),
          shiny::uiOutput(ns("tabela_indep_mutua"))
        )
      ),
      bslib::card(
        bslib::card_header(tr("Os Três Prisioneiros (Casella & Berger, Ex. 1.3.4)", "The Three Prisoners (Casella & Berger, Ex. 1.3.4)")),
        bslib::card_body(
          shiny::p(tr(
            "A, B e C têm chance 1/3 de perdão. A pede ao guarda que nomeie outro prisioneiro que será executado (W = 'B será executado'). A chance de A continua 1/3 porque o evento observado W não equivale a Bᶜ.",
            "A, B and C each have 1/3 chance of pardon. A asks the warden to name another prisoner to be executed (W = 'B will be executed'). A's chance remains 1/3 because the observed event W is not equivalent to Bᶜ."
          )),
          shiny::div(
            class = "d-flex align-items-center gap-3 mb-3",
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
        bslib::card_header(tr("Atualização sequencial e preditiva (Gelman et al., Seção 1.4)", "Sequential updating and prediction (Gelman et al., Sec. 1.4)")),
        bslib::card_body(
          shiny::p(tr(
            "Caso genético de hemofilia: priori P(θ = 1) = 0,50 de a mãe ser portadora. Cada filho observado atualiza a posteriori, que vira a priori da etapa seguinte e define a preditiva P(ỹ = 1 | y) para o próximo filho.",
            "Hemophilia genetics case: prior P(θ = 1) = 0.50 that the mother is a carrier. Each observed son updates the posterior, which becomes the next stage's prior and determines the posterior predictive P(ỹ = 1 | y) for the next son."
          )),
          shiny::div(
            class = "d-flex gap-2 mb-3",
            shiny::actionButton(ns("add_filho_0"), tr("+ Filho não afetado (0)", "+ Unaffected son (0)"), class = "btn-sm btn-outline-primary"),
            shiny::actionButton(ns("add_filho_1"), tr("+ Filho afetado (1)", "+ Affected son (1)"), class = "btn-sm btn-outline-danger"),
            shiny::actionButton(ns("reset_filhos"), tr("Restaurar 0, 0, 0", "Reset to 0, 0, 0"), class = "btn-sm btn-outline-secondary")
          ),
          shiny::plotOutput(ns("grafico_hemofilia"), height = "250px"),
          shiny::uiOutput(ns("tabela_hemofilia"))
        )
      ),
      bslib::card(
        bslib::card_header(tr("Crítica de modelo: 'radom' (Gelman et al., Passo 3)", "Model checking: 'radom' (Gelman et al., Step 3)")),
        bslib::card_body(
          shiny::p(tr(
            "Sem contexto, a frequência baixa do erro de digitação perde para a palavra rara 'radom' (posteriori 67,3%). Ajustar a priori ao contexto de um texto de estatística P(θ | x) corrige o modelo.",
            "Without context, the low typo rate loses to the rare word 'radom' (posterior 67.3%). Conditioning the prior on a statistics text context P(θ | x) refines the model."
          )),
          shiny::sliderInput(
            ns("mult_radom"),
            tr("Peso contextual para 'random' em estatística", "Contextual weight for 'random' in statistics"),
            min = 1, max = 50, value = 1, step = 1
          ),
          shiny::uiOutput(ns("tabela_radom"))
        )
      )
    )
  )
}

mod_alem_bayes_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    output$tabela_indep_mutua <- shiny::renderUI({
      shiny::req(input$caso_indep)
      r <- contraexemplo_independencia(input$caso_indep)
      df <- data.frame(
        Verificacao = c(
          "P(A), P(B), P(C)",
          "P(A ∩ B) vs. P(A)P(B)",
          "P(A ∩ C) vs. P(A)P(C)",
          "P(B ∩ C) vs. P(B)P(C)",
          "P(A ∩ B ∩ C) vs. P(A)P(B)P(C)"
        ),
        Resultado = c(
          paste(fmt_num(r$p_a, 3), fmt_num(r$p_b, 3), fmt_num(r$p_c, 3), sep = ", "),
          paste(fmt_num(r$p_ab, 3), "vs.", fmt_num(r$p_a * r$p_b, 3)),
          paste(fmt_num(r$p_ac, 3), "vs.", fmt_num(r$p_a * r$p_c, 3)),
          paste(fmt_num(r$p_bc, 3), "vs.", fmt_num(r$p_b * r$p_c, 3)),
          paste(fmt_num(r$p_abc, 3), "vs.", fmt_num(r$p_a * r$p_b * r$p_c, 3))
        ),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Condição", "Condition"), tr("Observado vs. Produto", "Observed vs. Product"))
      veredicto <- if (r$mutua) {
        tr("Independência mútua satisfeita.", "Mutual independence holds.")
      } else {
        tr("NÃO há independência mútua.", "Mutual independence FAILS.")
      }
      shiny::tagList(
        tabela_html(df, c("", "num")),
        shiny::p(class = "mt-2 mb-0 fw-semibold", veredicto)
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
        tr("Entre %d casos com W = B: A = %s, C = %s", "Among %d cases with W = B: A = %s, C = %s"),
        s$n_w_b, fmt_pct(s$prop_a_dado_w, 1), fmt_pct(s$prop_c_dado_w, 1)
      )
    })

    output$tabela_prisioneiros <- shiny::renderUI({
      s <- sim_p()
      df <- data.frame(
        Prisioneiro = c("A", "B", "C"),
        Priori = c(fmt_num(1 / 3, 4), fmt_num(1 / 3, 4), fmt_num(1 / 3, 4)),
        Verossim = c(fmt_num(0.5, 4), fmt_num(0, 4), fmt_num(1, 4)),
        Posteriori = c(fmt_num(1 / 3, 4), fmt_num(0, 4), fmt_num(2 / 3, 4)),
        Simulada = if (is.null(s)) c("—", "—", "—") else c(fmt_num(s$prop_a_dado_w, 4), "0.0000", fmt_num(s$prop_c_dado_w, 4)),
        stringsAsFactors = FALSE
      )
      names(df) <- c(
        tr("Prisioneiro", "Prisoner"),
        tr("Priori", "Prior"),
        "P(W = B | ·)",
        tr("Posteriori", "Posterior"),
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
          x = tr("Etapa (filho observado)", "Stage (observed son)"),
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
        tr("Etapa", "Stage"),
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
        tr("Priori p(θ|x)", "Prior p(θ|x)"),
        "p('radom' | θ)",
        tr("Posteriori", "Posterior")
      )
      tabela_html(df, c("", "num", "num", "num"))
    })
  })
}
