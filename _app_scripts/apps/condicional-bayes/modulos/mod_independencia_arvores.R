# =============================================================================
# Script:  mod_independencia_arvores.R (Aba 2: Independência e árvores)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-26
#
# Entrada:
#   - Controles de independência vs. disjunção e parâmetros da urna em 2 etapas
# Saída:
#   - Comparação numérica de P(A ∩ B) vs. P(A)P(B), árvore em 2 etapas e simulação
# =============================================================================

mod_independencia_arvores_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_columns(
    col_widths = c(6, 6),
    bslib::card(
      bslib::card_header(tr("Independência vs. eventos mutuamente exclusivos", "Independence vs. mutually exclusive events")),
      bslib::card_body(
        shiny::sliderInput(
          ns("p_a"),
          "P(A)",
          min = 0.05, max = 0.90, value = 0.40, step = 0.05
        ),
        shiny::sliderInput(
          ns("p_b"),
          "P(B)",
          min = 0.05, max = 0.90, value = 0.50, step = 0.05
        ),
        shiny::sliderInput(
          ns("p_ab"),
          tr("Interseção P(A ∩ B)", "Intersection P(A ∩ B)"),
          min = 0.00, max = 0.40, value = 0.20, step = 0.01
        ),
        shiny::div(
          class = "d-flex gap-2 mb-3",
          shiny::actionButton(ns("btn_indep"), tr("Ajustar para independência", "Set to independence"), class = "btn-sm btn-outline-primary"),
          shiny::actionButton(ns("btn_disj"), tr("Ajustar para disjuntos", "Set to disjoint"), class = "btn-sm btn-outline-danger")
        ),
        shiny::uiOutput(ns("resumo_indep"))
      )
    ),
    bslib::card(
      bslib::card_header(tr("Experimento em 2 etapas: com vs. sem reposição", "2-stage experiment: with vs. without replacement")),
      bslib::card_body(
        bslib::layout_columns(
          col_widths = c(6, 6),
          shiny::sliderInput(ns("n_azul"), tr("Bolas azuis (A)", "Blue balls (A)"), min = 1, max = 10, value = 4, step = 1),
          shiny::sliderInput(ns("n_verm"), tr("Bolas vermelhas (V)", "Red balls (V)"), min = 1, max = 10, value = 6, step = 1)
        ),
        shiny::radioButtons(
          ns("reposicao"),
          tr("Esquema de extração", "Sampling scheme"),
          choices = stats::setNames(
            c("com", "sem"),
            c(tr("Com reposição (etapas independentes)", "With replacement (independent stages)"),
              tr("Sem reposição (condicional muda na 2ª etapa)", "Without replacement (conditional changes at stage 2)"))
          ),
          inline = TRUE
        ),
        shiny::plotOutput(ns("grafico_arvore"), height = "270px"),
        shiny::div(
          class = "d-flex align-items-center gap-3 mt-2 mb-2",
          shiny::actionButton(ns("simular"), tr("Sortear 500 pares", "Draw 500 pairs"), class = "btn-sm btn-primary"),
          shiny::textOutput(ns("nota_reposicao"), inline = TRUE)
        ),
        shiny::uiOutput(ns("tabela_arvore"))
      )
    )
  )
}

mod_independencia_arvores_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      shiny::req(is.finite(input$p_a), is.finite(input$p_b))
      lim_min <- round(max(0, input$p_a + input$p_b - 1), 2)
      lim_max <- round(min(input$p_a, input$p_b), 2)
      val <- if (is.finite(input$p_ab)) min(max(input$p_ab, lim_min), lim_max) else lim_min
      shiny::updateSliderInput(session, "p_ab", min = lim_min, max = lim_max, value = val)
    })

    shiny::observeEvent(input$btn_indep, {
      shiny::req(is.finite(input$p_a), is.finite(input$p_b))
      shiny::updateSliderInput(session, "p_ab", value = round(input$p_a * input$p_b, 2))
    })

    shiny::observeEvent(input$btn_disj, {
      shiny::req(is.finite(input$p_a), is.finite(input$p_b))
      lim_min <- round(max(0, input$p_a + input$p_b - 1), 2)
      shiny::updateSliderInput(session, "p_ab", value = lim_min)
    })

    ind <- shiny::reactive({
      shiny::req(is.finite(input$p_a), is.finite(input$p_b), is.finite(input$p_ab))
      verificar_independencia(input$p_a, input$p_b, input$p_ab)
    })

    output$resumo_indep <- shiny::renderUI({
      r <- ind()
      diag_txt <- if (r$disjuntos) {
        tr(
          "Mutuamente exclusivos: P(A ∩ B) = 0, então P(A | B) = 0 ≠ P(A). Eventos disjuntos com probabilidade positiva nunca são independentes.",
          "Mutually exclusive: P(A ∩ B) = 0, so P(A | B) = 0 ≠ P(A). Disjoint events with positive probability are never independent."
        )
      } else if (r$independentes) {
        tr(
          "Independentes: saber que B ocorreu não muda a chance de A (P(A | B) = P(A) e P(A ∩ B) = P(A) · P(B)).",
          "Independent: knowing B occurred does not change the chance of A (P(A | B) = P(A) and P(A ∩ B) = P(A) · P(B))."
        )
      } else {
        tr(
          "Dependentes: saber que B ocorreu altera a probabilidade de A (P(A | B) ≠ P(A)).",
          "Dependent: knowing B occurred changes the probability of A (P(A | B) ≠ P(A))."
        )
      }

      df <- data.frame(
        Medida = c("P(A)", "P(B)", "P(A) · P(B)", "P(A ∩ B)", "P(A | B)"),
        Valor = c(
          fmt_num(r$p_a, 3),
          fmt_num(r$p_b, 3),
          fmt_num(r$prod_indep, 4),
          fmt_num(r$p_a_inter_b, 4),
          fmt_num(r$p_a_dado_b, 4)
        ),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Medida", "Measure"), tr("Valor", "Value"))

      shiny::tagList(
        tabela_html(df, c("", "num")),
        shiny::p(class = "mt-3 mb-0", diag_txt)
      )
    })

    arvore <- shiny::reactive({
      shiny::req(is.finite(input$n_azul), is.finite(input$n_verm))
      calcular_arvore_urna(input$n_azul, input$n_verm, identical(input$reposicao, "com"))
    })

    sim_freqs <- shiny::reactiveVal(NULL)
    shiny::observeEvent(list(input$n_azul, input$n_verm, input$reposicao), {
      sim_freqs(NULL)
    })
    shiny::observeEvent(input$simular, {
      shiny::req(is.finite(input$n_azul), is.finite(input$n_verm))
      sim_freqs(simular_urna(input$n_azul, input$n_verm, identical(input$reposicao, "com"), 500))
    })

    output$grafico_arvore <- shiny::renderPlot({
      arv <- arvore()
      p <- paleta_modo(modo())
      cam <- arv$caminhos
      rot_caminhos <- c("AA", "AV", "VA", "VV")
      if (identical(idioma, "en")) {
        rot_caminhos <- c("BB", "BR", "RB", "RR")
      }
      df_plot <- data.frame(
        caminho = factor(rot_caminhos, levels = rev(rot_caminhos)),
        prob = cam$prob,
        etiqueta = paste0(fmt_num(cam$etapa1, 3), " × ", fmt_num(cam$etapa2_cond, 3), " = ", fmt_pct(cam$prob, 1)),
        stringsAsFactors = FALSE
      )

      ggplot2::ggplot(df_plot, ggplot2::aes(x = prob, y = caminho)) +
        ggplot2::geom_col(fill = cores_app$destaque, width = 0.62) +
        ggplot2::geom_text(
          ggplot2::aes(label = etiqueta),
          hjust = -0.05, size = 3.8, colour = p$texto
        ) +
        ggplot2::scale_x_continuous(
          limits = c(0, max(df_plot$prob) * 1.55 + 0.05),
          labels = rotulos_eixo()
        ) +
        ggplot2::labs(
          title = tr("Probabilidade dos 4 caminhos na árvore", "Probability of the 4 paths in the tree"),
          subtitle = tr("Produto da 1ª etapa pela condicional da 2ª etapa", "Product of stage 1 by stage 2 conditional"),
          x = tr("Probabilidade", "Probability"),
          y = NULL
        ) +
        tema_grafico(modo())
    })

    output$nota_reposicao <- shiny::renderText({
      arv <- arvore()
      sprintf(
        tr("P(mesma cor) = %s", "P(same color) = %s"),
        fmt_pct(arv$p_mesma_cor, 1)
      )
    })

    output$tabela_arvore <- shiny::renderUI({
      arv <- arvore()
      cam <- arv$caminhos
      sim <- sim_freqs()
      rot <- if (identical(idioma, "en")) c("BB", "BR", "RB", "RR") else c("AA", "AV", "VA", "VV")
      df <- data.frame(
        Caminho = rot,
        Etapa1 = fmt_num(cam$etapa1, 3),
        Etapa2 = fmt_num(cam$etapa2_cond, 3),
        Teorica = fmt_pct(cam$prob, 1),
        Simulada = if (is.null(sim)) "—" else fmt_pct(sim, 1),
        stringsAsFactors = FALSE
      )
      names(df) <- c(
        tr("Caminho", "Path"),
        tr("1ª etapa", "Stage 1"),
        tr("2ª etapa | 1ª", "Stage 2 | 1st"),
        tr("Teórica", "Theoretical"),
        tr("Simulada (500)", "Simulated (500)")
      )
      tabela_html(df, c("", "num", "num", "num", "num"))
    })
  })
}
