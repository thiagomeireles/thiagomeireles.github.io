# =============================================================================
# Script:  mod_independencia_arvores.R (Aba 2: Independência e árvores)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Controles de independência vs. disjunção e parâmetros da urna em 2 etapas
# Saída:
#   - Comparação numérica de P(A ∩ B) vs. P(A)P(B), árvore em 2 etapas e simulação
# =============================================================================

# Rótulos das cores da urna: B/V em pt (branca, vermelha), W/R em en
rotulos_cores <- function() tr(c(B = "B", V = "V"), c(B = "W", V = "R"))
rotulos_caminhos <- function() {
  r <- rotulos_cores()
  paste0(r[c("B", "B", "V", "V")], r[c("B", "V", "B", "V")])
}

mod_independencia_arvores_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::layout_columns(
      col_widths = c(5, 7),
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
            min = 0.00, max = 0.40, value = 0.10, step = passo_intersecao
          ),
          shiny::div(
            class = "d-flex flex-wrap gap-2 mb-3",
            shiny::actionButton(ns("btn_indep"), tr("Ajustar para independência", "Set to independence"), class = "btn-sm btn-outline-primary"),
            shiny::actionButton(ns("btn_disj"), tr("Ajustar para disjuntos", "Set to disjoint"), class = "btn-sm btn-outline-danger")
          ),
          shiny::uiOutput(ns("resumo_indep"))
        )
      ),
      bslib::card(
        bslib::card_header(tr("Árvore em 2 etapas: com vs. sem reposição", "2-stage tree: with vs. without replacement")),
        bslib::card_body(
          bslib::layout_columns(
            col_widths = c(6, 6),
            shiny::sliderInput(ns("n_branca"), tr("Bolas brancas (B)", "White balls (W)"), min = 1, max = 10, value = 2, step = 1),
            shiny::sliderInput(ns("n_verm"), tr("Bolas vermelhas (V)", "Red balls (R)"), min = 1, max = 10, value = 3, step = 1)
          ),
          shiny::radioButtons(
            ns("reposicao"),
            tr("Esquema de extração", "Sampling scheme"),
            choices = stats::setNames(
              c("sem", "com"),
              c(tr("Sem reposição (Ex. 5.10)", "Without replacement (Ex. 5.10)"),
                tr("Com reposição (Ex. 5.11)", "With replacement (Ex. 5.11)"))
            ),
            inline = TRUE
          ),
          shiny::plotOutput(ns("grafico_arvore"), height = "340px"),
          shiny::div(
            class = "d-flex flex-wrap align-items-center gap-3 mt-2 mb-2",
            shiny::actionButton(ns("simular"), tr("Sortear 500 pares", "Draw 500 pairs"), class = "btn-sm btn-primary"),
            shiny::textOutput(ns("nota_reposicao"), inline = TRUE)
          ),
          shiny::uiOutput(ns("tabela_arvore"))
        )
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header(rotulo_entenda()),
        bslib::card_body(
          shiny::p(tr(
            "A e B são independentes quando P(A ∩ B) = P(A) · P(B), o que equivale a P(A | B) = P(A): saber que B ocorreu não muda a probabilidade de A (Morettin e Bussab, fórmula 5.9; Agresti e Finlay, seção 4.1; Casella e Berger, Def. 1.3.7). Eventos mutuamente exclusivos são o oposto: se A e B não se sobrepõem, saber que B ocorreu zera a probabilidade de A.",
            "A and B are independent when P(A ∩ B) = P(A) · P(B), which is the same as P(A | B) = P(A): knowing that B occurred does not change the probability of A (Morettin and Bussab, formula 5.9; Agresti and Finlay, section 4.1; Casella and Berger, Def. 1.3.7). Mutually exclusive events are the opposite: if A and B do not overlap, knowing that B occurred drops the probability of A to zero."
          )),
          shiny::p(tr(
            "A árvore começa com a urna dos Exemplos 5.10 e 5.11 de Morettin e Bussab, com 2 bolas brancas e 3 vermelhas. Cada galho da 2ª etapa traz uma probabilidade condicional, e a de cada caminho sai da regra do produto. Sem reposição, a 2ª extração depende da 1ª; com reposição, as duas são independentes.",
            "The tree starts from the urn in Morettin and Bussab's Examples 5.10 and 5.11, with 2 white and 3 red balls. Each stage-2 branch carries a conditional probability, and each path's probability comes from the product rule. Without replacement, the 2nd draw depends on the 1st; with replacement, the two are independent."
          )),
          shiny::p(
            shiny::strong(tr("Experimente:", "Try this:")),
            tr(
              "com P(A) = 0,70 e P(B) = 0,60, tente deixar A e B disjuntos. A interseção não desce de 0,30, porque P(A) + P(B) passa de 1. Na urna, troque o esquema de extração e compare P(branca na 2ª) com P(branca na 1ª).",
              "with P(A) = 0.70 and P(B) = 0.60, try to make A and B disjoint. The intersection cannot go below 0.30, because P(A) + P(B) exceeds 1. In the urn, switch the sampling scheme and compare P(white on 2nd) with P(white on 1st)."
            )
          )
        )
      ),
      cartao_cuidados(
        tr("Cuidados", "Watch out"),
        stats::setNames(
          list(
            tr(
              "dois eventos disjuntos com probabilidade positiva nunca são independentes, porque P(A | B) = 0 é diferente de P(A).",
              "two disjoint events with positive probability are never independent, because P(A | B) = 0 differs from P(A)."
            ),
            tr(
              "sem reposição, a 2ª etapa depende da 1ª, mas P(branca na 2ª) continua igual a P(branca na 1ª). Sem saber a 1ª cor, a 2ª bola é qualquer uma das cinco (Morettin e Bussab calculam P = 2/5 no Ex. 5.10).",
              "without replacement, stage 2 depends on stage 1, yet P(white on 2nd) still equals P(white on 1st). Without knowing the 1st color, the 2nd ball is any of the five (Morettin and Bussab get P = 2/5 in Ex. 5.10)."
            ),
            tr(
              "independência é uma conta com as probabilidades, e não algo que se veja no diagrama. A e B podem se sobrepor muito e ainda ser dependentes.",
              "independence is a computation with the probabilities, not something you see in a diagram. A and B may overlap a lot and still be dependent."
            )
          ),
          c(
            tr("Disjunto não é independente", "Disjoint is not independent"),
            tr("Dependência não muda a marginal", "Dependence does not change the marginal"),
            tr("Sobreposição não basta", "Overlap is not enough")
          )
        )
      )
    )
  )
}

mod_independencia_arvores_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    # Ajusta os limites da interseção só quando P(A) ou P(B) mudam; o valor
    # atual entra via isolate() para não reagir ao próprio slider
    shiny::observeEvent(list(input$p_a, input$p_b), {
      shiny::req(is.finite(input$p_a), is.finite(input$p_b))
      lim_min <- arredondar_passo(max(0, input$p_a + input$p_b - 1))
      lim_max <- arredondar_passo(min(input$p_a, input$p_b))
      atual <- shiny::isolate(input$p_ab)
      val <- if (is.finite(atual)) min(max(atual, lim_min), lim_max) else lim_min
      shiny::updateSliderInput(session, "p_ab", min = lim_min, max = lim_max, value = val)
    })

    shiny::observeEvent(input$btn_indep, {
      shiny::req(is.finite(input$p_a), is.finite(input$p_b))
      shiny::updateSliderInput(session, "p_ab", value = arredondar_passo(input$p_a * input$p_b))
    })

    shiny::observeEvent(input$btn_disj, {
      shiny::req(is.finite(input$p_a), is.finite(input$p_b))
      shiny::updateSliderInput(session, "p_ab", value = arredondar_passo(max(0, input$p_a + input$p_b - 1)))
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
          "Independentes: saber que B ocorreu não muda a probabilidade de A (P(A | B) = P(A) e P(A ∩ B) = P(A) · P(B)).",
          "Independent: knowing B occurred does not change the probability of A (P(A | B) = P(A) and P(A ∩ B) = P(A) · P(B))."
        )
      } else if (r$diferenca > 0) {
        tr(
          "Dependentes: saber que B ocorreu aumenta a probabilidade de A (P(A | B) > P(A)).",
          "Dependent: knowing B occurred raises the probability of A (P(A | B) > P(A))."
        )
      } else {
        tr(
          "Dependentes: saber que B ocorreu diminui a probabilidade de A (P(A | B) < P(A)).",
          "Dependent: knowing B occurred lowers the probability of A (P(A | B) < P(A))."
        )
      }
      aviso_disj <- if (!r$disjuncao_possivel) {
        shiny::p(
          class = "small text-muted mt-2 mb-0",
          sprintf(
            tr(
              "Com P(A) + P(B) = %s, maior que 1, A e B não podem ser disjuntos: a interseção é de pelo menos %s.",
              "With P(A) + P(B) = %s, above 1, A and B cannot be disjoint: the intersection is at least %s."
            ),
            fmt_num(r$p_a + r$p_b, 2), fmt_num(r$min_inter, 2)
          )
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
        shiny::p(class = "mt-3 mb-0", diag_txt),
        aviso_disj
      )
    })

    arvore <- shiny::reactive({
      shiny::req(is.finite(input$n_branca), is.finite(input$n_verm), input$reposicao)
      calcular_arvore_urna(input$n_branca, input$n_verm, identical(input$reposicao, "com"))
    })

    sim_freqs <- shiny::reactiveVal(NULL)
    shiny::observeEvent(list(input$n_branca, input$n_verm, input$reposicao), {
      sim_freqs(NULL)
    })
    shiny::observeEvent(input$simular, {
      shiny::req(is.finite(input$n_branca), is.finite(input$n_verm))
      sim_freqs(simular_urna(input$n_branca, input$n_verm, identical(input$reposicao, "com"), 500))
    })

    output$grafico_arvore <- shiny::renderPlot({
      arv <- arvore()
      p <- paleta_modo(modo())
      pos <- posicoes_arvore(arv)
      rc <- rotulos_cores()
      pos$nos$rotulo <- unname(rc[pos$nos$cor])
      pos$folhas$texto <- paste0(
        rotulos_caminhos(), ":  ", pos$folhas$frac1, " × ", pos$folhas$frac2, " = ", fmt_pct(pos$folhas$prob, 1)
      )
      cores_bola <- c(B = "#f8f9fa", V = cores_app$referencia)
      texto_bola <- c(B = "#212529", V = "#ffffff")

      ggplot2::ggplot() +
        ggplot2::geom_segment(
          data = pos$galhos,
          ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
          colour = p$secundario, linewidth = 0.8
        ) +
        ggplot2::geom_label(
          data = pos$galhos,
          ggplot2::aes(x = xm, y = ym, label = rotulo),
          size = 4, colour = p$texto, fill = p$fundo, linewidth = 0
        ) +
        ggplot2::geom_point(
          data = data.frame(x = 0, y = 0.5),
          ggplot2::aes(x = x, y = y),
          size = 4, colour = p$secundario
        ) +
        ggplot2::geom_point(
          data = pos$nos,
          ggplot2::aes(x = x, y = y, fill = cor),
          shape = 21, size = 9, colour = p$secundario, stroke = 0.8
        ) +
        ggplot2::geom_text(
          data = pos$nos,
          ggplot2::aes(x = x, y = y, label = rotulo, colour = cor),
          size = 4.2, fontface = "bold"
        ) +
        ggplot2::geom_text(
          data = pos$folhas,
          ggplot2::aes(x = x + 0.12, y = y, label = texto),
          hjust = 0, size = 4.2, colour = p$texto
        ) +
        ggplot2::scale_fill_manual(values = cores_bola) +
        ggplot2::scale_colour_manual(values = texto_bola) +
        ggplot2::scale_x_continuous(
          limits = c(-0.05, 3.6),
          breaks = c(1, 2),
          labels = c(tr("1ª extração", "1st draw"), tr("2ª extração", "2nd draw"))
        ) +
        ggplot2::scale_y_continuous(limits = c(0.05, 0.95)) +
        ggplot2::labs(
          title = tr("Diagrama em árvore", "Tree diagram"),
          subtitle = tr(
            "Galhos da 2ª etapa: probabilidade condicional à cor da 1ª",
            "Stage-2 branches: probability conditional on the 1st color"
          ),
          x = NULL, y = NULL
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(
          panel.grid.major = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank()
        )
    })

    output$nota_reposicao <- shiny::renderText({
      arv <- arvore()
      sprintf(
        tr(
          "P(branca na 1ª) = %s · P(branca na 2ª) = %s · P(mesma cor) = %s",
          "P(white on 1st) = %s · P(white on 2nd) = %s · P(same color) = %s"
        ),
        fmt_pct(arv$p_b1, 1), fmt_pct(arv$p_b2, 1), fmt_pct(arv$p_mesma_cor, 1)
      )
    })

    output$tabela_arvore <- shiny::renderUI({
      arv <- arvore()
      cam <- arv$caminhos
      sim <- sim_freqs()
      df <- data.frame(
        Caminho = rotulos_caminhos(),
        Etapa1 = cam$frac1,
        Etapa2 = cam$frac2,
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
