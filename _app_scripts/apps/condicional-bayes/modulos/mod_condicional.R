# =============================================================================
# Script:  mod_condicional.R (Aba 1: Condicional e regra do produto)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Controles de P(B), P(A | B) e P(A | Bᶜ)
# Saída:
#   - Diagrama proporcional do espaço unitário, tabela comparativa e alertas
# =============================================================================

# O que A e B significam em cada cenário de partida
descricao_cenario <- function(cenario) {
  switch(cenario,
    teste = tr(
      "A = teste positivo; B = ter a doença. P(B) é a prevalência, P(A | B) a sensibilidade e P(A | Bᶜ) a taxa de falso positivo.",
      "A = positive test; B = having the disease. P(B) is the prevalence, P(A | B) the sensitivity and P(A | Bᶜ) the false positive rate."
    ),
    churn = tr(
      "A = o cliente cancela o serviço; B = o cliente acionou o suporte técnico no mês.",
      "A = the customer cancels the service; B = the customer called tech support that month."
    ),
    tr(
      "A e B são eventos quaisquer. Mova os controles e veja as áreas mudarem.",
      "A and B are any two events. Move the controls and watch the areas change."
    )
  )
}

mod_condicional_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::selectInput(
        ns("cenario"),
        tr("Cenário de partida", "Starting scenario"),
        choices = stats::setNames(
          c("manual", "teste", "churn"),
          c(
            tr("Ajuste livre", "Free adjustment"),
            tr("Teste positivo vs. doença rara", "Positive test vs. rare disease"),
            tr("Cancelamento vs. suporte técnico", "Churn vs. tech support")
          )
        )
      ),
      shiny::uiOutput(ns("descricao")),
      shiny::sliderInput(
        ns("p_b"),
        tr("Probabilidade de B, P(B)", "Probability of B, P(B)"),
        min = 0.05, max = 0.95, value = 0.20, step = 0.01
      ),
      shiny::sliderInput(
        ns("p_a_dado_b"),
        tr("Condicional P(A | B)", "Conditional P(A | B)"),
        min = 0.00, max = 1.00, value = 0.90, step = 0.01
      ),
      shiny::sliderInput(
        ns("p_a_dado_bc"),
        tr("Condicional P(A | Bᶜ)", "Conditional P(A | Bᶜ)"),
        min = 0.00, max = 1.00, value = 0.10, step = 0.01
      )
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header(tr("Espaço amostral particionado por B e Bᶜ", "Sample space partitioned by B and Bᶜ")),
        bslib::card_body(
          shiny::plotOutput(ns("grafico_area"), height = "480px")
        )
      ),
      bslib::card(
        bslib::card_header(tr("Condicional direta vs. invertida", "Direct vs. inverted conditional")),
        bslib::card_body(
          shiny::uiOutput(ns("tabela_comparativa")),
          shiny::hr(),
          shiny::uiOutput(ns("leitura_regra"))
        )
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header(rotulo_entenda()),
        bslib::card_body(
          shiny::p(tr(
            "Condicionar em B significa descartar tudo o que está fora de B e reescalar a área restante para somar 1. Por isso, P(A | B) = P(A ∩ B) / P(B), com P(B) > 0, de onde vem a regra do produto P(A ∩ B) = P(B) · P(A | B) (Morettin e Bussab, seção 5.3, fórmulas 5.7 e 5.8). Agresti e Finlay (seção 4.1) dão um exemplo: 56% dos adultos são casados e, entre os casados, 40% se dizem muito felizes, então cerca de 0,56 × 0,40 = 22% são casados e muito felizes.",
            "Conditioning on B means discarding everything outside B and rescaling the remaining area to sum to 1. That is why P(A | B) = P(A ∩ B) / P(B), with P(B) > 0, which yields the product rule P(A ∩ B) = P(B) · P(A | B) (Morettin and Bussab, section 5.3, formulas 5.7 and 5.8). Agresti and Finlay (section 4.1) give an example: 56% of adults are married and, among the married, 40% say they are very happy, so about 0.56 × 0.40 = 22% are married and very happy."
          )),
          shiny::p(
            shiny::strong(tr("Experimente:", "Try this:")),
            tr(
              "escolha o teste para doença rara. O teste acerta 95% dos doentes, mas só um terço dos positivos tem a doença. Depois aumente P(B) e veja P(B | A) subir enquanto P(A | B) fica parada em 95%.",
              "pick the rare disease test. The test catches 95% of the sick, yet only a third of the positives have the disease. Then raise P(B) and watch P(B | A) go up while P(A | B) stays at 95%."
            )
          )
        )
      ),
      cartao_cuidados(
        tr("Cuidados", "Watch out"),
        stats::setNames(
          list(
            tr(
              "P(A | B) e P(B | A) dividem a mesma interseção P(A ∩ B) por denominadores diferentes, P(B) e P(A). Só coincidem quando P(A) = P(B).",
              "P(A | B) and P(B | A) divide the same intersection P(A ∩ B) by different denominators, P(B) and P(A). They match only when P(A) = P(B)."
            ),
            tr(
              "P(A | B) restringe o universo a B; P(A ∩ B) mede uma fração do espaço inteiro Ω. No quadrado, a primeira é uma altura e a segunda, uma área.",
              "P(A | B) restricts the universe to B; P(A ∩ B) measures a fraction of the whole space Ω. In the square, the first is a height and the second an area."
            )
          ),
          c(
            tr("Não inverta a condicional", "Do not invert the conditional"),
            tr("Condicional não é interseção", "Conditional is not intersection")
          )
        )
      )
    )
  )
}

mod_condicional_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$cenario, {
      if (identical(input$cenario, "teste")) {
        shiny::updateSliderInput(session, "p_b", value = 0.05)
        shiny::updateSliderInput(session, "p_a_dado_b", value = 0.95)
        shiny::updateSliderInput(session, "p_a_dado_bc", value = 0.10)
      } else if (identical(input$cenario, "churn")) {
        shiny::updateSliderInput(session, "p_b", value = 0.15)
        shiny::updateSliderInput(session, "p_a_dado_b", value = 0.60)
        shiny::updateSliderInput(session, "p_a_dado_bc", value = 0.20)
      }
    }, ignoreInit = TRUE)

    output$descricao <- shiny::renderUI({
      shiny::req(input$cenario)
      shiny::p(class = "small text-muted", descricao_cenario(input$cenario))
    })

    res <- shiny::reactive({
      shiny::req(
        is.finite(input$p_b),
        is.finite(input$p_a_dado_b),
        is.finite(input$p_a_dado_bc)
      )
      calcular_condicional(input$p_b, input$p_a_dado_b, input$p_a_dado_bc)
    })

    output$grafico_area <- shiny::renderPlot({
      r <- res()
      p <- paleta_modo(modo())
      cel <- r$celulas
      cel$rotulo <- c(
        paste0("A ∩ B\n", fmt_pct(cel$prob[1], 1)),
        paste0("A' ∩ B\n", fmt_pct(cel$prob[2], 1)),
        paste0("A ∩ B'\n", fmt_pct(cel$prob[3], 1)),
        paste0("A' ∩ B'\n", fmt_pct(cel$prob[4], 1))
      )
      cel$cx <- (cel$xmin + cel$xmax) / 2
      cel$cy <- (cel$ymin + cel$ymax) / 2
      cel$chave <- paste(cel$grupo_a, cel$grupo_b, sep = "_")

      cores <- c(
        "A_B"   = cores_app$destaque,
        "Ac_B"  = p$neutro,
        "A_Bc"  = cores_app$referencia,
        "Ac_Bc" = p$grade
      )
      cores_texto <- c(
        "A_B"   = "#ffffff",
        "Ac_B"  = p$texto,
        "A_Bc"  = "#ffffff",
        "Ac_Bc" = p$secundario
      )

      ggplot2::ggplot(cel) +
        ggplot2::geom_rect(
          ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = chave),
          colour = p$fundo, linewidth = 1.2
        ) +
        ggplot2::geom_text(
          # Coluna estreita não comporta o rótulo; o valor segue na tabela ao lado
          data = cel[cel$prob >= 0.015 & (cel$xmax - cel$xmin) >= 0.10, , drop = FALSE],
          ggplot2::aes(x = cx, y = cy, label = rotulo, colour = chave),
          size = 4.2, fontface = "bold"
        ) +
        ggplot2::scale_fill_manual(values = cores) +
        ggplot2::scale_colour_manual(values = cores_texto) +
        ggplot2::scale_x_continuous(
          breaks = c(r$p_b / 2, r$p_b + r$p_bc / 2),
          labels = c(paste0("B (", fmt_pct(r$p_b, 0), ")"), paste0("B' (", fmt_pct(r$p_bc, 0), ")")),
          expand = c(0, 0)
        ) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo(), expand = c(0, 0)) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::labs(
          title = tr("Quadrado unitário de Ω", "Unit square of Ω"),
          subtitle = tr(
            "Largura: P(B) e P(B'); altura: P(A | B) e P(A | B')",
            "Width: P(B) and P(B'); height: P(A | B) and P(A | B')"
          ),
          x = NULL,
          y = tr("Probabilidade condicional na coluna", "Conditional probability in column"),
          caption = tr("Área total = 1. B' é o complemento Bᶜ.", "Total area = 1. B' is the complement Bᶜ.")
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(legend.position = "none")
    })

    output$tabela_comparativa <- shiny::renderUI({
      r <- res()
      df <- data.frame(
        Medida = c(
          "P(A ∩ B) = P(B) · P(A | B)",
          "P(A) = P(A ∩ B) + P(A ∩ Bᶜ)",
          "P(A | B)",
          "P(B | A) = P(A ∩ B) / P(A)"
        ),
        Valor = c(
          fmt_num(r$p_a_inter_b, 4),
          fmt_num(r$p_a, 4),
          fmt_num(r$p_a_dado_b, 4),
          if (is.na(r$p_b_dado_a)) tr("indefinido (P(A) = 0)", "undefined (P(A) = 0)") else fmt_num(r$p_b_dado_a, 4)
        ),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Quantidade", "Quantity"), tr("Valor", "Value"))
      tabela_html(df, c("", "num"))
    })

    output$leitura_regra <- shiny::renderUI({
      r <- res()
      if (is.na(r$p_b_dado_a)) {
        return(shiny::p(tr(
          "Com P(A | B) = P(A | Bᶜ) = 0, A tem probabilidade zero, e P(B | A) não está definida.",
          "With P(A | B) = P(A | Bᶜ) = 0, A has probability zero, and P(B | A) is undefined."
        )))
      }
      base <- sprintf(
        tr(
          "As duas condicionais dividem a mesma interseção P(A ∩ B) = %s. P(A | B) = %s divide por P(B) = %s; P(B | A) = %s divide por P(A) = %s.",
          "Both conditionals divide the same intersection P(A ∩ B) = %s. P(A | B) = %s divides by P(B) = %s; P(B | A) = %s divides by P(A) = %s."
        ),
        fmt_pct(r$p_a_inter_b, 1), fmt_pct(r$p_a_dado_b, 1), fmt_pct(r$p_b, 1),
        fmt_pct(r$p_b_dado_a, 1), fmt_pct(r$p_a, 1)
      )
      complemento <- if (r$p_a_inter_bc == 0) {
        tr(
          "Aqui A só ocorre dentro de B (P(A ∩ Bᶜ) = 0), então P(B | A) = 100%: todo caso de A está em B.",
          "Here A only happens inside B (P(A ∩ Bᶜ) = 0), so P(B | A) = 100%: every case of A lies in B."
        )
      } else if (r$p_a > r$p_b) {
        sprintf(
          tr(
            "Somando a parte de A fora de B (P(A ∩ Bᶜ) = %s), P(A) passa de P(B), e por isso a condicional invertida P(B | A) fica menor que P(A | B).",
            "Adding the part of A outside B (P(A ∩ Bᶜ) = %s), P(A) exceeds P(B), so the inverted conditional P(B | A) is smaller than P(A | B)."
          ),
          fmt_pct(r$p_a_inter_bc, 1)
        )
      } else if (r$p_a < r$p_b) {
        tr(
          "Aqui P(A) é menor que P(B), então a condicional invertida P(B | A) fica maior que P(A | B).",
          "Here P(A) is smaller than P(B), so the inverted conditional P(B | A) is larger than P(A | B)."
        )
      } else {
        tr(
          "Com P(A) = P(B), as duas condicionais coincidem.",
          "With P(A) = P(B), the two conditionals coincide."
        )
      }
      shiny::tagList(shiny::p(base), shiny::p(class = "mb-0", complemento))
    })
  })
}
