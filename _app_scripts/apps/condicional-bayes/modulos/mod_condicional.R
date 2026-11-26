# =============================================================================
# Script:  mod_condicional.R (Aba 1: Condicional e regra do produto)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-26
#
# Entrada:
#   - Controles de P(B), P(A | B) e P(A | Bᶜ)
# Saída:
#   - Diagrama proporcional do espaço unitário, tabela comparativa e alertas
# =============================================================================

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
          shiny::plotOutput(ns("grafico_area"), height = "380px")
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
            "Condicionar em B significa descartar tudo o que está fora de B e reescalar a área restante para somar 1. Por isso, P(A | B) = P(A ∩ B) / P(B), de onde vem a regra do produto P(A ∩ B) = P(B) · P(A | B).",
            "Conditioning on B means discarding everything outside B and rescaling the remaining area to sum to 1. That is why P(A | B) = P(A ∩ B) / P(B), yielding the product rule P(A ∩ B) = P(B) · P(A | B)."
          ))
        )
      ),
      bslib::card(
        bslib::card_header(tr("Cuidados", "Watch out")),
        bslib::card_body(
          shiny::tags$ul(
            shiny::tags$li(tr(
              "Não inverta a condicional: P(A | B) e P(B | A) dividem a mesma interseção P(A ∩ B) por denominadores diferentes (P(B) contra P(A)).",
              "Do not invert the conditional: P(A | B) and P(B | A) divide the same intersection P(A ∩ B) by different denominators (P(B) versus P(A))."
            )),
            shiny::tags$li(tr(
              "Não confunda P(A | B) com P(A ∩ B): a primeira restringe o universo a B; a segunda mede a fração do espaço inteiro Ω.",
              "Do not confuse P(A | B) with P(A ∩ B): the first restricts the universe to B; the second measures a fraction of the whole space Ω."
            ))
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
        paste0("Aᶜ ∩ B\n", fmt_pct(cel$prob[2], 1)),
        paste0("A ∩ Bᶜ\n", fmt_pct(cel$prob[3], 1)),
        paste0("Aᶜ ∩ Bᶜ\n", fmt_pct(cel$prob[4], 1))
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
          data = cel[cel$prob >= 0.015, , drop = FALSE],
          ggplot2::aes(x = cx, y = cy, label = rotulo, colour = chave),
          size = 4.2, fontface = "bold"
        ) +
        ggplot2::scale_fill_manual(values = cores) +
        ggplot2::scale_colour_manual(values = cores_texto) +
        ggplot2::scale_x_continuous(
          breaks = c(r$p_b / 2, r$p_b + r$p_bc / 2),
          labels = c(paste0("B (", fmt_pct(r$p_b, 0), ")"), paste0("Bᶜ (", fmt_pct(r$p_bc, 0), ")")),
          expand = c(0, 0)
        ) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo(), expand = c(0, 0)) +
        ggplot2::labs(
          title = tr("Quadrado unitário de Ω", "Unit square of Ω"),
          subtitle = tr(
            "Largura = P(B) e P(Bᶜ); altura azul = P(A | B); altura vermelha = P(A | Bᶜ)",
            "Width = P(B) and P(Bᶜ); blue height = P(A | B); red height = P(A | Bᶜ)"
          ),
          x = NULL,
          y = tr("Probabilidade condicional dentro de cada coluna", "Conditional probability within each column"),
          caption = tr("Área total = 1", "Total area = 1")
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
      p_ba_txt <- if (is.na(r$p_b_dado_a)) tr("indefinido", "undefined") else fmt_pct(r$p_b_dado_a, 1)
      shiny::p(sprintf(
        tr(
          "Mesmo quando P(A | B) vale %s, a condicional invertida P(B | A) vale %s porque A também ocorre fora de B (P(A ∩ Bᶜ) = %s).",
          "Even when P(A | B) equals %s, the inverted conditional P(B | A) equals %s because A also happens outside B (P(A ∩ Bᶜ) = %s)."
        ),
        fmt_pct(r$p_a_dado_b, 1),
        p_ba_txt,
        fmt_pct(r$p_a_inter_bc, 1)
      ))
    })
  })
}
