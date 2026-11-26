# =============================================================================
# Script:  mod_bayes.R (Aba 3: Probabilidade total e Teorema de Bayes)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-26
#
# Entrada:
#   - Presets (teste diagnóstico, urnas de Bussab, Código Morse) e sliders
# Saída:
#   - Tabela completa de Bayes, razão de chances (Gelman Eq. 1.5), gráfico
#     priori vs. posteriori e grade de 1.000 frequências naturais
# =============================================================================

mod_bayes_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 330,
      shiny::selectInput(
        ns("preset"),
        tr("Exemplo clássico", "Classic example"),
        choices = stats::setNames(
          c("diagnostico", "urnas", "morse"),
          c(
            tr("Teste diagnóstico (taxa base)", "Diagnostic test (base rate)"),
            tr("Urnas (Morettin & Bussab, Cap. 5)", "Urns (Morettin & Bussab, Ch. 5)"),
            tr("Código Morse (Casella & Berger, Ex. 1.3.6)", "Morse code (Casella & Berger, Ex. 1.3.6)")
          )
        )
      ),
      shiny::sliderInput(
        ns("priori1"),
        tr("Priori da hipótese 1, P(C₁)", "Prior of hypothesis 1, P(C₁)"),
        min = 0.005, max = 0.995, value = 0.010, step = 0.005
      ),
      shiny::sliderInput(
        ns("verossim1"),
        tr("Verossimilhança P(A | C₁)", "Likelihood P(A | C₁)"),
        min = 0.01, max = 0.99, value = 0.95, step = 0.01
      ),
      shiny::sliderInput(
        ns("verossim2"),
        tr("Verossimilhança P(A | C₂)", "Likelihood P(A | C₂)"),
        min = 0.01, max = 0.99, value = 0.05, step = 0.01
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
        bslib::card_header(tr("Razão de chances (Gelman et al., Eq. 1.5)", "Posterior odds (Gelman et al., Eq. 1.5)")),
        bslib::card_body(
          shiny::uiOutput(ns("texto_odds"))
        )
      )
    )
  )
}

mod_bayes_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$preset, {
      if (identical(input$preset, "diagnostico")) {
        shiny::updateSliderInput(session, "priori1", value = 0.010)
        shiny::updateSliderInput(session, "verossim1", value = 0.95)
        shiny::updateSliderInput(session, "verossim2", value = 0.05)
      } else if (identical(input$preset, "urnas")) {
        shiny::updateSliderInput(session, "priori1", value = 0.500)
        shiny::updateSliderInput(session, "verossim1", value = 0.40)
        shiny::updateSliderInput(session, "verossim2", value = 0.60)
      } else if (identical(input$preset, "morse")) {
        shiny::updateSliderInput(session, "priori1", value = round(3 / 7, 3))
        shiny::updateSliderInput(session, "verossim1", value = 0.875)
        shiny::updateSliderInput(session, "verossim2", value = 0.125)
      }
    }, ignoreInit = TRUE)

    rotulos_hip <- shiny::reactive({
      if (identical(input$preset, "urnas")) {
        c(tr("C₁: Urna 1", "C₁: Urn 1"), tr("C₂: Urna 2", "C₂: Urn 2"))
      } else if (identical(input$preset, "morse")) {
        c(tr("C₁: Ponto enviado", "C₁: Dot sent"), tr("C₂: Traço enviado", "C₂: Dash sent"))
      } else {
        c(tr("C₁: Condição presente", "C₁: Condition present"), tr("C₂: Condição ausente", "C₂: Condition absent"))
      }
    })

    res <- shiny::reactive({
      shiny::req(
        is.finite(input$priori1),
        is.finite(input$verossim1),
        is.finite(input$verossim2)
      )
      calcular_bayes_particao(
        nomes = rotulos_hip(),
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
      tb <- r$tabela
      p <- paleta_modo(modo())
      n_tot <- 1000L
      n_vp <- round(n_tot * tb$conjunta[1])
      n_fn <- round(n_tot * (tb$priori[1] - tb$conjunta[1]))
      n_fp <- round(n_tot * tb$conjunta[2])
      n_vn <- max(0L, n_tot - n_vp - n_fn - n_fp)

      cats <- c(
        rep("VP", n_vp),
        rep("FP", n_fp),
        rep("FN", n_fn),
        rep("VN", n_vn)
      )[seq_len(n_tot)]

      grade <- data.frame(
        x = rep(1:40, each = 25),
        y = rep(25:1, times = 40),
        cat = factor(cats, levels = c("VP", "FP", "FN", "VN")),
        stringsAsFactors = FALSE
      )

      nomes_cat <- c(
        VP = tr("C₁ e A (verdadeiro +)", "C₁ and A (true +)"),
        FP = tr("C₂ e A (falso +)", "C₂ and A (false +)"),
        FN = tr("C₁ e Aᶜ", "C₁ and Aᶜ"),
        VN = tr("C₂ e Aᶜ", "C₂ and Aᶜ")
      )

      ggplot2::ggplot(grade, ggplot2::aes(x = x, y = y, colour = cat)) +
        ggplot2::geom_point(size = 2.0) +
        ggplot2::scale_colour_manual(
          values = c(VP = cores_app$destaque, FP = cores_app$referencia, FN = "#f59f00", VN = p$grade),
          labels = nomes_cat
        ) +
        ggplot2::labs(
          title = tr("Onde estão os casos positivos (A)?", "Where are the positive cases (A)?"),
          subtitle = sprintf(
            tr("%d em C₁∩A contra %d em C₂∩A (total A = %d)", "%d in C₁∩A vs. %d in C₂∩A (total A = %d)"),
            n_vp, n_fp, n_vp + n_fp
          ),
          x = NULL, y = NULL, colour = NULL
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(
          axis.text = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank(),
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
          tr("Chances posteriores = %s × %s = %s", "Posterior odds = %s × %s = %s"),
          fmt_num(o$odds_priori, 4),
          fmt_num(o$razao_verossim, 4),
          fmt_num(o$odds_posteriori, 4)
        ))),
        shiny::p(class = "text-muted mb-0", tr(
          "Note que a probabilidade total P(A) cancela na razão de chances, facilitando atualizações sucessivas.",
          "Notice that the total probability P(A) cancels out in the odds ratio, making sequential updates straightforward."
        ))
      )
    })
  })
}
