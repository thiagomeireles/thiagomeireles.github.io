# =============================================================================
# Script:  mod_eventos.R (módulo: operações de conjuntos, leis de De Morgan,
#          axiomas da probabilidade e regra da soma)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-26
#
# Entrada:
#   - objetos e funções de _app_scripts/apps/espaco-amostral/modulos/dados.R
# Saída:
#   - funções mod_eventos_ui() e mod_eventos_server()
# =============================================================================

texto_eventos <- function() {
  tr(
    shiny::withMathJax(
      shiny::p(
        "Tratar eventos como subconjuntos de \\(\\Omega\\) permite converter frases do cotidiano em operações",
        "exatas: \"não ocorrer \\(A\\)\" é o complementar \\(A^c\\), \"ocorrer \\(A\\) ou \\(B\\) (ou ambos)\" é a",
        "união \\(A \\cup B\\), e \"ocorrerem \\(A\\) e \\(B\\) juntos\" é a interseção \\(A \\cap B\\) (Morettin e",
        "Bussab, seção 5.1; Casella e Berger, seção 1.1). Dois eventos são mutuamente exclusivos (ou disjuntos)",
        "quando não compartilham nenhum resultado elementar, isto é, \\(A \\cap B = \\varnothing\\). Pelas leis de",
        "De Morgan (Teo. 1.1.4d), negar uma união equivale a exigir os dois complementares simultaneamente,",
        "\\((A \\cup B)^c = A^c \\cap B^c\\), ao passo que negar uma interseção equivale à união dos complementares,",
        "\\((A \\cap B)^c = A^c \\cup B^c\\)."
      ),
      shiny::p(
        "Toda a aritmética básica da incerteza decorre de apenas três axiomas (Morettin e Bussab, seção 5.2;",
        "Casella e Berger, Def. 1.2.4): (1) \\(P(A) \\ge 0\\); (2) \\(P(\\Omega) = 1\\); e (3) se eventos são",
        "mutuamente exclusivos dois a dois, a probabilidade da união é a soma das probabilidades individuais.",
        "De forma mais específica, partir \\(\\Omega = A \\cup A^c\\) em dois blocos sem sobreposição entrega",
        "imediatamente \\(P(A^c) = 1 - P(A)\\) e \\(P(\\varnothing) = 0\\) (Teo. 1.2.8). Da mesma maneira,",
        "escrever \\(A \\cup B\\) como a união disjunta de \\(A\\) com a parte exclusiva \\(B \\cap A^c\\) mostra",
        "por que a regra geral da soma precisa descontar \\(P(A \\cap B)\\):",
        "\\(P(A \\cup B) = P(A) + P(B) - P(A \\cap B)\\) (Teo. 1.2.9b). Sem essa subtração, todo ponto que pertence",
        "simultaneamente a \\(A\\) e \\(B\\) seria somado duas vezes."
      ),
      shiny::p(
        shiny::strong("Experimente:"),
        "na urna de 1 a 12, compare \"Pares\" com \"Múltiplos de 3\" para ver os dois pontos em comum (6 e 12)",
        "descontados na regra da soma; troque \\(B\\) para \"Ímpares\" para zerar a interseção; e ative os pesos",
        "desiguais para confirmar que todas as identidades continuam valendo exatamente sem equiprobabilidade."
      )
    ),
    shiny::withMathJax(
      shiny::p(
        "Treating events as subsets of the sample space turns everyday statements into exact set operations:",
        "\"not \\(A\\)\" is the complement \\(A^c\\), \"\\(A\\) or \\(B\\) (or both)\" is the union \\(A \\cup B\\),",
        "and \"both \\(A\\) and \\(B\\)\" is the intersection \\(A \\cap B\\) (Casella and Berger, section 1.1;",
        "Agresti and Finlay, section 4.1). Two events are mutually exclusive (disjoint) when they share no",
        "elementary outcome, \\(A \\cap B = \\varnothing\\). By De Morgan's laws (Thm. 1.1.4d), negating a union",
        "requires both complements at once, \\((A \\cup B)^c = A^c \\cap B^c\\), whereas negating an intersection",
        "yields the union of the complements, \\((A \\cap B)^c = A^c \\cup B^c\\)."
      ),
      shiny::p(
        "All basic probability rules follow from just three axioms (Casella and Berger, Def. 1.2.4; Morettin and",
        "Bussab, section 5.2): (1) \\(P(A) \\ge 0\\); (2) \\(P(S) = 1\\); and (3) for pairwise disjoint events,",
        "the probability of the union equals the sum of the individual probabilities. More specifically, splitting",
        "\\(S = A \\cup A^c\\) into two disjoint pieces immediately gives \\(P(A^c) = 1 - P(A)\\) and",
        "\\(P(\\varnothing) = 0\\) (Thm. 1.2.8). Likewise, decomposing \\(A \\cup B\\) into the disjoint union of",
        "\\(A\\) and \\(B \\cap A^c\\) explains why the general addition rule subtracts \\(P(A \\cap B)\\):",
        "\\(P(A \\cup B) = P(A) + P(B) - P(A \\cap B)\\) (Thm. 1.2.9b). Without that correction, every outcome in",
        "both \\(A\\) and \\(B\\) would be counted twice."
      ),
      shiny::p(
        shiny::strong("Try it:"),
        "in the 1-to-12 urn, pair \"Evens\" with \"Multiples of 3\" to watch the overlapping outcomes (6 and 12)",
        "get subtracted once; switch \\(B\\) to \"Odds\" to make the events disjoint; and toggle unequal weights to",
        "verify that every identity holds without assuming equal probabilities."
      )
    )
  )
}

cuidados_eventos <- function() {
  tr(
    c(
      "Diagrama de Venn não é prova" = paste(
        "desenhos ajudam a enxergar uniões e interseções, mas podem omitir casos particulares (como A ⊂ B ou A = ∅).",
        "Em demonstrações, Casella e Berger (seção 1.1) exigem mostrar a dupla inclusão A ⊂ B e B ⊂ A."
      ),
      "Mutuamente exclusivos ≠ independentes" = paste(
        "se A e B têm probabilidade positiva e são mutuamente exclusivos (A ∩ B = ∅), saber que A ocorreu impede B",
        "de ocorrer. Portanto, eventos exclusivos nunca são independentes — distinção retomada no próximo aplicativo."
      ),
      "\"Ou\" inclusivo" = paste(
        "em probabilidade, A ∪ B inclui sempre o caso em que A e B ocorrem juntos. Se o interesse for 'exatamente um",
        "dos dois', o evento correto é (A ∩ Bᶜ) ∪ (Aᶜ ∩ B), cuja probabilidade vale P(A) + P(B) − 2 P(A ∩ B)."
      )
    ),
    c(
      "A Venn diagram is not a proof" = paste(
        "diagrams build intuition for unions and intersections, yet can hide edge cases such as A ⊂ B or A = ∅.",
        "Formal proofs in Casella and Berger (section 1.1) show set inclusion in both directions, A ⊂ B and B ⊂ A."
      ),
      "Mutually exclusive ≠ independent" = paste(
        "when A and B have positive probability and are disjoint (A ∩ B = ∅), knowing A occurred rules B out",
        "completely. Disjoint events with positive probability are therefore strongly dependent — revisited in the next app."
      ),
      "Inclusive \"or\"" = paste(
        "in probability, A ∪ B always includes outcomes where both A and B happen. For 'exactly one of the two',",
        "the event is (A ∩ Bᶜ) ∪ (Aᶜ ∩ B), with probability P(A) + P(B) − 2 P(A ∩ B)."
      )
    )
  )
}

rotulos_operacoes <- function() {
  stats::setNames(
    c("uniao", "intersecao", "comp_a", "b_menos_a", "demorgan1", "demorgan2", "vazio"),
    tr(
      c(
        "União: A ∪ B (A ou B)",
        "Interseção: A ∩ B (A e B)",
        "Complementar: Aᶜ (não A)",
        "Diferença: B ∩ Aᶜ (em B, fora de A)",
        "1ª lei de De Morgan: (A ∪ B)ᶜ = Aᶜ ∩ Bᶜ",
        "2ª lei de De Morgan: (A ∩ B)ᶜ = Aᶜ ∪ Bᶜ",
        "Conjunto vazio: ∅ (evento impossível)"
      ),
      c(
        "Union: A ∪ B (A or B)",
        "Intersection: A ∩ B (A and B)",
        "Complement: Aᶜ (not A)",
        "Difference: B ∩ Aᶜ (in B, outside A)",
        "De Morgan's 1st law: (A ∪ B)ᶜ = Aᶜ ∩ Bᶜ",
        "De Morgan's 2nd law: (A ∩ B)ᶜ = Aᶜ ∪ Bᶜ",
        "Empty set: ∅ (impossible event)"
      )
    )
  )
}

mod_eventos_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 310,
      shiny::selectInput(
        ns("cenario"), tr("Espaço amostral", "Sample space"),
        choices = stats::setNames(names(cenarios_eventos), purrr::map_chr(cenarios_eventos, "rotulo"))
      ),
      shiny::radioButtons(
        ns("pesos"), tr("Atribuição de probabilidades pᵢ", "Probability assignment pᵢ"),
        choices = stats::setNames(
          c("iguais", "desiguais"),
          tr(
            c("Equiprovável (pᵢ = 1 / #Ω)", "Pesos desiguais (Σ pᵢ = 1)"),
            c("Equally likely (pᵢ = 1 / #S)", "Unequal weights (Σ pᵢ = 1)")
          )
        ),
        selected = "iguais"
      ),
      shiny::uiOutput(ns("seletores_ab")),
      shiny::radioButtons(
        ns("operacao"), tr("Operação destacada na grade", "Operation highlighted on the grid"),
        choices = rotulos_operacoes(),
        selected = "uniao"
      )
    ),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(
        title = shiny::textOutput(ns("vb_op_titulo")),
        value = shiny::textOutput(ns("vb_op_valor")),
        shiny::textOutput(ns("vb_op_elementos")),
        theme = "primary"
      ),
      bslib::value_box(
        title = tr("Regra da soma: P(A ∪ B)", "Addition rule: P(A ∪ B)"),
        value = shiny::textOutput(ns("vb_soma_valor")),
        shiny::textOutput(ns("vb_soma_conta")),
        theme = "info"
      ),
      bslib::value_box(
        title = tr("Relação entre A e B", "Relationship between A and B"),
        value = shiny::textOutput(ns("vb_rel_valor")),
        shiny::textOutput(ns("vb_rel_desc")),
        theme = NULL
      )
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico_grade"), height = "410px"))),
      bslib::card(
        bslib::card_header(tr("Axiomas e propriedades verificados neste espaço", "Axioms and derived rules verified on this space")),
        bslib::card_body(shiny::uiOutput(ns("tabela_regras")))
      )
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
      bslib::card(bslib::card_header(rotulo_entenda()), bslib::card_body(texto_eventos())),
      cartao_cuidados(tr("Cuidados", "Watch out"), cuidados_eventos())
    )
  )
}

mod_eventos_server <- function(id, modo = shiny::reactive("light")) {
  shiny::moduleServer(id, function(input, output, session) {

    cen <- shiny::reactive({
      shiny::req(input$cenario)
      cenarios_eventos[[input$cenario]]
    })

    output$seletores_ab <- shiny::renderUI({
      c_at <- cen()
      shiny::tagList(
        shiny::selectInput(
          session$ns("ev_a"), tr("Evento A", "Event A"),
          choices = stats::setNames(names(c_at$opcoes_a), purrr::map_chr(c_at$opcoes_a, "rotulo"))
        ),
        shiny::selectInput(
          session$ns("ev_b"), tr("Evento B", "Event B"),
          choices = stats::setNames(names(c_at$opcoes_b), purrr::map_chr(c_at$opcoes_b, "rotulo"))
        )
      )
    })

    estado <- shiny::reactive({
      c_at <- cen()
      n_pts <- length(c_at$pontos)
      probs <- if (identical(input$pesos, "desiguais")) c_at$pesos_desiguais else rep(1 / n_pts, n_pts)
      ch_a <- if (!is.null(input$ev_a) && input$ev_a %in% names(c_at$opcoes_a)) input$ev_a else names(c_at$opcoes_a)[1]
      ch_b <- if (!is.null(input$ev_b) && input$ev_b %in% names(c_at$opcoes_b)) input$ev_b else names(c_at$opcoes_b)[1]
      op <- input$operacao %||% "uniao"

      res <- operar_eventos(
        pontos = c_at$pontos,
        probs = probs,
        elem_a = c_at$opcoes_a[[ch_a]]$elem,
        elem_b = c_at$opcoes_b[[ch_b]]$elem,
        operacao = op
      )
      res$pontos <- c_at$pontos
      res$probs <- probs
      res$ncol <- c_at$ncol
      res$operacao <- op
      res
    })

    nome_curto_op <- shiny::reactive({
      op <- estado()$operacao
      switch(
        op,
        uniao      = "P(A ∪ B)",
        intersecao = "P(A ∩ B)",
        comp_a     = "P(Aᶜ)",
        b_menos_a  = "P(B ∩ Aᶜ)",
        demorgan1  = "P((A ∪ B)ᶜ)",
        demorgan2  = "P((A ∩ B)ᶜ)",
        vazio      = "P(∅)"
      )
    })

    output$vb_op_titulo <- shiny::renderText(
      paste(tr("Operação destacada:", "Highlighted event:"), nome_curto_op())
    )

    output$vb_op_valor <- shiny::renderText({
      p <- estado()$p_destaque
      paste0(fmt_num(p, 4), " (", fmt_pct(p, 1), ")")
    })

    output$vb_op_elementos <- shiny::renderText({
      els <- estado()$elementos_destaque
      if (length(els) == 0L) return("∅")
      paste0("{", paste(els, collapse = ", "), "}")
    })

    output$vb_soma_valor <- shiny::renderText(fmt_num(estado()$p_uniao, 4))

    output$vb_soma_conta <- shiny::renderText({
      st <- estado()
      sprintf(
        "%s + %s − %s",
        fmt_num(st$p_a, 3), fmt_num(st$p_b, 3), fmt_num(st$p_inter, 3)
      )
    })

    output$vb_rel_valor <- shiny::renderText({
      st <- estado()
      if (st$disjuntos) {
        tr("Disjuntos (A ∩ B = ∅)", "Disjoint (A ∩ B = ∅)")
      } else if (st$a_sub_b) {
        tr("A ⊂ B (incluído)", "A ⊂ B (subset)")
      } else {
        tr("Com sobreposição", "Overlapping")
      }
    })

    output$vb_rel_desc <- shiny::renderText({
      st <- estado()
      if (st$disjuntos) {
        tr("Aditividade direta: P(A ∪ B) = P(A) + P(B)", "Direct additivity: P(A ∪ B) = P(A) + P(B)")
      } else if (st$a_sub_b) {
        sprintf(tr("Monotonicidade: P(A) = %s ≤ P(B) = %s", "Monotonicity: P(A) = %s ≤ P(B) = %s"),
                fmt_num(st$p_a, 3), fmt_num(st$p_b, 3))
      } else {
        sprintf(tr("Dupla contagem descontada: P(A ∩ B) = %s", "Overlap subtracted once: P(A ∩ B) = %s"),
                fmt_num(st$p_inter, 3))
      }
    })

    output$grafico_grade <- shiny::renderPlot({
      st <- estado()
      n_pts <- length(st$pontos)
      idx <- seq_len(n_pts) - 1L
      col_x <- (idx %% st$ncol) + 1L
      lin_y <- -(idx %/% st$ncol)

      pert <- ifelse(
        st$em_a & st$em_b, "A ∩ B",
        ifelse(st$em_a, tr("Só A", "Only A"),
               ifelse(st$em_b, tr("Só B", "Only B"), tr("Fora de A ∪ B", "Outside A ∪ B")))
      )

      df <- data.frame(
        x = col_x,
        y = lin_y,
        ponto = st$pontos,
        prob = st$probs,
        pert = pert,
        ativo = ifelse(st$destaque, "sim", "nao"),
        stringsAsFactors = FALSE
      )
      df$rotulo <- paste0(df$ponto, "\np = ", fmt_num(df$prob, 3), "\n[", df$pert, "]")

      p_modo <- paleta_modo(modo())
      cor_texto_ativo <- "#ffffff"
      cor_texto_inativo <- p_modo$texto

      ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_tile(
          ggplot2::aes(fill = ativo),
          width = 0.90, height = 0.86, colour = p_modo$grade, linewidth = 0.8
        ) +
        ggplot2::geom_text(
          data = df[df$ativo == "sim", ],
          ggplot2::aes(label = rotulo),
          colour = cor_texto_ativo, size = 3.7, fontface = "bold", lineheight = 0.95
        ) +
        ggplot2::geom_text(
          data = df[df$ativo == "nao", ],
          ggplot2::aes(label = rotulo),
          colour = cor_texto_inativo, size = 3.6, lineheight = 0.95
        ) +
        ggplot2::scale_fill_manual(values = c(sim = cores_app$destaque, nao = p_modo$neutro)) +
        tema_grafico(modo()) +
        ggplot2::theme(
          axis.text = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank()
        ) +
        ggplot2::labs(
          title = paste(tr("Pontos elementares de Ω em", "Elementary outcomes in"), nome_curto_op()),
          subtitle = tr("Células azuis pertencem ao evento destacado; cada bloco mostra o elemento, pᵢ e sua região", "Blue tiles belong to the selected event; each tile displays the outcome, pᵢ and its region"),
          caption = tr("Morettin e Bussab, seções 5.1–5.2; Casella e Berger, Teos. 1.1.4 e 1.2.9", "Morettin and Bussab, sections 5.1–5.2; Casella and Berger, Thms. 1.1.4 and 1.2.9")
        )
    })

    output$tabela_regras <- shiny::renderUI({
      st <- estado()
      p_dem1 <- sum(st$probs[(!st$em_a) & (!st$em_b)])
      p_dem2 <- sum(st$probs[(!st$em_a) | (!st$em_b)])

      df <- data.frame(
        tr(
          c(
            "Axioma 1 e 2: 0 ≤ P(E) ≤ 1 e P(Ω) = 1",
            "Evento impossível: P(∅) = 0 (Teo. 1.2.8a)",
            "Complementar: P(Aᶜ) = 1 − P(A) (Teo. 1.2.8c)",
            "Diferença: P(B ∩ Aᶜ) = P(B) − P(A ∩ B) (Teo. 1.2.9a)",
            "Regra da soma: P(A ∪ B) = P(A) + P(B) − P(A ∩ B)",
            "De Morgan I: P((A ∪ B)ᶜ) = P(Aᶜ ∩ Bᶜ)",
            "De Morgan II: P((A ∩ B)ᶜ) = P(Aᶜ ∪ Bᶜ)"
          ),
          c(
            "Axioms 1 & 2: 0 ≤ P(E) ≤ 1 and P(S) = 1",
            "Impossible event: P(∅) = 0 (Thm. 1.2.8a)",
            "Complement: P(Aᶜ) = 1 − P(A) (Thm. 1.2.8c)",
            "Difference: P(B ∩ Aᶜ) = P(B) − P(A ∩ B) (Thm. 1.2.9a)",
            "Addition rule: P(A ∪ B) = P(A) + P(B) − P(A ∩ B)",
            "De Morgan I: P((A ∪ B)ᶜ) = P(Aᶜ ∩ Bᶜ)",
            "De Morgan II: P((A ∩ B)ᶜ) = P(Aᶜ ∪ Bᶜ)"
          )
        ),
        c(
          fmt_num(sum(st$probs), 4),
          fmt_num(0, 4),
          fmt_num(st$p_comp_a, 4),
          fmt_num(st$p_b_menos_a, 4),
          fmt_num(st$p_uniao, 4),
          fmt_num(p_dem1, 4),
          fmt_num(p_dem2, 4)
        ),
        c(
          sprintf("min pᵢ = %s ≥ 0", fmt_num(min(st$probs), 3)),
          tr("Soma sobre conjunto vazio", "Empty sum"),
          sprintf("1 − %s", fmt_num(st$p_a, 3)),
          sprintf("%s − %s", fmt_num(st$p_b, 3), fmt_num(st$p_inter, 3)),
          sprintf("%s + %s − %s", fmt_num(st$p_a, 3), fmt_num(st$p_b, 3), fmt_num(st$p_inter, 3)),
          sprintf("1 − P(A ∪ B) = 1 − %s", fmt_num(st$p_uniao, 3)),
          sprintf("1 − P(A ∩ B) = 1 − %s", fmt_num(st$p_inter, 3))
        ),
        stringsAsFactors = FALSE
      )
      names(df) <- tr(
        c("Propriedade", "Valor", "Decomposição neste espaço"),
        c("Property", "Value", "Breakdown on this space")
      )
      tabela_html(df, classes = c("", "num", "leitura"))
    })
  })
}
