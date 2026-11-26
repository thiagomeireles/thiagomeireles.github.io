# =============================================================================
# Script:  mod_experimento.R (módulo: experimento aleatório, espaço amostral Ω
#          e interpretações clássica, frequentista e subjetiva)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-26
#
# Entrada:
#   - objetos e funções de _app_scripts/apps/espaco-amostral/modulos/dados.R
# Saída:
#   - funções mod_experimento_ui() e mod_experimento_server()
# =============================================================================

texto_experimento <- function() {
  tr(
    shiny::withMathJax(
      shiny::p(
        "Antes de calcular qualquer chance, é preciso fixar com precisão quais resultados podem sair da observação.",
        "Em um experimento aleatório — cujo desfecho individual não pode ser previsto com certeza, ainda que o",
        "conjunto de possibilidades seja conhecido —, o espaço amostral \\(\\Omega\\) (denotado por \\(S\\) em",
        "Casella e Berger) reúne todos os resultados elementares possíveis, e qualquer subconjunto \\(A \\subset \\Omega\\)",
        "define um evento (Morettin e Bussab, seção 5.1). Dessa forma, a escolha de \\(\\Omega\\) depende da pergunta:",
        "ao lançar dois dados, anotar os \\(36\\) pares ordenados \\((i, j)\\) produz pontos equiprováveis de",
        "probabilidade \\(1/36\\), ao passo que registrar apenas as \\(11\\) somas \\(\\{2, \\dots, 12\\}\\) gera um",
        "espaço perfeitamente válido, porém não equiprovável, uma vez que a soma \\(7\\) reúne seis pares e a soma",
        "\\(2\\) apenas um."
      ),
      shiny::p(
        "Os axiomas da probabilidade exigem apenas que os pesos dos resultados sejam não negativos e somem \\(1\\),",
        "sem impor de onde esses números vêm (Casella e Berger, Ex. 1.2.5). Na prática, três leituras complementares",
        "atribuem significado a \\(P(A)\\): (1) na leitura clássica ou por simetria física, quando os \\(\\#\\Omega\\)",
        "pontos elementares têm a mesma chance, vale \\(P(A) = \\#A / \\#\\Omega\\); (2) na leitura frequentista de",
        "longo prazo (*long-run relative frequency*, Agresti e Finlay, seção 4.1), \\(P(A)\\) representa a proporção",
        "de vezes em que \\(A\\) ocorreria numa sequência muito longa de repetições sob as mesmas condições, o que",
        "conecta diretamente sorteios amostrais a proporções populacionais; e (3) na leitura subjetiva (Morettin e",
        "Bussab, seção 5.5; Gelman et al., seção 1.5), quando o fenômeno não admite repetição idêntica — como a estreia",
        "de uma política pública ou de um novo negócio — ou envolve parâmetros desconhecidos, \\(P(A)\\) quantifica",
        "conhecimento incompleto sob coerência de apostas: os números precisam obedecer aos axiomas para que nenhuma",
        "combinação de apostas garanta perda certa."
      ),
      shiny::p(
        shiny::strong("Experimente:"),
        "no dado de seis faces, altere o peso da face 6 para ver como \\(P(A)\\) deixa de coincidir com a contagem",
        "ingênua \\(\\#A / \\#\\Omega\\), mas a proporção acumulada \\(n_A / n\\) continua se estabilizando ao redor",
        "do verdadeiro \\(P(A)\\) quando \\(n\\) cresce. Em seguida, reduza \\(n\\) para \\(20\\) e sorteie novas",
        "trajetórias para notar por que amostras curtas oscilam longe do valor teórico."
      )
    ),
    shiny::withMathJax(
      shiny::p(
        "Before computing any chance, one has to specify every outcome an observation can produce. In a random",
        "experiment — whose individual outcome cannot be predicted with certainty even though the set of",
        "possibilities is known — the sample space \\(S\\) (written \\(\\Omega\\) in Morettin and Bussab) collects",
        "all elementary outcomes, and any subset \\(A \\subset S\\) defines an event (Casella and Berger, section",
        "1.1). Put differently, the choice of sample space follows the question: when rolling two fair dice,",
        "recording the \\(36\\) ordered pairs \\((i, j)\\) yields equally likely points of probability \\(1/36\\),",
        "whereas recording only the \\(11\\) sums \\(\\{2, \\dots, 12\\}\\) produces a valid sample space that is",
        "not equally likely, since sum \\(7\\) gathers six pairs and sum \\(2\\) only one."
      ),
      shiny::p(
        "The probability axioms require only that outcome weights be nonnegative and add up to \\(1\\), without",
        "dictating how those numbers are chosen (Casella and Berger, Ex. 1.2.5). In practice, three interpretations",
        "give substantive meaning to \\(P(A)\\): (1) under classical symmetry, when all \\(\\#S\\) elementary",
        "outcomes share the same weight, \\(P(A) = \\#A / \\#S\\); (2) under the *long-run relative frequency*",
        "view (Agresti and Finlay, section 4.1), \\(P(A)\\) is the proportion of times \\(A\\) would occur in a",
        "very long sequence of independent trials, tying random sampling directly to population proportions; and",
        "(3) under subjective probability (Morettin and Bussab, section 5.5; Gelman et al., section 1.5), when",
        "trials cannot be repeated under identical conditions — such as a one-off policy reform or a new venture —",
        "or when quantifying uncertainty about unknown parameters, \\(P(A)\\) measures incomplete knowledge under",
        "coherence of bets: probabilities must obey the axioms so no system of bets guarantees a sure loss."
      ),
      shiny::p(
        shiny::strong("Try it:"),
        "on the six-sided die, shift the weight of face 6 to watch \\(P(A)\\) depart from the naive ratio",
        "\\(\\#A / \\#S\\), while the cumulative relative frequency \\(n_A / n\\) still settles around the true",
        "\\(P(A)\\) as \\(n\\) grows. Then drop \\(n\\) to \\(20\\) and draw fresh paths to see how noisy short",
        "runs can be."
      )
    )
  )
}

cuidados_experimento <- function() {
  tr(
    c(
      "Equiprobabilidade não é axioma" = paste(
        "a fórmula #A / #Ω só vale quando há simetria ou sorteio aleatório simples entre os pontos de Ω.",
        "Tratar duas categorias quaisquer ('chover' vs. 'não chover') como 50% cada confunde cardinalidade com probabilidade."
      ),
      "Longo prazo ≠ compensação no curto prazo" = paste(
        "a estabilização da frequência relativa n_A / n vem da diluição das oscilações iniciais quando n cresce,",
        "e não de a moeda 'compensar' uma sequência de caras (falácia do apostador)."
      ),
      "Notação entre os livros" = paste(
        "Morettin e Bussab denotam o espaço amostral por Ω e o complementar por Aᶜ; Casella e Berger usam S e Aᶜ;",
        "Agresti e Finlay escrevem 'not A' e 'A or B' em texto corrido."
      )
    ),
    c(
      "Equal probability is not an axiom" = paste(
        "the ratio #A / #S holds only when physical symmetry or simple random sampling makes elementary outcomes",
        "equally likely. Treating any two labels ('rain' vs. 'no rain') as 50% each confuses counting with probability."
      ),
      "Long run ≠ short-run compensation" = paste(
        "relative frequency n_A / n stabilizes because early swings are diluted as n grows, not because a fair coin",
        "'owes' tails after a streak of heads (the gambler's fallacy)."
      ),
      "Notation across textbooks" = paste(
        "Casella and Berger write S and Aᶜ for the sample space and complement; Morettin and Bussab use Ω and Aᶜ;",
        "Agresti and Finlay write 'not A' and 'A or B' in plain words."
      )
    )
  )
}

tabela_interpretacoes <- function() {
  df <- data.frame(
    tr(
      c("Clássica (simetria)", "Frequentista (longo prazo)", "Subjetiva (grau de crença)"),
      c("Classical (symmetry)", "Frequentist (long run)", "Subjective (degree of belief)")
    ),
    tr(
      c("P(A) = #A / #Ω com pontos equiprováveis", "Proporção limite lim (n_A / n) em repetições", "Grau de crença coerente com os axiomas"),
      c("P(A) = #A / #S with equally likely points", "Limiting proportion lim (n_A / n) across trials", "Degree of belief obeying the axioms")
    ),
    tr(
      c("Dados, moedas honestas, AAS de cadastro finito", "Pesquisas amostrais (GSS, PNAD), testes clínicos", "Risco de evento único, inferência bayesiana"),
      c("Fair dice/coins, SRS from a finite frame", "Sample surveys (GSS, exit polls), clinical trials", "One-off events, Bayesian inference")
    ),
    tr(
      c("Exige Ω finito e simetria verificável", "Exige experimento repetível nas mesmas condições", "Pessoas distintas podem partir de prioris distintas"),
      c("Requires finite S and defensible symmetry", "Requires repeatable trials under like conditions", "Different analysts may hold different priors")
    ),
    stringsAsFactors = FALSE
  )
  names(df) <- tr(
    c("Interpretação", "Como define P(A)", "Onde aparece", "Limite principal"),
    c("Interpretation", "How P(A) is defined", "Typical setting", "Main limitation")
  )
  df
}

mod_experimento_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 310,
      shiny::selectInput(
        ns("exp"), tr("Experimento aleatório", "Random experiment"),
        choices = stats::setNames(names(experimentos_omega), purrr::map_chr(experimentos_omega, "rotulo"))
      ),
      shiny::uiOutput(ns("seletor_evento")),
      shiny::uiOutput(ns("controle_param")),
      shiny::hr(),
      shiny::sliderInput(
        ns("n_lanc"), tr("Repetições simuladas (n)", "Simulated trials (n)"),
        min = 20, max = 2000, value = 400, step = 20, sep = sep_milhar()
      ),
      shiny::sliderInput(
        ns("n_traj"), tr("Sequências simultâneas", "Simultaneous sequences"),
        min = 1, max = 12, value = 5, step = 1
      ),
      shiny::actionButton(
        ns("sortear"), tr("Repetir experimento", "Run trials again"),
        class = "btn-primary", icon = shiny::icon("dice")
      )
    ),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(
        title = tr("Probabilidade teórica P(A)", "Theoretical probability P(A)"),
        value = shiny::textOutput(ns("vb_prob")),
        shiny::textOutput(ns("vb_prob_sub")),
        theme = "primary"
      ),
      bslib::value_box(
        title = tr("Frequência relativa final (1ª sequência)", "Final relative frequency (1st sequence)"),
        value = shiny::textOutput(ns("vb_freq")),
        shiny::textOutput(ns("vb_freq_sub")),
        theme = "info"
      ),
      bslib::value_box(
        title = tr("Espaço amostral (Ω / S)", "Sample space (Ω / S)"),
        value = shiny::textOutput(ns("vb_card")),
        shiny::textOutput(ns("vb_tipo")),
        theme = NULL
      )
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(5, 7)),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico_omega"), height = "400px"))),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico_freq"), height = "400px")))
    ),
    bslib::card(
      bslib::card_header(tr("As três interpretações de P(A) nos livros-base", "The three interpretations of P(A) across the textbooks")),
      bslib::card_body(tabela_html(tabela_interpretacoes()))
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
      bslib::card(bslib::card_header(rotulo_entenda()), bslib::card_body(texto_experimento())),
      cartao_cuidados(tr("Cuidados", "Watch out"), cuidados_experimento())
    )
  )
}

mod_experimento_server <- function(id, modo = shiny::reactive("light")) {
  shiny::moduleServer(id, function(input, output, session) {

    exp_atual <- shiny::reactive({
      shiny::req(input$exp)
      experimentos_omega[[input$exp]]
    })

    output$seletor_evento <- shiny::renderUI({
      evs <- exp_atual()$eventos
      shiny::selectInput(
        session$ns("evento"), tr("Evento A de interesse", "Target event A"),
        choices = stats::setNames(names(evs), purrr::map_chr(evs, "rotulo"))
      )
    })

    output$controle_param <- shiny::renderUI({
      if (!isTRUE(exp_atual()$permite_vicio)) {
        return(shiny::helpText(exp_atual()$fonte))
      }
      if (identical(input$exp, "dado")) {
        shiny::sliderInput(
          session$ns("param"),
          tr("Peso da face 6 (1/6 ≈ 0,167 = dado honesto)", "Weight of face 6 (1/6 ≈ 0.167 = fair die)"),
          min = 0.05, max = 0.65, value = round(1 / 6, 3), step = 0.01
        )
      } else {
        shiny::sliderInput(
          session$ns("param"),
          tr("Probabilidade de cara em cada lançamento (p)", "Probability of heads on each toss (p)"),
          min = 0.10, max = 0.90, value = 0.50, step = 0.05
        )
      }
    })

    dist_atual <- shiny::reactive({
      e <- exp_atual()
      padrao <- if (identical(input$exp, "dado")) 1 / 6 else 0.50
      val <- if (isTRUE(e$permite_vicio) && !is.null(input$param) && is.finite(input$param)) input$param else padrao
      df <- e$distribuicao(val)
      evs <- e$eventos
      ev_chave <- if (!is.null(input$evento) && input$evento %in% names(evs)) input$evento else names(evs)[1]
      idx <- evs[[ev_chave]]$idx
      df$em_a <- seq_len(nrow(df)) %in% idx
      df
    })

    prob_a <- shiny::reactive(sum(dist_atual()$prob[dist_atual()$em_a]))

    trajetorias <- shiny::reactive({
      input$sortear
      shiny::req(input$n_lanc, input$n_traj)
      simular_trajetorias_freq(prob_a(), as.integer(input$n_lanc), as.integer(input$n_traj))
    })

    output$vb_prob <- shiny::renderText({
      p <- prob_a()
      paste0(fmt_num(p, 4), " (", fmt_pct(p, 1), ")")
    })

    output$vb_prob_sub <- shiny::renderText({
      df <- dist_atual()
      razao_ing <- mean(df$em_a)
      sprintf(
        tr("#A / #pontos = %d / %d (%s)", "#A / #points = %d / %d (%s)"),
        sum(df$em_a), nrow(df), fmt_pct(razao_ing, 1)
      )
    })

    output$vb_freq <- shiny::renderText({
      trj <- trajetorias()
      fim1 <- trj$freq_rel[trj$trajetoria == "1" & trj$passo == max(trj$passo)]
      paste0(fmt_num(fim1, 4), " (", fmt_pct(fim1, 1), ")")
    })

    output$vb_freq_sub <- shiny::renderText({
      trj <- trajetorias()
      fim1 <- trj$freq_rel[trj$trajetoria == "1" & trj$passo == max(trj$passo)]
      erro <- abs(fim1 - prob_a())
      sprintf(tr("Desvio |n_A/n − P(A)| = %s em n = %s", "Gap |n_A/n − P(A)| = %s at n = %s"),
              fmt_num(erro, 4), formatar_inteiro(max(trj$passo)))
    })

    output$vb_card <- shiny::renderText(exp_atual()$cardinalidade)
    output$vb_tipo <- shiny::renderText(exp_atual()$tipo_espaco)

    output$grafico_omega <- shiny::renderPlot({
      df <- dist_atual()
      df$ponto <- factor(df$ponto, levels = df$ponto)
      df$cor <- ifelse(df$em_a, "sim", "nao")
      p_modo <- paleta_modo(modo())

      ggplot2::ggplot(df, ggplot2::aes(x = ponto, y = prob, fill = cor)) +
        ggplot2::geom_col(width = 0.68) +
        ggplot2::geom_text(
          ggplot2::aes(label = fmt_num(prob, 3)),
          vjust = -0.45, size = 3.8, colour = p_modo$texto
        ) +
        ggplot2::scale_fill_manual(values = c(sim = cores_app$destaque, nao = p_modo$neutro)) +
        ggplot2::scale_y_continuous(
          labels = rotulos_eixo(),
          limits = c(0, max(df$prob) * 1.18),
          expand = ggplot2::expansion(mult = c(0, 0.02))
        ) +
        tema_grafico(modo()) +
        ggplot2::labs(
          title = tr("Pesos no espaço amostral", "Weights on the sample space"),
          subtitle = tr("Em azul, os pontos elementares que pertencem ao evento A", "Blue bars belong to the target event A"),
          x = exp_atual()$omega_desc,
          y = tr("Probabilidade pontual pᵢ", "Point probability pᵢ"),
          caption = exp_atual()$fonte
        )
    })

    output$grafico_freq <- shiny::renderPlot({
      trj <- trajetorias()
      alvo <- prob_a()
      p_modo <- paleta_modo(modo())
      trj_destaque <- trj[trj$trajetoria == "1", ]
      trj_fundo <- trj[trj$trajetoria != "1", ]

      g <- ggplot2::ggplot( mapping = ggplot2::aes(x = passo, y = freq_rel))
      if (nrow(trj_fundo) > 0L) {
        g <- g + ggplot2::geom_line(
          data = trj_fundo,
          ggplot2::aes(group = trajetoria),
          colour = p_modo$secundario, alpha = 0.40, linewidth = 0.65
        )
      }
      g +
        ggplot2::geom_hline(yintercept = alvo, colour = cores_app$referencia, linetype = "dashed", linewidth = 0.95) +
        ggplot2::geom_line(data = trj_destaque, colour = cores_app$destaque, linewidth = 1.1) +
        ggplot2::scale_x_continuous(labels = rotulos_eixo()) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo(), limits = c(0, 1)) +
        tema_grafico(modo()) +
        ggplot2::labs(
          title = tr("Frequência relativa acumulada n_A / n", "Cumulative relative frequency n_A / n"),
          subtitle = sprintf(
            tr("Linha tracejada vermelha: P(A) = %s | Azul: 1ª sequência (%d no total)", "Dashed red line: P(A) = %s | Blue: 1st sequence (%d total)"),
            fmt_num(alvo, 4), length(unique(trj$trajetoria))
          ),
          x = tr("Número de repetições (n)", "Number of trials (n)"),
          y = tr("Proporção acumulada n_A / n", "Cumulative proportion n_A / n"),
          caption = tr("Agresti e Finlay, seção 4.1; Morettin e Bussab, cap. 9", "Agresti and Finlay, section 4.1; Morettin and Bussab, ch. 9")
        )
    })
  })
}
