# =============================================================================
# Script:  mod_alem_discretas.R (Para ir além: hipergeométrica e geométrica)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Exemplo do livro ou parâmetros da população finita (N, r, n) e
#     probabilidade geométrica (p), fracassos já vistos s e espera extra t
# Saída:
#   - Comparação Hipergeométrica vs. Binomial e convenções da Geométrica, com
#     a distribuição condicional que mostra a falta de memória
# =============================================================================

mod_alem_discretas_ui <- function(id) {
  ns <- shiny::NS(id)
  exemplos <- exemplos_hiper()

  shiny::tagList(
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header(tr("Hipergeométrica vs. Binomial (sem vs. com reposição)", "Hypergeometric vs. Binomial (without vs. with replacement)")),
        bslib::card_body(
          shiny::selectInput(
            ns("exemplo_hiper"),
            tr("Exemplo", "Example"),
            choices = c(stats::setNames("livre", tr("Livre (ajuste os controles)", "Free (set the controls)")), stats::setNames(names(exemplos), purrr::map_chr(exemplos, "rotulo")))
          ),
          bslib::layout_columns(
            col_widths = c(4, 4, 4),
            shiny::sliderInput(ns("pop_n"), tr("População N", "Population N"), min = 15, max = 200, value = 30, step = 5),
            shiny::sliderInput(ns("r_suc"), tr("Sucessos r", "Successes r"), min = 2, max = 29, value = 12, step = 1),
            shiny::sliderInput(ns("n_am"), tr("Amostra n", "Sample n"), min = 2, max = 25, value = 10, step = 1)
          ),
          shiny::plotOutput(ns("grafico_hiper"), height = "270px"),
          shiny::uiOutput(ns("tabela_hiper"))
        )
      ),
      bslib::card(
        bslib::card_header(tr("Geométrica: duas convenções e falta de memória", "Geometric: two conventions and memorylessness")),
        bslib::card_body(
          shiny::radioButtons(
            ns("convencao"),
            tr("Convenção", "Convention"),
            choices = stats::setNames(
              c("ensaios", "fracassos"),
              c(tr("Ensaios até o 1º sucesso (x = 1, 2, ...)", "Trials until the 1st success (x = 1, 2, ...)"), tr("Fracassos antes do 1º sucesso (y = 0, 1, ...)", "Failures before the 1st success (y = 0, 1, ...)"))
            )
          ),
          bslib::layout_columns(
            col_widths = c(4, 4, 4),
            shiny::sliderInput(ns("p_geom"), tr("Probabilidade p", "Probability p"), min = 0.10, max = 0.80, value = 0.30, step = 0.05),
            shiny::sliderInput(ns("s_esp"), tr("Fracassos já vistos s", "Failures already seen s"), min = 1, max = 8, value = 3, step = 1),
            shiny::sliderInput(ns("t_extra"), tr("Espera extra t", "Extra wait t"), min = 1, max = 8, value = 2, step = 1)
          ),
          shiny::plotOutput(ns("grafico_geom"), height = "270px"),
          shiny::uiOutput(ns("resumo_geom"))
        )
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header(rotulo_entenda()),
        bslib::card_body(
          shiny::p(tr(
            "A hipergeométrica conta os sucessos numa amostra de n itens retirados sem reposição de uma população de N itens, r deles sucessos: P(X = k) = C(r, k) C(N − r, n − k) / C(N, n) (Morettin e Bussab, seção 6.6.4; Casella e Berger, seção 3.2). A média é a mesma da binomial, np com p = r/N, e a variância ganha o fator (N − n)/(N − 1). Quando N é grande diante de n, o fator fica perto de 1 e as duas distribuições quase coincidem.",
            "The hypergeometric counts successes in a sample of n items drawn without replacement from a population of N items, r of them successes: P(X = k) = C(r, k) C(N − r, n − k) / C(N, n) (Morettin and Bussab, section 6.6.4; Casella and Berger, section 3.2). The mean matches the binomial, np with p = r/N, and the variance gains the factor (N − n)/(N − 1). When N is large relative to n, the factor is close to 1 and the two distributions nearly match."
          )),
          shiny::p(tr(
            "A geométrica conta a espera até o primeiro sucesso em ensaios de Bernoulli. Casella e Berger (seção 3.2) contam ensaios, P(X = x) = p(1 − p)ˣ⁻¹ com x = 1, 2, ..., e E(X) = 1/p. A função stats::dgeom() do R conta fracassos, Y = X − 1, com E(Y) = (1 − p)/p. A variância, (1 − p)/p², é a mesma nas duas, porque subtrair 1 só desloca a distribuição. Falta de memória (3.2.11): depois de s fracassos, a espera que resta tem a mesma distribuição da espera inicial.",
            "The geometric counts the wait until the first success in Bernoulli trials. Casella and Berger (section 3.2) count trials, P(X = x) = p(1 − p)ˣ⁻¹ with x = 1, 2, ..., and E(X) = 1/p. R's stats::dgeom() counts failures, Y = X − 1, with E(Y) = (1 − p)/p. The variance, (1 − p)/p², is the same in both, because subtracting 1 only shifts the distribution. Memorylessness (3.2.11): after s failures, the remaining wait has the same distribution as the initial one."
          )),
          shiny::p(
            shiny::strong(tr("Experimente:", "Try this:")),
            tr(
              "escolha o lote de 100 peças: P(X = 0) = 0,584 sem reposição e 0,590 com reposição. Depois mude s na geométrica: os pontos vermelhos, calculados só com os casos em que X > s, continuam em cima das barras.",
              "pick the lot of 100 parts: P(X = 0) = 0.584 without replacement and 0.590 with replacement. Then change s in the geometric: the red dots, computed only from the cases where X > s, stay on top of the bars."
            )
          )
        )
      ),
      cartao_cuidados(
        tr("Cuidados", "Watch out"),
        stats::setNames(
          list(
            tr(
              "em R, dhyper(x, m, n, k) usa x para a contagem, m para os sucessos, n para os fracassos (N − r) e k para o tamanho da amostra. O n do R não é o n do livro.",
              "in R, dhyper(x, m, n, k) uses x for the count, m for successes, n for failures (N − r) and k for the sample size. R's n is not the book's n."
            ),
            tr(
              "antes de usar uma fórmula da geométrica, confira se ela conta ensaios ou fracassos. A média muda em 1; a variância, não.",
              "before using a geometric formula, check whether it counts trials or failures. The mean shifts by 1; the variance does not."
            ),
            tr(
              "a falta de memória não vale para tempos de vida que envelhecem. Se a peça falha mais à medida que é usada, a geométrica não serve (Casella e Berger, seção 3.2).",
              "memorylessness fails for lifetimes that age. If a part fails more often as it gets used, the geometric does not fit (Casella and Berger, section 3.2)."
            ),
            tr(
              "o Ex. 6.15 de Morettin e Bussab dá 0,426 para pelo menos uma defeituosa, mas 1 − 0,584 = 0,416.",
              "Morettin and Bussab's Ex. 6.15 gives 0.426 for at least one defective, but 1 − 0.584 = 0.416."
            )
          ),
          c(
            tr("Argumentos do R", "R arguments"),
            tr("Ensaios ou fracassos", "Trials or failures"),
            tr("Sem envelhecimento", "No aging"),
            tr("Errata", "Erratum")
          )
        )
      )
    )
  )
}

mod_alem_discretas_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$exemplo_hiper, {
      ex <- exemplos_hiper()[[input$exemplo_hiper]]
      shiny::req(ex)
      shiny::updateSliderInput(session, "pop_n", value = ex$N)
      shiny::updateSliderInput(session, "r_suc", max = ex$N - 1L, value = ex$r)
      shiny::updateSliderInput(session, "n_am", max = min(25L, ex$N - 1L), value = ex$n)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$pop_n, {
      shiny::req(is.finite(input$pop_n))
      shiny::updateSliderInput(session, "r_suc", max = input$pop_n - 1L, value = min(input$r_suc, input$pop_n - 1L))
      shiny::updateSliderInput(session, "n_am", max = min(25L, input$pop_n - 1L), value = min(input$n_am, input$pop_n - 1L))
    })

    hiper <- shiny::reactive({
      shiny::req(is.finite(input$pop_n), is.finite(input$r_suc), is.finite(input$n_am))
      dist_hiper_vs_bin(input$pop_n, min(input$r_suc, input$pop_n), min(input$n_am, input$pop_n))
    })

    output$grafico_hiper <- shiny::renderPlot({
      h <- hiper()
      df <- h$df
      ggplot2::ggplot(df, ggplot2::aes(x = k)) +
        ggplot2::geom_col(ggplot2::aes(y = hiper), fill = cores_app$destaque, width = 0.6) +
        ggplot2::geom_point(ggplot2::aes(y = binomial), colour = cores_app$referencia, size = 2.4) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = sprintf("hip(N = %d, r = %d, n = %d)", as.integer(input$pop_n), as.integer(input$r_suc), as.integer(input$n_am)),
          x = "k", y = "P(X = k)",
          caption = tr("Barras: hipergeométrica | Pontos vermelhos: binomial", "Bars: hypergeometric | Red dots: binomial")
        ) +
        tema_grafico(modo())
    })

    output$tabela_hiper <- shiny::renderUI({
      h <- hiper()
      df <- data.frame(
        Modelo = c(tr("Hipergeométrica (sem reposição)", "Hypergeometric (without replacement)"), tr("Binomial (com reposição)", "Binomial (with replacement)")),
        Media = c(fmt_num(h$media, 3), fmt_num(h$media, 3)),
        Variancia = c(fmt_num(h$var_hiper, 3), fmt_num(h$var_bin, 3)),
        Fator = c(fmt_num(h$fator_fpc, 3), fmt_num(1, 3)),
        P0 = c(fmt_num(h$p0_hiper, 3), fmt_num(h$p0_bin, 3)),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Modelo", "Model"), "E(X)", "Var(X)", tr("Fator (N−n)/(N−1)", "Factor (N−n)/(N−1)"), "P(X = 0)")
      tabela_html(df, c("", "num", "num", "num", "num"))
    })

    geom_res <- shiny::reactive({
      shiny::req(is.finite(input$p_geom), is.finite(input$s_esp), is.finite(input$t_extra))
      dist_geometrica_comp(input$p_geom, input$s_esp, input$t_extra)
    })

    output$grafico_geom <- shiny::renderPlot({
      g <- geom_res()
      df <- g$df
      fracassos <- identical(input$convencao, "fracassos")
      df$valor <- if (fracassos) df$x - 1L else df$x
      nome <- if (fracassos) "Y" else "X"

      ggplot2::ggplot(df, ggplot2::aes(x = valor)) +
        ggplot2::geom_col(ggplot2::aes(y = pmf), fill = cores_app$destaque, width = 0.65) +
        ggplot2::geom_point(ggplot2::aes(y = pmf_cond), colour = cores_app$referencia, size = 2.4) +
        ggplot2::scale_x_continuous(breaks = function(lim) unique(round(pretty(lim)))) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = if (fracassos) tr("Fracassos antes do 1º sucesso", "Failures before the 1st success") else tr("Ensaios até o 1º sucesso", "Trials until the 1st success"),
          subtitle = if (fracassos) "stats::dgeom(y, p)" else "stats::dgeom(x - 1, p)",
          x = tolower(nome), y = sprintf("P(%s = %s)", nome, tolower(nome)),
          caption = sprintf(
            tr("Barras: distribuição original | Pontos vermelhos: espera restante depois de %d fracassos", "Bars: original distribution | Red dots: remaining wait after %d failures"),
            as.integer(input$s_esp)
          )
        ) +
        tema_grafico(modo())
    })

    output$resumo_geom <- shiny::renderUI({
      g <- geom_res()
      s <- as.integer(input$s_esp)
      t <- as.integer(input$t_extra)
      shiny::tagList(
        shiny::p(sprintf(
          tr(
            "Média: E(X) = 1/p = %s ensaios, ou E(Y) = (1 − p)/p = %s fracassos. Variância nas duas: %s.",
            "Mean: E(X) = 1/p = %s trials, or E(Y) = (1 − p)/p = %s failures. Variance in both: %s."
          ),
          fmt_num(g$media_ensaios, 2), fmt_num(g$media_frac, 2), fmt_num(g$variancia, 2)
        )),
        shiny::p(class = "mb-0", sprintf(
          tr(
            "Falta de memória: P(X > %d | X > %d) = %s e P(X > %d) = %s.",
            "Memorylessness: P(X > %d | X > %d) = %s and P(X > %d) = %s."
          ),
          s + t, s, fmt_num(g$p_cond, 4), t, fmt_num(g$p_maior_t, 4)
        ))
      )
    })
  })
}
