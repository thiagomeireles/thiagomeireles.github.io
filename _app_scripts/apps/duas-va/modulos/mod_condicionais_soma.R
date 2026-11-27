# =============================================================================
# Script:  mod_condicionais_soma.R (Aba 2: Condicionais e soma Z = X + Y)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Conjunta escolhida na aba 1 e valor condicionante Y = y
# Saída:
#   - Comparação entre P(X | Y = y) e a marginal P(X), distribuição de
#     Z = X + Y e conferência de E(X + Y) e Var(X + Y)
# =============================================================================

mod_condicionais_soma_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(ns("nota_conjunta")),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header(tr("Distribuição condicional P(X = x | Y = y)", "Conditional distribution P(X = x | Y = y)")),
        bslib::card_body(
          shiny::uiOutput(ns("seletor_y")),
          shiny::plotOutput(ns("grafico_cond"), height = "310px"),
          shiny::uiOutput(ns("texto_cond"))
        )
      ),
      bslib::card(
        bslib::card_header(tr("Distribuição da soma Z = X + Y", "Distribution of the sum Z = X + Y")),
        bslib::card_body(
          shiny::plotOutput(ns("grafico_soma"), height = "310px"),
          shiny::uiOutput(ns("tabela_soma"))
        )
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header(rotulo_entenda()),
        bslib::card_body(
          shiny::p(tr(
            "Condicionar em Y = y é olhar só a coluna desse valor e dividir cada célula pelo total da coluna, P(Y = y), para que as probabilidades voltem a somar 1 (Morettin e Bussab, fórmula 8.1). A média dessa distribuição é a esperança condicional E(X | Y = y). No Ex. 8.1, saber que o primeiro filho é menino leva o número médio de meninos de E(X) = 1,5 para E(X | Y = 1) = 2.",
            "Conditioning on Y = y means looking only at that value's column and dividing each cell by the column total, P(Y = y), so the probabilities add up to 1 again (Morettin and Bussab, formula 8.1). The mean of this distribution is the conditional expectation E(X | Y = y). In Ex. 8.1, knowing the first child is a boy moves the mean number of boys from E(X) = 1.5 to E(X | Y = 1) = 2."
          )),
          shiny::p(tr(
            "Para a soma Z = X + Y, junte as células com o mesmo valor de x + y e some as probabilidades (seção 8.3, Tabela 8.10). A média da soma é sempre E(X) + E(Y) (Teorema 8.1). A variância é Var(X) + Var(Y) + 2·Cov(X, Y) e só perde o último termo quando a covariância é zero (Teorema 8.3).",
            "For the sum Z = X + Y, group the cells with the same value of x + y and add their probabilities (section 8.3, Table 8.10). The mean of the sum is always E(X) + E(Y) (Theorem 8.1). The variance is Var(X) + Var(Y) + 2·Cov(X, Y) and loses the last term only when the covariance is zero (Theorem 8.3)."
          )),
          shiny::p(
            class = "mb-0",
            shiny::strong(tr("Experimente:", "Try this:")),
            tr(
              "na aba 1, escolha o Ex. 8.10 (meninos × meninas). Aqui, a soma vira uma barra só em z = 3, com variância zero: Var(X) + Var(Y) = 1,5 e o termo 2·Cov(X, Y) = −1,5 anula essa soma.",
              "in tab 1, pick Ex. 8.10 (boys × girls). Here the sum becomes a single bar at z = 3 with zero variance: Var(X) + Var(Y) = 1.5 and the 2·Cov(X, Y) = −1.5 term cancels it out."
            )
          )
        )
      ),
      cartao_cuidados(
        tr("Cuidados", "Watch out"),
        stats::setNames(
          list(
            tr(
              "P(X = x | Y = y) divide pelo total da coluna, P(Y = y); p(x, y) divide pelo total da tabela, que é 1. Na Tabela 8.5, P(X = 2, Y = 1) = 2/8, mas P(X = 2 | Y = 1) = 1/2.",
              "P(X = x | Y = y) divides by the column total, P(Y = y); p(x, y) divides by the table total, which is 1. In Table 8.5, P(X = 2, Y = 1) = 2/8, but P(X = 2 | Y = 1) = 1/2."
            ),
            tr(
              "em Y = X², E(X | Y = y) = 0 para todo y, igual a E(X), e mesmo assim X e Y são dependentes: saber Y muda a distribuição de X, mas não a média dela.",
              "in Y = X², E(X | Y = y) = 0 for every y, equal to E(X), and still X and Y are dependent: knowing Y changes the distribution of X, but not its mean."
            ),
            tr(
              "a soma das médias vale sempre; a soma das variâncias, não. Var(X + Y) = Var(X) + Var(Y) só quando Cov(X, Y) = 0, o que a independência garante, mas não é o único caso (Ex. 8.7).",
              "adding the means always works; adding the variances does not. Var(X + Y) = Var(X) + Var(Y) only when Cov(X, Y) = 0, which independence guarantees, but it is not the only case (Ex. 8.7)."
            )
          ),
          c(
            tr("Condicional não é conjunta", "Conditional is not joint"),
            tr("Média condicional constante não é independência", "Constant conditional mean is not independence"),
            tr("Variâncias não se somam sempre", "Variances do not always add")
          )
        )
      )
    )
  )
}

mod_condicionais_soma_server <- function(id, modo, conjunta_reativa) {
  shiny::moduleServer(id, function(input, output, session) {
    output$nota_conjunta <- shiny::renderUI({
      an <- conjunta_reativa()
      shiny::req(an)
      nota_conjunta_em_uso(an)
    })

    output$seletor_y <- shiny::renderUI({
      an <- conjunta_reativa()
      shiny::req(an)
      ns <- session$ns
      # Só valores com P(Y = y) > 0; mantém a escolha anterior se ela ainda existir
      ys_validos <- as.character(an$ys[an$py > 1e-12])
      anterior <- shiny::isolate(input$y_esc)
      selecionado <- if (!is.null(anterior) && anterior %in% ys_validos) anterior else ys_validos[length(ys_validos)]
      shiny::radioButtons(
        ns("y_esc"),
        tr("Condicionar em Y =", "Condition on Y ="),
        choices = stats::setNames(ys_validos, paste0("Y = ", ys_validos)),
        selected = selecionado,
        inline = TRUE
      )
    })

    cond_sel <- shiny::reactive({
      an <- conjunta_reativa()
      shiny::req(an, input$y_esc)
      idx <- which(as.character(an$ys) == as.character(input$y_esc))
      shiny::req(length(idx) == 1L, an$cond_y[[idx]]$py > 1e-12)
      an$cond_y[[idx]]
    })

    output$grafico_cond <- shiny::renderPlot({
      an <- conjunta_reativa()
      c_y <- cond_sel()
      shiny::req(an, c_y)
      p <- paleta_modo(modo())
      rotulo_marg <- tr("Marginal P(X = x)", "Marginal P(X = x)")
      rotulo_cond <- sprintf("P(X = x | Y = %s)", c_y$y)

      df <- data.frame(
        x = rep(factor(an$xs), 2),
        tipo = factor(
          c(rep(rotulo_marg, length(an$xs)), rep(rotulo_cond, length(an$xs))),
          levels = c(rotulo_marg, rotulo_cond)
        ),
        prob = c(an$px, c_y$p_x_dado_y),
        stringsAsFactors = FALSE
      )

      ggplot2::ggplot(df, ggplot2::aes(x = x, y = prob, fill = tipo)) +
        ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.65), width = 0.6) +
        ggplot2::scale_fill_manual(values = c(p$neutro, cores_app$destaque)) +
        ggplot2::scale_y_continuous(limits = c(0, 1.05), labels = rotulos_eixo()) +
        ggplot2::labs(
          title = sprintf(tr("Condicional em Y = %s vs. marginal", "Conditional on Y = %s vs. marginal"), c_y$y),
          x = "x", y = tr("Probabilidade", "Probability"), fill = NULL
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(legend.position = "top")
    })

    output$texto_cond <- shiny::renderUI({
      an <- conjunta_reativa()
      c_y <- cond_sel()
      shiny::req(an, c_y)
      shiny::p(class = "mt-2 mb-0", sprintf(
        tr(
          "Dado Y = %s: E(X | Y = %s) = %s e Var(X | Y = %s) = %s. Sem a condição, E(X) = %s e Var(X) = %s.",
          "Given Y = %s: E(X | Y = %s) = %s and Var(X | Y = %s) = %s. Without the condition, E(X) = %s and Var(X) = %s."
        ),
        c_y$y, c_y$y, fmt_num(c_y$media, 3), c_y$y, fmt_num(c_y$var, 3),
        fmt_num(an$ex, 3), fmt_num(an$vx, 3)
      ))
    })

    output$grafico_soma <- shiny::renderPlot({
      an <- conjunta_reativa()
      shiny::req(an)
      df_z <- distribuicao_soma(an$grade)
      ggplot2::ggplot(df_z, ggplot2::aes(x = factor(z), y = prob)) +
        ggplot2::geom_col(fill = cores_app$destaque, width = 0.6) +
        ggplot2::scale_y_continuous(limits = c(0, max(df_z$prob) * 1.18), labels = rotulos_eixo()) +
        ggplot2::labs(
          title = tr("P(Z = z): soma das células com x + y = z", "P(Z = z): sum of the cells with x + y = z"),
          x = "z = x + y", y = "P(Z = z)"
        ) +
        tema_grafico(modo())
    })

    output$tabela_soma <- shiny::renderUI({
      an <- conjunta_reativa()
      shiny::req(an)
      df_z <- distribuicao_soma(an$grade)
      df_z$prob <- fmt_num(df_z$prob, 4)
      names(df_z) <- c("z = x + y", "P(Z = z)")

      m <- momentos_soma(an)
      df_m <- data.frame(
        Medida = c("E(X + Y)", "E(X) + E(Y)", "Var(X + Y)", "Var(X) + Var(Y)", "2·Cov(X, Y)"),
        Valor = c(fmt_num(m$ez, 4), fmt_num(m$soma_medias, 4), fmt_num(m$vz, 4), fmt_num(m$soma_vars, 4), fmt_num(m$termo_cov, 4)),
        stringsAsFactors = FALSE
      )
      names(df_m) <- c(tr("Medida", "Measure"), tr("Valor", "Value"))

      shiny::tagList(
        tabela_html(df_z, c("num", "num")),
        tabela_html(df_m, c("", "num"))
      )
    })
  })
}
