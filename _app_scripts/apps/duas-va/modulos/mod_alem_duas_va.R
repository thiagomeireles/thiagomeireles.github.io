# =============================================================================
# Script:  mod_alem_duas_va.R (Para ir além: leis totais e normal bidimensional)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Conjunta escolhida na aba 1 (leis totais) e ρ da normal bidimensional
# Saída:
#   - Verificação numérica da esperança total e da variância total e
#     dispersão simulada da normal bidimensional com a reta E(Y | X = x) = ρ·x
# =============================================================================

mod_alem_duas_va_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::layout_columns(
      col_widths = c(5, 7),
      bslib::card(
        bslib::card_header(tr("Esperança total e variância total", "Total expectation and total variance")),
        bslib::card_body(
          shiny::uiOutput(ns("nota_conjunta")),
          shiny::uiOutput(ns("tabela_totais")),
          shiny::uiOutput(ns("leitura_totais"))
        )
      ),
      bslib::card(
        bslib::card_header(tr("Normal bidimensional: ρ = 0 equivale à independência", "Bivariate normal: ρ = 0 is equivalent to independence")),
        bslib::card_body(
          bslib::layout_columns(
            col_widths = c(7, 5),
            shiny::sliderInput(ns("rho_bv"), tr("Correlação ρ", "Correlation ρ"), min = -0.95, max = 0.95, value = 0.70, step = 0.05),
            shiny::actionButton(ns("sim_bv"), tr("Sortear 400 pares", "Draw 400 pairs"), class = "btn-sm btn-primary mt-4")
          ),
          shiny::plotOutput(ns("grafico_bv"), height = "330px")
        )
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header(rotulo_entenda()),
        bslib::card_body(
          shiny::p(tr(
            "A média de X é a média das médias condicionais, E(X) = E[E(X | Y)]. A variância se divide em duas partes, Var(X) = E[Var(X | Y)] + Var[E(X | Y)]: a variação dentro de cada valor de Y mais a variação das médias entre os valores de Y (Gelman et al., seção 1.8, identidades 1.8 e 1.9; Casella e Berger, Teoremas 4.4.3 e 4.4.7). A tabela confere as duas com a conjunta da aba 1.",
            "The mean of X is the mean of the conditional means, E(X) = E[E(X | Y)]. The variance splits into two parts, Var(X) = E[Var(X | Y)] + Var[E(X | Y)]: the variation within each value of Y plus the variation of the means across values of Y (Gelman et al., section 1.8, identities 1.8 and 1.9; Casella and Berger, Theorems 4.4.3 and 4.4.7). The table checks both with the joint from tab 1."
          )),
          shiny::p(tr(
            "Na normal bidimensional, as marginais são normais e a condicional de Y dado X = x também é, com média μ_Y + ρ(σ_Y/σ_X)(x − μ_X) e variância σ²_Y(1 − ρ²) (Morettin e Bussab, seção 8.8). Com médias 0 e desvios 1, a reta das médias condicionais é ρ·x. Quando ρ = 0, a densidade conjunta vira o produto das marginais (fórmula 8.30). Nesse modelo, correlação zero garante independência, o que não vale em geral.",
            "In the bivariate normal, the marginals are normal and so is the conditional of Y given X = x, with mean μ_Y + ρ(σ_Y/σ_X)(x − μ_X) and variance σ²_Y(1 − ρ²) (Morettin and Bussab, section 8.8). With means 0 and standard deviations 1, the line of conditional means is ρ·x. When ρ = 0, the joint density becomes the product of the marginals (formula 8.30). In this model, zero correlation guarantees independence, which does not hold in general."
          )),
          shiny::p(
            class = "mb-0",
            shiny::strong(tr("Experimente:", "Try this:")),
            tr(
              "com o Ex. 8.10 na aba 1, E[Var(X | Y)] = 0: sabendo o número de meninas, o de meninos fica determinado, e toda a variância de X vem das médias condicionais. Na normal, leve ρ para 0,95 e veja os pontos se fecharem em torno da reta, com desvio condicional √(1 − ρ²) = 0,31.",
              "with Ex. 8.10 in tab 1, E[Var(X | Y)] = 0: once you know the number of girls, the number of boys is fixed, and all the variance of X comes from the conditional means. In the normal, push ρ to 0.95 and watch the points close in on the line, with conditional SD √(1 − ρ²) = 0.31."
            )
          )
        )
      ),
      cartao_cuidados(
        tr("Cuidados", "Watch out"),
        stats::setNames(
          list(
            tr(
              "a equivalência entre ρ = 0 e independência exige que X e Y sejam conjuntamente normais (Wooldridge, apêndice B, propriedade Normal.3). Duas variáveis com marginais normais, cada uma por si, não bastam.",
              "the equivalence between ρ = 0 and independence requires X and Y to be jointly normal (Wooldridge, appendix B, property Normal.3). Two variables that are each normal on their own are not enough."
            ),
            tr(
              "a correlação r dos 400 pontos sorteados oscila em torno de ρ de um sorteio para outro. ρ é o parâmetro do modelo; r é uma estimativa feita com a amostra.",
              "the correlation r of the 400 drawn points wobbles around ρ from one draw to the next. ρ is the model parameter; r is an estimate from the sample."
            ),
            tr(
              "Var[E(X | Y)] = 0 não quer dizer que X e Y sejam independentes. Em Y = X², a média condicional é constante e as variáveis são dependentes.",
              "Var[E(X | Y)] = 0 does not mean X and Y are independent. In Y = X², the conditional mean is constant and the variables are dependent."
            )
          ),
          c(
            tr("Normal conjunta, não só marginal", "Jointly normal, not just marginally"),
            tr("ρ não é r", "ρ is not r"),
            tr("Média condicional constante", "Constant conditional mean")
          )
        )
      )
    )
  )
}

mod_alem_duas_va_server <- function(id, modo, conjunta_reativa) {
  shiny::moduleServer(id, function(input, output, session) {
    output$nota_conjunta <- shiny::renderUI({
      an <- conjunta_reativa()
      shiny::req(an)
      nota_conjunta_em_uso(an)
    })

    output$tabela_totais <- shiny::renderUI({
      an <- conjunta_reativa()
      shiny::req(an)
      lt <- leis_totais(an)
      df <- data.frame(
        Identidade = c(
          "E(X)",
          "E[E(X | Y)]",
          "Var(X)",
          "E[Var(X | Y)]",
          "Var[E(X | Y)]",
          "E[Var(X | Y)] + Var[E(X | Y)]"
        ),
        Valor = c(
          fmt_num(lt$ex, 4),
          fmt_num(lt$e_da_cond, 4),
          fmt_num(lt$vx, 4),
          fmt_num(lt$e_da_var_cond, 4),
          fmt_num(lt$var_da_media_cond, 4),
          fmt_num(lt$soma_vars, 4)
        ),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Quantidade", "Quantity"), tr("Valor", "Value"))
      tabela_html(df, c("", "num"))
    })

    output$leitura_totais <- shiny::renderUI({
      an <- conjunta_reativa()
      shiny::req(an)
      lt <- leis_totais(an)
      shiny::req(lt$vx > 0)
      shiny::p(class = "small mb-0", sprintf(
        tr(
          "Saber Y explica %s da variância de X; os outros %s ficam dentro de cada valor de Y.",
          "Knowing Y explains %s of the variance of X; the other %s stays within each value of Y."
        ),
        fmt_pct(lt$var_da_media_cond / lt$vx, 1), fmt_pct(lt$e_da_var_cond / lt$vx, 1)
      ))
    })

    # A amostra guarda o ρ com que foi sorteada; reta e pontos usam o mesmo ρ
    amostra_bv <- shiny::reactiveVal(list(rho = 0.70, df = simular_normal_bivariada(400L, 0.70)))
    shiny::observeEvent(list(input$rho_bv, input$sim_bv), {
      shiny::req(is.finite(input$rho_bv))
      amostra_bv(list(rho = input$rho_bv, df = simular_normal_bivariada(400L, input$rho_bv)))
    }, ignoreInit = TRUE)

    output$grafico_bv <- shiny::renderPlot({
      amostra <- amostra_bv()
      shiny::req(amostra)
      df <- amostra$df
      rho <- amostra$rho
      r_amostral <- stats::cor(df$x, df$y)
      ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point(colour = cores_app$destaque, alpha = 0.55, size = 1.9) +
        ggplot2::geom_abline(intercept = 0, slope = rho, colour = cores_app$referencia, linewidth = 1.0) +
        ggplot2::coord_fixed(ratio = 1, xlim = c(-3.5, 3.5), ylim = c(-3.5, 3.5)) +
        ggplot2::labs(
          title = sprintf(tr("ρ = %s; r da amostra = %s", "ρ = %s; sample r = %s"), fmt_num(rho, 2), fmt_num(r_amostral, 2)),
          subtitle = sprintf(
            tr("E(Y | X = x) = %s·x; DP condicional √(1 − ρ²) = %s", "E(Y | X = x) = %s·x; conditional SD √(1 − ρ²) = %s"),
            fmt_num(rho, 2), fmt_num(sqrt(1 - rho^2), 3)
          ),
          x = "X", y = "Y",
          caption = tr("Médias 0, desvios padrão 1. Linha vermelha: E(Y | X = x).", "Means 0, standard deviations 1. Red line: E(Y | X = x).")
        ) +
        tema_grafico(modo())
    })
  })
}
