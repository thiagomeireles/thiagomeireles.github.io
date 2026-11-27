# =============================================================================
# Script:  mod_conjunta.R (Aba 1: Conjunta, marginais e independência)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Conjunta de partida (exemplos de Morettin e Bussab, cap. 8, e Y = X²)
#     e peso da mistura com o produto das marginais
# Saída:
#   - Mapa de calor de p(x, y), tabela de dupla entrada com marginais,
#     momentos e verificação de independência célula a célula
# =============================================================================

mod_conjunta_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::selectInput(
        ns("caso"),
        tr("Distribuição conjunta de partida", "Starting joint distribution"),
        choices = rotulos_presets()
      ),
      shiny::uiOutput(ns("descricao")),
      shiny::sliderInput(
        ns("mistura_indep"),
        tr("Peso w da mistura com p_X(x)·p_Y(y)", "Weight w of the blend with p_X(x)·p_Y(y)"),
        min = 0, max = 1, value = 0, step = 0.1
      ),
      shiny::p(class = "small text-muted", tr(
        "Com w = 0 você vê a tabela do livro; com w = 1, a tabela que as mesmas marginais teriam se X e Y fossem independentes.",
        "With w = 0 you see the book's table; with w = 1, the table the same marginals would have if X and Y were independent."
      ))
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header(tr("Distribuição conjunta p(x, y) e marginais", "Joint distribution p(x, y) and marginals")),
        bslib::card_body(
          shiny::plotOutput(ns("grafico_conjunta"), height = "320px"),
          shiny::uiOutput(ns("tabela_dupla"))
        )
      ),
      bslib::card(
        bslib::card_header(tr("Verificação de independência e momentos", "Independence check and moments")),
        bslib::card_body(
          shiny::uiOutput(ns("tabela_resumo")),
          shiny::hr(),
          shiny::uiOutput(ns("diagnostico_indep"))
        )
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header(rotulo_entenda()),
        bslib::card_body(
          shiny::p(tr(
            "A distribuição conjunta dá a probabilidade de cada par, p(x, y) = P(X = x, Y = y). Somando a linha ou a coluna de um valor, você obtém as marginais p_X(x) e p_Y(y), que são as distribuições de cada variável sozinha (Morettin e Bussab, seções 8.1 e 8.2). X e Y são independentes quando p(x, y) = p_X(x)·p_Y(y) em todas as células; basta uma célula diferente para serem dependentes (fórmula 8.3).",
            "The joint distribution gives the probability of each pair, p(x, y) = P(X = x, Y = y). Adding up the row or column of a value gives the marginals p_X(x) and p_Y(y), the distributions of each variable on its own (Morettin and Bussab, sections 8.1 and 8.2). X and Y are independent when p(x, y) = p_X(x)·p_Y(y) in every cell; a single different cell makes them dependent (formula 8.3)."
          )),
          shiny::p(tr(
            "A covariância Cov(X, Y) = E(XY) − E(X)E(Y) e a correlação ρ = Cov(X, Y)/(DP(X)·DP(Y)) medem só a parte linear da relação (fórmulas 8.14 e 8.18). O peso w mistura a tabela com o produto das marginais: as marginais não mudam e a covariância passa a ser (1 − w)·Cov(X, Y).",
            "Covariance Cov(X, Y) = E(XY) − E(X)E(Y) and correlation ρ = Cov(X, Y)/(SD(X)·SD(Y)) measure only the linear part of the relation (formulas 8.14 and 8.18). The weight w blends the table with the product of the marginals: the marginals stay put and the covariance becomes (1 − w)·Cov(X, Y)."
          )),
          shiny::p(
            class = "mb-0",
            shiny::strong(tr("Experimente:", "Try this:")),
            tr(
              "no Ex. 8.1, confira Cov(X, Y) = 0,25 e ρ = 0,58, como no Ex. 8.9 do livro. Depois troque para o Ex. 8.7: a covariância é zero, mas a célula (X = 0, Y = 3) tem 0,200 contra 0,160 do produto das marginais.",
              "in Ex. 8.1, check Cov(X, Y) = 0.25 and ρ = 0.58, as in the book's Ex. 8.9. Then switch to Ex. 8.7: the covariance is zero, yet cell (X = 0, Y = 3) holds 0.200 against 0.160 from the product of the marginals."
            )
          )
        )
      ),
      cartao_cuidados(
        tr("Cuidados", "Watch out"),
        stats::setNames(
          list(
            tr(
              "independência implica Cov(X, Y) = 0 (Proposição 8.1), mas a recíproca não vale. No Ex. 8.7 e em Y = X², a covariância é zero e as variáveis são dependentes.",
              "independence implies Cov(X, Y) = 0 (Proposition 8.1), but not the other way around. In Ex. 8.7 and in Y = X², the covariance is zero and the variables are dependent."
            ),
            tr(
              "as marginais não determinam a conjunta. Mova o peso w: a tabela muda, p_X(x) e p_Y(y) ficam iguais.",
              "the marginals do not determine the joint. Move the weight w: the table changes while p_X(x) and p_Y(y) stay the same."
            ),
            tr(
              "ρ = −1 no Ex. 8.10 não quer dizer que um menino \"causa\" uma menina a menos. Quer dizer que X + Y = 3 em toda família, uma relação linear exata (Teorema 8.4).",
              "ρ = −1 in Ex. 8.10 does not mean one more boy \"causes\" one fewer girl. It means X + Y = 3 in every family, an exact linear relation (Theorem 8.4)."
            )
          ),
          c(
            tr("Covariância zero não é independência", "Zero covariance is not independence"),
            tr("Marginais não bastam", "Marginals are not enough"),
            tr("Correlação mede reta, não causa", "Correlation measures a line, not a cause")
          )
        )
      )
    )
  )
}

mod_conjunta_server <- function(id, modo, conjunta_reativa) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      shiny::req(input$caso, is.finite(input$mistura_indep))
      base_p <- presets_conjunta(input$caso)
      px <- rowSums(base_p$mat)
      py <- colSums(base_p$mat)
      w <- input$mistura_indep
      mat_mista <- (1 - w) * base_p$mat + w * outer(px, py)
      an <- analisar_conjunta(mat_mista, base_p$xs, base_p$ys)
      rotulo <- names(rotulos_presets())[rotulos_presets() == input$caso]
      an$rotulo <- if (w > 0) sprintf(tr("%s, com w = %s", "%s, with w = %s"), rotulo, fmt_num(w, 1)) else rotulo
      conjunta_reativa(an)
    })

    output$descricao <- shiny::renderUI({
      shiny::req(input$caso)
      shiny::p(class = "small text-muted", descricao_preset(input$caso))
    })

    output$grafico_conjunta <- shiny::renderPlot({
      an <- conjunta_reativa()
      shiny::req(an)
      p <- paleta_modo(modo())
      g <- an$grade
      g$rotulo <- paste0(fmt_num(g$p, 4), "\n(", fmt_num(g$p_indep, 4), ")")
      # Texto claro só nas células escuras, para não sumir no fundo
      g$cor_texto <- ifelse(g$p > max(g$p) / 2, "#ffffff", "#212529")

      ggplot2::ggplot(g, ggplot2::aes(x = factor(x), y = factor(y), fill = p)) +
        ggplot2::geom_tile(colour = p$fundo, linewidth = 1.2) +
        ggplot2::geom_text(ggplot2::aes(label = rotulo, colour = cor_texto), fontface = "bold", size = 3.8) +
        ggplot2::scale_colour_identity() +
        ggplot2::scale_fill_gradient(low = "#e9ecef", high = cores_app$destaque) +
        ggplot2::labs(
          title = tr("p(x, y) e, entre parênteses, p_X(x)·p_Y(y)", "p(x, y) and, in parentheses, p_X(x)·p_Y(y)"),
          x = "X", y = "Y"
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(panel.grid.major = ggplot2::element_blank())
    })

    output$tabela_dupla <- shiny::renderUI({
      an <- conjunta_reativa()
      shiny::req(an)
      df <- tabela_dupla_entrada(an)
      shiny::tagList(
        shiny::p(class = "small text-muted mt-2 mb-1", tr(
          "Tabela de dupla entrada: as somas das linhas dão p_Y(y) e as das colunas, p_X(x).",
          "Two-way table: row sums give p_Y(y) and column sums give p_X(x)."
        )),
        tabela_html(df, c("", rep("num", ncol(df) - 1)))
      )
    })

    output$tabela_resumo <- shiny::renderUI({
      an <- conjunta_reativa()
      shiny::req(an)
      df <- data.frame(
        Medida = c(
          "E(X)", "E(Y)", "Var(X)", "Var(Y)", "E(XY)", "Cov(X, Y)", "ρ(X, Y)",
          tr("Maior |p(x, y) − p_X(x)·p_Y(y)|", "Largest |p(x, y) − p_X(x)·p_Y(y)|")
        ),
        Valor = c(
          fmt_num(an$ex, 4), fmt_num(an$ey, 4),
          fmt_num(an$vx, 4), fmt_num(an$vy, 4),
          fmt_num(an$exy, 4), fmt_num(an$cov_xy, 4),
          if (is.na(an$rho)) "—" else fmt_num(an$rho, 4),
          fmt_num(an$desvio_max, 4)
        ),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Medida", "Measure"), tr("Valor", "Value"))
      tabela_html(df, c("", "num"))
    })

    output$diagnostico_indep <- shiny::renderUI({
      an <- conjunta_reativa()
      shiny::req(an)
      txt <- if (an$independentes) {
        tr(
          "Independentes: p(x, y) = p_X(x)·p_Y(y) em todas as células e, por isso, Cov(X, Y) = 0.",
          "Independent: p(x, y) = p_X(x)·p_Y(y) in every cell, and therefore Cov(X, Y) = 0."
        )
      } else if (an$cov_xy == 0) {
        tr(
          "Covariância nula, mas dependentes: há células com p(x, y) ≠ p_X(x)·p_Y(y). Independência exige a igualdade em todas as células, e ρ = 0 não garante isso.",
          "Zero covariance, yet dependent: some cells have p(x, y) ≠ p_X(x)·p_Y(y). Independence requires equality in every cell, and ρ = 0 does not guarantee it."
        )
      } else {
        tr(
          "Dependentes: ao menos uma célula tem p(x, y) ≠ p_X(x)·p_Y(y).",
          "Dependent: at least one cell has p(x, y) ≠ p_X(x)·p_Y(y)."
        )
      }
      shiny::p(class = "mb-0", txt)
    })
  })
}
