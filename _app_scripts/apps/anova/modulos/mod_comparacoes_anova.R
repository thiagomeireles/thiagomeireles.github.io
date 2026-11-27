# =============================================================================
# Script:  mod_comparacoes_anova.R (Aba 3: Comparações múltiplas)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Objeto reativo anova_reativa() da Aba 1
# Saída:
#   - Tabela comparando pares sem ajuste vs. Bonferroni vs. Tukey HSD
# =============================================================================

mod_comparacoes_anova_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_columns(
    col_widths = c(7, 5),
    bslib::card(
      bslib::card_header(tr("Comparações par a par: sem ajuste vs. Bonferroni vs. Tukey", "Pairwise comparisons: unadjusted vs. Bonferroni vs. Tukey")),
      bslib::card_body(shiny::uiOutput(ns("tabela_comp")))
    ),
    bslib::card(
      bslib::card_header(rotulo_entenda()),
      bslib::card_body(
        shiny::p(tr(
          "Um teste F significativo indica que ao menos um par de médias difere, mas não diz qual. Fazer testes t par a par sem correção infla o erro Tipo I global.",
          "A significant F test indicates that at least one pair of means differs, without telling which. Running uncorrected pairwise t tests inflates the family-wise Type I error."
        )),
        shiny::p(class = "mb-0", tr(
          "Bonferroni divide α pelo número de pares c = I(I − 1)/2 (ou multiplica cada valor-p por c). O método de Tukey HSD controla exatamente a taxa de erro global em todos os pares.",
          "Bonferroni divides α by the number of pairs c = I(I − 1)/2 (or multiplies each p-value by c). Tukey's HSD controls the exact family-wise error rate across all pairs."
        ))
      )
    )
  )
}

mod_comparacoes_anova_server <- function(id, modo, anova_reativa) {
  shiny::moduleServer(id, function(input, output, session) {
    output$tabela_comp <- shiny::renderUI({
      an <- anova_reativa()
      shiny::req(an)
      comp <- comparacoes_pares(an)
      df <- data.frame(
        Par = comp$par,
        Diff = fmt_num(comp$diff, 2),
        PSem = fmt_num(comp$p_sem, 4),
        PBonf = fmt_num(comp$p_bonf, 4),
        PTukey = fmt_num(comp$p_tukey, 4),
        stringsAsFactors = FALSE
      )
      names(df) <- c(
        tr("Par", "Pair"),
        tr("Diferença", "Difference"),
        tr("p sem ajuste", "Unadjusted p"),
        tr("p Bonferroni", "Bonferroni p"),
        tr("p Tukey HSD", "Tukey HSD p")
      )
      tabela_html(df, c("", "num", "num", "num", "num"))
    })
  })
}
