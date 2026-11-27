# =============================================================================
# Script:  app.R (tópico Análise de variância - ANOVA)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - _app_scripts/apps/comum/utilitarios.R
#   - _app_scripts/apps/anova/modulos/*.R
# Saída:
#   - aplicativo Shiny (publicado no Space do Hugging Face em /anova/
#     e, em inglês, em /en/anova/)
# =============================================================================

# Este arquivo roda no Shiny Server, montado por _app_scripts/hf-space/montar_apps.R. Não
# limpa a sessão nem usa here/pacman: os caminhos são relativos à pasta do app.
options(scipen = 999)

pasta_comum <- if (dir.exists("comum")) "comum" else file.path("..", "comum")

modulos <- list.files("modulos", pattern = "\\.R$", full.names = TRUE)
modulos <- c(modulos[basename(modulos) == "dados.R"], modulos[basename(modulos) != "dados.R"])

c(list.files(pasta_comum, pattern = "\\.R$", full.names = TRUE), modulos) |>
  purrr::walk(source)

ui <- pagina_tema(
  titulo = tr("Análise de variância", "Analysis of variance"),
  versao = tr("0.1 (novembro de 2026)", "0.1 (November 2026)"),
  descricao = tr(
    paste(
      "Como comparar várias médias simultaneamente pela ANOVA de um fator: decomposição da soma de quadrados",
      "(SQTotal = SQEntre + SQDentro), teste F, verificação de homocedasticidade (Bartlett) e normalidade dos",
      "resíduos, e comparações múltiplas par a par com Bonferroni e Tukey HSD. O menu \"Para ir além\" faz a",
      "ponte com modelos hierárquicos e agrupamento parcial (partial pooling). O conteúdo segue Morettin e",
      "Bussab (Estatística Básica, cap. 15) e Agresti e Finlay (Statistical Methods for the Social Sciences,",
      "cap. 12), com Casella e Berger (Statistical Inference, cap. 11) e Gelman et al. (Bayesian Data Analysis,",
      "cap. 5) no aprofundamento."
    ),
    paste(
      "How to compare several means simultaneously via one-way ANOVA: sum-of-squares decomposition",
      "(SSTotal = SSBetween + SSWithin), F test, homoscedasticity (Bartlett) and residual normality checks,",
      "and pairwise multiple comparisons with Bonferroni and Tukey HSD. The \"Going further\" menu bridges",
      "to hierarchical models and partial pooling. Content follows Agresti and Finlay (Statistical Methods for",
      "the Social Sciences, ch. 12) and Morettin and Bussab (Estatística Básica, ch. 15), with Casella and",
      "Berger (Statistical Inference, ch. 11) and Gelman et al. (Bayesian Data Analysis, ch. 5)."
    )
  ),
  referencias = list(
    referencia_livro("Agresti, A., & Finlay, B.", 2009, "Statistical methods for the social sciences", "Pearson Prentice Hall", edicao = 4),
    referencia_livro("Casella, G., & Berger, R. L.", 2002, "Statistical inference", "Duxbury", edicao = 2),
    referencia_livro("Gelman, A., Carlin, J. B., Stern, H. S., Dunson, D. B., Vehtari, A., & Rubin, D. B.", 2013, "Bayesian data analysis", "Chapman & Hall/CRC", edicao = 3),
    referencia_livro("Morettin, P. A., & Bussab, W. O.", 2010, "Estatística básica", "Saraiva", edicao = 6)
  ),
  bslib::nav_panel(
    tr("Decomposição e F", "Decomposition and F"),
    value = "decomposicao",
    shiny::tags$style(".tabela-app th { text-transform: none; }"),
    mod_decomposicao_anova_ui("decomposicao")
  ),
  bslib::nav_panel(tr("Pressupostos e Bartlett", "Assumptions and Bartlett"), value = "pressupostos", mod_pressupostos_anova_ui("pressupostos")),
  bslib::nav_panel(tr("Comparações múltiplas", "Multiple comparisons"), value = "comparacoes", mod_comparacoes_anova_ui("comparacoes")),
  bslib::nav_menu(
    tr("Para ir além", "Going further"),
    bslib::nav_panel(
      tr("Agrupamento parcial (hierárquico)", "Partial pooling (hierarchical)"),
      value = "alem_anova",
      mod_alem_anova_ui("alem_anova")
    )
  )
)

server <- function(input, output, session) {
  modo <- modo_app(input)
  anova_reativa <- shiny::reactiveVal(NULL)

  mod_decomposicao_anova_server("decomposicao", modo, anova_reativa)
  mod_pressupostos_anova_server("pressupostos", modo, anova_reativa)
  mod_comparacoes_anova_server("comparacoes", modo, anova_reativa)
  mod_alem_anova_server("alem_anova", modo)
}

shiny::shinyApp(ui, server)
