# =============================================================================
# Script:  app.R (tópico Comparação de duas populações)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - _app_scripts/apps/comum/utilitarios.R
#   - _app_scripts/apps/duas-populacoes/modulos/*.R
# Saída:
#   - aplicativo Shiny (publicado no Space do Hugging Face em /duas-populacoes/
#     e, em inglês, em /en/two-populations/)
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
  titulo = tr("Comparação de duas populações", "Comparing two populations"),
  versao = tr("0.1 (novembro de 2026)", "0.1 (November 2026)"),
  descricao = tr(
    paste(
      "Como comparar dois grupos na inferência estatística: amostras independentes (teste F para variâncias, t",
      "com variâncias iguais, t de Welch e Wilcoxon/Mann-Whitney), amostras pareadas (ganho de precisão ao",
      "eliminar a variação entre unidades) e comparação de duas proporções. O menu \"Para ir além\" apresenta",
      "o teste de permutação aleatória e compara seu poder ao de t e Wilcoxon sob assimetria. O conteúdo segue",
      "Morettin e Bussab (Estatística Básica, cap. 13) e Agresti e Finlay (Statistical Methods for the Social",
      "Sciences, cap. 7), com Casella e Berger (Statistical Inference, cap. 8) no aprofundamento."
    ),
    paste(
      "How to compare two groups in statistical inference: independent samples (F test for variances, pooled t,",
      "Welch's t and Wilcoxon/Mann-Whitney), paired samples (precision gain from removing between-unit variation)",
      "and two proportions. The \"Going further\" menu introduces the random permutation test and compares its",
      "power to t and Wilcoxon under skewness. Content follows Agresti and Finlay (Statistical Methods for the",
      "Social Sciences, ch. 7) and Morettin and Bussab (Estatística Básica, ch. 13), with Casella and Berger",
      "(Statistical Inference, ch. 8) in the advanced tab."
    )
  ),
  referencias = list(
    referencia_livro("Agresti, A., & Finlay, B.", 2009, "Statistical methods for the social sciences", "Pearson Prentice Hall", edicao = 4),
    referencia_livro("Casella, G., & Berger, R. L.", 2002, "Statistical inference", "Duxbury", edicao = 2),
    referencia_livro("Morettin, P. A., & Bussab, W. O.", 2010, "Estatística básica", "Saraiva", edicao = 6)
  ),
  bslib::nav_panel(
    tr("Independentes", "Independent"),
    value = "indep",
    shiny::tags$style(".tabela-app th { text-transform: none; }"),
    mod_independentes_ui("indep")
  ),
  bslib::nav_panel(tr("Pareadas", "Paired"), value = "pareadas", mod_pareadas_ui("pareadas")),
  bslib::nav_panel(tr("Duas proporções", "Two proportions"), value = "prop2", mod_proporcoes_2_ui("prop2")),
  bslib::nav_menu(
    tr("Para ir além", "Going further"),
    bslib::nav_panel(
      tr("Permutação e robustez", "Permutation and robustness"),
      value = "alem_duas_pop",
      mod_alem_duas_pop_ui("alem_duas_pop")
    )
  )
)

server <- function(input, output, session) {
  modo <- modo_app(input)
  mod_independentes_server("indep", modo)
  mod_pareadas_server("pareadas", modo)
  mod_proporcoes_2_server("prop2", modo)
  mod_alem_duas_pop_server("alem_duas_pop", modo)
}

shiny::shinyApp(ui, server)
