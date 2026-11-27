# =============================================================================
# Script:  app.R (tópico Qui-quadrado de aderência e independência)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - _app_scripts/apps/comum/utilitarios.R
#   - _app_scripts/apps/qui-quadrado/modulos/*.R
# Saída:
#   - aplicativo Shiny (publicado no Space do Hugging Face em /qui-quadrado/
#     e, em inglês, em /en/chi-squared/)
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
  titulo = tr("Qui-quadrado de aderência e independência", "Chi-squared goodness of fit and independence"),
  versao = tr("0.1 (novembro de 2026)", "0.1 (November 2026)"),
  descricao = tr(
    paste(
      "Como testar aderência a modelos e associação entre variáveis categóricas: teste qui-quadrado de aderência",
      "(com desconto de graus de liberdade para parâmetros estimados), independência versus homogeneidade em",
      "tabelas de contingência, teste de Kolmogorov-Smirnov e teste t para correlação linear (ρ = 0). O menu",
      "\"Para ir além\" compara o qui-quadrado ao teste exato de Fisher em amostras pequenas. O conteúdo segue",
      "Morettin e Bussab (Estatística Básica, cap. 14) e Agresti e Finlay (Statistical Methods for the Social",
      "Sciences, cap. 8), com Casella e Berger (Statistical Inference, cap. 8) no aprofundamento."
    ),
    paste(
      "How to test goodness of fit and association between categorical variables: chi-squared goodness-of-fit",
      "test (adjusting degrees of freedom for estimated parameters), independence versus homogeneity in",
      "contingency tables, Kolmogorov-Smirnov test and t test for linear correlation (ρ = 0). The \"Going",
      "further\" menu compares chi-squared against Fisher's exact test in small samples. Content follows Agresti",
      "and Finlay (Statistical Methods for the Social Sciences, ch. 8) and Morettin and Bussab (Estatística",
      "Básica, ch. 14), with Casella and Berger (Statistical Inference, ch. 8) in the advanced tab."
    )
  ),
  referencias = list(
    referencia_livro("Agresti, A., & Finlay, B.", 2009, "Statistical methods for the social sciences", "Pearson Prentice Hall", edicao = 4),
    referencia_livro("Casella, G., & Berger, R. L.", 2002, "Statistical inference", "Duxbury", edicao = 2),
    referencia_livro("Morettin, P. A., & Bussab, W. O.", 2010, "Estatística básica", "Saraiva", edicao = 6)
  ),
  bslib::nav_panel(
    tr("Aderência", "Goodness of fit"),
    value = "aderencia",
    shiny::tags$style(".tabela-app th { text-transform: none; }"),
    mod_aderencia_ui("aderencia")
  ),
  bslib::nav_panel(tr("Independência e homogeneidade", "Independence and homogeneity"), value = "contingencia", mod_contingencia_ui("contingencia")),
  bslib::nav_panel(tr("K-S e correlação", "K-S and correlation"), value = "ks_corr", mod_ks_corr_ui("ks_corr")),
  bslib::nav_menu(
    tr("Para ir além", "Going further"),
    bslib::nav_panel(
      tr("Amostras pequenas (Fisher)", "Small samples (Fisher)"),
      value = "alem_qui2",
      mod_alem_qui2_ui("alem_qui2")
    )
  )
)

server <- function(input, output, session) {
  modo <- modo_app(input)
  mod_aderencia_server("aderencia", modo)
  mod_contingencia_server("contingencia", modo)
  mod_ks_corr_server("ks_corr", modo)
  mod_alem_qui2_server("alem_qui2", modo)
}

shiny::shinyApp(ui, server)
