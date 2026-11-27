# =============================================================================
# Script:  app.R (tópico Estimação e intervalos de confiança)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - _app_scripts/apps/comum/utilitarios.R
#   - _app_scripts/apps/estimacao-ic/modulos/*.R
# Saída:
#   - aplicativo Shiny (publicado no Space do Hugging Face em /estimacao-ic/
#     e, em inglês, em /en/estimation-ci/)
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
  titulo = tr("Estimação e intervalos de confiança", "Estimation and confidence intervals"),
  versao = tr("0.1 (novembro de 2026)", "0.1 (November 2026)"),
  descricao = tr(
    paste(
      "Como avaliar estimadores e construir intervalos de confiança: viés, variância, erro quadrático médio (EQM)",
      "e eficiência; cobertura frequentista de 100 intervalos simultâneos para a média (Z versus t); e",
      "comparação entre o intervalo de Wald e o de Wilson para proporções. O menu \"Para ir além\" aproxima a",
      "máxima verossimilhança (MLE) do intervalo credível bayesiano Beta-Binomial. O conteúdo segue Morettin e",
      "Bussab (Estatística Básica, cap. 11) e Agresti e Finlay (Statistical Methods for the Social Sciences,",
      "cap. 5), com Casella e Berger (Statistical Inference, caps. 7 e 9) e Gelman et al. (Bayesian Data",
      "Analysis, cap. 2) no aprofundamento."
    ),
    paste(
      "How to evaluate estimators and construct confidence intervals: bias, variance, mean squared error (MSE)",
      "and efficiency; frequentist coverage of 100 simultaneous intervals for the mean (Z versus t); and",
      "comparing Wald versus Wilson intervals for proportions. The \"Going further\" menu connects maximum",
      "likelihood (MLE) to the Bayesian Beta-Binomial credible interval. Content follows Agresti and Finlay",
      "(Statistical Methods for the Social Sciences, ch. 5) and Morettin and Bussab (Estatística Básica, ch. 11),",
      "with Casella and Berger (Statistical Inference, chs. 7 and 9) and Gelman et al. (Bayesian Data Analysis,",
      "ch. 2) in the advanced tab."
    )
  ),
  referencias = list(
    referencia_livro("Agresti, A., & Finlay, B.", 2009, "Statistical methods for the social sciences", "Pearson Prentice Hall", edicao = 4),
    referencia_livro("Casella, G., & Berger, R. L.", 2002, "Statistical inference", "Duxbury", edicao = 2),
    referencia_livro("Gelman, A., Carlin, J. B., Stern, H. S., Dunson, D. B., Vehtari, A., & Rubin, D. B.", 2013, "Bayesian data analysis", "Chapman & Hall/CRC", edicao = 3),
    referencia_livro("Morettin, P. A., & Bussab, W. O.", 2010, "Estatística básica", "Saraiva", edicao = 6)
  ),
  bslib::nav_panel(
    tr("Propriedades", "Properties"),
    value = "propriedades",
    shiny::tags$style(".tabela-app th { text-transform: none; }"),
    mod_propriedades_ui("propriedades")
  ),
  bslib::nav_panel(tr("IC para média", "CI for mean"), value = "ic_media", mod_ic_media_ui("ic_media")),
  bslib::nav_panel(tr("IC para proporção", "CI for proportion"), value = "ic_prop", mod_ic_prop_ui("ic_prop")),
  bslib::nav_menu(
    tr("Para ir além", "Going further"),
    bslib::nav_panel(
      tr("MLE e intervalo credível bayesiano", "MLE and Bayesian credible interval"),
      value = "alem_estimacao",
      mod_alem_estimacao_ui("alem_estimacao")
    )
  )
)

server <- function(input, output, session) {
  modo <- modo_app(input)
  mod_propriedades_server("propriedades", modo)
  mod_ic_media_server("ic_media", modo)
  mod_ic_prop_server("ic_prop", modo)
  mod_alem_estimacao_server("alem_estimacao", modo)
}

shiny::shinyApp(ui, server)
