# =============================================================================
# Script:  app.R (tema Análise bidimensional)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-19
#
# Entrada:
#   - _app_scripts/apps/comum/utilitarios.R
#   - _app_scripts/apps/bidimensional/modulos/*.R
# Saída:
#   - aplicativo Shiny (publicado no Space do Hugging Face em /bidimensional/ e,
#     em inglês, em /en/bivariate/)
# =============================================================================

# Este arquivo roda no Shiny Server, montado por _app_scripts/hf-space/montar_apps.R. Não
# limpa a sessão nem usa here/pacman: os caminhos são relativos à pasta do app.
options(scipen = 999)

pasta_comum <- if (dir.exists("comum")) "comum" else file.path("..", "comum")
c(
  list.files(pasta_comum, pattern = "\\.R$", full.names = TRUE),
  list.files("modulos", pattern = "\\.R$", full.names = TRUE)
) |>
  purrr::walk(source)

ui <- pagina_tema(
  titulo = tr("Análise bidimensional", "Bivariate analysis"),
  versao = tr("0.1 (novembro de 2026)", "0.1 (November 2026)"),
  descricao = tr(
    paste(
      "Como descrever a relação entre duas variáveis conforme o par de tipos: tabela de dupla entrada, qui-quadrado",
      "e coeficientes de contingência para duas qualitativas; gráfico de dispersão, covariância, correlação e reta",
      "de regressão para duas quantitativas; box plots, R² e gráfico q × q para uma de cada. Os conceitos seguem",
      "Morettin e Bussab (Estatística Básica, Cap. 4) e Agresti e Finlay (Statistical Methods for the Social Sciences, Cap. 3)."
    ),
    paste(
      "How to describe the relationship between two variables depending on the pair of types: contingency table,",
      "chi-squared and contingency coefficients for two categorical variables; scatterplot, covariance, correlation",
      "and regression line for two quantitative ones; box plots, R² and Q-Q plot for one of each. The concepts follow",
      "Agresti and Finlay (Statistical Methods for the Social Sciences, ch. 3) and Morettin and Bussab (Estatística Básica, ch. 4)."
    )
  ),
  bslib::nav_panel(tr("Como escolher", "How to choose"), value = "escolher", mod_escolher_ui("escolher")),
  bslib::nav_panel(pares_tipo$quali_quali$aba, value = "quali_quali", mod_quali_quali_ui("quali_quali")),
  bslib::nav_panel(pares_tipo$quanti_quanti$aba, value = "quanti_quanti", mod_quanti_quanti_ui("quanti_quanti")),
  bslib::nav_panel(pares_tipo$quali_quanti$aba, value = "quali_quanti", mod_quali_quanti_ui("quali_quanti")),
  bslib::nav_panel(tr("Gráfico q × q", "Q-Q plot"), value = "qq", mod_qq_ui("qq"))
)

server <- function(input, output, session) {
  modo <- modo_app(input)

  mod_escolher_server("escolher", ir_para = \(par) bslib::nav_select("abas", par, session = session))
  mod_quali_quali_server("quali_quali", modo)
  mod_quanti_quanti_server("quanti_quanti", modo)
  mod_quali_quanti_server("quali_quanti", modo)
  mod_qq_server("qq", modo)
}

shiny::shinyApp(ui, server)
