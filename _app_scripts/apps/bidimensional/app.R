# =============================================================================
# Script:  app.R (tópico Análise bidimensional)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
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

# estatisticas.R primeiro: os outros módulos usam os pares e exemplos na definição da interface
modulos <- list.files("modulos", pattern = "\\.R$", full.names = TRUE)
modulos <- c(modulos[basename(modulos) == "estatisticas.R"], modulos[basename(modulos) != "estatisticas.R"])

c(list.files(pasta_comum, pattern = "\\.R$", full.names = TRUE), modulos) |>
  purrr::walk(source)

ui <- pagina_tema(
  titulo = tr("Análise bidimensional", "Bivariate analysis"),
  versao = tr("0.3 (novembro de 2026)", "0.3 (November 2026)"),
  descricao = tr(
    paste(
      "Como descrever a relação entre duas variáveis conforme o par de tipos: tabela de dupla entrada, qui-quadrado",
      "e coeficientes de contingência para duas qualitativas; gráfico de dispersão, covariância, correlação e reta",
      "de regressão para duas quantitativas; box plots, R² e gráfico q × q para uma de cada. A aba final avança",
      "para a função de esperança condicional E(Y | X), a lei da variância total, a reta como melhor aproximação",
      "linear e a diferença entre covariância nula e independência. O conteúdo segue Morettin e Bussab (Estatística",
      "Básica, cap. 4) e Agresti e Finlay (Statistical Methods for the Social Sciences, cap. 3), com Casella e",
      "Berger (Statistical Inference, cap. 4), Wooldridge (Introductory Econometrics, apêndice B) e Angrist e",
      "Pischke (Mostly Harmless Econometrics, seção 3.1) no aprofundamento."
    ),
    paste(
      "How to describe the relationship between two variables depending on the pair of types: contingency table,",
      "chi-squared and contingency coefficients for two categorical variables; scatterplot, covariance, correlation",
      "and regression line for two quantitative ones; box plots, R² and Q-Q plot for one of each. The final tab",
      "moves on to the conditional expectation function E(Y | X), the law of total variance, the regression line",
      "as the best linear approximation and the distinction between zero covariance and independence. The content",
      "follows Morettin and Bussab (Estatística Básica, ch. 4) and Agresti and Finlay (Statistical Methods for",
      "the Social Sciences, ch. 3), with Casella and Berger (Statistical Inference, ch. 4), Wooldridge",
      "(Introductory Econometrics, appendix B) and Angrist and Pischke (Mostly Harmless Econometrics, section",
      "3.1) in the advanced tab."
    )
  ),
  referencias = list(
    referencia_livro("Agresti, A., & Finlay, B.", 2009, "Statistical methods for the social sciences", "Pearson Prentice Hall", edicao = 4),
    referencia_livro("Angrist, J. D., & Pischke, J.-S.", 2009, "Mostly harmless econometrics: An empiricist's companion", "Princeton University Press"),
    referencia_livro("Casella, G., & Berger, R. L.", 2002, "Statistical inference", "Duxbury", edicao = 2),
    referencia_livro("Gelman, A., Carlin, J. B., Stern, H. S., Dunson, D. B., Vehtari, A., & Rubin, D. B.", 2013, "Bayesian data analysis", "CRC Press", edicao = 3),
    referencia_livro("Morettin, P. A., & Bussab, W. O.", 2010, "Estatística básica", "Saraiva", edicao = 6),
    referencia_livro("Wooldridge, J. M.", 2013, "Introductory econometrics: A modern approach", "Cengage Learning", edicao = 5)
  ),
  bslib::nav_panel(
    tr("Como escolher", "How to choose"), value = "escolher",
    shiny::tags$style(".tabela-app th { text-transform: none; }"),
    mod_escolher_ui("escolher")
  ),
  bslib::nav_panel(pares_tipo$quali_quali$aba, value = "quali_quali", mod_quali_quali_ui("quali_quali")),
  bslib::nav_panel(pares_tipo$quanti_quanti$aba, value = "quanti_quanti", mod_quanti_quanti_ui("quanti_quanti")),
  bslib::nav_panel(pares_tipo$quali_quanti$aba, value = "quali_quanti", mod_quali_quanti_ui("quali_quanti")),
  bslib::nav_panel(tr("Gráfico q × q", "Q-Q plot"), value = "qq", mod_qq_ui("qq")),
  bslib::nav_menu(
    tr("Para ir além", "Going further"),
    bslib::nav_panel(
      tr("Esperança condicional, variância total e independência", "Conditional expectation, total variance and independence"),
      value = "alem",
      mod_alem_ui("alem")
    )
  )
)

server <- function(input, output, session) {
  modo <- modo_app(input)

  mod_escolher_server("escolher", ir_para = \(par) bslib::nav_select("abas", par, session = session))
  mod_quali_quali_server("quali_quali", modo)
  mod_quanti_quanti_server("quanti_quanti", modo)
  mod_quali_quanti_server("quali_quanti", modo)
  mod_qq_server("qq", modo)
  mod_alem_server("alem", modo)
}

shiny::shinyApp(ui, server)
