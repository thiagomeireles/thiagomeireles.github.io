# =============================================================================
# Script:  app.R (tópico Duas variáveis aleatórias)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - _app_scripts/apps/comum/utilitarios.R
#   - _app_scripts/apps/duas-va/modulos/*.R
# Saída:
#   - aplicativo Shiny (publicado no Space do Hugging Face em /duas-va/
#     e, em inglês, em /en/two-random-variables/)
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
  titulo = tr("Duas variáveis aleatórias", "Two random variables"),
  versao = tr("0.1 (novembro de 2026)", "0.1 (November 2026)"),
  descricao = tr(
    paste(
      "Como analisar duas variáveis aleatórias em conjunto: distribuição conjunta p(x, y), marginais, teste de",
      "independência, distribuições condicionais P(X | Y = y), distribuição da soma Z = X + Y, covariância,",
      "correlação e variância de combinações lineares aX + bY. O menu \"Para ir além\" verifica numericamente",
      "as leis da esperança e variância totais (incluindo o contraexemplo Y = X² com covariância nula) e simula",
      "a normal bidimensional. O conteúdo segue Morettin e Bussab (Estatística Básica, cap. 8), com Casella e",
      "Berger (Statistical Inference, cap. 4), Gelman et al. (Bayesian Data Analysis, seção 1.8) e Wooldridge",
      "(Introductory Econometrics, apêndice B) no aprofundamento."
    ),
    paste(
      "How to analyze two random variables together: joint distribution p(x, y), marginals, independence check,",
      "conditional distributions P(X | Y = y), distribution of the sum Z = X + Y, covariance, correlation and",
      "variance of linear combinations aX + bY. The \"Going further\" menu numerically verifies the laws of total",
      "expectation and variance (including the Y = X² counterexample with zero covariance) and simulates the",
      "bivariate normal. Content follows Morettin and Bussab (Estatística Básica, ch. 8), with Casella and",
      "Berger (Statistical Inference, ch. 4), Gelman et al. (Bayesian Data Analysis, section 1.8) and Wooldridge",
      "(Introductory Econometrics, appendix B) in the advanced tab."
    )
  ),
  referencias = list(
    referencia_livro("Casella, G., & Berger, R. L.", 2002, "Statistical inference", "Duxbury", edicao = 2),
    referencia_livro("Gelman, A., Carlin, J. B., Stern, H. S., Dunson, D. B., Vehtari, A., & Rubin, D. B.", 2013, "Bayesian data analysis", "Chapman & Hall/CRC", edicao = 3),
    referencia_livro("Morettin, P. A., & Bussab, W. O.", 2010, "Estatística básica", "Saraiva", edicao = 6),
    referencia_livro("Wooldridge, J. M.", 2013, "Introductory econometrics: A modern approach", "Cengage Learning", edicao = 5)
  ),
  bslib::nav_panel(
    tr("Conjunta e marginais", "Joint and marginals"),
    value = "conjunta",
    shiny::tags$style(".tabela-app th { text-transform: none; }"),
    mod_conjunta_ui("conjunta")
  ),
  bslib::nav_panel(tr("Condicionais e soma", "Conditionals and sum"), value = "condicionais", mod_condicionais_soma_ui("condicionais")),
  bslib::nav_panel(tr("Covariância e aX + bY", "Covariance and aX + bY"), value = "cov_comb", mod_cov_comb_ui("cov_comb")),
  bslib::nav_menu(
    tr("Para ir além", "Going further"),
    bslib::nav_panel(
      tr("Leis totais e normal bidimensional", "Total laws and bivariate normal"),
      value = "alem_duas_va",
      mod_alem_duas_va_ui("alem_duas_va")
    )
  )
)

server <- function(input, output, session) {
  modo <- modo_app(input)
  conjunta_reativa <- shiny::reactiveVal(NULL)

  mod_conjunta_server("conjunta", modo, conjunta_reativa)
  mod_condicionais_soma_server("condicionais", modo, conjunta_reativa)
  mod_cov_comb_server("cov_comb", modo, conjunta_reativa)
  mod_alem_duas_va_server("alem_duas_va", modo, conjunta_reativa)
}

shiny::shinyApp(ui, server)
