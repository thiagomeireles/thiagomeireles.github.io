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
  versao = tr("0.2 (novembro de 2026)", "0.2 (November 2026)"),
  descricao = tr(
    paste(
      "Como analisar duas variáveis aleatórias juntas: distribuição conjunta p(x, y), marginais, independência,",
      "distribuições condicionais P(X | Y = y), distribuição da soma Z = X + Y, covariância, correlação e",
      "variância de combinações lineares aX + bY. Os exemplos são os do cap. 8 de Morettin e Bussab (famílias",
      "com três crianças e a tabela de covariância nula do Ex. 8.7), mais o caso Y = X². Todas as abas usam a",
      "conjunta escolhida na primeira. O menu \"Para ir além\" confere a esperança total e a variância total",
      "(Gelman et al., seção 1.8; Casella e Berger, seção 4.4) e simula a normal bidimensional."
    ),
    paste(
      "How to analyze two random variables together: joint distribution p(x, y), marginals, independence,",
      "conditional distributions P(X | Y = y), distribution of the sum Z = X + Y, covariance, correlation and",
      "variance of linear combinations aX + bY. The examples are those from chapter 8 of Morettin and Bussab",
      "(families with three children and the zero covariance table of Ex. 8.7), plus the Y = X² case. Every tab",
      "uses the joint chosen in the first one. The \"Going further\" menu checks total expectation and total",
      "variance (Gelman et al., section 1.8; Casella and Berger, section 4.4) and simulates the bivariate normal."
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
