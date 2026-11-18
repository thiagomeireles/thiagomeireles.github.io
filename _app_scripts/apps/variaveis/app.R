# =============================================================================
# Script:  app.R (tema Tipos de variáveis)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-18
#
# Entrada:
#   - _app_scripts/apps/comum/utilitarios.R
#   - _app_scripts/apps/variaveis/modulos/*.R
# Saída:
#   - aplicativo Shiny (publicado via Shinylive em apps/variaveis/ e,
#     em inglês, em apps/en/variables/)
# =============================================================================

# Este arquivo roda dentro do navegador (webR) depois de exportado. Por isso não
# limpa a sessão nem usa here/pacman: os caminhos são relativos à pasta do app.
options(scipen = 999)

pasta_comum <- if (dir.exists("comum")) "comum" else file.path("..", "comum")
c(
  list.files(pasta_comum, pattern = "\\.R$", full.names = TRUE),
  list.files("modulos", pattern = "\\.R$", full.names = TRUE)
) |>
  purrr::walk(source)

ui <- pagina_tema(
  titulo = tr("Tipos de variáveis", "Types of variables"),
  descricao = tr(
    paste(
      "Como classificar uma variável (qualitativa nominal ou ordinal, quantitativa discreta ou contínua) e",
      "quais estatísticas descritivas fazem sentido para cada tipo, com quiz e dados simulados. Os conceitos",
      "seguem Morettin e Bussab (Estatística Básica) e Agresti e Finlay (Statistical Methods for the Social Sciences)."
    ),
    paste(
      "How to classify a variable (nominal or ordinal categorical, discrete or continuous quantitative) and",
      "which descriptive statistics make sense for each type, with a quiz and simulated data. The concepts",
      "follow Agresti and Finlay (Statistical Methods for the Social Sciences) and Morettin and Bussab (Estatística Básica)."
    )
  ),
  bslib::nav_panel(tr("Como classificar", "How to classify"), value = "tipos", mod_tipos_ui("tipos")),
  bslib::nav_panel("Nominal", value = "nominal", mod_nominal_ui("nominal")),
  bslib::nav_panel("Ordinal", value = "ordinal", mod_ordinal_ui("ordinal")),
  bslib::nav_panel(tr("Discreta", "Discrete"), value = "discreta", mod_discreta_ui("discreta")),
  bslib::nav_panel(tr("Contínua", "Continuous"), value = "continua", mod_continua_ui("continua"))
)

server <- function(input, output, session) {
  modo <- modo_app(input)

  mod_tipos_server("tipos", ir_para = \(tipo) bslib::nav_select("abas", tipo, session = session))
  mod_nominal_server("nominal", modo)
  mod_ordinal_server("ordinal", modo)
  mod_discreta_server("discreta", modo)
  mod_continua_server("continua", modo)
}

shiny::shinyApp(ui, server)
