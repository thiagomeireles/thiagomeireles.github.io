# =============================================================================
# Script:  app.R (tema Estatística descritiva: tipos de variáveis)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-18
#
# Entrada:
#   - _app_scripts/apps/comum/utilitarios.R
#   - _app_scripts/apps/descritiva/modulos/*.R
# Saída:
#   - aplicativo Shiny (publicado via Shinylive em apps/descritiva/)
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
  titulo = tr("Estatística descritiva", "Descriptive statistics"),
  descricao = tr(
    paste(
      "Demonstrações interativas sobre os tipos de variáveis (categóricas nominais e ordinais,",
      "quantitativas discretas e contínuas) e as estatísticas descritivas que fazem sentido para cada uma."
    ),
    paste(
      "Interactive demos on the types of variables (nominal and ordinal categorical, discrete and",
      "continuous quantitative) and the descriptive statistics that make sense for each one."
    )
  ),
  bslib::nav_panel(tr("Tipos de variáveis", "Types of variables"), value = "tipos", mod_tipos_ui("tipos")),
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
