# =============================================================================
# Script:  app.R (tema Teoremas limite: LGN e TCL)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-18
#
# Entrada:
#   - _app_scripts/apps/comum/utilitarios.R
#   - _app_scripts/apps/teoremas-limite/modulos/*.R
# Saída:
#   - aplicativo Shiny (publicado via Shinylive em apps/teoremas-limite/)
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
  titulo = tr("Teoremas limite", "Limit theorems"),
  descricao = tr(
    paste(
      "Demonstrações interativas dos dois resultados que sustentam boa parte da inferência estatística:",
      "a Lei dos Grandes Números e o Teorema Central do Limite."
    ),
    paste(
      "Interactive demos of the two results behind much of statistical inference:",
      "the Law of Large Numbers and the Central Limit Theorem."
    )
  ),
  bslib::nav_panel(tr("Lei dos Grandes Números", "Law of Large Numbers"), mod_lgn_ui("lgn")),
  bslib::nav_panel(tr("Teorema Central do Limite", "Central Limit Theorem"), mod_tcl_ui("tcl"))
)

server <- function(input, output, session) {
  modo <- modo_app(input)

  mod_tcl_server("tcl", modo)
  mod_lgn_server("lgn", modo)
}

shiny::shinyApp(ui, server)
