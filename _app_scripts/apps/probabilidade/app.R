# =============================================================================
# Script:  app.R (tema Probabilidade: distribuições)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-18
#
# Entrada:
#   - _app_scripts/apps/comum/utilitarios.R
#   - _app_scripts/apps/probabilidade/modulos/*.R
# Saída:
#   - aplicativo Shiny (publicado via Shinylive em apps/probabilidade/)
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
  titulo = "Probabilidade",
  descricao = paste(
    "Demonstrações interativas de distribuições de probabilidade: como calcular áreas,",
    "valores críticos e comparar distribuições."
  ),
  bslib::nav_panel("Distribuição Normal (Z e t)", mod_normal_ui("normal")),
  bslib::nav_menu(
    "Outras distribuições",
    item_em_breve("Binomial"),
    item_em_breve("Poisson")
  )
)

server <- function(input, output, session) {
  modo <- modo_app(input)

  mod_normal_server("normal", modo)
}

shiny::shinyApp(ui, server)
