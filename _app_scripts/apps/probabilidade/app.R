# =============================================================================
# Script:  app.R (app Distribuições normal e t)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-25
#
# Entrada:
#   - _app_scripts/apps/comum/utilitarios.R
#   - _app_scripts/apps/probabilidade/modulos/*.R
# Saída:
#   - aplicativo Shiny (publicado no Space do Hugging Face em /probabilidade/)
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
  titulo = tr("Distribuições normal e t", "Normal and t distributions"),
  descricao = tr(
    paste(
      "Demonstrações interativas de distribuições de probabilidade: como calcular áreas,",
      "valores críticos e comparar distribuições."
    ),
    paste(
      "Interactive demos of probability distributions: how to compute areas,",
      "critical values and compare distributions."
    )
  ),
  bslib::nav_panel(tr("Distribuição Normal (Z e t)", "Normal distribution (Z and t)"), mod_normal_ui("normal")),
  bslib::nav_menu(
    tr("Outras distribuições", "Other distributions"),
    item_em_breve("Binomial"),
    item_em_breve("Poisson")
  )
)

server <- function(input, output, session) {
  modo <- modo_app(input)

  mod_normal_server("normal", modo)
}

shiny::shinyApp(ui, server)
