# =============================================================================
# Script:  app.R (Estatística na prática: app didático em Shiny)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-18
#
# Entrada:
#   - _app_scripts/app_estatistica/modulos/*.R
# Saída:
#   - aplicativo Shiny (publicado via Shinylive em apps/estatistica/)
# =============================================================================

# Este arquivo roda dentro do navegador (webR) depois de exportado. Por isso não
# limpa a sessão nem usa here/pacman: os caminhos são relativos à pasta do app.
options(scipen = 999)

list.files("modulos", pattern = "\\.R$", full.names = TRUE) |>
  purrr::walk(source)

ggplot2::theme_set(tema_grafico())

pagina_sobre <- function() {
  bslib::card(
    bslib::card_header("Sobre este aplicativo"),
    bslib::card_body(
      shiny::p(
        "Demonstrações interativas de conceitos básicos de estatística e amostragem,",
        "feitas para aulas, estudo individual e consulta rápida."
      ),
      shiny::p(
        "O aplicativo roda inteiro no seu navegador, com R compilado para WebAssembly (webR e Shinylive).",
        "Nenhum dado sai do seu computador. A primeira abertura demora alguns segundos;",
        "as próximas usam o cache do navegador."
      ),
      shiny::p("Autor: Thiago Meireles | Versão 0.1 (novembro de 2026)"),
      shiny::p(shiny::a("www.thiagomeireles.com.br", href = "https://www.thiagomeireles.com.br", target = "_blank"))
    )
  )
}

ui <- bslib::page_navbar(
  title = "Estatística na prática",
  theme = tema_app(),
  bslib::nav_panel("Normal (Z e t)", mod_normal_ui("normal")),
  bslib::nav_panel("Teorema Central do Limite", mod_tcl_ui("tcl")),
  bslib::nav_panel("Lei dos Grandes Números", mod_lgn_ui("lgn")),
  bslib::nav_spacer(),
  bslib::nav_panel("Sobre", pagina_sobre())
)

server <- function(input, output, session) {
  mod_normal_server("normal")
  mod_tcl_server("tcl")
  mod_lgn_server("lgn")
}

shiny::shinyApp(ui, server)
