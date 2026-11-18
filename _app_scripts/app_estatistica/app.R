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

# Tópico planejado: aparece no menu do tema, desabilitado, até o módulo existir
item_em_breve <- function(rotulo) {
  bslib::nav_item(
    shiny::tags$span(
      class = "dropdown-item disabled",
      rotulo, shiny::tags$small(class = "text-muted ms-1", "(em breve)")
    )
  )
}

# Cada grande tema vira um menu suspenso no topo; os tópicos ficam dentro dele.
# Para incluir um módulo novo, troque o item_em_breve() pelo nav_panel().
ui <- bslib::page_navbar(
  title = "Estatística na prática",
  theme = tema_app(),
  header = shiny::tagList(dependencia_tema(), estilo_app()),
  bslib::nav_menu(
    "Probabilidade",
    bslib::nav_panel("Distribuição Normal (Z e t)", mod_normal_ui("normal"))
  ),
  bslib::nav_menu(
    "Teoremas limite",
    bslib::nav_panel("Lei dos Grandes Números", mod_lgn_ui("lgn")),
    bslib::nav_panel("Teorema Central do Limite", mod_tcl_ui("tcl"))
  ),
  bslib::nav_menu(
    "Inferência",
    item_em_breve("Intervalos de confiança"),
    item_em_breve("Testes de hipótese")
  ),
  bslib::nav_menu(
    "Amostragem e metodologia",
    item_em_breve("Desenhos amostrais"),
    item_em_breve("Tamanho de amostra")
  ),
  bslib::nav_spacer(),
  bslib::nav_panel("Sobre", pagina_sobre()),
  bslib::nav_item(bslib::input_dark_mode(id = "modo"))
)

server <- function(input, output, session) {
  # O tema global fica aqui, e não no topo do arquivo, para o ggplot2 carregar
  # depois que a interface já apareceu na tela
  ggplot2::theme_set(tema_grafico())

  # Modo claro/escuro escolhido no botão do topo; os gráficos acompanham
  modo <- shiny::reactive(if (identical(input$modo, "dark")) "dark" else "light")

  mod_normal_server("normal", modo)
  mod_tcl_server("tcl", modo)
  mod_lgn_server("lgn", modo)
}

shiny::shinyApp(ui, server)
