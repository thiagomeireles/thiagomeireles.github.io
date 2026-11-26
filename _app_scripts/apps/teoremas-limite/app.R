# =============================================================================
# Script:  app.R (tema Teoremas limite: LGN, TCL e modos de convergência)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-25
#
# Entrada:
#   - _app_scripts/apps/comum/utilitarios.R
#   - _app_scripts/apps/teoremas-limite/modulos/*.R
# Saída:
#   - aplicativo Shiny (publicado no Space do Hugging Face em /teoremas-limite/
#     e, em inglês, em /en/teoremas-limite/)
# =============================================================================

# Este arquivo roda no Shiny Server, montado por _app_scripts/hf-space/montar_apps.R. Não
# limpa a sessão nem usa here/pacman: os caminhos são relativos à pasta do app.
options(scipen = 999)

pasta_comum <- if (dir.exists("comum")) "comum" else file.path("..", "comum")
c(
  list.files(pasta_comum, pattern = "\\.R$", full.names = TRUE),
  sort(list.files("modulos", pattern = "\\.R$", full.names = TRUE))
) |>
  purrr::walk(source)

ui <- pagina_tema(
  titulo = tr("Teoremas limite", "Limit theorems"),
  versao = tr("0.3 (novembro de 2026)", "0.3 (November 2026)"),
  descricao = tr(
    paste(
      "Demonstrações interativas dos dois resultados assintóticos que sustentam a inferência estatística: a Lei dos",
      "Grandes Números (para onde a média amostral converge) e o Teorema Central do Limite (a forma normal da sua",
      "flutuação em torno de μ). A aba de aprofundamento distingue convergência quase certa, em probabilidade e em",
      "distribuição e mostra o que acontece quando a esperança ou a variância não existem, como na distribuição de",
      "Cauchy e na t com 2 graus de liberdade. O conteúdo segue Morettin e Bussab (Estatística Básica, cap. 10),",
      "com Casella e Berger (Statistical Inference, seção 5.5) e Wooldridge (Introductory Econometrics, apêndice C)",
      "nas definições formais."
    ),
    paste(
      "Interactive demos of the two asymptotic results behind statistical inference: the Law of Large Numbers",
      "(where the sample mean converges) and the Central Limit Theorem (the normal shape of its fluctuation around",
      "μ). The advanced tab distinguishes almost sure convergence, convergence in probability and convergence in",
      "distribution, and shows what breaks down when the mean or variance fails to exist, as under the Cauchy and",
      "t(2) distributions. The content follows Morettin and Bussab (Estatística Básica, ch. 10), with Casella and",
      "Berger (Statistical Inference, section 5.5) and Wooldridge (Introductory Econometrics, appendix C) for the",
      "formal theory."
    )
  ),
  bslib::nav_panel(
    tr("Lei dos Grandes Números", "Law of Large Numbers"), value = "lgn",
    shiny::tags$style(".tabela-app th { text-transform: none; }"),
    mod_lgn_ui("lgn")
  ),
  bslib::nav_panel(tr("Teorema Central do Limite", "Central Limit Theorem"), value = "tcl", mod_tcl_ui("tcl")),
  bslib::nav_menu(
    tr("Para ir além", "Going further"),
    bslib::nav_panel(
      tr("Modos de convergência e quando a lei falha (Cauchy)", "Modes of convergence and when the laws fail (Cauchy)"),
      value = "alem_limites",
      mod_alem_limites_ui("alem_limites")
    )
  )
)

server <- function(input, output, session) {
  modo <- modo_app(input)

  mod_lgn_server("lgn", modo)
  mod_tcl_server("tcl", modo)
  mod_alem_limites_server("alem_limites", modo)
}

shiny::shinyApp(ui, server)
