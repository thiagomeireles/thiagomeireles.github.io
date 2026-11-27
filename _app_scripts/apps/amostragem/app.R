# =============================================================================
# Script:  app.R (tópico Amostragem)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - _app_scripts/apps/comum/utilitarios.R
#   - _app_scripts/apps/amostragem/modulos/*.R
# Saída:
#   - aplicativo Shiny (publicado no Space do Hugging Face em /amostragem/
#     e, em inglês, em /en/sampling/)
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
  titulo = tr("Amostragem", "Sampling"),
  versao = tr("0.1 (novembro de 2026)", "0.1 (November 2026)"),
  descricao = tr(
    paste(
      "Como selecionar uma amostra e dimensionar o tamanho n: comparação entre amostragem aleatória simples",
      "com e sem reposição (fator de correção para população finita), comparação entre AAS, amostragem",
      "estratificada e por conglomerados, e cálculo do tamanho amostral para média e proporção. O menu",
      "\"Para ir além\" ilustra o impacto de mecanismos de coleta não ignoráveis. O conteúdo segue Morettin e",
      "Bussab (Estatística Básica, cap. 10) e Agresti e Finlay (Statistical Methods for the Social Sciences,",
      "cap. 2), com Casella e Berger (Statistical Inference, cap. 5) e Gelman et al. (Bayesian Data Analysis,",
      "cap. 8) no aprofundamento."
    ),
    paste(
      "How to select a sample and size n: comparing simple random sampling with and without replacement",
      "(finite population correction factor), comparing SRS, stratified and cluster sampling, and computing",
      "sample size for means and proportions. The \"Going further\" menu illustrates nonignorable data",
      "collection mechanisms. Content follows Agresti and Finlay (Statistical Methods for the Social Sciences,",
      "ch. 2) and Morettin and Bussab (Estatística Básica, ch. 10), with Casella and Berger (Statistical",
      "Inference, ch. 5) and Gelman et al. (Bayesian Data Analysis, ch. 8) in the advanced tab."
    )
  ),
  referencias = list(
    referencia_livro("Agresti, A., & Finlay, B.", 2009, "Statistical methods for the social sciences", "Pearson Prentice Hall", edicao = 4),
    referencia_livro("Casella, G., & Berger, R. L.", 2002, "Statistical inference", "Duxbury", edicao = 2),
    referencia_livro("Gelman, A., Carlin, J. B., Stern, H. S., Dunson, D. B., Vehtari, A., & Rubin, D. B.", 2013, "Bayesian data analysis", "Chapman & Hall/CRC", edicao = 3),
    referencia_livro("Morettin, P. A., & Bussab, W. O.", 2010, "Estatística básica", "Saraiva", edicao = 6)
  ),
  bslib::nav_panel(
    tr("AAS e reposição", "SRS and replacement"),
    value = "aas_fpc",
    shiny::tags$style(".tabela-app th { text-transform: none; }"),
    mod_aas_fpc_ui("aas_fpc")
  ),
  bslib::nav_panel(tr("Desenhos amostrais", "Sampling designs"), value = "desenhos", mod_desenhos_ui("desenhos")),
  bslib::nav_panel(tr("Tamanho de amostra", "Sample size"), value = "tamanho_n", mod_tamanho_n_ui("tamanho_n")),
  bslib::nav_menu(
    tr("Para ir além", "Going further"),
    bslib::nav_panel(
      tr("Coleta não ignorável", "Nonignorable collection"),
      value = "alem_amostragem",
      mod_alem_amostragem_ui("alem_amostragem")
    )
  )
)

server <- function(input, output, session) {
  modo <- modo_app(input)
  mod_aas_fpc_server("aas_fpc", modo)
  mod_desenhos_server("desenhos", modo)
  mod_tamanho_n_server("tamanho_n", modo)
  mod_alem_amostragem_server("alem_amostragem", modo)
}

shiny::shinyApp(ui, server)
