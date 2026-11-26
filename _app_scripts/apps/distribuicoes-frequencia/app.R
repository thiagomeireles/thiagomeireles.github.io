# =============================================================================
# Script:  app.R (tópico Distribuições de frequência e gráficos)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-26
#
# Entrada:
#   - _app_scripts/apps/comum/utilitarios.R
#   - _app_scripts/apps/distribuicoes-frequencia/modulos/*.R
# Saída:
#   - aplicativo Shiny (publicado no Space do Hugging Face em
#     /distribuicoes-frequencia/ e, em inglês, em /en/frequency-distributions/)
# =============================================================================

# Este arquivo roda no Shiny Server, montado por _app_scripts/hf-space/montar_apps.R. Não
# limpa a sessão nem usa here/pacman: os caminhos são relativos à pasta do app.
options(scipen = 999)

pasta_comum <- if (dir.exists("comum")) "comum" else file.path("..", "comum")

# dados.R primeiro: os outros módulos usam os exemplos na definição da interface
modulos <- list.files("modulos", pattern = "\\.R$", full.names = TRUE)
modulos <- c(modulos[basename(modulos) == "dados.R"], modulos[basename(modulos) != "dados.R"])

c(list.files(pasta_comum, pattern = "\\.R$", full.names = TRUE), modulos) |>
  purrr::walk(source)

ui <- pagina_tema(
  titulo = tr("Distribuições de frequência e gráficos", "Frequency distributions and graphs"),
  versao = tr("0.1 (novembro de 2026)", "0.1 (November 2026)"),
  descricao = tr(
    paste(
      "Como organizar os dados numa tabela de frequências e representá-los no gráfico de barras, no histograma com",
      "densidade e no ramo-e-folhas, com os salários da Companhia MB de Morettin e Bussab e dados simulados.",
      "As duas últimas abas tratam do histograma como estimador da densidade da população, com a escolha da",
      "largura das classes pelo MISE e a passagem para o estimador de kernel, e da amostra ordenada, com as",
      "estatísticas de ordem e a função de distribuição empírica. O conteúdo segue Morettin e Bussab",
      "(Estatística Básica, cap. 2) e Agresti e Finlay (Statistical Methods for the Social Sciences, cap. 3), com",
      "Yuksel e Aydede (Causal Inference and Machine Learning, cap. 8) na estimação não paramétrica e Casella e",
      "Berger (Statistical Inference, seção 5.4) nas estatísticas de ordem."
    ),
    paste(
      "How to organize data in a frequency table and represent them in a bar graph, a density histogram and a",
      "stem-and-leaf plot, using the MB Company salaries from Morettin and Bussab and simulated data. The last two",
      "tabs deal with the histogram as an estimator of the population density, with the bin width chosen by MISE",
      "and the step to the kernel estimator, and with the sorted sample, with order statistics and the empirical",
      "distribution function. The content follows Morettin and Bussab (Estatística Básica, ch. 2) and Agresti and",
      "Finlay (Statistical Methods for the Social Sciences, ch. 3), with Yuksel and Aydede (Causal Inference and",
      "Machine Learning, ch. 8) for nonparametric estimation and Casella and Berger (Statistical Inference,",
      "section 5.4) for order statistics."
    )
  ),
  referencias = list(
    referencia_livro("Agresti, A., & Finlay, B.", 2009, "Statistical methods for the social sciences", "Pearson Prentice Hall", edicao = 4),
    referencia_livro("Casella, G., & Berger, R. L.", 2002, "Statistical inference", "Duxbury", edicao = 2),
    referencia_livro("Morettin, P. A., & Bussab, W. O.", 2010, "Estatística básica", "Saraiva", edicao = 6),
    referencia_livro("Yuksel, M., & Aydede, Y.", tr("s.d.", "n.d."), "Causal inference and machine learning: In economics, social, and health sciences", "https://www.causalmlbook.com")
  ),
  # O estilo vale para o app todo: cabeçalhos das tabelas sem caixa alta,
  # porque nᵢ e Nᵢ (ou fᵢ e Fᵢ) são coisas diferentes
  bslib::nav_panel(
    tr("Tabela e barras", "Table and bars"), value = "barras",
    shiny::tags$style(".tabela-app th { text-transform: none; }"),
    mod_barras_ui("barras")
  ),
  bslib::nav_panel(tr("Histograma", "Histogram"), value = "histograma", mod_histograma_ui("histograma")),
  bslib::nav_panel(tr("Ramo-e-folhas", "Stem-and-leaf"), value = "ramo", mod_ramo_folhas_ui("ramo")),
  bslib::nav_menu(
    tr("Para ir além", "Going further"),
    bslib::nav_panel(tr("Histograma como estimador", "The histogram as an estimator"), value = "estimador", mod_estimador_ui("estimador")),
    bslib::nav_panel(tr("Estatísticas de ordem e distribuição empírica", "Order statistics and the empirical CDF"), value = "ordem", mod_ordem_ui("ordem"))
  )
)

server <- function(input, output, session) {
  modo <- modo_app(input)

  mod_barras_server("barras", modo)
  mod_histograma_server("histograma", modo)
  mod_ramo_folhas_server("ramo", modo)
  mod_estimador_server("estimador", modo)
  mod_ordem_server("ordem", modo)
}

shiny::shinyApp(ui, server)
