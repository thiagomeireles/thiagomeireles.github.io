# =============================================================================
# Script:  app.R (tópico Medidas de posição e dispersão)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - _app_scripts/apps/comum/utilitarios.R
#   - _app_scripts/apps/posicao-dispersao/modulos/*.R
# Saída:
#   - aplicativo Shiny (publicado no Space do Hugging Face em
#     /posicao-dispersao/ e, em inglês, em /en/center-spread/)
# =============================================================================

# Este arquivo roda no Shiny Server, montado por _app_scripts/hf-space/montar_apps.R. Não
# limpa a sessão nem usa here/pacman: os caminhos são relativos à pasta do app.
options(scipen = 999)

pasta_comum <- if (dir.exists("comum")) "comum" else file.path("..", "comum")

# dados.R primeiro: os outros módulos usam os exemplos e funções na definição da interface
modulos <- list.files("modulos", pattern = "\\.R$", full.names = TRUE)
modulos <- c(modulos[basename(modulos) == "dados.R"], modulos[basename(modulos) != "dados.R"])

c(list.files(pasta_comum, pattern = "\\.R$", full.names = TRUE), modulos) |>
  purrr::walk(source)

ui <- pagina_tema(
  titulo = tr("Medidas de posição e dispersão", "Measures of center and spread"),
  versao = tr("0.2 (novembro de 2026)", "0.2 (November 2026)"),
  descricao = tr(
    paste(
      "Como resumir o centro e a variabilidade de um conjunto de dados com média, mediana, quantis, variância,",
      "desvio padrão, box plot e escore z, usando exemplos de Morettin e Bussab, de Agresti e Finlay e dados",
      "simulados. As abas contrastam as convenções dos dois livros (divisor n contra n − 1 na variância; quantis",
      "por pᵢ = (i − 0,5)/n contra medianas das metades; pontos exteriores contra valores atípicos potenciais),",
      "a regra empírica com a desigualdade de Chebyshev e o gráfico de simetria. A última aba avança para",
      "medidas de posição como problemas de otimização (incluindo a perda da regressão quantílica), ponto de",
      "ruptura, M-estimador de Huber e eficiência assintótica da mediana frente à média. O conteúdo segue",
      "Morettin e Bussab (Estatística Básica, cap. 3) e Agresti e Finlay (Statistical Methods for the Social",
      "Sciences, cap. 3), com Casella e Berger (Statistical Inference, seções 2.2, 3.6 e 10.1–10.2) e",
      "Angrist e Pischke (Mostly Harmless Econometrics, seção 7.1) no aprofundamento."
    ),
    paste(
      "How to summarize the center and variability of a data set with the mean, median, quantiles, variance,",
      "standard deviation, box plot and z-score, using examples from Morettin and Bussab, from Agresti and",
      "Finlay and simulated data. The tabs contrast the conventions of the two books (divisor n versus n − 1",
      "in the variance; quantiles via pᵢ = (i − 0.5)/n versus medians of the halves; outside points versus",
      "potential outliers), the Empirical Rule against Chebyshev's inequality and the symmetry plot. The final",
      "tab moves on to location measures as optimization problems (including the quantile regression check",
      "loss), breakdown point, Huber's M-estimator and the asymptotic efficiency of the median relative to the",
      "mean. The content follows Morettin and Bussab (Estatística Básica, ch. 3) and Agresti and Finlay",
      "(Statistical Methods for the Social Sciences, ch. 3), with Casella and Berger (Statistical Inference,",
      "sections 2.2, 3.6 and 10.1–10.2) and Angrist and Pischke (Mostly Harmless Econometrics, section 7.1)",
      "in the advanced tab."
    )
  ),
  referencias = list(
    referencia_livro("Agresti, A., & Finlay, B.", 2009, "Statistical methods for the social sciences", "Pearson Prentice Hall", edicao = 4),
    referencia_livro("Angrist, J. D., & Pischke, J.-S.", 2009, "Mostly harmless econometrics: An empiricist's companion", "Princeton University Press"),
    referencia_livro("Casella, G., & Berger, R. L.", 2002, "Statistical inference", "Duxbury", edicao = 2),
    referencia_livro("Morettin, P. A., & Bussab, W. O.", 2010, "Estatística básica", "Saraiva", edicao = 6)
  ),
  # Cabeçalhos sem caixa alta: preserva q(0,25), d_q, s², zᵢ e DAM
  bslib::nav_panel(
    tr("Posição e quantis", "Center and quantiles"), value = "posicao",
    shiny::tags$style(".tabela-app th { text-transform: none; }"),
    mod_posicao_ui("posicao")
  ),
  bslib::nav_panel(tr("Dispersão e escore z", "Spread and z-score"), value = "dispersao", mod_dispersao_ui("dispersao")),
  bslib::nav_panel(tr("Box plot e simetria", "Box plot and symmetry"), value = "boxplot", mod_boxplot_ui("boxplot")),
  bslib::nav_menu(
    tr("Para ir além", "Going further"),
    bslib::nav_panel(
      tr("Robustez, ponto de ruptura e quantis", "Robustness, breakdown point and quantiles"),
      value = "robustez",
      mod_robustez_ui("robustez")
    )
  )
)

server <- function(input, output, session) {
  modo <- modo_app(input)

  mod_posicao_server("posicao", modo)
  mod_dispersao_server("dispersao", modo)
  mod_boxplot_server("boxplot", modo)
  mod_robustez_server("robustez", modo)
}

shiny::shinyApp(ui, server)
