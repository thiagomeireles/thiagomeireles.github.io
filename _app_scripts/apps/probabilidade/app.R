# =============================================================================
# Script:  app.R (app Distribuições normal e t)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - _app_scripts/apps/comum/utilitarios.R
#   - _app_scripts/apps/probabilidade/modulos/*.R
# Saída:
#   - aplicativo Shiny (publicado no Space do Hugging Face em /probabilidade/
#     e, em inglês, em /en/probabilidade/)
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
  titulo = tr("Distribuições normal e t", "Normal and t distributions"),
  versao = tr("0.4 (novembro de 2026)", "0.4 (November 2026)"),
  descricao = tr(
    paste(
      "Como calcular áreas, quantis e valores críticos na normal, tanto numa escala qualquer N(μ, σ²) quanto na",
      "normal padrão Z, e na t de Student. O app também mostra de onde vem a t quando você troca o desvio padrão",
      "populacional σ pelo amostral S. A aba Para ir além simula a construção T = Z / √(U/ν) a partir da normal",
      "e da qui-quadrado, mostra quanto a cobertura cai ao usar 1,96 em amostras pequenas e discute quais",
      "momentos da t existem. O conteúdo segue Morettin e Bussab (Estatística Básica, seções 7.1, 7.4, 7.7.3,",
      "12.9 e 12.10) e Agresti e Finlay (Statistical Methods for the Social Sciences, seções 4.3 e 5.3), com",
      "Casella e Berger (Statistical Inference, seções 3.5 e 5.3) e Lukacs (1942) nos resultados formais."
    ),
    paste(
      "How to compute areas, quantiles and critical values under the normal, both on any N(μ, σ²) scale and on",
      "the standard normal Z, and under Student's t. The app also shows where t comes from when you replace the",
      "population standard deviation σ with the sample one, s. The Going further tab simulates the ratio",
      "T = Z / √(U/ν) from normal and chi-squared pieces, shows how much coverage drops when you use ±1.96 in",
      "small samples and discusses which moments of t exist. The content follows Morettin and Bussab",
      "(Estatística Básica, sections 7.1, 7.4, 7.7.3, 12.9 and 12.10) and Agresti and Finlay (Statistical",
      "Methods for the Social Sciences, sections 4.3 and 5.3), with Casella and Berger (Statistical Inference, sections 3.5 and 5.3) and Lukacs (1942) for the formal results."
    )
  ),
  referencias = list(
    referencia_livro("Agresti, A., & Finlay, B.", 2009, "Statistical methods for the social sciences", "Pearson Prentice Hall", edicao = 4),
    referencia_livro("Casella, G., & Berger, R. L.", 2002, "Statistical inference", "Duxbury", edicao = 2),
    referencia_artigo(
      "Lukacs, E.", 1942, "A characterization of the normal distribution", "The Annals of Mathematical Statistics",
      13, "91–93", numero = 1, doi = "10.1214/aoms/1177731647"
    ),
    referencia_livro("Morettin, P. A., & Bussab, W. O.", 2010, "Estatística básica", "Saraiva", edicao = 6)
  ),
  bslib::nav_panel(
    tr("Normal (Z) e t de Student", "Normal (Z) and Student's t"), value = "normal",
    shiny::tags$style(".tabela-app th { text-transform: none; }"),
    mod_normal_ui("normal")
  ),
  bslib::nav_menu(
    tr("Para ir além", "Going further"),
    bslib::nav_panel(
      tr("De onde nasce a t (Z, χ² e momentos)", "Where Student's t comes from (Z, χ² and moments)"),
      value = "origem_t",
      mod_origem_t_ui("origem_t")
    )
  )
)

server <- function(input, output, session) {
  modo <- modo_app(input)

  mod_normal_server("normal", modo)
  mod_origem_t_server("origem_t", modo)
}

shiny::shinyApp(ui, server)
