# =============================================================================
# Script:  app.R (app Distribuições normal e t)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-25
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
  versao = tr("0.3 (novembro de 2026)", "0.3 (November 2026)"),
  descricao = tr(
    paste(
      "Como calcular áreas, quantis e valores críticos nas distribuições normal — em qualquer escala N(μ, σ²) e na",
      "normal padrão Z — e t de Student, e entender de onde surge a t quando o desvio padrão populacional σ é",
      "substituído por S. A aba de aprofundamento simula a construção T = Z / √(U/ν) a partir da normal e da",
      "qui-quadrado, mostra a perda de cobertura ao usar 1,96 em amostras pequenas e discute a existência de",
      "momentos. O conteúdo segue Morettin e Bussab (Estatística Básica, caps. 7 e 10) e Agresti e Finlay",
      "(Statistical Methods for the Social Sciences, cap. 4), com Casella e Berger (Statistical Inference,",
      "seções 3.3, 3.5 e 5.3) na derivação formal."
    ),
    paste(
      "How to compute areas, quantiles and critical values under the normal distribution — on any N(μ, σ²) scale",
      "and on the standard normal Z — and under Student's t, and see where t comes from when the unknown population",
      "standard deviation σ is replaced by s. The advanced tab simulates the ratio T = Z / √(U/ν) from normal and",
      "chi-squared components, shows the undercoverage of ±1.96 in small samples and discusses finite moments. The",
      "content follows Agresti and Finlay (Statistical Methods for the Social Sciences, ch. 4) and Morettin and",
      "Bussab (Estatística Básica, chs. 7 and 10), with Casella and Berger (Statistical Inference, sections 3.3,",
      "3.5 and 5.3) for the formal results."
    )
  ),
  referencias = list(
    referencia_livro("Agresti, A., & Finlay, B.", 2009, "Statistical methods for the social sciences", "Pearson Prentice Hall", edicao = 4),
    referencia_livro("Casella, G., & Berger, R. L.", 2002, "Statistical inference", "Duxbury", edicao = 2),
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
