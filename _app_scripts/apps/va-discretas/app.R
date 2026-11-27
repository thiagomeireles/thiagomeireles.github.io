# =============================================================================
# Script:  app.R (tópico Variáveis aleatórias discretas)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - _app_scripts/apps/comum/utilitarios.R
#   - _app_scripts/apps/va-discretas/modulos/*.R
# Saída:
#   - aplicativo Shiny (publicado no Space do Hugging Face em /va-discretas/
#     e, em inglês, em /en/discrete-random-variables/)
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
  titulo = tr("Variáveis aleatórias discretas", "Discrete random variables"),
  versao = tr("0.1 (novembro de 2026)", "0.1 (November 2026)"),
  descricao = tr(
    paste(
      "Como descrever contagens incertas com funções de probabilidade p(x), funções de distribuição em escada",
      "F(x), esperança, variância e os principais modelos discretos: uniforme discreto, Bernoulli, binomial",
      "b(n, p), Poisson e aproximação binomial-Poisson. O menu \"Para ir além\" compara a hipergeométrica",
      "(sem reposição) à binomial e contrasta as duas convenções da distribuição geométrica (ensaios versus",
      "fracassos). O conteúdo segue Morettin e Bussab (Estatística Básica, cap. 6) e Agresti e Finlay",
      "(Statistical Methods for the Social Sciences, cap. 4, seção 4.2), com Casella e Berger (Statistical",
      "Inference, caps. 1 e 3) e Gelman et al. (Bayesian Data Analysis, apêndice A) no formalismo."
    ),
    paste(
      "How to describe uncertain counts using probability mass functions p(x), step cumulative distribution",
      "functions F(x), expected value, variance and the core discrete models: discrete uniform, Bernoulli,",
      "binomial b(n, p), Poisson and the binomial-Poisson approximation. The \"Going further\" menu compares",
      "the hypergeometric (without replacement) with the binomial and contrasts the two geometric conventions",
      "(trials versus failures). Content follows Agresti and Finlay (Statistical Methods for the Social",
      "Sciences, ch. 4, section 4.2) and Morettin and Bussab (Estatística Básica, ch. 6), with Casella and",
      "Berger (Statistical Inference, chs. 1 and 3) and Gelman et al. (Bayesian Data Analysis, appendix A)."
    )
  ),
  referencias = list(
    referencia_livro("Agresti, A., & Finlay, B.", 2009, "Statistical methods for the social sciences", "Pearson Prentice Hall", edicao = 4),
    referencia_livro("Casella, G., & Berger, R. L.", 2002, "Statistical inference", "Duxbury", edicao = 2),
    referencia_livro("Gelman, A., Carlin, J. B., Stern, H. S., Dunson, D. B., Vehtari, A., & Rubin, D. B.", 2013, "Bayesian data analysis", "Chapman & Hall/CRC", edicao = 3),
    referencia_livro("Morettin, P. A., & Bussab, W. O.", 2010, "Estatística básica", "Saraiva", edicao = 6)
  ),
  bslib::nav_panel(
    tr("FP, FDA e momentos", "PMF, CDF and moments"),
    value = "fp_fda",
    shiny::tags$style(".tabela-app th { text-transform: none; }"),
    mod_fp_fda_ui("fp_fda")
  ),
  bslib::nav_panel(tr("Binomial b(n, p)", "Binomial b(n, p)"), value = "binomial", mod_binomial_ui("binomial")),
  bslib::nav_panel(tr("Poisson e processo", "Poisson and process"), value = "poisson", mod_poisson_ui("poisson")),
  bslib::nav_menu(
    tr("Para ir além", "Going further"),
    bslib::nav_panel(
      tr("Hipergeométrica e geométrica", "Hypergeometric and geometric"),
      value = "alem_discretas",
      mod_alem_discretas_ui("alem_discretas")
    )
  )
)

server <- function(input, output, session) {
  modo <- modo_app(input)
  mod_fp_fda_server("fp_fda", modo)
  mod_binomial_server("binomial", modo)
  mod_poisson_server("poisson", modo)
  mod_alem_discretas_server("alem_discretas", modo)
}

shiny::shinyApp(ui, server)
