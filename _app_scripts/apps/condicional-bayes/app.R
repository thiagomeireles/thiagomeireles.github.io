# =============================================================================
# Script:  app.R (tópico Probabilidade condicional e Bayes)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-26
#
# Entrada:
#   - _app_scripts/apps/comum/utilitarios.R
#   - _app_scripts/apps/condicional-bayes/modulos/*.R
# Saída:
#   - aplicativo Shiny (publicado no Space do Hugging Face em
#     /condicional-bayes/ e, em inglês, em /en/conditional-bayes/)
# =============================================================================

# Este arquivo roda no Shiny Server, montado por _app_scripts/hf-space/montar_apps.R. Não
# limpa a sessão nem usa here/pacman: os caminhos são relativos à pasta do app.
options(scipen = 999)

pasta_comum <- if (dir.exists("comum")) "comum" else file.path("..", "comum")

# dados.R primeiro: os outros módulos usam as funções de cálculo na execução
modulos <- list.files("modulos", pattern = "\\.R$", full.names = TRUE)
modulos <- c(modulos[basename(modulos) == "dados.R"], modulos[basename(modulos) != "dados.R"])

c(list.files(pasta_comum, pattern = "\\.R$", full.names = TRUE), modulos) |>
  purrr::walk(source)

ui <- pagina_tema(
  titulo = tr("Probabilidade condicional e Bayes", "Conditional probability and Bayes"),
  versao = tr("0.1 (novembro de 2026)", "0.1 (November 2026)"),
  descricao = tr(
    paste(
      "Como atualizar probabilidades quando surge informação nova: da restrição do espaço amostral à",
      "regra de Bayes. As três abas principais cobrem: (1) probabilidade condicional P(A | B), regra do",
      "produto e o erro de inverter P(A | B) com P(B | A); (2) independência versus eventos mutuamente",
      "exclusivos e árvores de probabilidade com e sem reposição; e (3) probabilidade total, teorema de",
      "Bayes, razão de chances e frequências naturais para enxergar o efeito da taxa base. O menu \"Para ir",
      "além\" reúne independência dois a dois versus mútua, o problema dos Três Prisioneiros, a",
      "atualização sequencial com distribuição preditiva a posteriori (caso genético de hemofilia) e a",
      "crítica de modelo no exemplo 'radom'. O conteúdo segue Morettin e Bussab (Estatística Básica, cap.",
      "5, seções 5.3–5.5) e Agresti e Finlay (Statistical Methods for the Social Sciences, cap. 4, seção",
      "4.1), com Casella e Berger (Statistical Inference, cap. 1, seção 1.3), Gelman et al. (Bayesian",
      "Data Analysis, cap. 1) e McElreath (Statistical Rethinking, cap. 2) no aprofundamento."
    ),
    paste(
      "How to update probabilities when new information arrives: from restricting the sample space to",
      "Bayes' rule. The three main tabs cover: (1) conditional probability P(A | B), the product rule",
      "and the fallacy of inverting P(A | B) with P(B | A); (2) independence versus mutually exclusive",
      "events and two-stage probability trees with and without replacement; and (3) total probability,",
      "Bayes' theorem, posterior odds and natural frequencies to highlight the base-rate effect. The",
      "\"Going further\" menu gathers pairwise versus mutual independence, the Three Prisoners problem,",
      "sequential updating with posterior predictive probability (hemophilia genetics case) and model",
      "checking in the 'radom' example. Content follows Agresti and Finlay (Statistical Methods for the",
      "Social Sciences, ch. 4, section 4.1) and Morettin and Bussab (Estatística Básica, ch. 5, sections",
      "5.3–5.5), with Casella and Berger (Statistical Inference, ch. 1, section 1.3), Gelman et al.",
      "(Bayesian Data Analysis, ch. 1) and McElreath (Statistical Rethinking, ch. 2) in the advanced tab."
    )
  ),
  referencias = list(
    referencia_livro("Agresti, A., & Finlay, B.", 2009, "Statistical methods for the social sciences", "Pearson Prentice Hall", edicao = 4),
    referencia_livro("Casella, G., & Berger, R. L.", 2002, "Statistical inference", "Duxbury", edicao = 2),
    referencia_livro("Gelman, A., Carlin, J. B., Stern, H. S., Dunson, D. B., Vehtari, A., & Rubin, D. B.", 2013, "Bayesian data analysis", "Chapman & Hall/CRC", edicao = 3),
    referencia_livro("McElreath, R.", 2020, "Statistical rethinking: A Bayesian course with examples in R and Stan", "CRC Press", edicao = 2),
    referencia_livro("Morettin, P. A., & Bussab, W. O.", 2010, "Estatística básica", "Saraiva", edicao = 6)
  ),
  bslib::nav_panel(
    tr("Condicional e produto", "Conditional and product"),
    value = "condicional",
    shiny::tags$style(shiny::HTML(".tabela-app th { text-transform: none; }")),
    mod_condicional_ui("condicional")
  ),
  bslib::nav_panel(
    tr("Independência e árvores", "Independence and trees"),
    value = "independencia",
    mod_independencia_arvores_ui("independencia")
  ),
  bslib::nav_panel(
    tr("Probabilidade total e Bayes", "Total probability and Bayes"),
    value = "bayes",
    mod_bayes_ui("bayes")
  ),
  bslib::nav_menu(
    tr("Para ir além", "Going further"),
    bslib::nav_panel(
      tr("Independência mútua e Bayes sequencial", "Mutual independence and sequential Bayes"),
      value = "alem_bayes",
      mod_alem_bayes_ui("alem_bayes")
    )
  )
)

server <- function(input, output, session) {
  modo <- modo_app(input)
  mod_condicional_server("condicional", modo)
  mod_independencia_arvores_server("independencia", modo)
  mod_bayes_server("bayes", modo)
  mod_alem_bayes_server("alem_bayes", modo)
}

shiny::shinyApp(ui, server)
