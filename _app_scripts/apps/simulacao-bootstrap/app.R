# =============================================================================
# Script:  app.R (tópico Simulação e bootstrap)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - _app_scripts/apps/comum/utilitarios.R
#   - _app_scripts/apps/simulacao-bootstrap/modulos/*.R
# Saída:
#   - aplicativo Shiny (publicado no Space do Hugging Face em
#     /simulacao-bootstrap/ e, em inglês, em /en/simulation-bootstrap/)
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
  titulo = tr("Simulação e bootstrap", "Simulation and bootstrap"),
  versao = tr("0.1 (novembro de 2026)", "0.1 (November 2026)"),
  descricao = tr(
    paste(
      "Como gerar variáveis aleatórias no computador, aproximar probabilidades e momentos por Monte Carlo e",
      "estimar erros padrão e intervalos de confiança por reamostragem bootstrap quando não há fórmula fechada.",
      "As três abas principais percorrem o gerador congruencial linear e o método da transformação inversa,",
      "a transformação de Box-Müller com o erro padrão de Monte Carlo na escala 1/√M e o bootstrap não paramétrico",
      "em dados reais e simulados. O menu \"Para ir além\" compara o bootstrap ao Método Delta em funções não",
      "monótonas, contrasta o bootstrap paramétrico com o não paramétrico e mostra a falha do bootstrap na",
      "fronteira de U(0, θ). O conteúdo segue Morettin e Bussab (Estatística Básica, caps. 9 e 11) e Agresti e",
      "Finlay (Statistical Methods for the Social Sciences, cap. 5), com Casella e Berger (Statistical Inference,",
      "caps. 1, 2 e 10), Box e Muller (1958) e Efron (1979)."
    ),
    paste(
      "How to generate random variables on a computer, approximate probabilities and moments by Monte Carlo and",
      "estimate standard errors and confidence intervals by bootstrap resampling when closed-form formulas are",
      "unavailable. The three main tabs cover linear congruential generators and the inverse transform method,",
      "the Box-Muller transform alongside Monte Carlo standard error at the 1/√M rate and nonparametric bootstrap",
      "on real and simulated datasets. The \"Going further\" menu compares the bootstrap against the Delta Method",
      "for non-monotone functions, contrasts parametric and nonparametric bootstrap and illustrates bootstrap",
      "failure at the boundary of U(0, θ). Content follows Morettin and Bussab (Estatística Básica, chs. 9 and 11)",
      "and Agresti and Finlay (Statistical Methods for the Social Sciences, ch. 5), with Casella and Berger",
      "(Statistical Inference, chs. 1, 2 and 10), Box and Muller (1958) and Efron (1979)."
    )
  ),
  referencias = list(
    referencia_livro("Agresti, A., & Finlay, B.", 2009, "Statistical methods for the social sciences", "Pearson Prentice Hall", edicao = 4),
    referencia_artigo("Box, G. E. P., & Muller, M. E.", 1958, "A note on the generation of random normal deviates", "The Annals of Mathematical Statistics", 29, "610–611", numero = 2, doi = "10.1214/aoms/1177706645"),
    referencia_livro("Casella, G., & Berger, R. L.", 2002, "Statistical inference", "Duxbury", edicao = 2),
    referencia_artigo("Efron, B.", 1979, "Bootstrap methods: Another look at the jackknife", "The Annals of Statistics", 7, "1–26", numero = 1, doi = "10.1214/aos/1176344552"),
    referencia_livro("Morettin, P. A., & Bussab, W. O.", 2010, "Estatística básica", "Saraiva", edicao = 6)
  ),
  bslib::nav_panel(
    tr("Pseudoaleatórios e inversa", "Pseudorandom and inverse"),
    value = "prng_inversa",
    shiny::tags$style(".tabela-app th { text-transform: none; }"),
    mod_prng_inversa_ui("prng_inversa")
  ),
  bslib::nav_panel(
    tr("Box-Müller e Monte Carlo", "Box-Muller and Monte Carlo"),
    value = "box_muller_mc",
    mod_box_muller_mc_ui("box_muller_mc")
  ),
  bslib::nav_panel(
    tr("Bootstrap não paramétrico", "Nonparametric bootstrap"),
    value = "bootstrap",
    mod_bootstrap_ui("bootstrap")
  ),
  bslib::nav_menu(
    tr("Para ir além", "Going further"),
    bslib::nav_panel(
      tr("Delta, paramétrico e fronteira", "Delta, parametric and boundary"),
      value = "alem_bootstrap",
      mod_alem_bootstrap_ui("alem_bootstrap")
    )
  )
)

server <- function(input, output, session) {
  modo <- modo_app(input)
  mod_prng_inversa_server("prng_inversa", modo)
  mod_box_muller_mc_server("box_muller_mc", modo)
  mod_bootstrap_server("bootstrap", modo)
  mod_alem_bootstrap_server("alem_bootstrap", modo)
}

shiny::shinyApp(ui, server)
