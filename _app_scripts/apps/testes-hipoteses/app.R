# =============================================================================
# Script:  app.R (tópico Testes de hipóteses)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - _app_scripts/apps/comum/utilitarios.R
#   - _app_scripts/apps/testes-hipoteses/modulos/*.R
# Saída:
#   - aplicativo Shiny (publicado no Space do Hugging Face em /testes-hipoteses/
#     e, em inglês, em /en/hypothesis-testing/)
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
  titulo = tr("Testes de hipóteses", "Hypothesis testing"),
  versao = tr("0.1 (novembro de 2026)", "0.1 (November 2026)"),
  descricao = tr(
    paste(
      "Como construir e interpretar testes de significância para uma população: os cinco passos com região",
      "crítica e valor-p (Z, t e proporção), o compromisso entre erro tipo I (α), erro tipo II (β) e poder",
      "(1 − β), e a curva de função poder por tamanho amostral. O menu \"Para ir além\" simula a inflação de",
      "falsos positivos em múltiplos testes e a correção de Bonferroni. O conteúdo segue Morettin e Bussab",
      "(Estatística Básica, cap. 12) e Agresti e Finlay (Statistical Methods for the Social Sciences, cap. 6),",
      "com Casella e Berger (Statistical Inference, cap. 8) e Wooldridge (Introductory Econometrics, cap. 4)."
    ),
    paste(
      "How to build and interpret significance tests for one population: the five-step procedure with critical",
      "region and p-value (Z, t and proportion), the trade-off between type I error (α), type II error (β) and",
      "power (1 − β), and the power function across sample sizes. The \"Going further\" menu simulates false",
      "positive inflation under multiple testing and Bonferroni correction. Content follows Agresti and Finlay",
      "(Statistical Methods for the Social Sciences, ch. 6) and Morettin and Bussab (Estatística Básica, ch. 12),",
      "with Casella and Berger (Statistical Inference, ch. 8) and Wooldridge (Introductory Econometrics, ch. 4)."
    )
  ),
  referencias = list(
    referencia_livro("Agresti, A., & Finlay, B.", 2009, "Statistical methods for the social sciences", "Pearson Prentice Hall", edicao = 4),
    referencia_livro("Casella, G., & Berger, R. L.", 2002, "Statistical inference", "Duxbury", edicao = 2),
    referencia_livro("Morettin, P. A., & Bussab, W. O.", 2010, "Estatística básica", "Saraiva", edicao = 6),
    referencia_livro("Wooldridge, J. M.", 2013, "Introductory econometrics: A modern approach", "Cengage Learning", edicao = 5)
  ),
  bslib::nav_panel(
    tr("Região crítica e valor-p", "Critical region and p-value"),
    value = "passos",
    shiny::tags$style(".tabela-app th { text-transform: none; }"),
    mod_passos_valor_p_ui("passos")
  ),
  bslib::nav_panel(tr("Erros α, β e Poder", "Errors α, β and Power"), value = "erros_poder", mod_erros_poder_ui("erros_poder")),
  bslib::nav_panel(tr("Função poder", "Power function"), value = "curva_poder", mod_curva_poder_ui("curva_poder")),
  bslib::nav_menu(
    tr("Para ir além", "Going further"),
    bslib::nav_panel(
      tr("Múltiplos testes e Bonferroni", "Multiple testing and Bonferroni"),
      value = "alem_testes",
      mod_alem_testes_ui("alem_testes")
    )
  )
)

server <- function(input, output, session) {
  modo <- modo_app(input)
  mod_passos_valor_p_server("passos", modo)
  mod_erros_poder_server("erros_poder", modo)
  mod_curva_poder_server("curva_poder", modo)
  mod_alem_testes_server("alem_testes", modo)
}

shiny::shinyApp(ui, server)
