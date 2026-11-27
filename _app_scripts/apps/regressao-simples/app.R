# =============================================================================
# Script:  app.R (tópico Regressão linear simples)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - _app_scripts/apps/comum/utilitarios.R
#   - _app_scripts/apps/regressao-simples/modulos/*.R
# Saída:
#   - aplicativo Shiny (publicado no Space do Hugging Face em /regressao-simples/
#     e, em inglês, em /en/simple-regression/)
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
  titulo = tr("Regressão linear simples", "Simple linear regression"),
  versao = tr("0.1 (novembro de 2026)", "0.1 (November 2026)"),
  descricao = tr(
    paste(
      "Como ajustar e interpretar uma reta por mínimos quadrados ordinários (MQO): minimização da soma dos",
      "quadrados dos resíduos e R², intervalos de confiança para a média condicional E(Y | X) versus intervalos",
      "de predição para uma nova observação, e diagnóstico de resíduos (linearidade e heterocedasticidade). O",
      "menu \"Para ir além\" compara erros-padrão clássicos de OLS aos robustos de White (HC1) e situa a",
      "regressão como aproximação linear da CEF (sem implicação causal automática). O conteúdo segue Morettin e",
      "Bussab (Estatística Básica, cap. 16) e Agresti e Finlay (Statistical Methods for the Social Sciences,",
      "cap. 9), com Wooldridge (Introductory Econometrics, caps. 2 e 8), Angrist e Pischke (Mostly Harmless",
      "Econometrics, cap. 3) e Casella e Berger (Statistical Inference, cap. 11)."
    ),
    paste(
      "How to fit and interpret an ordinary least squares (OLS) line: minimizing the residual sum of squares",
      "and R², confidence intervals for the conditional mean E(Y | X) versus prediction intervals for a new",
      "observation, and residual diagnostics (non-linearity and heteroscedasticity). The \"Going further\" menu",
      "compares classic OLS standard errors against White robust (HC1) standard errors and frames regression",
      "as the best linear approximation to the CEF (without automatic causal claims). Content follows Agresti",
      "and Finlay (Statistical Methods for the Social Sciences, ch. 9) and Morettin and Bussab (Estatística",
      "Básica, ch. 16), with Wooldridge (Introductory Econometrics, chs. 2 and 8), Angrist and Pischke (Mostly",
      "Harmless Econometrics, ch. 3) and Casella and Berger (Statistical Inference, ch. 11)."
    )
  ),
  referencias = list(
    referencia_livro("Agresti, A., & Finlay, B.", 2009, "Statistical methods for the social sciences", "Pearson Prentice Hall", edicao = 4),
    referencia_livro("Angrist, J. D., & Pischke, J.-S.", 2009, "Mostly harmless econometrics: An empiricist's companion", "Princeton University Press"),
    referencia_livro("Casella, G., & Berger, R. L.", 2002, "Statistical inference", "Duxbury", edicao = 2),
    referencia_livro("Morettin, P. A., & Bussab, W. O.", 2010, "Estatística básica", "Saraiva", edicao = 6),
    referencia_livro("Wooldridge, J. M.", 2013, "Introductory econometrics: A modern approach", "Cengage Learning", edicao = 5)
  ),
  bslib::nav_panel(
    tr("Mínimos quadrados e R²", "Least squares and R²"),
    value = "mqo",
    shiny::tags$style(".tabela-app th { text-transform: none; }"),
    mod_mqo_ui("mqo")
  ),
  bslib::nav_panel(tr("Inferência e predição", "Inference and prediction"), value = "inferencia_pred", mod_inferencia_pred_ui("inferencia_pred")),
  bslib::nav_panel(tr("Diagnóstico de resíduos", "Residual diagnostics"), value = "residuos", mod_residuos_ui("residuos")),
  bslib::nav_menu(
    tr("Para ir além", "Going further"),
    bslib::nav_panel(
      tr("CEF e erros robustos (White/HC)", "CEF and robust SEs (White/HC)"),
      value = "alem_reg",
      mod_alem_reg_ui("alem_reg")
    )
  )
)

server <- function(input, output, session) {
  modo <- modo_app(input)
  mod_mqo_server("mqo", modo)
  mod_inferencia_pred_server("inferencia_pred", modo)
  mod_residuos_server("residuos", modo)
  mod_alem_reg_server("alem_reg", modo)
}

shiny::shinyApp(ui, server)
