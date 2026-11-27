# =============================================================================
# Script:  app.R (tópico População, amostra, parâmetro e estatística)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - _app_scripts/apps/comum/utilitarios.R
#   - _app_scripts/apps/populacao-amostra/modulos/*.R
# Saída:
#   - aplicativo Shiny (publicado no Space do Hugging Face em /populacao-amostra/
#     e, em inglês, em /en/population-sample/)
# =============================================================================

# Este arquivo roda no Shiny Server, montado por _app_scripts/hf-space/montar_apps.R. Não
# limpa a sessão nem usa here/pacman: os caminhos são relativos à pasta do app.
options(scipen = 999)

pasta_comum <- if (dir.exists("comum")) "comum" else file.path("..", "comum")

# populacao.R primeiro: os outros módulos usam a população na definição da interface
modulos <- list.files("modulos", pattern = "\\.R$", full.names = TRUE)
modulos <- c(modulos[basename(modulos) == "populacao.R"], modulos[basename(modulos) != "populacao.R"])

c(list.files(pasta_comum, pattern = "\\.R$", full.names = TRUE), modulos) |>
  purrr::walk(source)

ui <- pagina_tema(
  titulo = tr("População, amostra, parâmetro e estatística", "Population, sample, parameter and statistic"),
  versao = tr("0.3 (novembro de 2026)", "0.3 (November 2026)"),
  descricao = tr(
    paste(
      "O que se quer conhecer (a população e seus parâmetros), o que de fato se observa (a amostra e suas estatísticas)",
      "e como chamar cada medida, com quiz, uma população simulada de 2.000 pessoas para sortear amostras e uma",
      "simulação que compara dois estimadores da variância. Os conceitos seguem Morettin e Bussab (Estatística Básica,",
      "caps. 10 e 11) e Agresti e Finlay (Statistical Methods for the Social Sciences, caps. 1 a 3), e as definições",
      "formais foram conferidas em Casella e Berger (Statistical Inference, caps. 5 e 7) e Wooldridge (Introductory",
      "Econometrics, seção 9.5 e apêndice C)."
    ),
    paste(
      "What we want to know (the population and its parameters), what we actually observe (the sample and its",
      "statistics) and what to call each measure, with a quiz, a simulated population of 2,000 people to draw samples",
      "from and a simulation that compares two estimators of the variance. The concepts follow Agresti and Finlay",
      "(Statistical Methods for the Social Sciences, chs. 1 to 3) and Morettin and Bussab (Estatística Básica, chs. 10",
      "and 11), and the formal definitions were checked against Casella and Berger (Statistical Inference, chs. 5 and 7)",
      "and Wooldridge (Introductory Econometrics, section 9.5 and appendix C)."
    )
  ),
  referencias = list(
    referencia_livro("Agresti, A., & Finlay, B.", 2009, "Statistical methods for the social sciences", "Pearson Prentice Hall", edicao = 4),
    referencia_livro("Casella, G., & Berger, R. L.", 2002, "Statistical inference", "Duxbury", edicao = 2),
    referencia_livro("Morettin, P. A., & Bussab, W. O.", 2010, "Estatística básica", "Saraiva", edicao = 6),
    referencia_livro("Wooldridge, J. M.", 2013, "Introductory econometrics: A modern approach", "Cengage Learning", edicao = 5)
  ),
  bslib::nav_panel(tr("Os conceitos", "The concepts"), value = "conceitos", mod_conceitos_ui("conceitos")),
  bslib::nav_panel(tr("Uma amostra", "One sample"), value = "amostra", mod_amostra_ui("amostra")),
  bslib::nav_panel(tr("Estimador e estimativa", "Estimator and estimate"), value = "estimador", mod_estimador_ui("estimador"))
)

server <- function(input, output, session) {
  modo <- modo_app(input)

  mod_conceitos_server("conceitos", ir_para = \(aba) bslib::nav_select("abas", aba, session = session))
  mod_amostra_server("amostra", modo)
  mod_estimador_server("estimador", modo)
}

shiny::shinyApp(ui, server)
