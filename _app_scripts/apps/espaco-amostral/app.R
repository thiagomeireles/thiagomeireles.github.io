# =============================================================================
# Script:  app.R (tópico Espaço amostral, eventos e axiomas)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-26
#
# Entrada:
#   - _app_scripts/apps/comum/utilitarios.R
#   - _app_scripts/apps/espaco-amostral/modulos/*.R
# Saída:
#   - aplicativo Shiny (publicado no Space do Hugging Face em
#     /espaco-amostral/ e, em inglês, em /en/sample-space/)
# =============================================================================

# Este arquivo roda no Shiny Server, montado por _app_scripts/hf-space/montar_apps.R. Não
# limpa a sessão nem usa here/pacman: os caminhos são relativos à pasta do app.
options(scipen = 999)

pasta_comum <- if (dir.exists("comum")) "comum" else file.path("..", "comum")

# dados.R primeiro: os outros módulos usam os experimentos e funções na definição da interface
modulos <- list.files("modulos", pattern = "\\.R$", full.names = TRUE)
modulos <- c(modulos[basename(modulos) == "dados.R"], modulos[basename(modulos) != "dados.R"])

c(list.files(pasta_comum, pattern = "\\.R$", full.names = TRUE), modulos) |>
  purrr::walk(source)

ui <- pagina_tema(
  titulo = tr("Espaço amostral, eventos e axiomas", "Sample space, events and axioms"),
  versao = tr("0.1 (novembro de 2026)", "0.1 (November 2026)"),
  descricao = tr(
    paste(
      "Como traduzir experimentos com resultado incerto em conjuntos e números entre 0 e 1, partindo do espaço",
      "amostral, das operações entre eventos, dos axiomas da probabilidade e das técnicas de contagem. As três",
      "abas principais separam o essencial: (1) o contraste entre a probabilidade por simetria, a frequência",
      "relativa de longo prazo e a probabilidade subjetiva; (2) a álgebra de eventos (complemento, união,",
      "interseção, eventos mutuamente exclusivos e leis de De Morgan) ligada à regra da soma; e (3) a tabela",
      "2 × 2 de contagem (ordenado ou não × com ou sem reposição), mostrando por que resultados não ordenados",
      "deixam de ser equiprováveis quando há reposição. O menu \"Para ir além\" isola o formalismo de",
      "σ-álgebras, as desigualdades de Boole e Bonferroni e as cotas alternadas de inclusão-exclusão. O conteúdo",
      "segue Morettin e Bussab (Estatística Básica, cap. 5, seções 5.1, 5.2 e 5.5; cap. 9) e Agresti e Finlay",
      "(Statistical Methods for the Social Sciences, cap. 4, seção 4.1), com Casella e Berger (Statistical",
      "Inference, cap. 1, seções 1.1, 1.2 e 1.8.1) na contagem completa e no aprofundamento."
    ),
    paste(
      "How to translate uncertain experiments into sets and numbers between 0 and 1, starting from the sample",
      "space, set operations on events, the probability axioms and counting rules. The three main tabs focus on",
      "the essentials: (1) contrasting classical symmetry, long-run relative frequency and subjective probability;",
      "(2) event algebra (complement, union, intersection, mutually exclusive events and De Morgan's laws) tied",
      "to the addition rule; and (3) the 2 × 2 counting table (ordered or unordered × with or without",
      "replacement), showing why unordered outcomes stop being equally likely under sampling with replacement.",
      "The \"Going further\" menu sets aside σ-algebras, the Boole and Bonferroni inequalities and the",
      "alternating inclusion-exclusion bounds. Content follows Agresti and Finlay (Statistical Methods for the",
      "Social Sciences, ch. 4, section 4.1) and Morettin and Bussab (Estatística Básica, ch. 5, sections 5.1,",
      "5.2 and 5.5; ch. 9), with Casella and Berger (Statistical Inference, ch. 1, sections 1.1, 1.2 and 1.8.1)",
      "in the full counting table and the advanced tab."
    )
  ),
  # Cabeçalhos sem caixa alta: preserva Ω, S, Aᶜ, P(A ∩ B), C(n, r) e P_k
  bslib::nav_panel(
    tr("Experimento e Ω", "Experiment and Ω"), value = "experimento",
    shiny::tags$style(".tabela-app th { text-transform: none; }"),
    mod_experimento_ui("experimento")
  ),
  bslib::nav_panel(tr("Eventos e axiomas", "Events and axioms"), value = "eventos", mod_eventos_ui("eventos")),
  bslib::nav_panel(tr("Contagem e equiprobabilidade", "Counting and equal probability"), value = "contagem", mod_contagem_ui("contagem")),
  bslib::nav_menu(
    tr("Para ir além", "Going further"),
    bslib::nav_panel(
      tr("σ-álgebras, Boole-Bonferroni e inclusão-exclusão", "σ-algebras, Boole-Bonferroni and inclusion-exclusion"),
      value = "alem",
      mod_alem_axiomas_ui("alem")
    )
  )
)

server <- function(input, output, session) {
  modo <- modo_app(input)

  mod_experimento_server("experimento", modo)
  mod_eventos_server("eventos", modo)
  mod_contagem_server("contagem", modo)
  mod_alem_axiomas_server("alem", modo)
}

shiny::shinyApp(ui, server)
