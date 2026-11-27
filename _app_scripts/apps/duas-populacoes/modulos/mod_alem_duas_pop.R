# =============================================================================
# Script:  mod_alem_duas_pop.R (Para ir além: permutação e robustez)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - Cenário (normal vs. assimétrico) e botão de permutação
# Saída:
#   - Histograma da distribuição de permutação e comparação de valores-p
# =============================================================================

mod_alem_duas_pop_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::radioButtons(
        ns("cenario_perm"),
        tr("Forma das populações", "Population shape"),
        choices = stats::setNames(c("normal", "assim"), c(tr("Normais (μ₁ = 53, μ₂ = 50)", "Normal (μ₁ = 53, μ₂ = 50)"), tr("Assimétricas (exponenciais)", "Skewed (exponential)"))),
        inline = FALSE
      ),
      shiny::actionButton(ns("rodar_perm"), tr("Rodar 500 permutações", "Run 500 permutations"), class = "btn-sm btn-primary mt-2")
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header(tr("Distribuição nula por permutação aleatória", "Null distribution via random permutation")),
        bslib::card_body(shiny::plotOutput(ns("grafico_perm"), height = "340px"))
      ),
      bslib::card(
        bslib::card_header(tr("Comparação: permutação, t e Wilcoxon", "Comparison: permutation, t and Wilcoxon")),
        bslib::card_body(
          shiny::uiOutput(ns("tabela_perm")),
          shiny::hr(),
          shiny::p(class = "mb-0", tr(
            "O teste de permutação não exige normalidade: sob H₀, os rótulos de grupo são permutáveis. Sob assimetria forte, Wilcoxon costuma ter mais poder que o teste t.",
            "The permutation test requires no normality assumption: under H₀, group labels are exchangeable. Under strong skewness, Wilcoxon often outperforms the t test in power."
          ))
        )
      )
    )
  )
}

mod_alem_duas_pop_server <- function(id, modo) {
  shiny::moduleServer(id, function(input, output, session) {
    sim <- shiny::reactive({
      input$rodar_perm
      shiny::req(input$cenario_perm)
      simular_permutacao_dados(20L, 20L, identical(input$cenario_perm, "assim"), 500L)
    })

    output$grafico_perm <- shiny::renderPlot({
      s <- sim()
      p <- paleta_modo(modo())
      df <- data.frame(diff = s$diffs_perm)
      ggplot2::ggplot(df, ggplot2::aes(x = diff)) +
        ggplot2::geom_histogram(bins = 28, fill = p$neutro, colour = p$fundo) +
        ggplot2::geom_vline(xintercept = c(-abs(s$diff_obs), abs(s$diff_obs)), colour = cores_app$referencia, linetype = "dashed", linewidth = 0.9) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = sprintf(tr("Diferença observada = %s", "Observed difference = %s"), fmt_num(s$diff_obs, 2)),
          x = tr("Diferença sob permutação (X̄₁ − X̄₂)", "Permuted difference (X̄₁ − X̄₂)"),
          y = tr("Contagem", "Count"),
          caption = tr("Linhas vermelhas: ±|diferença observada|", "Red lines: ±|observed difference|")
        ) +
        tema_grafico(modo())
    })

    output$tabela_perm <- shiny::renderUI({
      s <- sim()
      df <- data.frame(
        Teste = c(tr("Permutação (500)", "Permutation (500)"), "t (Welch)", "Wilcoxon / Mann-Whitney"),
        ValorP = c(fmt_num(s$p_perm, 4), fmt_num(s$p_t, 4), fmt_num(s$p_wilcox, 4)),
        stringsAsFactors = FALSE
      )
      names(df) <- c(tr("Teste", "Test"), tr("Valor-p", "p-value"))
      tabela_html(df, c("", "num"))
    })
  })
}
