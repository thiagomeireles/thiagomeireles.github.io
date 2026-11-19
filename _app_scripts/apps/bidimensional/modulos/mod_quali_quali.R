# =============================================================================
# Script:  mod_quali_quali.R (módulo: duas variáveis qualitativas)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-19
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/apps/bidimensional/app.R)
# Saída:
#   - funções mod_quali_quali_ui() e mod_quali_quali_server()
# =============================================================================

texto_quali_quali <- function() {
  tr(
    shiny::withMathJax(
      shiny::p(
        "Com duas variáveis qualitativas, o ponto de partida é a tabela de dupla entrada: cada casela conta as",
        "observações que caem ao mesmo tempo numa categoria da linha e numa da coluna. A leitura depende da base",
        "das porcentagens, e a base sai da pergunta. Quando uma variável ajuda a explicar a outra, como a",
        "escolaridade explica o uso de internet, a explicativa vai nas linhas e as porcentagens são calculadas",
        "dentro de cada linha. Agresti e Finlay descrevem a tabela dessa forma, com a resposta em porcentagem",
        "dentro de cada categoria da explicativa."
      ),
      shiny::p(
        "Se as variáveis fossem independentes, todas as linhas teriam o mesmo perfil, igual ao da linha Total.",
        "Morettin e Bussab medem o quanto a tabela se afasta disso comparando cada frequência observada",
        "\\(n_{ij}\\) com a esperada sob independência, \\(n^*_{ij} = n_{i\\cdot}\\, n_{\\cdot j} / n\\).",
        "O resultado é o qui-quadrado de Pearson, \\(\\chi^2 = \\sum_i \\sum_j (n_{ij} - n^*_{ij})^2 / n^*_{ij}\\),",
        "que vale 0 quando observado e esperado coincidem e cresce com o afastamento. Como o \\(\\chi^2\\) também",
        "cresce com \\(n\\), o livro propõe dois coeficientes que não dependem do tamanho da amostra: o de contingência,",
        "\\(C = \\sqrt{\\chi^2 / (\\chi^2 + n)}\\), e o \\(T = \\sqrt{(\\chi^2 / n) / ((r - 1)(s - 1))}\\), em que",
        "\\(r\\) e \\(s\\) são os números de linhas e de colunas."
      ),
      shiny::p(
        shiny::strong("Experimente:"),
        "ponha a força da associação em 0 e sorteie algumas amostras. As variáveis são independentes na população,",
        "mas o \\(\\chi^2\\) da amostra quase nunca dá exatamente 0, já que o acaso sempre afasta um pouco as linhas.",
        "Depois aumente a força e veja os perfis se separarem no gráfico."
      )
    ),
    shiny::withMathJax(
      shiny::p(
        "With two categorical variables, the starting point is the contingency table: each cell counts the",
        "observations that fall at the same time in one row category and one column category. How you read it",
        "depends on the base of the percentages, and the base comes from the question. When one variable helps",
        "explain the other, as education explains internet use, the explanatory variable goes in the rows and the",
        "percentages are computed within each row. Agresti and Finlay describe the table this way, with the",
        "response as percentages within each category of the explanatory variable."
      ),
      shiny::p(
        "If the variables were independent, every row would have the same profile as the Total row. Morettin and",
        "Bussab measure how far the table is from that by comparing each observed frequency \\(n_{ij}\\) with the",
        "frequency expected under independence, \\(n^*_{ij} = n_{i\\cdot}\\, n_{\\cdot j} / n\\). The result is",
        "Pearson's chi-squared, \\(\\chi^2 = \\sum_i \\sum_j (n_{ij} - n^*_{ij})^2 / n^*_{ij}\\), which is 0 when observed",
        "and expected match and grows as they move apart. Since \\(\\chi^2\\) also grows with \\(n\\), the book offers",
        "two coefficients that do not depend on sample size: the contingency coefficient,",
        "\\(C = \\sqrt{\\chi^2 / (\\chi^2 + n)}\\), and \\(T = \\sqrt{(\\chi^2 / n) / ((r - 1)(s - 1))}\\), where",
        "\\(r\\) and \\(s\\) are the numbers of rows and columns."
      ),
      shiny::p(
        shiny::strong("Try it:"),
        "set the strength of the association to 0 and draw a few samples. The variables are independent in the",
        "population, but the sample \\(\\chi^2\\) is almost never exactly 0, since chance always pulls the rows a",
        "little apart. Then raise the strength and watch the profiles separate in the chart."
      )
    )
  )
}

cuidados_quali_quali <- function() {
  tr(
    c(
      "Base das porcentagens" = paste(
        "porcentagem na linha e porcentagem na coluna respondem perguntas diferentes. \"Que parte de quem tem",
        "ensino superior usa internet\" não é o mesmo que \"que parte de quem usa internet tem ensino superior\"."
      ),
      "χ² sozinho" = "cresce com o tamanho da amostra. Para comparar tabelas de tamanhos diferentes, use C ou T.",
      "Valor máximo" = paste(
        "C nunca chega a 1, e T só chega a 1 em tabelas 2 × 2. Leia cada um em relação ao máximo possível",
        "para o tamanho da tabela, mostrado no quadro de medidas."
      ),
      "Associação não é causa" = paste(
        "a tabela mostra que as variáveis andam juntas, não por quê. Uma terceira variável, como a renda,",
        "pode estar por trás das duas."
      ),
      "Teste formal" = paste(
        "decidir se a associação vai além do acaso é tarefa do teste qui-quadrado de independência",
        "(Morettin e Bussab, Cap. 14; Agresti e Finlay, Cap. 8). No R, chisq.test() aplica a correção de Yates em tabelas 2 × 2; use",
        "correct = FALSE para obter o χ² descritivo deste app."
      )
    ),
    c(
      "Base of the percentages" = paste(
        "row and column percentages answer different questions. \"What share of college graduates use the",
        "internet\" is not the same as \"what share of internet users are college graduates\"."
      ),
      "χ² on its own" = "grows with sample size. To compare tables of different sizes, use C or T.",
      "Maximum value" = paste(
        "C never reaches 1, and T only reaches 1 in 2 × 2 tables. Read each one against the maximum possible",
        "for the size of the table, shown in the measures panel."
      ),
      "Association is not causation" = paste(
        "the table shows that the variables go together, not why. A third variable, such as income, may be",
        "behind both."
      ),
      "Formal test" = paste(
        "deciding whether the association goes beyond chance is the job of the chi-squared test of independence",
        "(Morettin and Bussab, ch. 14; Agresti and Finlay, ch. 8). In R, chisq.test() applies Yates' correction",
        "to 2 × 2 tables; use correct = FALSE to get the descriptive χ² shown here."
      )
    )
  )
}

opcoes_conteudo <- function() {
  stats::setNames(
    c("n", "linha", "coluna", "total", "esperado"),
    tr(
      c("Frequências observadas", "% na linha", "% na coluna", "% do total geral", "Frequências esperadas sob independência"),
      c("Observed frequencies", "Row %", "Column %", "% of the grand total", "Expected frequencies under independence")
    )
  )
}

# Tabela de dupla entrada com totais. Nas porcentagens por linha, a linha Total
# mostra o perfil marginal, que seria o de todas as linhas sob independência.
tabela_dupla_html <- function(m, conteudo, nome_linhas) {
  n <- sum(m)
  lin <- rowSums(m)
  col <- colSums(m)
  corpo <- switch(conteudo,
    n = m, esperado = outer(lin, col) / n, total = m / n,
    linha = m / lin, coluna = sweep(m, 2, col, "/")
  )
  margem_linhas <- switch(conteudo, n = , esperado = lin, linha = rep(1, length(lin)), lin / n)
  margem_colunas <- switch(conteudo, n = , esperado = col, coluna = rep(1, length(col)), col / n)
  canto <- if (conteudo %in% c("n", "esperado")) n else 1
  fmt <- switch(conteudo, n = \(v) fmt_num(v, 0), esperado = \(v) fmt_num(v, 1), \(v) fmt_pct(v))

  miolo <- cbind(matrix(fmt(corpo), nrow = nrow(m)), fmt(margem_linhas)) |>
    rbind(c(fmt(margem_colunas), fmt(canto)))
  tabela <- data.frame(c(rownames(m), "Total"), miolo)
  names(tabela) <- c(nome_linhas, colnames(m), "Total")
  tabela_html(tabela, classes = c("", rep("num", ncol(m) + 1)))
}

mod_quali_quali_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      seletor_exemplo(ns("exemplo"), exemplos_quali_quali),
      slider_amostra(ns("n"), min = 30, max = 2000, value = 300),
      shiny::sliderInput(ns("forca"), tr("Força da associação na população", "Strength of the association in the population"), min = 0, max = 1, value = 0.5, step = 0.05),
      shiny::helpText(tr("0: variáveis independentes na população.", "0: the variables are independent in the population.")),
      shiny::radioButtons(ns("conteudo"), tr("Mostrar na tabela", "Show in the table"), choices = opcoes_conteudo(), selected = "linha"),
      shiny::radioButtons(
        ns("grafico"), tr("Gráfico", "Chart"),
        choices = stats::setNames(c("perfis", "desvios"), tr(c("Perfis das linhas", "Desvios da independência"), c("Row profiles", "Departures from independence")))
      ),
      botao_sortear(ns("sortear"))
    ),
    cabecalho_par("quali_quali"),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(title = tr("Qui-quadrado (χ²)", "Chi-squared (χ²)"), value = shiny::textOutput(ns("qui2")), shiny::textOutput(ns("qui2_desc")), theme = "primary"),
      bslib::value_box(title = tr("Coeficiente de contingência (C)", "Contingency coefficient (C)"), value = shiny::textOutput(ns("C")), shiny::textOutput(ns("C_desc")), theme = "info"),
      bslib::value_box(title = tr("Coeficiente T", "Coefficient T"), value = shiny::textOutput(ns("T")), shiny::textOutput(ns("T_desc")), theme = "info")
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico"), height = "420px"))),
      bslib::card(
        bslib::card_header(tr("Tabela de dupla entrada", "Contingency table")),
        bslib::card_body(shiny::uiOutput(ns("tabela")))
      )
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(bslib::card_header(tr("Medidas de associação", "Measures of association")), bslib::card_body(shiny::uiOutput(ns("medidas")))),
      cartao_cuidados(tr("Cuidados", "Watch out"), cuidados_quali_quali())
    ),
    bslib::card(bslib::card_header(rotulo_entenda()), bslib::card_body(texto_quali_quali()))
  )
}

mod_quali_quali_server <- function(id, modo = shiny::reactive("light")) {
  shiny::moduleServer(id, function(input, output, session) {

    exemplo <- shiny::reactive(exemplos_quali_quali[[input$exemplo]])

    tabela <- shiny::reactive({
      input$sortear
      gerar_quali_quali(exemplo(), input$forca, input$n)
    })

    assoc <- shiny::reactive(associacao_quali(tabela()))

    output$qui2 <- shiny::renderText(fmt_num(assoc()$qui2, 2))
    output$qui2_desc <- shiny::renderText(sprintf(tr("Tabela %d × %d, n = %s", "%d × %d table, n = %s"), assoc()$r, assoc()$s, fmt_num(assoc()$n, 0)))
    output$C <- shiny::renderText(fmt_num(assoc()$C, 3))
    output$C_desc <- shiny::renderText(sprintf(tr("Máximo nesta tabela: %s", "Maximum for this table: %s"), fmt_num(assoc()$C_max, 3)))
    output$T <- shiny::renderText(fmt_num(assoc()$T, 3))
    output$T_desc <- shiny::renderText(sprintf(tr("Máximo nesta tabela: %s", "Maximum for this table: %s"), fmt_num(assoc()$T_max, 3)))

    output$tabela <- shiny::renderUI({
      shiny::tagList(
        tabela_dupla_html(tabela(), input$conteudo, exemplo()$nome_linhas),
        shiny::p(class = "small text-muted mb-0", sprintf(
          tr(
            "Linhas: %s (explicativa). Colunas: %s (resposta). Dados simulados, com proporções ilustrativas.",
            "Rows: %s (explanatory). Columns: %s (response). Simulated data with illustrative proportions."
          ),
          tolower(exemplo()$nome_linhas), tolower(exemplo()$nome_colunas)
        ))
      )
    })

    output$medidas <- shiny::renderUI({
      a <- assoc()
      tabela <- linha_estatistica(
        tr(
          c("Tamanho da amostra (n)", "Dimensão (r × s)", "Qui-quadrado (χ²)", "χ²/n", "Coeficiente de contingência (C)", "Coeficiente T"),
          c("Sample size (n)", "Size (r × s)", "Chi-squared (χ²)", "χ²/n", "Contingency coefficient (C)", "Coefficient T")
        ),
        c(
          fmt_num(a$n, 0), sprintf("%d × %d", a$r, a$s), fmt_num(a$qui2, 2), fmt_num(a$qui2 / a$n, 3),
          fmt_num(a$C, 3), fmt_num(a$T, 3)
        ),
        c(
          tr("Número de observações", "Number of observations"),
          tr("Linhas e colunas da tabela", "Rows and columns of the table"),
          tr(
            "Soma de (observada − esperada)² / esperada; 0 sob independência perfeita, e cresce com n",
            "Sum of (observed − expected)² / expected; 0 under perfect independence, and grows with n"
          ),
          sprintf(
            tr("Não depende de n; vai de 0 a min(r, s) − 1 = %d", "Does not depend on n; ranges from 0 to min(r, s) − 1 = %d"),
            min(a$r, a$s) - 1L
          ),
          sprintf(
            tr("Entre 0 e %s nesta tabela: associação %s", "Between 0 and %s for this table: %s association"),
            fmt_num(a$C_max, 3), leitura_forca(a$C, a$C_max)
          ),
          sprintf(
            tr("Entre 0 e %s nesta tabela: associação %s", "Between 0 and %s for this table: %s association"),
            fmt_num(a$T_max, 3), leitura_forca(a$T, a$T_max)
          )
        )
      )
      names(tabela) <- nomes_estatisticas()
      shiny::tagList(
        tabela_html(tabela, classes = c("", "num", "leitura")),
        shiny::p(class = "small text-muted mt-2 mb-0", tr(
          paste(
            "Rótulos de força (fraca, moderada, forte) pela fração do máximo possível: abaixo de 10%, 30% e 60%.",
            "São uma regra prática do app, não dos livros."
          ),
          paste(
            "Strength labels (weak, moderate, strong) use the share of the maximum possible: below 10%, 30% and 60%.",
            "They are a rule of thumb used in this app, not taken from the books."
          )
        ))
      )
    })

    output$grafico <- shiny::renderPlot({
      m <- tabela()
      ex <- exemplo()
      pal <- paleta_modo(modo())

      if (identical(input$grafico, "desvios")) {
        esperado <- outer(rowSums(m), colSums(m)) / sum(m)
        d <- data.frame(
          linha = factor(rep(rownames(m), ncol(m)), levels = rev(rownames(m))),
          coluna = factor(rep(colnames(m), each = nrow(m)), levels = colnames(m)),
          obs = as.vector(m), esp = as.vector(esperado)
        )
        d$residuo <- ifelse(d$esp > 0, (d$obs - d$esp) / sqrt(d$esp), 0)
        d$rotulo <- sprintf("%s\n(%s %s)", fmt_num(d$obs - d$esp, 1), tr("esp.", "exp."), fmt_num(d$esp, 1))
        limite <- max(2, abs(d$residuo))
        # Texto branco nas caselas de cor forte, a cor do tema nas claras
        d$cor_texto <- ifelse(abs(d$residuo) > limite / 2, "#ffffff", pal$texto)

        return(
          ggplot2::ggplot(d, ggplot2::aes(x = coluna, y = linha, fill = residuo)) +
            ggplot2::geom_tile(colour = pal$fundo, linewidth = 1.5) +
            ggplot2::geom_text(ggplot2::aes(label = rotulo, colour = cor_texto), size = 4.5, lineheight = 0.9) +
            ggplot2::scale_colour_identity() +
            ggplot2::scale_fill_gradient2(
              low = cores_app$referencia, mid = pal$fundo, high = cores_app$destaque,
              midpoint = 0, limits = c(-limite, limite)
            ) +
            ggplot2::scale_x_discrete(position = "top") +
            ggplot2::labs(
              title = tr("Observado menos esperado", "Observed minus expected"),
              subtitle = ex$rotulo,
              x = NULL, y = NULL,
              caption = tr(
                paste(
                  "Azul: mais casos que o esperado sob independência; vermelho: menos.\n",
                  "A cor segue (obs. − esp.) / √esp., e a soma desses valores ao quadrado é o χ²."
                ),
                paste(
                  "Blue: more cases than expected under independence; red: fewer.\n",
                  "Color follows (obs. − exp.) / √exp., and the sum of these values squared is χ²."
                )
              )
            ) +
            tema_grafico(modo()) +
            ggplot2::theme(panel.grid = ggplot2::element_blank(), axis.text = ggplot2::element_text(size = 13, colour = pal$texto))
        )
      }

      rotulo_total <- tr("Total", "Total")
      perfis <- rbind(m / rowSums(m), colSums(m) / sum(m))
      rownames(perfis) <- c(rownames(m), rotulo_total)
      d <- data.frame(
        linha = factor(rep(rownames(perfis), ncol(perfis)), levels = rev(rownames(perfis))),
        coluna = factor(rep(colnames(perfis), each = nrow(perfis)), levels = rev(colnames(perfis))),
        prop = as.vector(perfis)
      )
      d$prop[is.nan(d$prop)] <- 0
      d$rotulo <- ifelse(d$prop >= 0.06, fmt_pct(d$prop, 0), "")
      cores <- stats::setNames(cores_categorias(ncol(m)), colnames(m))
      # Texto escuro sobre a cor clara (amarela), branco sobre as demais
      cores_texto <- stats::setNames(ifelse(cores == "#f4bd61", "#212529", "#ffffff"), colnames(m))

      ggplot2::ggplot(d, ggplot2::aes(x = prop, y = linha, fill = coluna)) +
        ggplot2::geom_col(width = 0.7, colour = pal$fundo) +
        ggplot2::geom_text(ggplot2::aes(label = rotulo, colour = coluna), position = ggplot2::position_stack(vjust = 0.5), size = 4.3) +
        ggplot2::geom_hline(yintercept = 1.5, colour = pal$secundario, linetype = "dotted") +
        ggplot2::scale_fill_manual(values = cores, breaks = colnames(m), name = NULL) +
        ggplot2::scale_colour_manual(values = cores_texto, guide = "none") +
        ggplot2::scale_x_continuous(labels = \(v) fmt_pct(v, 0), expand = ggplot2::expansion(mult = c(0, 0.02))) +
        ggplot2::labs(
          title = sprintf(tr("%s em cada categoria de %s", "%s within each category of %s"), ex$nome_colunas, tolower(ex$nome_linhas)),
          subtitle = ex$rotulo,
          x = tr("% na linha", "Row %"), y = NULL,
          caption = sprintf(
            tr(
              "Dados simulados, n = %s | Sob independência, todas as barras teriam o perfil da barra Total",
              "Simulated data, n = %s | Under independence, every bar would match the Total bar"
            ),
            fmt_num(sum(m), 0)
          )
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(legend.position = "top", legend.justification = "left", legend.text = ggplot2::element_text(size = 12), panel.grid.major.y = ggplot2::element_blank())
    })
  })
}
