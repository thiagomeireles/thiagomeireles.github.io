# =============================================================================
# Script:  mod_ramo_folhas.R (módulo: diagrama de ramo-e-folhas, simples e lado a lado)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-25
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/apps/distribuicoes-frequencia/app.R)
# Saída:
#   - funções mod_ramo_folhas_ui() e mod_ramo_folhas_server()
# =============================================================================

# Conjuntos do diagrama. `unidade` é o valor de uma folha: 0,1 nos salários
# (4,56 vira ramo 4, folha 5) e 1 nas notas e alturas (172 vira ramo 17, folha 2).
# Em `grupos`, dois conjuntos para o diagrama lado a lado.
exemplos_ramo <- list(
  salarios = list(
    rotulo = tr("Salários da Cia. MB (n = 36)", "Salaries at MB Co. (n = 36)"),
    unidade = 0.1, medida = tr("salários mínimos", "minimum wages"),
    gerar = function() list(salarios_mb)
  ),
  notas = list(
    rotulo = tr("Notas de uma prova (simuladas, n = 40)", "Exam scores (simulated, n = 40)"),
    unidade = 1, medida = tr("pontos", "points"),
    gerar = function() list(pmin(100, pmax(20, round(stats::rnorm(40, 68, 14)))))
  ),
  altura = list(
    rotulo = tr("Altura de adultos (simulada, n = 60)", "Adult height (simulated, n = 60)"),
    unidade = 1, medida = "cm",
    gerar = function() list(round(stats::rnorm(60, 169, 9)))
  ),
  altura_sexo = list(
    rotulo = tr("Altura: mulheres × homens (simulada, lado a lado)", "Height: women × men (simulated, back-to-back)"),
    unidade = 1, medida = "cm",
    grupos = tr(c("Mulheres", "Homens"), c("Women", "Men")),
    gerar = function() list(round(stats::rnorm(40, 162, 7)), round(stats::rnorm(40, 176, 7)))
  )
)

# Ramo e folha de cada valor. As folhas são truncadas, não arredondadas
# (4,56 fica com folha 5). O 1e-9 evita que 4,6/0,1 vire 45,999...
partes_ramo <- function(x, unidade) {
  v <- floor(x / unidade + 1e-9)
  data.frame(ramo = v %/% 10, folha = v %% 10)
}

# Linhas do diagrama: cada ramo se divide em `por_ramo` linhas (1, 2 ou 5),
# cada uma com uma faixa de folhas (2 linhas: 0–4 e 5–9; 5 linhas: 0–1, 2–3...).
# Linhas vazias no meio continuam no diagrama, porque o vazio também é forma.
linhas_ramo <- function(partes, por_ramo, ramos) {
  largura_faixa <- 10 / por_ramo
  grade <- expand.grid(sub = seq_len(por_ramo) - 1, ramo = ramos)
  grade$folhas <- purrr::map2_chr(grade$ramo, grade$sub, \(r, s) {
    f <- sort(partes$folha[partes$ramo == r & partes$folha %/% largura_faixa == s])
    paste(f, collapse = "")
  })
  grade
}

# Texto do diagrama simples ou lado a lado (o grupo da esquerda tem as folhas
# espelhadas, crescendo do ramo para fora)
desenhar_ramo <- function(valores, unidade, por_ramo) {
  partes <- purrr::map(valores, partes_ramo, unidade = unidade)
  todos <- do.call(rbind, partes)
  ramos <- seq(min(todos$ramo), max(todos$ramo))
  linhas <- purrr::map(partes, linhas_ramo, por_ramo = por_ramo, ramos = ramos)
  marca <- if (por_ramo == 1) "" else if (por_ramo == 2) c("*", ".")[linhas[[1]]$sub + 1] else c("*", "t", "f", "s", ".")[linhas[[1]]$sub + 1]
  rotulo_ramo <- formatC(paste0(linhas[[1]]$ramo, marca), width = max(nchar(paste0(ramos, "*"))))

  if (length(valores) == 1) {
    return(paste(rotulo_ramo, "|", linhas[[1]]$folhas))
  }
  esquerda <- purrr::map_chr(strsplit(linhas[[1]]$folhas, ""), \(f) paste(rev(f), collapse = ""))
  esquerda <- formatC(esquerda, width = max(1, nchar(esquerda)))
  paste(esquerda, "|", rotulo_ramo, "|", linhas[[2]]$folhas)
}

texto_ramo <- function() {
  tr(
    shiny::tagList(
      shiny::p(
        "O ramo-e-folhas é um histograma deitado que guarda os próprios dados. Cada valor se divide em duas partes:",
        "os primeiros dígitos formam o ramo, à esquerda da barra, e o dígito seguinte forma a folha, à direita. O",
        "salário de 4,56 salários mínimos vira ramo 4 e folha 5. As folhas de um mesmo ramo ficam em fila, e o",
        "comprimento da fila faz o papel da altura do retângulo."
      ),
      shiny::p(
        "Ao contrário do histograma, o diagrama ainda permite ler cada valor (com a precisão da folha), achar o menor",
        "e o maior e contar até a posição \\((n + 1)/2\\) para chegar à mediana. Funciona bem com poucas dezenas de",
        "observações; com centenas, as filas ficam longas demais."
      ),
      shiny::p(
        "Quando os ramos são poucos, o diagrama fica achatado. A saída é dividir cada ramo em duas linhas (folhas de",
        "0 a 4 com *, de 5 a 9 com .) ou em cinco. Morettin e Bussab descrevem essa escolha como um equilíbrio: poucas",
        "linhas destacam o padrão geral dos dados; muitas destacam os desvios em torno dele. Para comparar dois",
        "grupos, Agresti e Finlay usam o diagrama lado a lado, com um ramo comum no meio e as folhas de cada grupo",
        "para um lado."
      ),
      shiny::p(
        shiny::strong("Experimente:"),
        "na altura, passe de 1 para 2 e para 5 linhas por ramo e veja quando a forma de sino aparece. Depois compare",
        "mulheres e homens lado a lado."
      )
    ),
    shiny::tagList(
      shiny::p(
        "A stem-and-leaf plot is a histogram turned on its side that keeps the data themselves. Each value is split",
        "in two: the leading digits form the stem, to the left of the bar, and the next digit forms the leaf, to the",
        "right. A salary of 4.56 minimum wages becomes stem 4 and leaf 5. The leaves of a stem line up in a row, and",
        "the length of the row plays the role of the bar's height."
      ),
      shiny::p(
        "Unlike a histogram, the plot still lets you read each value (to the precision of the leaf), find the",
        "smallest and largest and count up to position \\((n + 1)/2\\) to reach the median. It works well with a few",
        "dozen observations; with hundreds, the rows get too long."
      ),
      shiny::p(
        "When there are few stems, the plot is squashed. The way out is to split each stem into two lines (leaves",
        "0 to 4 marked *, 5 to 9 marked .) or into five. Morettin and Bussab describe this choice as a balance: few",
        "lines bring out the overall pattern of the data; many bring out the deviations around it. To compare two",
        "groups, Agresti and Finlay use a back-to-back plot, with a shared stem in the middle and each group's",
        "leaves going out to one side."
      ),
      shiny::p(
        shiny::strong("Try it:"),
        "for height, go from 1 to 2 to 5 lines per stem and see when the bell shape appears. Then compare women and",
        "men back-to-back."
      )
    )
  )
}

mod_ramo_folhas_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::selectInput(
        ns("exemplo"), tr("Dados", "Data"),
        choices = stats::setNames(names(exemplos_ramo), purrr::map_chr(exemplos_ramo, "rotulo"))
      ),
      shiny::radioButtons(
        ns("por_ramo"), tr("Linhas por ramo", "Lines per stem"),
        choices = stats::setNames(c(1, 2, 5), tr(c("1 (folhas 0–9)", "2 (0–4 e 5–9)", "5 (0–1, 2–3, ...)"), c("1 (leaves 0–9)", "2 (0–4 and 5–9)", "5 (0–1, 2–3, ...)"))),
        selected = 1
      ),
      shiny::conditionalPanel(
        "input.exemplo != 'salarios'", ns = ns,
        shiny::actionButton(ns("sortear"), tr("Sortear novos dados", "Draw new data"), class = "btn-primary", icon = shiny::icon("dice"))
      )
    ),
    shiny::uiOutput(ns("caixas")),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)),
      bslib::card(
        bslib::card_header(tr("Ramo-e-folhas", "Stem-and-leaf plot")),
        bslib::card_body(
          shiny::uiOutput(ns("diagrama")),
          shiny::p(class = "small text-muted mb-0", shiny::textOutput(ns("legenda"), inline = TRUE))
        )
      ),
      bslib::card(bslib::card_header(rotulo_entenda()), bslib::card_body(shiny::withMathJax(texto_ramo())))
    )
  )
}

mod_ramo_folhas_server <- function(id, modo = shiny::reactive("light")) {
  shiny::moduleServer(id, function(input, output, session) {

    exemplo <- shiny::reactive(exemplos_ramo[[input$exemplo]])

    valores <- shiny::reactive({
      input$sortear
      exemplo()$gerar()
    })

    output$diagrama <- shiny::renderUI({
      linhas <- desenhar_ramo(valores(), exemplo()$unidade, as.integer(input$por_ramo))
      shiny::tags$pre(
        style = "font-size: 1.05rem; line-height: 1.45; background: var(--bs-tertiary-bg); padding: 0.75rem; border-radius: 0.375rem;",
        paste(linhas, collapse = "\n")
      )
    })

    output$legenda <- shiny::renderText({
      ex <- exemplo()
      lados <- if (!is.null(ex$grupos)) {
        sprintf(tr("À esquerda do ramo: %s; à direita: %s. ", "Left of the stem: %s; right: %s. "), ex$grupos[1], ex$grupos[2])
      } else ""
      paste0(lados, legenda_unidade(ex))
    })

    legenda_unidade <- function(ex) {
      if (ex$unidade == 1) {
        sprintf(tr("Unidade da folha: 1 %s. 17 | 2 = 172 %s.", "Leaf unit: 1 %s. 17 | 2 = 172 %s."), ex$medida, ex$medida)
      } else {
        sprintf(
          tr("Unidade da folha: 0,1 %s. 4 | 5 = 4,5 %s. Folhas truncadas: 4,56 entra como 4,5.", "Leaf unit: 0.1 %s. 4 | 5 = 4.5 %s. Leaves are truncated: 4.56 enters as 4.5."),
          ex$medida, ex$medida
        )
      }
    }

    output$caixas <- shiny::renderUI({
      v <- valores()
      ex <- exemplo()
      dig <- if (ex$unidade == 1) 0 else 2
      resumo <- function(x) sprintf("%s | %s | %s", fmt_num(min(x), dig), fmt_num(stats::median(x), dig), fmt_num(max(x), dig))
      if (length(v) == 1) {
        x <- v[[1]]
        n <- length(x)
        bslib::layout_columns(
          fill = FALSE,
          bslib::value_box(title = tr("Observações", "Observations"), value = fmt_num(n, 0), ex$medida, theme = "primary"),
          bslib::value_box(
            title = tr("Mediana", "Median"), value = fmt_num(stats::median(x), dig),
            sprintf(tr("Posição (n + 1)/2 = %s na lista ordenada", "Position (n + 1)/2 = %s in the sorted list"), fmt_num((n + 1) / 2, 1)),
            theme = "info"
          ),
          bslib::value_box(title = tr("Menor e maior", "Smallest and largest"), value = sprintf("%s – %s", fmt_num(min(x), dig), fmt_num(max(x), dig)), tr("Primeira e última folha", "First and last leaf"), theme = NULL)
        )
      } else {
        bslib::layout_columns(
          fill = FALSE,
          bslib::value_box(title = sprintf(tr("%s: mín. | mediana | máx.", "%s: min | median | max"), ex$grupos[1]), value = resumo(v[[1]]), sprintf("n = %d", length(v[[1]])), theme = "primary"),
          bslib::value_box(title = sprintf(tr("%s: mín. | mediana | máx.", "%s: min | median | max"), ex$grupos[2]), value = resumo(v[[2]]), sprintf("n = %d", length(v[[2]])), theme = "info")
        )
      }
    })
  })
}
