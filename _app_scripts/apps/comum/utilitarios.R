# =============================================================================
# Script:  utilitarios.R (modelo comum a todos os apps de tema)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-19
#
# Entrada:
#   - idioma.txt (opcional): "pt" ou "en", gravado por
#     _app_scripts/hf-space/montar_apps.R
# Saída:
#   - objetos em memória: idioma, tr(), cores_app, tema_zephyr(), estilo_app(),
#     texto_execucao(), pagina_tema(), item_em_breve(),
#     modo_app(), paleta_modo(), tema_grafico(), fmt_num(), fmt_pct(),
#     sep_milhar(), tabela_html()
# =============================================================================

# Fica em _app_scripts/apps/comum/. Na montagem do Space, esta pasta é copiada
# para dentro de cada app; rodando localmente, o app.R a procura em ../comum.
# Por ser carregado antes dos módulos, define o idioma usado por todos eles.

# Idioma do app: idioma.txt (montagem) ou variável APP_IDIOMA (teste local)
idioma_app <- function() {
  if (file.exists("idioma.txt")) return(trimws(readLines("idioma.txt", warn = FALSE))[1])
  Sys.getenv("APP_IDIOMA", "pt")
}

idioma <- idioma_app()

# Texto no idioma do app. Os dois textos ficam lado a lado no código, para a
# tradução nunca se perder do original. Funciona com vetores e listas.
tr <- function(pt, en) {
  if (identical(idioma, "en")) en else pt
}

# Cores do Bootswatch zephyr usadas nos gráficos (primary e danger)
cores_app <- list(
  destaque   = "#3459e6",
  referencia = "#da292e",
  neutro     = "#adb5bd"
)

url_temas <- tr("https://thiagomeireles.com.br/aplicativos/", "https://thiagomeireles.com.br/en/apps/")

# Definição única do tema. O Dockerfile compila este mesmo preset no build,
# e os apps reaproveitam o cache do sass.
tema_zephyr <- function() {
  bslib::bs_theme(preset = "zephyr")
}

# Ajustes que o tema não cobre: cores do slider, itens desabilitados do menu e
# tabelas
estilo_app <- function() {
  shiny::tags$style(shiny::HTML("
    .irs.irs--shiny .irs-bar { background: #3459e6; border-top-color: #3459e6; border-bottom-color: #3459e6; }
    .irs.irs--shiny .irs-single, .irs.irs--shiny .irs-from, .irs.irs--shiny .irs-to { background-color: #3459e6; }
    .navbar .dropdown-item.disabled { color: var(--bs-secondary-color); }
    .tabela-app { margin-bottom: 0; }
    .tabela-app td.num, .tabela-app th.num { text-align: right; font-variant-numeric: tabular-nums; }
    .tabela-app td.leitura { color: var(--bs-secondary-color); font-size: 0.92em; }
  "))
}

# Como o app roda (aba Sobre)
texto_execucao <- function() {
  tr(
    paste(
      "O aplicativo roda em R no servidor (Shiny Server, hospedado no Hugging Face). Os dados são",
      "simulados e nada do que você escolhe aqui fica guardado. Se o servidor estiver parado, a primeira",
      "abertura pode levar até um minuto."
    ),
    paste(
      "The app runs in R on a server (Shiny Server, hosted on Hugging Face). The data are simulated",
      "and nothing you choose here is stored. If the server is asleep, the first load can take up to a minute."
    )
  )
}

pagina_sobre <- function(titulo, descricao, versao) {
  bslib::card(
    bslib::card_header(paste(tr("Sobre:", "About:"), titulo)),
    bslib::card_body(
      shiny::p(descricao),
      shiny::p(texto_execucao()),
      shiny::p(sprintf(tr("Autor: Thiago Meireles | Versão %s", "Author: Thiago Meireles | Version %s"), versao)),
      shiny::p(
        shiny::a(tr("Ver todos os temas", "See all topics"), href = url_temas, target = "_top"),
        " | ",
        shiny::a("thiagomeireles.com.br", href = tr("https://thiagomeireles.com.br", "https://thiagomeireles.com.br/en/"), target = "_blank")
      )
    )
  )
}

# Tópico planejado: aparece no menu, desabilitado, até o módulo existir
item_em_breve <- function(rotulo) {
  bslib::nav_item(
    shiny::tags$span(
      class = "dropdown-item disabled",
      rotulo, shiny::tags$small(class = "text-muted ms-1", tr("(em breve)", "(coming soon)"))
    )
  )
}

# Página padrão de um tema: os tópicos (nav_panel/nav_menu) vão em `...`.
# Para trocar de aba pelo server: bslib::nav_select("abas", "<value do nav_panel>").
# fillable = FALSE: as abas têm várias linhas de cards e rolam como página comum.
# Todo app abre na aba "Sobre" (value = "sobre"), padrão de todos os temas.
pagina_tema <- function(titulo, ..., descricao, versao = tr("0.2 (novembro de 2026)", "0.2 (November 2026)")) {
  bslib::page_navbar(
    id           = "abas",
    selected     = "sobre",
    fillable     = FALSE,
    lang         = tr("pt-BR", "en"),
    title        = titulo,
    window_title = paste(titulo, tr("| Estatística na prática", "| Statistics in practice")),
    theme        = tema_zephyr(),
    header       = estilo_app(),
    ...,
    bslib::nav_spacer(),
    bslib::nav_panel(tr("Sobre", "About"), value = "sobre", pagina_sobre(titulo, descricao, versao)),
    bslib::nav_item(shiny::tags$a(class = "nav-link", href = url_temas, target = "_top", tr("Outros temas", "Other topics"))),
    bslib::nav_item(bslib::input_dark_mode(id = "modo"))
  )
}

# Títulos de cards repetidos em todos os módulos
rotulo_entenda <- function() tr("Entenda", "Key ideas")

# Chamar no início do server. O tema do ggplot2 é definido aqui, e não no topo
# do app.R, para o ggplot2 carregar depois que a interface já apareceu na tela.
# Devolve o modo claro/escuro escolhido no botão do topo, como reativo.
modo_app <- function(input) {
  ggplot2::theme_set(tema_grafico())
  shiny::reactive(if (identical(input$modo, "dark")) "dark" else "light")
}

# Cores dos gráficos conforme o modo (claro ou escuro) escolhido no app
paleta_modo <- function(modo = "light") {
  if (identical(modo, "dark")) {
    list(
      fundo = "#212529", texto = "#dee2e6", secundario = "#adb5bd",
      grade = "#343a40", linha = "#f8f9fa", trajetorias = c(0.45, 1),
      neutro = "#6c757d"
    )
  } else {
    list(
      fundo = "#ffffff", texto = "#495057", secundario = "#6c757d",
      grade = "#e9ecef", linha = "#212529", trajetorias = c(0.1, 0.75),
      neutro = "#ced4da"
    )
  }
}

# Título à esquerda, caption à direita (padrão do projeto)
tema_grafico <- function(modo = "light") {
  p <- paleta_modo(modo)

  ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      text                  = ggplot2::element_text(colour = p$texto),
      plot.background       = ggplot2::element_rect(fill = p$fundo, colour = NA),
      panel.background      = ggplot2::element_rect(fill = p$fundo, colour = NA),
      panel.grid.major      = ggplot2::element_line(colour = p$grade),
      panel.grid.minor      = ggplot2::element_blank(),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.title    = ggplot2::element_text(size = 20, face = "bold", hjust = 0),
      plot.subtitle = ggplot2::element_text(size = 14, hjust = 0, colour = p$secundario),
      plot.caption  = ggplot2::element_text(size = 11, hjust = 1, colour = p$secundario),
      axis.title    = ggplot2::element_text(size = 14),
      axis.text     = ggplot2::element_text(size = 12, colour = p$secundario),
      legend.position = "none"
    )
}

# Números no padrão do idioma: 1.234,5 (pt) ou 1,234.5 (en)
fmt_num <- function(x, digitos = 4) {
  formatC(x, format = "f", digits = digitos, decimal.mark = tr(",", "."), big.mark = tr(".", ","))
}

# Separador de milhar dos sliders e rótulos de eixo
sep_milhar <- function() tr(".", ",")

rotulos_eixo <- function() {
  scales::label_number(big.mark = tr(".", ","), decimal.mark = tr(",", "."))
}

fmt_pct <- function(x, digitos = 1) {
  paste0(fmt_num(100 * x, digitos), "%")
}

# Tabela HTML com as classes do Bootstrap (acompanha o modo claro/escuro).
# `classes` diz a classe CSS de cada coluna ("num", "leitura" ou "").
tabela_html <- function(dados, classes = rep("", ncol(dados))) {
  celulas <- function(valores, tag) {
    purrr::map2(valores, classes, \(v, cl) tag(class = cl, v))
  }
  linhas <- purrr::map(seq_len(nrow(dados)), \(i) {
    shiny::tags$tr(celulas(as.character(unlist(dados[i, ])), shiny::tags$td))
  })
  shiny::div(
    class = "table-responsive",
    shiny::tags$table(
      class = "table table-sm table-hover tabela-app",
      shiny::tags$thead(shiny::tags$tr(celulas(names(dados), shiny::tags$th))),
      shiny::tags$tbody(linhas)
    )
  )
}
