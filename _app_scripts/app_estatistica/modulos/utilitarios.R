# =============================================================================
# Script:  utilitarios.R (tema visual e funções auxiliares do app)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-18
#
# Entrada:
#   - tema/ (opcional): CSS do zephyr pré-compilado por
#     _app_scripts/shinylive/exportar_app.R
# Saída:
#   - objetos em memória: cores_app, tema_zephyr(), tema_app(),
#     dependencia_tema(), estilo_app(), paleta_modo(), tema_grafico(), fmt_num()
# =============================================================================

# Cores do Bootswatch zephyr usadas nos gráficos (primary e danger)
cores_app <- list(
  destaque   = "#3459e6",
  referencia = "#da292e",
  neutro     = "#adb5bd"
)

# Definição única do tema. O exportar_app.R também lê esta função para
# compilar o CSS antes de publicar.
tema_zephyr <- function() {
  bslib::bs_theme(preset = "zephyr")
}

# Compilar o Sass do zephyr no navegador (webR) custaria vários segundos a cada
# abertura. Na exportação, o CSS é compilado uma vez e gravado em tema/. Quando
# essa pasta existe, o app usa o tema padrão do bslib (pré-compilado, sem custo)
# e troca o CSS do Bootstrap pelo do zephyr via dependencia_tema(). Rodando
# localmente, sem tema/, o zephyr é compilado normalmente.
tema_app <- function() {
  if (dir.exists("tema")) bslib::bs_theme() else tema_zephyr()
}

# Versão alta de propósito: entre dependências com o mesmo nome, o htmltools
# mantém a de maior versão, então esta substitui o Bootstrap padrão.
dependencia_tema <- function() {
  if (!dir.exists("tema")) return(NULL)
  htmltools::htmlDependency(
    name       = "bootstrap",
    version    = "999.0.0",
    src        = c(file = normalizePath("tema")),
    stylesheet = "bootstrap.min.css",
    script     = "bootstrap.bundle.min.js"
  )
}

# Ajustes que o tema não cobre: o slider do Shiny é compilado contra o tema da
# sessão (padrão), então recebe a cor do zephyr aqui
estilo_app <- function() {
  shiny::tags$style(shiny::HTML("
    .irs.irs--shiny .irs-bar { background: #3459e6; border-top-color: #3459e6; border-bottom-color: #3459e6; }
    .irs.irs--shiny .irs-single, .irs.irs--shiny .irs-from, .irs.irs--shiny .irs-to { background-color: #3459e6; }
    .navbar .dropdown-item.disabled { color: var(--bs-secondary-color); }
  "))
}

# Cores dos gráficos conforme o modo (claro ou escuro) escolhido no app
paleta_modo <- function(modo = "light") {
  if (identical(modo, "dark")) {
    list(
      fundo = "#212529", texto = "#dee2e6", secundario = "#adb5bd",
      grade = "#343a40", linha = "#f8f9fa", trajetorias = c(0.45, 1)
    )
  } else {
    list(
      fundo = "#ffffff", texto = "#495057", secundario = "#6c757d",
      grade = "#e9ecef", linha = "#212529", trajetorias = c(0.1, 0.75)
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

# Números no padrão brasileiro (vírgula decimal)
fmt_num <- function(x, digitos = 4) {
  formatC(x, format = "f", digits = digitos, decimal.mark = ",", big.mark = ".")
}
