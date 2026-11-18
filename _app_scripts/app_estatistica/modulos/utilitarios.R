# =============================================================================
# Script:  utilitarios.R (tema visual e funções auxiliares do app)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-18
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/app_estatistica/app.R)
# Saída:
#   - objetos em memória: cores_app, tema_app(), tema_grafico(), fmt_num()
# =============================================================================

# Paleta alinhada ao tema Bulma do site (primary #00d1b2)
cores_app <- list(
  destaque   = "#00d1b2",
  referencia = "#485fc7",
  texto      = "#363636",
  neutro     = "#b5b5b5"
)

tema_app <- function() {
  bslib::bs_theme(
    version   = 5,
    bg        = "#ffffff",
    fg        = cores_app$texto,
    primary   = cores_app$destaque,
    secondary = cores_app$referencia,
    base_font = bslib::font_collection(
      "BlinkMacSystemFont", "-apple-system", "Segoe UI", "Roboto",
      "Helvetica Neue", "Arial", "sans-serif"
    )
  )
}

# Título à esquerda, caption à direita (padrão do projeto)
tema_grafico <- function() {
  ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.title    = ggplot2::element_text(size = 20, face = "bold", hjust = 0),
      plot.subtitle = ggplot2::element_text(size = 14, hjust = 0, colour = "#4a4a4a"),
      plot.caption  = ggplot2::element_text(size = 11, hjust = 1, colour = "#7a7a7a"),
      axis.title    = ggplot2::element_text(size = 14),
      axis.text     = ggplot2::element_text(size = 12),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position  = "none"
    )
}

# Números no padrão brasileiro (vírgula decimal)
fmt_num <- function(x, digitos = 4) {
  formatC(x, format = "f", digits = digitos, decimal.mark = ",", big.mark = ".")
}
