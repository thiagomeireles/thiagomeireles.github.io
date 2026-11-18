# =============================================================================
# Script:  utilitarios.R (tema visual e funções auxiliares do app)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-18
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/app_estatistica/app.R)
# Saída:
#   - objetos em memória: cores_app, tema_app(), estilo_app(), tema_grafico(), fmt_num()
# =============================================================================

# Paleta alinhada ao tema Bulma do site (primary #00d1b2). O tom escuro é usado
# em fundos com texto branco (botões, value boxes) para manter o contraste.
cores_app <- list(
  destaque        = "#00d1b2",
  destaque_escuro = "#00947e",
  referencia      = "#485fc7",
  texto           = "#363636",
  neutro          = "#b5b5b5"
)

# Tema padrão do bslib: já vem pré-compilado. Qualquer customização via
# bs_theme() obriga o webR a compilar o Sass do Bootstrap no navegador a cada
# abertura (~2 s no R nativo, bem mais em WebAssembly). As cores do site entram
# por CSS em estilo_app().
tema_app <- function() {
  bslib::bs_theme()
}

estilo_app <- function() {
  shiny::tags$style(shiny::HTML("
    :root, [data-bs-theme=light] {
      --bs-primary: #00947e; --bs-primary-rgb: 0, 148, 126;
      --bs-secondary: #485fc7; --bs-secondary-rgb: 72, 95, 199;
      --bs-link-color: #00947e; --bs-link-color-rgb: 0, 148, 126;
      --bs-link-hover-color: #006b5b; --bs-link-hover-color-rgb: 0, 107, 91;
      --bs-body-color: #363636; --bs-body-color-rgb: 54, 54, 54;
      --bs-body-font-family: BlinkMacSystemFont, -apple-system, 'Segoe UI', Roboto,
        'Helvetica Neue', Arial, sans-serif;
    }
    .btn-primary {
      --bs-btn-color: #fff; --bs-btn-bg: #00947e; --bs-btn-border-color: #00947e;
      --bs-btn-hover-color: #fff; --bs-btn-hover-bg: #007a68; --bs-btn-hover-border-color: #007a68;
      --bs-btn-active-color: #fff; --bs-btn-active-bg: #006b5b; --bs-btn-active-border-color: #006b5b;
      --bs-btn-focus-shadow-rgb: 0, 148, 126;
    }
    .form-check-input:checked { background-color: #00947e; border-color: #00947e; }
    .form-check-input:focus, .form-control:focus, .form-select:focus {
      border-color: #00d1b2; box-shadow: 0 0 0 .25rem rgba(0, 209, 178, .25);
    }
    .irs.irs--shiny .irs-bar { background: #00d1b2; border-top-color: #00d1b2; border-bottom-color: #00d1b2; }
    .irs.irs--shiny .irs-single, .irs.irs--shiny .irs-from, .irs.irs--shiny .irs-to { background-color: #00947e; }
    .navbar .nav-link.active { color: #00947e !important; border-bottom-color: #00947e !important; }
  "))
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
