# =============================================================================
# Script:  montar_apps.R (monta os apps de tema dentro da imagem Docker)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-19
#
# Entrada:
#   - <origem>: pasta com comum/ e uma pasta por tema (app.R + modulos/)
#   - <temas>: apps.yml (cópia de _data/apps.yml), com slug e slug_en
# Saída:
#   - <destino>/<slug>/ (português) e <destino>/en/<slug_en ou slug>/ (inglês),
#     cada um com comum/, idioma.txt e metadados.txt
# =============================================================================

rm(list = ls()); gc()
cat("\014")
options(scipen = 999)

# Roda no build do Docker, fora do projeto RStudio (sem here): os caminhos
# chegam como argumentos. Uso: Rscript montar_apps.R <origem> <temas> <destino>
argumentos <- commandArgs(trailingOnly = TRUE)
if (length(argumentos) != 3) stop("Uso: Rscript montar_apps.R <origem> <apps.yml> <destino>")
origem  <- argumentos[1]
temas   <- yaml::read_yaml(argumentos[2])
destino <- argumentos[3]

# Cada tema sai em dois idiomas, nos mesmos endereços usados no site
versoes_tema <- function(tema) {
  list(
    list(pasta = tema$slug, titulo = tema$titulo, idioma = "pt", subdir = tema$slug),
    list(pasta = tema$slug, titulo = tema$title,  idioma = "en", subdir = file.path("en", tema$slug_en %||% tema$slug))
  )
}

gravar_metadados <- function(versao, alvo) {
  writeLines(
    c(
      paste("App:", versao$titulo, "(Estatística na prática)"),
      "Autor: Thiago Meireles",
      paste("Idioma:", versao$idioma),
      paste("Montado em:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
      paste0("Fonte: _app_scripts/apps/", versao$pasta, "/ (+ _app_scripts/apps/comum/)"),
      "Gerado por: _app_scripts/hf-space/montar_apps.R (build do Docker)",
      paste("R:", R.version.string),
      paste("shiny / bslib / ggplot2:", paste(
        purrr::map_chr(c("shiny", "bslib", "ggplot2"), \(p) as.character(utils::packageVersion(p))),
        collapse = " / "
      ))
    ),
    file.path(alvo, "metadados.txt"),
    useBytes = TRUE
  )
}

# Copia o app, o modelo comum e grava o idioma. O app.R procura comum/ dentro
# da própria pasta, e utilitarios.R lê idioma.txt.
montar_versao <- function(versao, origem, destino) {
  fonte <- file.path(origem, versao$pasta)
  if (!file.exists(file.path(fonte, "app.R"))) stop("Tema sem app.R: ", fonte)

  alvo <- file.path(destino, versao$subdir)
  if (dir.exists(alvo)) unlink(alvo, recursive = TRUE)
  dir.create(alvo, recursive = TRUE)

  file.copy(list.files(fonte, full.names = TRUE), alvo, recursive = TRUE)
  file.copy(file.path(origem, "comum"), alvo, recursive = TRUE)
  writeLines(versao$idioma, file.path(alvo, "idioma.txt"))
  gravar_metadados(versao, alvo)

  message("Montado: /", versao$subdir, "/")
  invisible(alvo)
}

if (!dir.exists(destino)) dir.create(destino, recursive = TRUE)

temas |>
  purrr::map(versoes_tema) |>
  purrr::list_flatten() |>
  purrr::walk(montar_versao, origem = origem, destino = destino)
