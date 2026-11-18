# =============================================================================
# Script:  exportar_app.R (exporta o app Shiny para site estático via Shinylive)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-18
#
# Entrada:
#   - _app_scripts/app_estatistica/ (app.R e modulos/)
# Saída:
#   - apps/estatistica/ (HTML + webR, publicado pelo GitHub Pages)
#   - apps/estatistica/metadados.txt
# =============================================================================

rm(list = ls()); gc()
cat("\014")
options(scipen = 999)
options(timeout = 600) # os pacotes webR são baixados de repo.r-wasm.org e podem ser lentos

pacman::p_load(here, shinylive, httpuv)

origem  <- here::here("_app_scripts", "app_estatistica")
destino <- here::here("apps", "estatistica")

# Preserva a data da primeira exportação antes de apagar a pasta de destino
ler_data_criacao <- function(destino) {
  arquivo <- file.path(destino, "metadados.txt")
  if (!file.exists(arquivo)) return(format(Sys.Date()))
  linha <- grep("^Criado em:", readLines(arquivo, encoding = "UTF-8"), value = TRUE)
  if (length(linha) == 0) format(Sys.Date()) else trimws(sub("^Criado em:", "", linha[1]))
}

exportar_app <- function(origem, destino) {
  if (!dir.exists(origem)) stop("Pasta do app não encontrada: ", origem)
  if (dir.exists(destino)) unlink(destino, recursive = TRUE)
  if (!dir.exists(dirname(destino))) dir.create(dirname(destino), recursive = TRUE)
  shinylive::export(appdir = origem, destdir = destino)
  invisible(destino)
}

gravar_metadados <- function(destino, criado_em) {
  writeLines(
    c(
      "App: Estatística na prática",
      "Autor: Thiago Meireles",
      paste("Criado em:", criado_em),
      paste("Modificado em:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
      paste("Fonte: _app_scripts/app_estatistica/"),
      paste("Gerado por: _app_scripts/shinylive/exportar_app.R"),
      paste("shinylive (pacote R):", as.character(utils::packageVersion("shinylive"))),
      paste("R:", R.version.string)
    ),
    file.path(destino, "metadados.txt"),
    useBytes = TRUE
  )
}

criado_em <- ler_data_criacao(destino)
exportar_app(origem, destino)
gravar_metadados(destino, criado_em)

# Pré-visualização local (só em sessão interativa). Abre http://127.0.0.1:7446
if (interactive()) httpuv::runStaticServer(destino, port = 7446)
