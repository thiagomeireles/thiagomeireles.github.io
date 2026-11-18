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

pacman::p_load(here, shinylive, httpuv, bslib, htmltools, purrr, jsonlite)

origem  <- here::here("_app_scripts", "app_estatistica")
destino <- here::here("apps", "estatistica")

# Preserva a data da primeira exportação antes de apagar a pasta de destino
ler_data_criacao <- function(destino) {
  arquivo <- file.path(destino, "metadados.txt")
  if (!file.exists(arquivo)) return(format(Sys.Date()))
  linha <- grep("^Criado em:", readLines(arquivo, encoding = "UTF-8"), value = TRUE)
  if (length(linha) == 0) format(Sys.Date()) else trimws(sub("^Criado em:", "", linha[1]))
}

# Cópia do app numa pasta temporária, para o CSS compilado não sujar o código-fonte
preparar_copia <- function(origem) {
  copia <- file.path(tempdir(), "app_estatistica")
  if (dir.exists(copia)) unlink(copia, recursive = TRUE)
  dir.create(copia, recursive = TRUE)
  file.copy(list.files(origem, full.names = TRUE), copia, recursive = TRUE)
  copia
}

# Arquivos locais citados em url(...) dentro de um CSS (fontes, ícones)
urls_locais_css <- function(arquivo_css) {
  css <- paste(readLines(arquivo_css, warn = FALSE), collapse = "\n")
  urls <- regmatches(css, gregexpr("url\\([\"']?[^\"')]+", css))[[1]]
  urls <- sub("^url\\([\"']?", "", urls)
  unique(urls[!grepl("^(data:|https?:|#|/)", urls)])
}

# Compila o tema definido em tema_zephyr() (modulos/utilitarios.R) e grava em
# <app>/tema/ só o CSS, o JS do Bootstrap e as fontes que o CSS usa
compilar_tema <- function(pasta_app) {
  ambiente <- new.env()
  sys.source(file.path(pasta_app, "modulos", "utilitarios.R"), envir = ambiente)

  deps <- bslib::bs_theme_dependencies(ambiente$tema_zephyr())
  bootstrap <- purrr::keep(deps, \(d) d$name == "bootstrap")[[1]]
  compilado <- bootstrap$src$file
  pasta_tema <- file.path(pasta_app, "tema")

  arquivos_css <- intersect(c("bootstrap.min.css", "font.css"), list.files(compilado))
  necessarios <- c(
    arquivos_css,
    "bootstrap.bundle.min.js",
    purrr::map(file.path(compilado, arquivos_css), urls_locais_css) |> unlist()
  ) |>
    unique() |>
    purrr::keep(\(f) file.exists(file.path(compilado, f)))

  purrr::walk(necessarios, \(f) {
    alvo <- file.path(pasta_tema, f)
    if (!dir.exists(dirname(alvo))) dir.create(dirname(alvo), recursive = TRUE)
    file.copy(file.path(compilado, f), alvo, overwrite = TRUE)
  })

  message(sprintf("Tema compilado: %d arquivos em tema/", length(necessarios)))
  invisible(pasta_tema)
}

exportar_app <- function(pasta_app, destino) {
  if (dir.exists(destino)) unlink(destino, recursive = TRUE)
  if (!dir.exists(dirname(destino))) dir.create(dirname(destino), recursive = TRUE)
  shinylive::export(appdir = pasta_app, destdir = destino)
  invisible(destino)
}

# Versão de um pacote dentro da biblioteca base do webR exportada
versao_pacote_webr <- function(destino, pacote) {
  pasta_webr <- file.path(destino, "shinylive", "webr")
  arquivo_dados <- file.path(pasta_webr, "library.data.gz")
  dados <- memDecompress(readBin(arquivo_dados, "raw", file.info(arquivo_dados)$size), "gzip")
  indice <- jsonlite::fromJSON(file.path(pasta_webr, "library.js.metadata"))$files
  linha <- indice[indice$filename == paste0("/", pacote, "/DESCRIPTION"), ]
  if (nrow(linha) == 0) return(NA_character_)
  descricao <- strsplit(rawToChar(dados[(linha$start + 1):linha$end]), "\n")[[1]]
  trimws(sub("^Version:", "", grep("^Version:", descricao, value = TRUE)))
}

# O CSS do tema é compilado com o bslib local; os componentes rodam com o bslib
# do webR. Versões diferentes podem causar pequenas diferenças visuais.
verificar_bslib <- function(destino) {
  local <- as.character(utils::packageVersion("bslib"))
  webr  <- versao_pacote_webr(destino, "bslib")
  if (!identical(local, webr)) {
    warning(sprintf(
      "bslib local (%s) difere do bslib do webR (%s). Para alinhar: remotes::install_version('bslib', '%s')",
      local, webr, webr
    ), call. = FALSE)
  }
  invisible(c(local = local, webr = webr))
}

gravar_metadados <- function(destino, criado_em, versoes_bslib) {
  writeLines(
    c(
      "App: Estatística na prática",
      "Autor: Thiago Meireles",
      paste("Criado em:", criado_em),
      paste("Modificado em:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
      "Fonte: _app_scripts/app_estatistica/",
      "Gerado por: _app_scripts/shinylive/exportar_app.R",
      "Tema: Bootswatch zephyr (CSS pré-compilado em tema/)",
      paste("shinylive (pacote R):", as.character(utils::packageVersion("shinylive"))),
      paste("bslib local / webR:", versoes_bslib[["local"]], "/", versoes_bslib[["webr"]]),
      paste("R:", R.version.string)
    ),
    file.path(destino, "metadados.txt"),
    useBytes = TRUE
  )
}

criado_em <- ler_data_criacao(destino)
copia <- preparar_copia(origem)
compilar_tema(copia)
exportar_app(copia, destino)
versoes_bslib <- verificar_bslib(destino)
gravar_metadados(destino, criado_em, versoes_bslib)

# Pré-visualização local (só em sessão interativa). Abre http://127.0.0.1:7446
if (interactive()) httpuv::runStaticServer(destino, port = 7446)
