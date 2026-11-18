# =============================================================================
# Script:  exportar_apps.R (exporta os apps de tema para site estático via Shinylive)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-18
#
# Entrada:
#   - _app_scripts/apps/comum/ (modelo comum: tema, paleta, página padrão)
#   - _app_scripts/apps/<tema>/ (app.R e modulos/ de cada tema)
# Saída:
#   - apps/shinylive/ e apps/shinylive-sw.js (runtime webR, compartilhado)
#   - apps/<tema>/ (um app por tema, publicado pelo GitHub Pages)
#   - apps/<tema>/metadados.txt
# =============================================================================

rm(list = ls()); gc()
cat("\014")
options(scipen = 999)
options(timeout = 600) # os pacotes webR são baixados de repo.r-wasm.org e podem ser lentos

pacman::p_load(here, shinylive, httpuv, bslib, htmltools, purrr, jsonlite)

# Um app por tema. A pasta de origem e a URL publicada usam o mesmo nome.
# Para criar um tema novo: crie _app_scripts/apps/<nome>/ e inclua-o aqui.
temas <- list(
  list(pasta = "descritiva",      titulo = "Estatística descritiva"),
  list(pasta = "probabilidade",   titulo = "Probabilidade"),
  list(pasta = "teoremas-limite", titulo = "Teoremas limite")
)

origem_apps  <- here::here("_app_scripts", "apps")
destino_raiz <- here::here("apps")

# Preserva a data da primeira exportação de cada app antes de apagar a pasta
ler_data_criacao <- function(destino) {
  arquivo <- file.path(destino, "metadados.txt")
  if (!file.exists(arquivo)) return(format(Sys.Date()))
  linha <- grep("^Criado em:", readLines(arquivo, encoding = "UTF-8"), value = TRUE)
  if (length(linha) == 0) format(Sys.Date()) else trimws(sub("^Criado em:", "", linha[1]))
}

# Apaga só o que este script gera: o runtime compartilhado e as pastas dos temas
limpar_destino <- function(destino_raiz, temas) {
  alvos <- file.path(destino_raiz, c("shinylive", "shinylive-sw.js", purrr::map_chr(temas, "pasta")))
  unlink(alvos[file.exists(alvos)], recursive = TRUE)
  if (!dir.exists(destino_raiz)) dir.create(destino_raiz, recursive = TRUE)
  invisible(destino_raiz)
}

# Arquivos locais citados em url(...) dentro de um CSS (fontes, ícones)
urls_locais_css <- function(arquivo_css) {
  css <- paste(readLines(arquivo_css, warn = FALSE), collapse = "\n")
  urls <- regmatches(css, gregexpr("url\\([\"']?[^\"')]+", css))[[1]]
  urls <- sub("^url\\([\"']?", "", urls)
  unique(urls[!grepl("^(data:|https?:|#|/)", urls)])
}

# Compila uma vez o tema definido em tema_zephyr() (comum/utilitarios.R) e grava
# numa pasta temporária só o CSS, o JS do Bootstrap e as fontes que o CSS usa
compilar_tema <- function(pasta_comum) {
  ambiente <- new.env()
  sys.source(file.path(pasta_comum, "utilitarios.R"), envir = ambiente)

  deps <- bslib::bs_theme_dependencies(ambiente$tema_zephyr())
  bootstrap <- purrr::keep(deps, \(d) d$name == "bootstrap")[[1]]
  compilado <- bootstrap$src$file
  pasta_tema <- file.path(tempdir(), "tema_compilado")
  if (dir.exists(pasta_tema)) unlink(pasta_tema, recursive = TRUE)

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

  message(sprintf("Tema compilado: %d arquivos", length(necessarios)))
  pasta_tema
}

# Cópia temporária do app com comum/ e tema/ dentro, para o código-fonte
# continuar limpo e o app exportado não depender de ../comum
preparar_copia <- function(pasta, origem_apps, pasta_tema) {
  copia <- file.path(tempdir(), "apps_exportacao", pasta)
  if (dir.exists(copia)) unlink(copia, recursive = TRUE)
  dir.create(copia, recursive = TRUE)
  file.copy(list.files(file.path(origem_apps, pasta), full.names = TRUE), copia, recursive = TRUE)
  file.copy(file.path(origem_apps, "comum"), copia, recursive = TRUE)
  file.copy(pasta_tema, copia, recursive = TRUE)
  file.rename(file.path(copia, basename(pasta_tema)), file.path(copia, "tema"))
  copia
}

# Todos os temas vão para o mesmo destdir, cada um no seu subdir: o runtime
# (shinylive/) é gravado uma vez só, e o navegador o reaproveita do cache
exportar_tema <- function(tema, origem_apps, destino_raiz, pasta_tema) {
  copia <- preparar_copia(tema$pasta, origem_apps, pasta_tema)
  shinylive::export(appdir = copia, destdir = destino_raiz, subdir = tema$pasta)
  invisible(file.path(destino_raiz, tema$pasta))
}

# Versão de um pacote dentro da biblioteca base do webR exportada
versao_pacote_webr <- function(destino_raiz, pacote) {
  pasta_webr <- file.path(destino_raiz, "shinylive", "webr")
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
verificar_bslib <- function(destino_raiz) {
  local <- as.character(utils::packageVersion("bslib"))
  webr  <- versao_pacote_webr(destino_raiz, "bslib")
  if (!identical(local, webr)) {
    warning(sprintf(
      "bslib local (%s) difere do bslib do webR (%s). Para alinhar: remotes::install_version('bslib', '%s')",
      local, webr, webr
    ), call. = FALSE)
  }
  invisible(c(local = local, webr = webr))
}

gravar_metadados <- function(tema, destino_raiz, criado_em, versoes_bslib) {
  writeLines(
    c(
      paste("App:", tema$titulo, "(Estatística na prática)"),
      "Autor: Thiago Meireles",
      paste("Criado em:", criado_em),
      paste("Modificado em:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
      paste0("Fonte: _app_scripts/apps/", tema$pasta, "/ (+ _app_scripts/apps/comum/)"),
      "Gerado por: _app_scripts/shinylive/exportar_apps.R",
      "Runtime: apps/shinylive/ (compartilhado entre os temas)",
      "Tema: Bootswatch zephyr (CSS pré-compilado em tema/)",
      paste("shinylive (pacote R):", as.character(utils::packageVersion("shinylive"))),
      paste("bslib local / webR:", versoes_bslib[["local"]], "/", versoes_bslib[["webr"]]),
      paste("R:", R.version.string)
    ),
    file.path(destino_raiz, tema$pasta, "metadados.txt"),
    useBytes = TRUE
  )
}

datas_criacao <- purrr::map_chr(temas, \(t) ler_data_criacao(file.path(destino_raiz, t$pasta)))
limpar_destino(destino_raiz, temas)
pasta_tema <- compilar_tema(file.path(origem_apps, "comum"))
purrr::walk(temas, exportar_tema, origem_apps = origem_apps, destino_raiz = destino_raiz, pasta_tema = pasta_tema)
versoes_bslib <- verificar_bslib(destino_raiz)
purrr::walk2(temas, datas_criacao, gravar_metadados, destino_raiz = destino_raiz, versoes_bslib = versoes_bslib)

# Pré-visualização local (só em sessão interativa). Abre http://127.0.0.1:7446/descritiva/
if (interactive()) httpuv::runStaticServer(destino_raiz, port = 7446)
