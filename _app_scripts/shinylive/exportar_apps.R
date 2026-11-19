# =============================================================================
# Script:  exportar_apps.R (exporta os apps de tema para site estático via Shinylive)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-19
#
# Entrada:
#   - _app_scripts/apps/comum/ (modelo comum: tema, paleta, página padrão)
#   - _app_scripts/apps/<tema>/ (app.R e modulos/ de cada tema)
# Saída:
#   - apps/shinylive/ e apps/shinylive-sw.js (runtime webR, compartilhado)
#   - apps/<tema>/ (versão em português de cada tema, publicada pelo GitHub Pages)
#   - apps/en/<tema>/ (versão em inglês)
#   - metadados.txt dentro de cada pasta de app
#   - apps/<endereço antigo>/index.html (redirecionamentos; lista enderecos_antigos)
# =============================================================================

rm(list = ls()); gc()
cat("\014")
options(scipen = 999)
options(timeout = 600) # os pacotes webR são baixados de repo.r-wasm.org e podem ser lentos

pacman::p_load(here, shinylive, httpuv, bslib, htmltools, purrr, jsonlite)

# Um app por tema. A URL publicada usa o nome da pasta de origem; `slug_en`,
# quando existe, dá outro nome à versão em inglês (apps/en/<slug_en>/).
# Para criar um tema novo: crie _app_scripts/apps/<nome>/ e inclua-o aqui.
temas <- list(
  list(pasta = "variaveis",       titulo = "Tipos de variáveis", slug_en = "variables"),
  list(pasta = "probabilidade",   titulo = "Probabilidade"),
  list(pasta = "teoremas-limite", titulo = "Teoremas limite")
)

# Endereços que já foram publicados e deixaram de existir. Cada um vira uma
# página que redireciona para o endereço atual, para links já compartilhados
# (WhatsApp, e-mail) não caírem no 404 do GitHub Pages. Ao renomear um tema,
# incluir o endereço antigo aqui.
enderecos_antigos <- list(
  list(antigo = "estatistica",   destino = "/aplicativos/",       idioma = "pt", nome = "Estatística na prática"),
  list(antigo = "descritiva",    destino = "/apps/variaveis/",    idioma = "pt", nome = "Tipos de variáveis"),
  list(antigo = "en/descritiva", destino = "/apps/en/variables/", idioma = "en", nome = "Types of variables")
)

# Cada tema sai em dois idiomas. Os textos ficam lado a lado no código, via
# tr() (comum/utilitarios.R); o idioma de cada cópia vai em idioma.txt.
idiomas <- list(
  list(codigo = "pt", prefixo = ""),
  list(codigo = "en", prefixo = "en")
)

# Combinações tema x idioma, com o subdiretório publicado de cada uma
slug_versao <- function(tema, idioma) {
  if (identical(idioma, "en") && !is.null(tema$slug_en)) tema$slug_en else tema$pasta
}

subdir_versao <- function(prefixo, slug) {
  if (nzchar(prefixo)) file.path(prefixo, slug) else slug
}

versoes <- idiomas |>
  purrr::map(\(i) purrr::map(temas, \(t) {
    c(t, idioma = i$codigo, subdir = subdir_versao(i$prefixo, slug_versao(t, i$codigo)))
  })) |>
  purrr::list_flatten()

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
limpar_destino <- function(destino_raiz, versoes) {
  alvos <- file.path(destino_raiz, c("shinylive", "shinylive-sw.js", purrr::map_chr(versoes, "subdir")))
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

# Cópia temporária do app com comum/, tema/ e idioma.txt dentro, para o
# código-fonte continuar limpo e o app exportado não depender de ../comum
preparar_copia <- function(versao, origem_apps, pasta_tema) {
  copia <- file.path(tempdir(), "apps_exportacao", versao$idioma, versao$pasta)
  if (dir.exists(copia)) unlink(copia, recursive = TRUE)
  dir.create(copia, recursive = TRUE)
  file.copy(list.files(file.path(origem_apps, versao$pasta), full.names = TRUE), copia, recursive = TRUE)
  file.copy(file.path(origem_apps, "comum"), copia, recursive = TRUE)
  file.copy(pasta_tema, copia, recursive = TRUE)
  file.rename(file.path(copia, basename(pasta_tema)), file.path(copia, "tema"))
  writeLines(versao$idioma, file.path(copia, "idioma.txt"))
  copia
}

# Todas as versões vão para o mesmo destdir, cada uma no seu subdir: o runtime
# (shinylive/) é gravado uma vez só, e o navegador o reaproveita do cache
exportar_versao <- function(versao, origem_apps, destino_raiz, pasta_tema) {
  copia <- preparar_copia(versao, origem_apps, pasta_tema)
  shinylive::export(appdir = copia, destdir = destino_raiz, subdir = versao$subdir)
  invisible(file.path(destino_raiz, versao$subdir))
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

gravar_metadados <- function(versao, destino_raiz, criado_em, versoes_bslib) {
  writeLines(
    c(
      paste("App:", versao$titulo, "(Estatística na prática)"),
      "Autor: Thiago Meireles",
      paste("Idioma:", versao$idioma),
      paste("Criado em:", criado_em),
      paste("Modificado em:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
      paste0("Fonte: _app_scripts/apps/", versao$pasta, "/ (+ _app_scripts/apps/comum/)"),
      "Gerado por: _app_scripts/shinylive/exportar_apps.R",
      "Runtime: apps/shinylive/ (compartilhado entre os temas)",
      "Tema: Bootswatch zephyr (CSS pré-compilado em tema/)",
      paste("shinylive (pacote R):", as.character(utils::packageVersion("shinylive"))),
      paste("bslib local / webR:", versoes_bslib[["local"]], "/", versoes_bslib[["webr"]]),
      paste("R:", R.version.string)
    ),
    file.path(destino_raiz, versao$subdir, "metadados.txt"),
    useBytes = TRUE
  )
}

# Página mínima que manda o visitante do endereço antigo para o atual,
# mantendo parâmetros e âncora (#) do link original
gravar_redirecionamento <- function(endereco, destino_raiz) {
  textos <- if (identical(endereco$idioma, "en")) {
    list(lang = "en", titulo = "This app has moved", aviso = "This app has a new address:")
  } else {
    list(lang = "pt-BR", titulo = "Este app mudou de endereço", aviso = "Este app tem um novo endereço:")
  }
  pasta <- file.path(destino_raiz, endereco$antigo)
  if (!dir.exists(pasta)) dir.create(pasta, recursive = TRUE)
  url <- paste0("https://thiagomeireles.com.br", endereco$destino)
  writeLines(
    c(
      "<!doctype html>",
      "<!-- Redirecionamento gerado por _app_scripts/shinylive/exportar_apps.R | Autor: Thiago Meireles -->",
      sprintf("<!-- Gerado em: %s | Destino: %s -->", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), endereco$destino),
      sprintf('<html lang="%s">', textos$lang),
      "<head>",
      '  <meta charset="utf-8">',
      '  <meta name="viewport" content="width=device-width, initial-scale=1">',
      '  <meta name="robots" content="noindex">',
      sprintf("  <title>%s</title>", textos$titulo),
      sprintf('  <link rel="canonical" href="%s">', url),
      sprintf('  <meta http-equiv="refresh" content="0; url=%s">', endereco$destino),
      sprintf('  <script>location.replace("%s" + location.search + location.hash);</script>', endereco$destino),
      "</head>",
      sprintf('<body><p>%s <a href="%s">%s</a></p></body>', textos$aviso, endereco$destino, endereco$nome),
      "</html>"
    ),
    file.path(pasta, "index.html"),
    useBytes = TRUE
  )
}

# Um endereço antigo nunca pode coincidir com um app atual (sobrescreveria o app)
conflitos <- intersect(purrr::map_chr(enderecos_antigos, "antigo"), purrr::map_chr(versoes, "subdir"))
if (length(conflitos) > 0) stop("Endereço antigo igual a um app atual: ", paste(conflitos, collapse = ", "))

datas_criacao <- purrr::map_chr(versoes, \(v) ler_data_criacao(file.path(destino_raiz, v$subdir)))
limpar_destino(destino_raiz, versoes)
pasta_tema <- compilar_tema(file.path(origem_apps, "comum"))
purrr::walk(versoes, exportar_versao, origem_apps = origem_apps, destino_raiz = destino_raiz, pasta_tema = pasta_tema)
versoes_bslib <- verificar_bslib(destino_raiz)
purrr::walk2(versoes, datas_criacao, gravar_metadados, destino_raiz = destino_raiz, versoes_bslib = versoes_bslib)
purrr::walk(enderecos_antigos, gravar_redirecionamento, destino_raiz = destino_raiz)

# Pré-visualização local (só em sessão interativa). Abre http://127.0.0.1:7446/variaveis/
# (ou /en/variables/ para a versão em inglês)
if (interactive()) httpuv::runStaticServer(destino_raiz, port = 7446)
