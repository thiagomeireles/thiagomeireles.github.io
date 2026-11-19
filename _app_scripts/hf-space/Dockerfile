# =============================================================================
# Script:  Dockerfile (Space "estatistica-na-pratica" no Hugging Face)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-19
#
# Entrada:
#   - apps/ (cópia de _app_scripts/apps/, feita pelo GitHub Action)
#   - apps.yml (cópia de _data/apps.yml: lista de temas e slugs)
#   - montar_apps.R, shiny-server.conf, index.html (esta pasta)
# Saída:
#   - imagem com Shiny Server na porta 7860, um app por tema e idioma em
#     /<tema>/ (pt) e /en/<slug_en>/ (en)
# =============================================================================

# Versão fixa do R: a mesma usada no desenvolvimento local
FROM rocker/shiny:4.6.1

ENV SHINY_LOG_STDERR=1 \
    HOME=/home/user

# Biblioteca de sistema exigida pelo binário do pacote fs (usado pelo sass/bslib
# para compilar o tema); o -dev puxa a versão certa do Ubuntu
RUN apt-get update \
 && apt-get install -y --no-install-recommends libuv1-dev \
 && rm -rf /var/lib/apt/lists/*

# Pacotes dos apps. shiny, htmltools e yaml já vêm na imagem.
RUN install2.r --error --skipinstalled bslib ggplot2 purrr scales yaml \
 && rm -rf /tmp/downloaded_packages

# O Hugging Face roda o container com UID 1000. O Shiny Server e os processos
# R rodam com esse usuário, que precisa escrever nos logs e nos bookmarks.
RUN if id -u ubuntu >/dev/null 2>&1; then userdel ubuntu; fi \
 && useradd -m -u 1000 user \
 && install -d -o user -g user /var/log/shiny-server /var/lib/shiny-server

COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# Monta /srv/shiny-server: cada tema em pt e en, com comum/ e idioma.txt dentro
COPY montar_apps.R apps.yml /tmp/montagem/
COPY apps /tmp/montagem/apps
RUN rm -rf /srv/shiny-server/* \
 && Rscript /tmp/montagem/montar_apps.R /tmp/montagem/apps /tmp/montagem/apps.yml /srv/shiny-server \
 && rm -rf /tmp/montagem
COPY index.html /srv/shiny-server/index.html

USER user
WORKDIR /home/user

# Compila o tema zephyr no build: os apps reaproveitam o cache do sass e não
# pagam essa compilação quando um processo R sobe
RUN Rscript -e 'invisible(bslib::bs_theme_dependencies(bslib::bs_theme(preset = "zephyr")))'

EXPOSE 7860
CMD ["/usr/bin/shiny-server"]
