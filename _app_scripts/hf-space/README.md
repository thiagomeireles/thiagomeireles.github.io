---
title: Estatística na prática
emoji: 📊
colorFrom: blue
colorTo: indigo
sdk: docker
app_port: 7860
pinned: false
short_description: Apps Shiny de estatística de thiagomeireles.com.br
---

# Estatística na prática

Autor: Thiago Meireles | Atualizado: 2026-11-19

Apps Shiny de estatística usados em <https://thiagomeireles.com.br/aplicativos/>. Cada tema roda no Shiny Server, com processo R próprio, em dois idiomas:

- `/<tema>/`: português
- `/en/<tema>/`: inglês

**Não edite os arquivos deste Space aqui.** A fonte fica no repositório `thiagomeireles/thiagomeireles.github.io`:

- `_app_scripts/apps/`: código dos apps (um tema por pasta, mais `comum/`)
- `_app_scripts/hf-space/`: Dockerfile, configuração do Shiny Server e este README
- `_data/apps.yml`: lista de temas

O workflow `.github/workflows/hf-space.yml` envia tudo para cá a cada push na `main` que mexa nesses caminhos.
