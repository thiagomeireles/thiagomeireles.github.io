---
layout: page
title: "Aplicativos"
permalink: /aplicativos/
show_sidebar: false
author: "Thiago Meireles"
---

<style>
  .app-frame {
    width: 100%;
    height: 920px;
    border: 1px solid #eaeaea;
    border-radius: 8px;
  }
</style>

## Estatística na prática

Demonstrações interativas de conceitos básicos de estatística e amostragem: distribuição normal (Z e t), Teorema Central do Limite e Lei dos Grandes Números. Novos tópicos entram aos poucos.

<div class="notification is-light">
  O aplicativo roda inteiro no seu navegador (R via WebAssembly). A primeira abertura leva alguns segundos; depois disso, ele fica em cache.
  <a href="{{ '/apps/estatistica/' | relative_url }}" target="_blank" rel="noopener">Abrir em tela cheia</a>.
</div>

<iframe class="app-frame"
        src="{{ '/apps/estatistica/' | relative_url }}"
        title="Estatística na prática: aplicativo interativo"
        loading="lazy"></iframe>
