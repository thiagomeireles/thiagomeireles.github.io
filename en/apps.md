---
layout: page
title: "Apps"
permalink: /en/apps/
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

## Statistics in practice

Interactive demos of basic statistics and sampling concepts: the normal distribution (Z and t), the Central Limit Theorem and the Law of Large Numbers. The app is in Portuguese for now.

<div class="notification is-light">
  The app runs entirely in your browser (R compiled to WebAssembly). The first load takes a few seconds; after that it is cached.
  <a href="{{ '/apps/estatistica/' | relative_url }}" target="_blank" rel="noopener">Open full screen</a>.
</div>

<iframe class="app-frame"
        src="{{ '/apps/estatistica/' | relative_url }}"
        title="Statistics in practice: interactive app"
        loading="lazy"></iframe>
