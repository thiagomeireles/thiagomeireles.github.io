---
layout: page
title: "Aplicativos"
permalink: /aplicativos/
show_sidebar: false
author: "Thiago Meireles"
---

<style>
  .app-temas .card { height: 100%; cursor: pointer; border: 2px solid transparent; transition: border-color 0.15s; }
  .app-temas .card:hover { border-color: #b5c3f5; }
  .app-temas .card.is-ativo { border-color: #3459e6; }
  .app-temas .card .tag { margin-left: 0.4rem; vertical-align: middle; }
  .app-frame { width: 100%; height: 920px; border: 1px solid #eaeaea; border-radius: 8px; }
</style>

## Estatística na prática

Demonstrações interativas de conceitos de estatística e amostragem, organizadas por tema. Escolha um tema abaixo; novos temas e tópicos entram aos poucos.

<div class="columns is-multiline app-temas">
{%- for app in site.data.apps -%}
  <div class="column is-one-third">
    <a class="card" href="#{{ app.slug }}" data-app="{{ app.slug }}" data-titulo="{{ app.titulo }}">
      <div class="card-content">
        <p class="title is-5">{{ app.titulo }}{% if app.novo %}<span class="tag is-info is-light">novo</span>{% endif %}</p>
        <p class="content">{{ app.descricao }}</p>
      </div>
    </a>
  </div>
{%- endfor -%}
</div>

<div class="notification is-light">
  Os aplicativos rodam em R num servidor. Se ninguém os usou nos últimos dias, o servidor está parado e a primeira abertura pode levar até um minuto; depois disso, os temas abrem rápido.
  <a id="app-tela-cheia" href="{{ '/apps/' | relative_url }}{{ site.data.apps[0].slug }}/" target="_blank" rel="noopener">Abrir este tema em tela cheia</a>.
</div>

<iframe id="app-frame" class="app-frame"
        src="{{ site.apps_servidor }}/{{ site.data.apps[0].slug }}/"
        title="{{ site.data.apps[0].titulo }}: aplicativo interativo"
        loading="lazy"></iframe>

<script>
  // Um iframe só: trocar de tema troca o src. O app vem do Space no Hugging Face;
  // a tela cheia é uma página do próprio site (_layouts/app-tela-cheia.html).
  (function () {
    var servidor = "{{ site.apps_servidor }}/";
    var base = "{{ '/apps/' | relative_url }}";
    var frame = document.getElementById("app-frame");
    var telaCheia = document.getElementById("app-tela-cheia");
    var cartoes = document.querySelectorAll(".app-temas .card");

    function abrir(slug, rolar) {
      var cartao = document.querySelector('.app-temas .card[data-app="' + slug + '"]');
      if (!cartao) return;
      var url = servidor + slug + "/";
      cartoes.forEach(function (c) { c.classList.toggle("is-ativo", c === cartao); });
      if (frame.getAttribute("src") !== url) frame.setAttribute("src", url);
      frame.title = cartao.dataset.titulo + ": aplicativo interativo";
      telaCheia.href = base + slug + "/";
      if (rolar) frame.scrollIntoView({ behavior: "smooth", block: "start" });
    }

    cartoes.forEach(function (c) {
      c.addEventListener("click", function (e) {
        e.preventDefault();
        history.replaceState(null, "", "#" + c.dataset.app);
        abrir(c.dataset.app, true);
      });
    });

    abrir(location.hash.slice(1) || cartoes[0].dataset.app, false);
  })();
</script>
