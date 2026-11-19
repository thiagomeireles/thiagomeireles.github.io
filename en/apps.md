---
layout: page
title: "Apps"
permalink: /en/apps/
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

## Statistics in practice

Interactive demos of statistics and sampling concepts, organized by topic. Pick a topic below; new topics and demos will be added over time.

{%- comment -%} The English URL uses slug_en when the topic has one (apps/en/<slug_en>/) {%- endcomment -%}
{%- assign primeiro_slug = site.data.apps[0].slug_en | default: site.data.apps[0].slug -%}
<div class="columns is-multiline app-temas">
{%- for app in site.data.apps -%}
  {%- assign slug = app.slug_en | default: app.slug -%}
  <div class="column is-one-third">
    <a class="card" href="#{{ slug }}" data-app="{{ slug }}" data-titulo="{{ app.title }}">
      <div class="card-content">
        <p class="title is-5">{{ app.title }}{% if app.novo %}<span class="tag is-info is-light">new</span>{% endif %}</p>
        <p class="content">{{ app.description }}</p>
      </div>
    </a>
  </div>
{%- endfor -%}
</div>

<div class="notification is-light">
  The apps run in R on a server. If nobody has used them for a few days, the server is asleep and the first load can take up to a minute; after that, topics open quickly.
  <a id="app-tela-cheia" href="{{ '/apps/en/' | relative_url }}{{ primeiro_slug }}/" target="_blank" rel="noopener">Open this topic full screen</a>.
</div>

<iframe id="app-frame" class="app-frame"
        src="{{ site.apps_servidor }}/en/{{ primeiro_slug }}/"
        title="{{ site.data.apps[0].title }}: interactive app"
        loading="lazy"></iframe>

<script>
  // One iframe: switching topics swaps the src. The app comes from the Hugging Face
  // Space; full screen is a page on this site (_layouts/app-tela-cheia.html).
  (function () {
    var servidor = "{{ site.apps_servidor }}/en/";
    var base = "{{ '/apps/en/' | relative_url }}";
    var frame = document.getElementById("app-frame");
    var telaCheia = document.getElementById("app-tela-cheia");
    var cartoes = document.querySelectorAll(".app-temas .card");

    function abrir(slug, rolar) {
      var cartao = document.querySelector('.app-temas .card[data-app="' + slug + '"]');
      if (!cartao) return;
      var url = servidor + slug + "/";
      cartoes.forEach(function (c) { c.classList.toggle("is-ativo", c === cartao); });
      if (frame.getAttribute("src") !== url) frame.setAttribute("src", url);
      frame.title = cartao.dataset.titulo + ": interactive app";
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
