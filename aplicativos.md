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
  .pix-contribua { margin-top: 2.5rem; }
  .pix-contribua .pix-qr { width: 200px; max-width: 100%; }
  .pix-contribua .pix-codigo { font-family: monospace; font-size: 0.8rem; word-break: break-all; }
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

<div class="box pix-contribua">
  <div class="columns is-vcentered">
    <div class="column is-narrow has-text-centered">
      <img class="pix-qr" src="{{ '/assets/images/pix_qr.svg' | relative_url }}" alt="QR code Pix para contribuir com o projeto">
    </div>
    <div class="column">
      <p class="title is-4">Contribua com o projeto</p>
      <p>Os aplicativos são gratuitos e abertos. Se eles foram úteis para você, considere contribuir com qualquer valor via Pix: escaneie o QR code ou copie o código abaixo e cole no app do seu banco, na opção "Pix copia e cola".</p>
      <p class="pix-codigo" id="pix-codigo">{{ site.data.pix.copia_e_cola }}</p>
      <button class="button is-primary is-small" id="pix-copiar" type="button">Copiar código Pix</button>
    </div>
  </div>
</div>

<script>
  (function () {
    var botao = document.getElementById("pix-copiar");
    var codigo = document.getElementById("pix-codigo").textContent.trim();
    botao.addEventListener("click", function () {
      navigator.clipboard.writeText(codigo).then(function () {
        botao.textContent = "Código copiado";
        setTimeout(function () { botao.textContent = "Copiar código Pix"; }, 2500);
      });
    });
  })();
</script>
