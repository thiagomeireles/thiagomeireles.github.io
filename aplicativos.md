---
layout: page
title: "Aplicativos"
permalink: /aplicativos/
show_sidebar: false
author: "Thiago Meireles"
---

<style>
  .app-lista-temas .card { display: block; height: 100%; border: 2px solid transparent; transition: border-color 0.15s; }
  .app-lista-temas .card:hover { border-color: #b5c3f5; }
  .app-lista-temas .card .tag { margin-left: 0.4rem; vertical-align: middle; }
  .app-lista-temas .topicos { color: #6c757d; font-size: 0.9rem; }
</style>

## Estatística e probabilidade interativas

Os aplicativos são demonstrações de conceitos de estatística e probabilidade, em que você muda os parâmetros e acompanha o resultado na tela. Estão organizados em temas, na ordem em que costumam ser estudados, e cada tema reúne alguns tópicos. Nem todo tópico tem aplicativo ainda, e os que faltam entram aos poucos. Escolha um tema abaixo para começar ou use a busca para ir direto a um conceito.

{% include busca-apps.html idioma="pt" %}

<div class="columns is-multiline app-lista-temas">
{%- for tema in site.data.temas -%}
  {%- assign com_app = tema.topicos | where_exp: "t", "t.app" -%}
  <div class="column is-one-third">
    <a class="card" href="{{ '/aplicativos/' | relative_url }}{{ tema.slug }}/">
      <div class="card-content">
        <p class="title is-5">{{ tema.titulo }}{% include etiqueta-app.html tema=tema idioma="pt" %}</p>
        <p class="content">{{ tema.descricao }}</p>
        <p class="topicos">{{ tema.topicos.size }} tópicos, {% if com_app.size == 0 %}nenhum{% else %}{{ com_app.size }}{% endif %} com aplicativo{% if com_app.size == 0 %} ainda{% endif %}: {{ tema.topicos | map: "titulo" | join: ", " }}</p>
      </div>
    </a>
  </div>
{%- endfor -%}
</div>
{% include etiqueta-app-script.html %}

<script>
  // Endereços antigos (/aplicativos/#<app>) vão direto para a tela cheia do app
  (function () {
    var slug = location.hash.slice(1);
    var apps = [{% for app in site.data.apps %}{{ app.slug | jsonify }}{% unless forloop.last %}, {% endunless %}{% endfor %}];
    if (apps.indexOf(slug) >= 0) location.replace("{{ '/apps/' | relative_url }}" + slug + "/");
  })();
</script>

{% include pix-contribua.html %}
