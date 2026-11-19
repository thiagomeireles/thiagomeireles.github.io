---
layout: page
title: "Aplicativos"
permalink: /aplicativos/
show_sidebar: false
author: "Thiago Meireles"
---

<style>
  .app-topicos .card { display: block; height: 100%; border: 2px solid transparent; transition: border-color 0.15s; }
  .app-topicos .card:hover { border-color: #b5c3f5; }
  .app-topicos .card .tag { margin-left: 0.4rem; vertical-align: middle; }
  .app-topicos .temas { color: #6c757d; font-size: 0.9rem; }
</style>

## Estatística na prática

Demonstrações interativas de conceitos de estatística e amostragem, organizadas por tópico. Escolha um tópico abaixo para ver os temas que ele reúne; novos tópicos e temas entram aos poucos.

<div class="columns is-multiline app-topicos">
{%- for topico in site.data.topicos -%}
  {%- assign temas = site.data.apps | where: "topico", topico.slug -%}
  <div class="column is-one-third">
    <a class="card" href="{{ '/aplicativos/' | relative_url }}{{ topico.slug }}/">
      <div class="card-content">
        <p class="title is-5">{{ topico.titulo }}{% if topico.novo %}<span class="tag is-info is-light">novo</span>{% endif %}</p>
        <p class="content">{{ topico.descricao }}</p>
        <p class="temas">{{ temas.size }} {% if temas.size == 1 %}tema{% else %}temas{% endif %}: {{ temas | map: "titulo" | join: ", " }}</p>
      </div>
    </a>
  </div>
{%- endfor -%}
</div>

<script>
  // Endereços antigos (/aplicativos/#<tema>) vão para a subpágina do tópico do tema
  (function () {
    var destinos = {
      {%- for app in site.data.apps %}
      "{{ app.slug }}": "{{ '/aplicativos/' | relative_url }}{{ app.topico }}/#{{ app.slug }}"{% unless forloop.last %},{% endunless %}
      {%- endfor %}
    };
    var destino = destinos[location.hash.slice(1)];
    if (destino) location.replace(destino);
  })();
</script>

{% include pix-contribua.html %}
