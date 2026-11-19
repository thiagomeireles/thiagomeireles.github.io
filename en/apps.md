---
layout: page
title: "Apps"
permalink: /en/apps/
show_sidebar: false
author: "Thiago Meireles"
---

<style>
  .app-topicos .card { display: block; height: 100%; border: 2px solid transparent; transition: border-color 0.15s; }
  .app-topicos .card:hover { border-color: #b5c3f5; }
  .app-topicos .card .tag { margin-left: 0.4rem; vertical-align: middle; }
  .app-topicos .temas { color: #6c757d; font-size: 0.9rem; }
</style>

## Statistics in practice

The apps are interactive demos of statistics and sampling concepts, organized by topic. Each topic covers a few themes, and new topics and themes are added over time. Pick a topic below to start, or search for the concept you need.

{% include busca-apps.html idioma="en" %}

{% comment %} English URLs use slug_en when there is one (topics: /en/apps/<slug_en>/; themes: #<slug_en>) {% endcomment %}
<div class="columns is-multiline app-topicos">
{%- for topico in site.data.topicos -%}
  {%- assign temas = site.data.apps | where: "topico", topico.slug -%}
  {%- assign slug = topico.slug_en | default: topico.slug -%}
  <div class="column is-one-third">
    <a class="card" href="{{ '/en/apps/' | relative_url }}{{ slug }}/">
      <div class="card-content">
        <p class="title is-5">{{ topico.title }}{% if topico.novo %}<span class="tag is-info is-light">new</span>{% endif %}</p>
        <p class="content">{{ topico.description }}</p>
        <p class="temas">{{ temas.size }} {% if temas.size == 1 %}theme{% else %}themes{% endif %}: {{ temas | map: "title" | join: ", " }}</p>
      </div>
    </a>
  </div>
{%- endfor -%}
</div>

<script>
  // Old addresses (/en/apps/#<theme>) go to the subpage of the theme's topic
  (function () {
    var destinos = {
      {%- for app in site.data.apps %}
      {%- assign topico = site.data.topicos | where: "slug", app.topico | first %}
      "{{ app.slug_en | default: app.slug }}": "{{ '/en/apps/' | relative_url }}{{ topico.slug_en | default: topico.slug }}/#{{ app.slug_en | default: app.slug }}"{% unless forloop.last %},{% endunless %}
      {%- endfor %}
    };
    var destino = destinos[location.hash.slice(1)];
    if (destino) location.replace(destino);
  })();
</script>
