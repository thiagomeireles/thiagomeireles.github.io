---
layout: page
title: "Apps"
permalink: /en/apps/
show_sidebar: false
author: "Thiago Meireles"
---

<style>
  .app-lista-temas .card { display: block; height: 100%; border: 2px solid transparent; transition: border-color 0.15s; }
  .app-lista-temas .card:hover { border-color: #b5c3f5; }
  .app-lista-temas .card .tag { margin-left: 0.4rem; vertical-align: middle; }
  .app-lista-temas .topicos { color: #6c757d; font-size: 0.9rem; }
</style>

## Statistics in practice

The apps are interactive demos of statistics and probability concepts. They are organized into themes, in the order they are usually studied, and each theme covers a few topics. Topics get their apps over time. Pick a theme below to start, or search for the concept you need.

{% include busca-apps.html idioma="en" %}

{% comment %} English URLs use slug_en when there is one (themes: /en/apps/<slug_en>/; apps: /apps/en/<slug_en>/) {% endcomment %}
<div class="columns is-multiline app-lista-temas">
{%- for tema in site.data.temas -%}
  {%- assign com_app = tema.topicos | where_exp: "t", "t.app" -%}
  {%- assign slug = tema.slug_en | default: tema.slug -%}
  <div class="column is-one-third">
    <a class="card" href="{{ '/en/apps/' | relative_url }}{{ slug }}/">
      <div class="card-content">
        <p class="title is-5">{{ tema.title }}{% include etiqueta-app.html tema=tema idioma="en" %}</p>
        <p class="content">{{ tema.description }}</p>
        <p class="topicos">{{ tema.topicos.size }} topics, {% if com_app.size == 0 %}none with an app yet{% else %}{{ com_app.size }} with an app{% endif %}: {{ tema.topicos | map: "title" | join: ", " }}</p>
      </div>
    </a>
  </div>
{%- endfor -%}
</div>
{% include etiqueta-app-script.html %}

<script>
  // Old addresses (/en/apps/#<app>) go straight to the app's full-screen page
  (function () {
    var slug = location.hash.slice(1);
    var apps = [{% for app in site.data.apps %}{{ app.slug_en | default: app.slug | jsonify }}{% unless forloop.last %}, {% endunless %}{% endfor %}];
    if (apps.indexOf(slug) >= 0) location.replace("{{ '/apps/en/' | relative_url }}" + slug + "/");
  })();
</script>
