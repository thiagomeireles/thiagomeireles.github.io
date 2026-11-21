---
layout: page
title: "Artigos e Tutoriais"
permalink: /blog/
#hero_image: /assets/images/blog-banner.jpg
---

Escrevo aqui sobre estatística e *machine learning*, principalmente inferência causal, surveys e representatividade amostral. Os textos trazem o código para quem quiser reproduzir as análises.

<div class="columns is-multiline" style="margin-top: 2rem;">
    {% for post in site.posts %}
        {% unless post.url contains '/en/' %}
            <div class="column is-12">
                {% include post-card.html %}
            </div>
        {% endunless %}
    {% endfor %}
</div>
