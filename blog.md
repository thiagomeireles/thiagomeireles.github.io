---
layout: page
title: "Artigos e Tutoriais"
permalink: /blog/
#hero_image: /assets/images/blog-banner.jpg
---

{% include banner.html %}

Aqui compartilho textos sobre estatística e machine learning, especialmente Causalidade, Survey e Representatividade Amostral.

<div class="columns is-multiline" style="margin-top: 2rem;">
    {% for post in site.posts %}
        {% unless post.url contains '/en/' %}
            <div class="column is-12">
                {% include post-card.html %}
            </div>
        {% endunless %}
    {% endfor %}
</div>