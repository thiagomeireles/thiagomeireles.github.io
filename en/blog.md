---
layout: page
title: "Articles and Tutorials"
permalink: /en/blog/
#hero_image: /assets/images/blog-banner.jpg
---

{% include banner_en.html %}

Here I share posts about statistics and machine learning, particularly Causal Inference, Survey Methodology, and Representativeness.

<div class="columns is-multiline" style="margin-top: 2rem;">
    {% for post in site.posts %}
        {% if post.url contains '/en/' %}
            <div class="column is-12">
                {% include post-card.html %}
            </div>
        {% endif %}
    {% endfor %}
</div>