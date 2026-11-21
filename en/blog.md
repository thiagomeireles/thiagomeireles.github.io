---
layout: page
title: "Articles and Tutorials"
permalink: /en/blog/
#hero_image: /assets/images/blog-banner.jpg
---

I write here about statistics and machine learning, mainly causal inference, survey methodology and sample representativeness. Posts include the code for anyone who wants to reproduce the analyses.

<div class="columns is-multiline" style="margin-top: 2rem;">
    {% for post in site.posts %}
        {% if post.url contains '/en/' %}
            <div class="column is-12">
                {% include post-card.html %}
            </div>
        {% endif %}
    {% endfor %}
</div>
