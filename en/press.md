---
layout: page
title: "Media coverage"
permalink: /en/press/
show_sidebar: false
---

<style>
  /* --- Base Page Adjustments --- */
  .hero-body { display: none !important; }
  .hero { min-height: 0 !important; }
  .skill-tag { margin-bottom: 0.5rem; }

  /* Desktop margin adjustment */
  @media screen and (min-width: 1024px) {
    .margem-desktop {
      padding-left: 3rem;
      padding-right: 3rem;
    }
  }

  /* Section Titles */
  .section-title {
    font-size: 1.5rem;
    font-weight: 700;
    margin-top: 3rem;
    margin-bottom: 1.5rem;
    border-bottom: 2px solid #eaeaea;
    padding-bottom: 0.5rem;
    color: #333;
  }

  /* --- GRID 1: YOUTUBE VIDEOS --- */
  .video-grid {
    display: grid;
    grid-template-columns: 1fr; /* 1 column on mobile */
    gap: 2rem;
    margin-bottom: 2rem;
  }

  @media (min-width: 768px) {
    .video-grid {
      grid-template-columns: repeat(2, 1fr); /* 2 columns on desktop */
    }
  }

  /* Responsive video wrapper (16:9) */
  .video-wrapper {
    position: relative;
    padding-bottom: 56.25%; 
    height: 0;
    overflow: hidden;
    border-radius: 8px;
    box-shadow: 0 4px 10px rgba(0,0,0,0.1);
    background: #000;
  }

  .video-wrapper iframe {
    position: absolute;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    border: 0;
  }

  .video-caption {
    margin-top: 0.75rem;
    font-size: 1rem;
    font-weight: 600;
    color: #444;
    line-height: 1.3;
  }

  /* --- GRID 2: ARTICLES & NEWS --- */
  .news-grid {
    display: grid;
    grid-template-columns: 1fr;
    gap: 1.5rem;
  }

  @media (min-width: 768px) {
    .news-grid {
      grid-template-columns: repeat(2, 1fr);
    }
  }
  @media (min-width: 1100px) {
    .news-grid {
      grid-template-columns: repeat(3, 1fr);
    }
  }

  /* News Card Style */
  .news-card {
    background-color: #fff;
    border: 1px solid #e0e0e0;
    border-radius: 8px;
    padding: 1.5rem;
    display: flex;
    flex-direction: column;
    justify-content: space-between;
    transition: transform 0.2s, box-shadow 0.2s;
    height: 100%;
  }

  .news-card:hover {
    transform: translateY(-3px);
    box-shadow: 0 8px 20px rgba(0,0,0,0.08);
    border-color: #d0d0d0;
  }

  .news-source-date {
    font-size: 0.75rem;
    color: #888;
    text-transform: uppercase;
    font-weight: 700;
    margin-bottom: 0.75rem;
    letter-spacing: 0.5px;
  }

  .news-title {
    font-size: 1.15rem;
    font-weight: bold;
    color: #222;
    margin-bottom: 0.75rem;
    line-height: 1.3;
    text-decoration: none;
  }
  
  .news-title:hover {
    color: #0056b3;
  }

  .news-snippet {
    font-size: 0.95rem;
    color: #666;
    margin-bottom: 1.5rem;
    flex-grow: 1;
  }

  .news-link-btn {
    display: inline-block;
    font-size: 0.9rem;
    font-weight: 600;
    color: #0056b3;
    text-decoration: none;
    border: 1px solid #0056b3;
    padding: 0.5rem 1rem;
    border-radius: 4px;
    text-align: center;
    transition: background 0.2s, color 0.2s;
  }

  .news-link-btn:hover {
    background-color: #0056b3;
    color: #fff;
    text-decoration: none;
  }
</style>


<div style="text-align: center; margin-bottom: 2rem;">
  <img src="/assets/images/banner_en.jpg" alt="Thiago Meireles" 
       style="width: auto; height: auto; object-fit: cover; border-radius: 6px;">
</div>


<div class="margem-desktop">

  <h2 class="section-title">Videos & Interviews</h2>
  
  <div class="video-grid">
    
    <div>
      <div class="video-wrapper">
        <iframe src="https://www.youtube.com/embed/NPUyT9El5cQ" 
                title="Opinião Program (06/27/2025) - Is your job at risk? AI impacts on the world of work" allowfullscreen></iframe>
      </div>
      <div class="video-caption">Opinião Program (June 2025) - Is your job at risk? The impacts of AI on the world of work</div>
    </div>

    <div>
      <div class="video-wrapper">
        <iframe src="https://www.youtube.com/embed/p4t6fqXmPWA" 
                title="Highlight: Thiago Meireles reveals that Engineering will be one of the most affected careers by AI" allowfullscreen></iframe>
      </div>
      <div class="video-caption">Highlight: Thiago Meireles discusses how engineering careers will be impacted by AI</div>
    </div>

  </div>


  <h2 class="section-title" style="margin-top: 4rem;">In the Press</h2>
  
  <div class="news-grid">

    <div class="news-card">
      <div>
        <div class="news-source-date">MAR 28, 2023 | CLARO INSTITUTE</div>
        <a href="https://www.institutoclaro.org.br/cidadania/nossas-novidades/reportagens/desemprego-tecnologico-inteligencias-artificiais-podem-realmente-substituir-profissoes/" target="_blank" class="news-title">
          Technological unemployment: can artificial intelligence truly replace professions?
        </a>
        <p class="news-snippet">
          Researchers point out the limitations of technologies like ChatGPT within the labor market.
        </p>
      </div>
      <a href="https://www.institutoclaro.org.br/cidadania/nossas-novidades/reportagens/desemprego-tecnologico-inteligencias-artificiais-podem-realmente-substituir-profissoes/" target="_blank" class="news-link-btn">Read Article</a>
    </div>

  </div>

</div>