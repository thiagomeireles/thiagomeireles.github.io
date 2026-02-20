---
layout: page
title: "Imprensa"
permalink: /imprensa/
show_sidebar: false
---

<style>

  /* --- GRID 1: VÍDEOS DO YOUTUBE --- */
  .video-grid {
    display: grid;
    grid-template-columns: 1fr; /* 1 coluna no celular */
    gap: 2rem;
    margin-bottom: 2rem;
  }

  @media (min-width: 768px) {
    .video-grid {
      grid-template-columns: repeat(2, 1fr); /* 2 colunas no computador */
    }
  }

  /* Mantém o vídeo responsivo (16:9) */
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

  /* --- GRID 2: MATÉRIAS E NOTÍCIAS --- */
  .news-grid {
    display: grid;
    grid-template-columns: 1fr; /* 1 coluna no celular */
    gap: 1.5rem;
  }

  /* Responsividade para notícias: até 3 cartões por linha em telas grandes */
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

  /* Estilo do Cartão de Notícia */
  .news-card {
    background-color: #fff;
    border: 1px solid #e0e0e0;
    border-radius: 8px;
    padding: 1.5rem;
    display: flex;
    flex-direction: column;
    justify-content: space-between; /* Empurra o botão para o final */
    transition: transform 0.2s, box-shadow 0.2s;
    height: 100%; /* Faz todos os cards da linha terem a mesma altura */
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
    flex-grow: 1; /* Ocupa espaço disponível para alinhar botões */
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


{% include banner.html %}


<div class="margem-desktop">

  <h2 class="section-title">Vídeos e Entrevistas</h2>
  
  <div class="video-grid">
    
    <div>
      <div class="video-wrapper">
        <iframe src="https://www.youtube.com/embed/NPUyT9El5cQ" 
                title="Programa Opinião (27/06/2025) - Seu emprego está em Risco? Os impactos da IA no mundo do trabalho" allowfullscreen></iframe>
      </div>
      <div class="video-caption">Programa Opinião (27/06/2025) - Seu emprego está em Risco? Os impactos da IA no mundo do trabalho</div>
    </div>

    <div>
      <div class="video-wrapper">
        <iframe src="https://www.youtube.com/embed/p4t6fqXmPWA" 
                title="Corte: Thiago Meireles revela que engenharia será uma das carreiras mais prejudicadas pela IA, no #Opinião" allowfullscreen></iframe>
      </div>
      <div class="video-caption">Corte: Thiago Meireles revela que engenharia será uma das carreiras mais prejudicadas pela IA, no #Opinião</div>
    </div>

  </div>


  <h2 class="section-title" style="margin-top: 4rem;">Na Imprensa</h2>
  
  <div class="news-grid">

    <div class="news-card">
      <div>
        <div class="news-source-date">28 MAR 2023 | INSTITUTO CLARO</div>
        <a href="https://www.institutoclaro.org.br/cidadania/nossas-novidades/reportagens/desemprego-tecnologico-inteligencias-artificiais-podem-realmente-substituir-profissoes/" target="_blank" class="news-title">
          Desemprego tecnológico: inteligências artificiais podem realmente substituir profissões?
        </a>
        <p class="news-snippet">
          Pesquisadores apontam os limites de tecnologias como o ChatGPT no mercado de trabalho

Autor.
        </p>
      </div>
      <a href="https://www.institutoclaro.org.br/cidadania/nossas-novidades/reportagens/desemprego-tecnologico-inteligencias-artificiais-podem-realmente-substituir-profissoes/" target="_blank" class="news-link-btn">Ler Matéria</a>
    </div>

  </div>

</div>