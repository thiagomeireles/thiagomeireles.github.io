---
layout: page
title: "Portfólio"
permalink: /portfolio/
show_sidebar: false
---
  
<style>
  /* Ajustes visuais para limpar a home */
  .hero-body { display: none !important; }
  .hero { min-height: 0 !important; }
  .skill-tag { margin-bottom: 0.5rem; }
</style>


<style>
  .hero-body { display: none !important; }
  .hero { min-height: 0 !important; }
</style>

<style>
  /* Outros estilos que você já tem... */

  /* Regra aplicada apenas para telas maiores que 1024px (Computadores/Laptops) */
  @media screen and (min-width: 1024px) {
    .margem-desktop {
      padding-left: 3rem;  /* Ajuste o valor conforme necessário */
      padding-right: 3rem; /* Use padding para evitar quebrar a largura total */
    }
  }
</style>

<div style="text-align: center; margin-bottom: 2rem;">
  <img src="/assets/images/banner_pt.jpg" alt="Thiago Meireles" 
       style="width: auto; height: auto; object-fit: cover; border-radius: 6px;">
</div>

Aqui estarão alguns dos projetos desenvolvidos ao longo da minha trajetória acadêmica e profissional, focados em **Ciência de Dados**, **Visualização** e **Inferência Causal**.

<style>
  /* --- Ajustes da Página Base (Seus originais) --- */
  .hero-body { display: none !important; }
  .hero { min-height: 0 !important; }
  .skill-tag { margin-bottom: 0.5rem; }

  /* Ajuste de margem desktop */
  @media screen and (min-width: 1024px) {
    .margem-desktop {
      padding-left: 3rem;
      padding-right: 3rem;
    }
  }

  /* Títulos das Seções */
  .section-title {
    font-size: 1.5rem;
    font-weight: 700;
    margin-top: 3rem;
    margin-bottom: 1.5rem;
    border-bottom: 2px solid #eaeaea;
    padding-bottom: 0.5rem;
    color: #333;
  }

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

<div class="margem-desktop">

  <h2 class="section-title">Webinários</h2>
  
  <div class="video-grid">
    
    <div>
      <div class="video-wrapper">
        <iframe src="https://www.youtube.com/embed/SUVZkIjpfms" 
                title="Leveraging non-probability samples and organic data for producing public statistics" allowfullscreen></iframe>
      </div>
      <div class="video-caption">UN Stats (Inter-Secretariat Working Group on Household Surveys (ISWGHS) and the Global Network of Data Officers and Statisticians): Leveraging non-probability samples and organic data for producing public statistics</div>
    </div>
    
    <div>
      <div class="video-wrapper">
        <iframe src="https://www.youtube.com/embed/Ya3PEgiChqw" 
                title="Ponderação de uma amostra não probabilística para estimar indicadores de qualidade de Internet para escolas brasileiras" allowfullscreen></iframe>
      </div>
      <div class="video-caption">UN Big Data Regional Hub: Ponderação de uma amostra não probabilística para estimar indicadores de qualidade de Internet para escolas brasileiras</div>
    </div>

  </div>

  <h2 class="section-title" style="margin-top: 4rem;">Outros Projetos</h2>
  <p>
    Para a lista completa de repositórios, visite meu 
    <a href="https://github.com/thiagomeireles" target="_blank" style="color: #0056b3; font-weight: bold;">GitHub</a>.
  </p>

</div>