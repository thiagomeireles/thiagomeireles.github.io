---
layout: page
title: "Home"
permalink: /
---

<style>
  /* --- 1. NOVO: Aumenta a fonte do Menu (Navbar) --- */
  /* Tamanho 2rem é equivalente ao is-size-3 */
  .navbar-item, .navbar-link {
    font-size: 2rem !important; 
    padding-top: 1rem !important;    /* Mais espaço vertical para não ficar espremido */
    padding-bottom: 1rem !important;
  }
  
  /* Garante que o seu nome (Logo) também fique grande */
  .navbar-brand .navbar-item {
    font-size: 2rem !important;
    font-weight: bold;
  }

  /* --- 2. REMOÇÃO DO BANNER PADRÃO (Hero) --- */
  /* Esconde o corpo do banner gigante */
  .hero-body { 
    display: none !important; 
  }
  /* Remove a altura mínima e tira o fundo verde que vazava */
  .hero { 
    min-height: 0 !important; 
    background-color: transparent !important; 
  }

  /* --- 3. COR DO MENU --- */
  /* Garante que o fundo do menu seja da cor sólida do tema */
  .navbar { 
    background-color: #00d1b2 !important; 
  }

  /* --- 4. CENTRALIZAÇÃO E MARGENS (Desktop) --- */
  /* No celular, usa 100% da tela */
  .meu-conteudo-centralizado {
    width: 100%;
  }

  /* No PC (acima de 1024px), trava a largura e centraliza */
  @media screen and (min-width: 1024px) {
    .meu-conteudo-centralizado {
      max-width: 850px; 
      margin-left: auto;
      margin-right: auto;
    }
  }
  
  /* Ajuste menor para tags de skills */
  .skill-tag { margin-bottom: 0.5rem; }
</style>

<div class="content margem-desktop">
  <div class="columns is-vcentered">
  
    <div class="column is-4 has-text-centered">
      <figure class="image is-square" style="max-width: 300px; margin: 0 auto;">
        <img class="is-rounded" src="/assets/images/perfil.png" alt="Foto de Thiago Meireles" style="border: 4px solid #f5f5f5; box-shadow: 0 4px 10px rgba(0,0,0,0.1);">
      </figure>
    </div>

    <div class="column">
      <div class="content">

        <div style="margin-top: 1.5rem;">
          <img src="/assets/images/banner_pt.jpg" alt="Thiago Meireles" 
               style="width: 100%; height: auto; object-fit: cover; border-radius: 6px;">
        </div>

        <p class="is-size-3">
          Sou <strong>Thiago Meireles</strong>, Cientista de Dados e Doutor em Ciência Política pela USP.
        </p>
        
        <p class="is-size-4">
          Lidero projetos de análise de dados e inovação metodológica no <strong>Cetic.br</strong>, combinando técnicas avançadas de <strong>Machine Learning</strong> e <strong>Estatística</strong> para resolver problemas complexos e oferecer soluções acessíveis para gestores e stakeholders.
        </p>

      </div>
    </div>

  </div>
</div>

<div class="content margem-desktop">
  <hr>

  <div class="columns is-variable is-6">
    <div class="column is-two-thirds">
      
      <h4 class="title is-4"><i class="fas fa-briefcase"></i> O que faço</h4>
      
      <p class="is-size-5">
        No <strong>Cetic.br (NIC.br)</strong>, atuo na inovação metodológica desenvolvendo ferramentas de visualização e aplicando <strong>NLP</strong> e <strong>Web Scraping</strong> para a produção de estatísticas TIC.
      </p>
      
      <p class="is-size-5">
        Como consultor independente, foco em <strong>automação de análises (R/Python)</strong> e <strong>Inferência Causal</strong> (incluindo Testes A/B) para resolver problemas de negócio complexos.
      </p>

      <p class="is-size-5">
        Doutor em Ciência Política pela <strong>USP</strong>, com tese sobre os impactos da Inteligência Artificial na estrutura do emprego e renda.
      </p>
    </div>
    
    <div class="column">
      <h4 class="title is-4"><i class="fas fa-tools"></i> Expertise</h4>
      
      <strong class="is-size-5">Tecnologias:</strong><br>
      <div class="tags">
        <span class="tag is-info is-light is-medium">R (Expert)</span>
        <span class="tag is-info is-light is-medium">Python</span>
        <span class="tag is-info is-light is-medium">SQL</span>
        <span class="tag is-info is-light is-medium">Git</span>
      </div>

      <strong class="is-size-5">Métodos:</strong><br>
      <div class="tags">
        <span class="tag is-success is-light is-medium">Survey Analysis</span>
        <span class="tag is-success is-light is-medium">Survey Design</span>
        <span class="tag is-success is-light is-medium">Inferência Causal</span>
        <span class="tag is-success is-light is-medium">Machine Learning</span>
        <span class="tag is-success is-light is-medium">NLP</span>
        <span class="tag is-success is-light is-medium">Web Scraping</span>
        <span class="tag is-success is-light is-medium">Análise geoespacial</span>
      </div>
    </div>
  </div>

</div>

<div class="content margem-desktop">
<div class="columns is-centered" style="margin-top: 2rem;">
  
  <div class="column is-one-third">
    <div class="card h-100" style="border: 1px solid #eee;">
      <div class="card-content has-text-centered">
        <p class="title is-5"><i class="fas fa-project-diagram has-text-primary"></i> Portfólio</p>
        <p class="subtitle is-6">Veja a aplicação prática em meus projetos</p>
        <a href="/portfolio/" class="button is-primary is-outlined is-fullwidth">Acessar Projetos</a>
      </div>
    </div>
  </div>

  <div class="column is-one-third">
    <div class="card h-100" style="border: 1px solid #eee;">
      <div class="card-content has-text-centered">
        <p class="title is-5"><i class="fas fa-rss has-text-info"></i> Blog</p>
        <p class="subtitle is-6">Sobre Causalidade, Survey e Representatividade</p>
        <a href="/blog/" class="button is-info is-outlined is-fullwidth">Ler Artigos</a>
      </div>
    </div>
  </div>

  <div class="column is-one-third">
    <div class="card h-100" style="border: 1px solid #eee;">
      <div class="card-content has-text-centered">
        <p class="title is-5"><i class="fas fa-file-pdf has-text-link"></i> Currículo</p>
        <p class="subtitle is-6">Minha trajetória acadêmica e profissional</p>
        <a href="/cv/" class="button is-link is-outlined is-fullwidth">Ver CV Completo</a>
      </div>
    </div>
  </div>

</div>

</div>

<div class="content has-text-centered" style="margin-top: 4rem; margin-bottom: 3rem;">
  
  <hr>
  
  <h4 class="title is-5">Vamos conectar?</h4>

  <div class="buttons is-centered">
    
    <a href="https://www.linkedin.com/in/thiago-meireles-84605823" class="button is-medium is-white" target="_blank" title="LinkedIn">
      <span class="icon is-medium has-text-link">
        <i class="fab fa-linkedin fa-lg"></i>
      </span>
    </a>

    <a href="https://substack.com/@thiagomeireles1" class="button is-medium is-white" target="_blank" title="Newsletter / Substack">
      <span class="icon is-medium has-text-warning">
        <i class="fas fa-bookmark fa-lg"></i>
      </span>
    </a>

    <a href="https://github.com/thiagomeireles" class="button is-medium is-white" target="_blank" title="GitHub">
      <span class="icon is-medium has-text-dark">
        <i class="fab fa-github fa-lg"></i>
      </span>
    </a>

    <a href="https://x.com/thiago_meireles" class="button is-medium is-white" target="_blank" title="X / Twitter">
      <span class="icon is-medium has-text-grey-dark">
        <i class="fab fa-twitter fa-lg"></i>
      </span>
    </a>

    <a href="http://lattes.cnpq.br/8257251303675712" class="button is-medium is-white" target="_blank" title="Currículo Lattes">
      <span class="icon is-medium has-text-info">
        <i class="fas fa-graduation-cap fa-lg"></i>
      </span>
    </a>

    <a href="mailto:thiago.omeireles@gmail.com" class="button is-medium is-white" title="Entre em contato">
      <span class="icon is-medium has-text-danger">
        <i class="fas fa-envelope fa-lg"></i>
      </span>
    </a>

  </div>
</div>