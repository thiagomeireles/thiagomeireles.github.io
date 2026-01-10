---
layout: page
title: "Home"
permalink: /
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
          Lidero projetos de análise de dados e inovação metodológica, combinando técnicas avançadas de <strong>Machine Learning</strong> e <strong>Estatística</strong> para resolver problemas complexos e oferecer soluções acessíveis para gestores e stakeholders.
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
        Como consultor, foco em <strong>automação de análises (R/Python)</strong> e <strong>Inferência Causal</strong> (incluindo Testes A/B) para resolver problemas de negócio complexos.
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

<div class="content margem-desktop">
  <div class="columns is-centered" style="margin-top: 2rem;">
    
    <div class="column is-half">
      <div class="card h-100" style="border: 1px solid #eee;">
        <div class="card-content has-text-centered">
          <p class="title is-5"><i class="fas fa-comments has-text-primary"></i> Solicitação de Entrevistas e Palestras</p>
          <p class="subtitle is-6">Quer me convidar para um evento, podcast ou entrevista?</p>
          
          <button class="button is-primary is-outlined" 
                  data-tally-open="J9pykz" 
                  data-tally-layout="modal" 
                  data-tally-overlay="1" 
                  data-tally-emoji-text="🎙️"
                  data-tally-emoji-animation="wave">
            Solicitar Participação
          </button>
          
        </div>
      </div>
    </div>

    <div class="column is-half">
      <div class="card h-100" style="border: 1px solid #eee;">
        <div class="card-content has-text-centered">
          
          <p class="title is-5">
            <i class="fas fa-chart-line has-text-info"></i> Consultoria & Projetos
          </p>
          <p class="subtitle is-6">Soluções em Causalidade e ML para seu negócio</p>
          
          <button class="button is-info is-outlined is-fullwidth"
                  data-tally-open="44JZqX" 
                  data-tally-emoji-text="📊" 
                  data-tally-emoji-animation="heart-beat">
            Discutir um Projeto
          </button>

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

    <a href="http://lattes.cnpq.br/8257251303675712" class="button is-medium is-white" target="_blank" title="Currículo Lattes">
      <span class="icon is-medium has-text-info">
        <i class="fas fa-graduation-cap fa-lg"></i>
      </span>
    </a>

    <button class="button is-medium is-white" title="Clique para copiar o e-mail" onclick="copiarEmail()">
  <span class="icon is-medium has-text-danger">
    <i class="fas fa-envelope fa-lg" id="icon-email"></i>
  </span>
</button>

<script>
  function copiarEmail() {
    // 1. Define o e-mail
    const email = "thiago.omeireles@gmail.com";
    
    // 2. Copia para a área de transferência
    navigator.clipboard.writeText(email).then(() => {
      
      // 3. Feedback Visual: Troca o ícone de carta por um check
      const icon = document.getElementById('icon-email');
      
      // Remove o envelope e põe o check
      icon.classList.remove('fa-envelope');
      icon.classList.add('fa-check');
      
      // (Opcional) Muda a cor para verde para reforçar o sucesso
      icon.parentElement.classList.remove('has-text-danger');
      icon.parentElement.classList.add('has-text-success');

      // 4. Espera 2 segundos e volta ao normal
      setTimeout(() => {
        icon.classList.remove('fa-check');
        icon.classList.add('fa-envelope');
        
        icon.parentElement.classList.remove('has-text-success');
        icon.parentElement.classList.add('has-text-danger');
      }, 2000);
    });
  }
</script>

  </div> 
</div>

<script async src="https://tally.so/widgets/embed.js"></script>
