---
layout: page
title: "Home"
permalink: /en/
---

<style>
  /* --- 1. NEW: Increase Menu Font Size (Navbar) --- */
  /* Size 2rem is equivalent to is-size-3 */
  .navbar-item, .navbar-link {
    font-size: 2rem !important; 
    padding-top: 1rem !important;    /* More vertical space */
    padding-bottom: 1rem !important;
  }
  
  /* Ensures your name (Logo) is also large */
  .navbar-brand .navbar-item {
    font-size: 2rem !important;
    font-weight: bold;
  }

  /* --- 2. REMOVE DEFAULT BANNER (Hero) --- */
  /* Hides the giant banner body */
  .hero-body { 
    display: none !important; 
  }
  /* Removes min-height and transparent background */
  .hero { 
    min-height: 0 !important; 
    background-color: transparent !important; 
  }

  /* --- 3. MENU COLOR --- */
  /* Ensures the menu background is the theme's solid color */
  .navbar { 
    background-color: #00d1b2 !important; 
  }

  /* --- 4. CENTRALIZATION AND MARGINS (Desktop) --- */
  /* On mobile, uses 100% of the screen */
  .skill-tag { margin-bottom: 0.5rem; }

  /* Rule applied only for screens larger than 1024px (Computers/Laptops) */
  @media screen and (min-width: 1024px) {
    .margem-desktop {
      padding-left: 3rem;  /* Adjust value as needed */
      padding-right: 3rem; /* Use padding to avoid breaking total width */
    }
  }
</style>

<div class="content margem-desktop">
  <div class="columns is-vcentered">
  
    <div class="column is-4 has-text-centered">
      <figure class="image is-square" style="max-width: 300px; margin: 0 auto;">
        <img class="is-rounded" src="/assets/images/perfil.png" alt="Photo of Thiago Meireles" style="border: 4px solid #f5f5f5; box-shadow: 0 4px 10px rgba(0,0,0,0.1);">
      </figure>
    </div>

    <div class="column">
      <div class="content">

        <div style="margin-top: 1.5rem;">
          <img src="/assets/images/banner_en.jpg" alt="Thiago Meireles" 
               style="width: 100%; height: auto; object-fit: cover; border-radius: 6px;">
        </div>

        <p class="is-size-3">
          I am <strong>Thiago Meireles</strong>, Data Scientist and Ph.D. in Political Science from USP.
        </p>
        
        <p class="is-size-4">
          I lead data analysis and methodological innovation projects at <strong>Cetic.br</strong>, combining advanced <strong>Machine Learning</strong> and <strong>Statistics</strong> techniques to solve complex problems and offer accessible solutions for managers and stakeholders.
        </p>

      </div>
    </div>

  </div>
</div>

<div class="content margem-desktop">
  <hr>

  <div class="columns is-variable is-6">
    <div class="column is-two-thirds">
      
      <h4 class="title is-4"><i class="fas fa-briefcase"></i> What I do</h4>
      
      <p class="is-size-5">
        At <strong>Cetic.br (NIC.br)</strong>, I work on methodological innovation by developing visualization tools and applying <strong>NLP</strong> and <strong>Web Scraping</strong> for the production of ICT statistics.
      </p>
      
      <p class="is-size-5">
        As an independent consultant, I focus on <strong>analysis automation (R/Python)</strong> and <strong>Causal Inference</strong> (including A/B Testing) to solve complex business problems.
      </p>

      <p class="is-size-5">
        Ph.D. in Political Science from <strong>USP</strong>, with a thesis on the impacts of Artificial Intelligence on employment structure and income.
      </p>
    </div>
    
    <div class="column">
      <h4 class="title is-4"><i class="fas fa-tools"></i> Expertise</h4>
      
      <strong class="is-size-5">Technologies:</strong><br>
      <div class="tags">
        <span class="tag is-info is-light is-medium">R (Expert)</span>
        <span class="tag is-info is-light is