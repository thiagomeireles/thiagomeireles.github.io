---
layout: page
title: "Currículo"
permalink: /cv/
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

<div style="text-align: center; margin-bottom: 2rem;">
  <img src="/assets/images/banner_pt.jpg" alt="Thiago Meireles" 
       style="width: auto; height: auto; object-fit: cover; border-radius: 6px;">
</div>

<div style="text-align: center; margin-bottom: 20px;">
  <a href="/assets/pdf/cv.pdf" class="button is-link is-outlined" target="_blank">
    <span class="icon"><i class="fas fa-download"></i></span>
    <span>Baixar PDF Completo</span>
  </a>
</div>

<div style="width: 100%; height: 800px;">
    <object data="/assets/pdf/cv.pdf" type="application/pdf" width="100%" height="100%">
        <p>Seu navegador não suporta visualização de PDF. 
        <a href="/assets/pdf/cv.pdf">Clique aqui para baixar.</a></p>
    </object>
</div>