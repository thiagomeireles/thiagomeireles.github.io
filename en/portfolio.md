---
layout: page
title: "Portfolio"
permalink: /en/portfolio/
show_sidebar: false
---

<style>
  /* --- GRID 1: YOUTUBE VIDEOS --- */
  .video-grid {
    display: grid;
    grid-template-columns: 1fr;
    gap: 2rem;
    margin-bottom: 2rem;
  }

  @media (min-width: 768px) {
    .video-grid {
      grid-template-columns: repeat(2, 1fr);
    }
  }

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
      grid-template-columns: repeat(3, 1fr);
    }
  }

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

{% include banner_en.html %}

A selection of academic and professional work: webinars, conference papers and technical notes. Most of it deals with survey methodology, the use of nonprobability samples and the Brazilian labor market.

<div class="margem-desktop">

  <h2 class="section-title">Webinars</h2>
  
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
        <iframe src="https://www.youtube.com/embed/j7LAC6QqAAw" 
                title="Weighting a nonprobability sample to estimate Internet quality indicator for Brazilian schools" allowfullscreen></iframe>
      </div>
      <div class="video-caption">UN Big Data Regional Hub: Weighting a nonprobability sample to estimate Internet quality indicator for Brazilian schools</div>
    </div>

  </div>

  <h2 class="section-title" style="margin-top: 4rem;">Papers and technical reports</h2>
  
  <div class="news-grid">

    <div class="news-card">
      <div>
        <div class="news-source-date">OCT 2026 | WAPOR Annual Conference</div>
        <a href="https://access.wapor.org/conf18/uploads/wapor/195/4193/92/Classifying_Open_Ended_Questions.pdf?" target="_blank" class="news-title">
          Classifying Open-Ended Questions: A Comparison of Supervised Learning and Large Language Models
        </a>
        <p class="news-snippet">
          The study compares two strategies for classifying answers to the question “What does privacy mean to you?” into six predefined categories. The data come from the 2025 edition of the ICT Panel, conducted by Cetic.br and dedicated to online privacy.
          <br><br>
          <strong>Authors:</strong> Thiago Meireles & Winston Oyadomari.
        </p>
      </div>
      <a href="https://access.wapor.org/conf18/uploads/wapor/195/4193/92/Classifying_Open_Ended_Questions.pdf?" target="_blank" class="news-link-btn">Read the publication</a>
    </div>

    <div class="news-card">
      <div>
        <div class="news-source-date">MAY 2025 | WAPOR Annual Conference</div>
        <a href="https://access.wapor.org/conf18/uploads/wapor/53/4193/26/Exploring_Methodological_Alternatives_in_the_ICT_Panel_Survey.pdf?" target="_blank" class="news-title">
          Exploring Methodological Alternatives in the ICT Panel Survey: Insights from Open-Ended Questions
        </a>
        <p class="news-snippet">
          The study tests methodological alternatives for analyzing open-ended responses in large-scale online surveys, using Brazil's ICT Panel as the case. To classify the texts, it combines qualitative and quantitative approaches through supervised machine learning models.
          <br><br>
          <strong>Authors:</strong> Thiago Meireles, Winston Oyadomari & Marcelo Pitta.
        </p>
      </div>
      <a href="https://access.wapor.org/conf18/uploads/wapor/53/4193/26/Exploring_Methodological_Alternatives_in_the_ICT_Panel_Survey.pdf?" target="_blank" class="news-link-btn">Read the publication</a>
    </div>

    <div class="news-card">
      <div>
        <div class="news-source-date">NOV 2024 | Internet Sector Panorama</div>
        <a href="https://cetic.br/media/docs/publicacoes/6/20241218183020/ano-xvi-n-4-ia-mercado-trabalho.pdf" target="_blank" class="news-title">
          Artificial Intelligence and the Labor Market: Is it different this time?
        </a>
        <p class="news-snippet">
          Developing indicators to measure the impact of AI on the world of work requires understanding what workers actually do.
          <br><br>
          <strong>Author:</strong> Thiago Meireles.
        </p>
      </div>
      <a href="https://cetic.br/media/docs/publicacoes/6/20241218183020/ano-xvi-n-4-ia-mercado-trabalho.pdf" target="_blank" class="news-link-btn">Read the publication</a>
    </div>

    <div class="news-card">
      <div>
        <div class="news-source-date">NOV 2023 | VI School of Sampling and Survey Methods (ESAMP)</div>
        <a href="assets/pdf/esamp2023.pdf" target="_blank" class="news-title">
          Weighting the Simet measurement database for public schools
        </a>
        <p class="news-snippet">
          Since the population of basic education schools is known and its characteristics are measured by the School Census, the weighting allows Internet quality to be estimated for all schools from measurements taken only at the schools that take part in Simet.
          <br><br>
          <strong>Authors:</strong> Marcelo Pitta, Thiago Meireles & Pedro Luis do Nascimento Silva.
        </p>
      </div>
      <a href="/assets/pdf/esamp2023.pdf" target="_blank" class="news-link-btn">Read the publication</a>
    </div>

    <div class="news-card">
      <div>
        <div class="news-source-date">AUG 2020 | Solidarity Research Network</div>
        <a href="..." target="_blank" class="news-title">
          Technical Note No. 22: Without guidelines for remote learning, the Federal Government repeats the health tragedy in education.
        </a>
        <p class="news-snippet">
          During the pandemic, the Ministry of Education neither guided nor spread successful remote learning methods. States set their own strategies and got very different results, and more than 8 million children aged 6 to 14 had no school activities to do at home.
          <br><br>
          <strong>Authors:</strong> Ian Prates, Hellen Guicheney, Thiago Meireles, et al.
        </p>
      </div>
      <a href="..." target="_blank" class="news-link-btn">Read the publication</a>
    </div>

    <div class="news-card">
      <div>
        <div class="news-source-date">JUL 2020 | Solidarity Research Network</div>
        <a href="..." target="_blank" class="news-title">
          Technical Note No. 17: Crisis alters the profile of home-based work. Digital inequality reduces earnings.
        </a>
        <p class="news-snippet">
          The share of people working from home jumped from 4.9% in 2019 to 10.3% in May 2020. In 2019, self-employed workers were 88.3% of those working from home; by May 2020 they were less than 15%.
          <br><br>
          <strong>Authors:</strong> Rogério Jerônimo Barbosa, Ian Prates, Fábio Senne, Leonardo Lins, Thiago Meireles, et al.
        </p>
      </div>
      <a href="..." target="_blank" class="news-link-btn">Read the publication</a>
    </div>

    <div class="news-card">
      <div>
        <div class="news-source-date">JUL 2020 | Solidarity Research Network</div>
        <a href="..." target="_blank" class="news-title">
          Technical Note No. 14: Dramatic unemployment situation is hidden in official indicators.
        </a>
        <p class="news-snippet">
          For the first time on record, the employment rate fell below 50% between March and April, according to IBGE's PNAD-Covid: more people were out of work than working. Without the emergency income, poverty would have reached 30% of the population.
          <br><br>
          <strong>Authors:</strong> Ian Prates, Rogério Jerônimo Barbosa, Thiago Meireles, et al.
        </p>
      </div>
      <a href="..." target="_blank" class="news-link-btn">Read the publication</a>
    </div>

    <div class="news-card">
      <div>
        <div class="news-source-date">MAY 2020 | Solidarity Research Network</div>
        <a href="https://redepesquisasolidaria.org/boletins/boletim-8/auxilio-de-r-60000-precisa-continuar-e-pode-ser-financiado-por-contribuicao-emergencial-sobre-altas-rendas/" target="_blank" class="news-title">
          Technical Note No. 8: R$ 600.00 aid must continue and can be financed by emergency contribution on high incomes
        </a>
        <p class="news-snippet">
          Prolonged social distancing disrupted the labor market, hitting workers' jobs and family income. As a result, extending the Emergency Basic Income became an unavoidable debate, both in society and in the National Congress.
          <br><br>
          <strong>Authors:</strong> Ian Prates, Rogério Jerônimo Barbosa, Thiago Meireles, et al.
        </p>
      </div>
      <a href="https://redepesquisasolidaria.org/boletins/boletim-8/auxilio-de-r-60000-precisa-continuar-e-pode-ser-financiado-por-contribuicao-emergencial-sobre-altas-rendas/" target="_blank" class="news-link-btn">Read the publication</a>
    </div>

    <div class="news-card">
      <div>
        <div class="news-source-date">MAY 2020 | Solidarity Research Network</div>
        <a href="https://redepesquisasolidaria.org/wp-content/uploads/2020/05/boletim5.pdf" target="_blank" class="news-title">
          Technical Note No. 5: Difficulties with the application and non-use of the current protection network limit access to emergency aid
        </a>
        <p class="news-snippet">
          The government chose an app-based implementation that runs into the low-income population's limited access to, and familiarity with, Information and Communication Technologies (apps, phones and computers).
          <br><br>
          <strong>Authors:</strong> Rogério Jerônimo Barbosa, Ian Prates, Thiago Meireles, et al.
        </p>
      </div>
      <a href="https://redepesquisasolidaria.org/wp-content/uploads/2020/05/boletim5.pdf" target="_blank" class="news-link-btn">Read the publication</a>
    </div>

    <div class="news-card">
      <div>
        <div class="news-source-date">APR 2020 | Solidarity Research Network</div>
        <a href="https://redepesquisasolidaria.org/wp-content/uploads/2020/05/boletim2.pdf" target="_blank" class="news-title">
          Technical Note No. 2: The vulnerability of Brazilian workers in the Covid-19 pandemic
        </a>
        <p class="news-snippet">
          The note identifies the most fragile segments of Brazilian workers and measures how vulnerable each one is, a necessary step for designing public policies that reach those who need them.
          <br><br>
          <strong>Authors:</strong> Rogério Jerônimo Barbosa, Ian Prates and Thiago Meireles.
        </p>
      </div>
      <a href="https://redepesquisasolidaria.org/wp-content/uploads/2020/05/boletim2.pdf" target="_blank" class="news-link-btn">Read the publication</a>
    </div>

  </div>

  <h2 class="section-title" style="margin-top: 4rem;">Other projects</h2>
  <p>
    My code repositories are on 
    <a href="https://github.com/thiagomeireles" target="_blank" style="color: #0056b3; font-weight: bold;">GitHub</a>.
  </p>

</div>