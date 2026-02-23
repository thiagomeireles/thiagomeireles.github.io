---
layout: page
title: "Portfólio"
permalink: /portfolio/
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
      grid-template-columns: repeat(2, 1fr);
    }
  }
  @media (min-width: 1100px) {
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

{% include banner.html %}

Aqui estão alguns dos projetos desenvolvidos ao longo da minha trajetória acadêmica e profissional, focados em **Ciência de Dados**, **Visualização** e **Inferência Causal**.

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

  <h2 class="section-title" style="margin-top: 4rem;">Relatórios Técnicos</h2>
  
  <div class="news-grid">


<div class="news-card">
  <div>
    <div class="news-source-date">MAI 2025 | WAPOR Annual Conference
</div>
    
    <a href="https://access.wapor.org/conf18/uploads/wapor/53/4193/26/Exploring_Methodological_Alternatives_in_the_ICT_Panel_Survey.pdf?" target="_blank" class="news-title">
      Exploring Methodological Alternatives in the ICT Panel Survey: Insights from Open-Ended Questions

    </a>
    
    <p class="news-snippet">
      This study explores methodological alternatives for analysing open-ended responses in large-scale digital surveys, using the ICT Panel Survey in Brazil as a case study. It integrates qualitative and quantitative approaches through supervised machine learning models to enhance text classification.
      <br><br>
      <strong>Autores:</strong> Thiago Meireles, Winston Oyadomari & Marcelo Pitta.
    </p>
  </div>

  <a href="https://access.wapor.org/conf18/uploads/wapor/53/4193/26/Exploring_Methodological_Alternatives_in_the_ICT_Panel_Survey.pdf?" target="_blank" class="news-link-btn">
    Acessar Publicação
  </a>
</div>


 <div class="news-card">
  <div>
    <div class="news-source-date">NOV 2024 | Panorama Setorial da Internet, Número 4, Ano 16</div>
    
    <a href="https://cetic.br/media/docs/publicacoes/6/20241218183020/ano-xvi-n-4-ia-mercado-trabalho.pdf" target="_blank" class="news-title">
      Inteligência Artificial e mercado de trabalho. Dessa vez é diferente?
    </a>
    
    <p class="news-snippet">
      Pensar em indicadores para medir o impacto da Inteligência Artificial sobre o mundo do trabalho passa por saber o que os trabalhadores fazem.
      <br><br>
      <strong>Autor:</strong> Thiago Meireles.
    </p>
  </div>

  <a href="https://cetic.br/media/docs/publicacoes/6/20241218183020/ano-xvi-n-4-ia-mercado-trabalho.pdf" target="_blank" class="news-link-btn">
    Acessar Publicação
  </a>
</div>

<div class="news-card">
  <div>
    <div class="news-source-date">NOV 2023 | VI Escola de Amostragem e Métodos de Pesquisa</div>
    
    <a href="assets/pdf/esamp2023.pdf" target="_blank" class="news-title">
      Ponderação da base de dados de medições Simet em escolas públicas

    </a>
    
    <p class="news-snippet">
      Sendo a população de escolas básicas conhecida e suas características medidas pelo Censo Escolar, a metodologia de ponderação apresentada tem como objetivo permitir a estimação de medidas de qualidade da Internet para o conjunto de todas as escolas a partir das medições disponíveis somente para as escolas da amostra de escolas participantes no Simet.
      <br><br>
      <strong>Autores:</strong> Marcelo Pitta, Thiago Meireles & Pedro Luis do Nascimento Silva.
    </p>
  </div>

  <a href="/assets/pdf/esamp2023.pdf" target="_blank" class="news-link-btn">
    Acessar Publicação
  </a>
</div>

<div class="news-card">
  <div>
    <div class="news-source-date">28 AGO 2020 | Rede de Políticas Públicas & Sociedade</div>
    
    <a href="https://redepesquisasolidaria.org/boletins/boletim-22/sem-diretrizes-para-o-ensino-remoto-e-a-volta-as-aulas-governo-federal-repete-na-educacao-a-tragedia-da-saude-milhoes-de-criancas-ficaram-em-casa-sem-atividades-escolares-e-os-mais-pobres-perderam-a/" target="_blank" class="news-title">
      Nota Técnica N° 22: Sem diretrizes para o ensino remoto e a volta às aulas, governo Federal repete na educação a tragédia da saúde. Milhões de crianças ficaram em casa sem atividades escolares e os mais pobres perderam até 50 dias letivos de aula
    </a>
    
    <p class="news-snippet">
     Na pandemia, o Ministério da Educação não orientou nem difundiu metodologias bem-sucedidas de ensino à distância. Os estados definiram suas próprias estratégias e colheram resultados bem diferentes com as atividades escolares realizadas remotamente. Mais de 8 milhões de crianças entre 6 e 14 anos ficaram sem atividades escolares para fazer em casa.
     <strong>Autores:</strong> Ian Prates, Hellen Guicheney, Thiago Meireles, et al.
    </p>
  </div>

  <a href="https://redepesquisasolidaria.org/boletins/boletim-22/sem-diretrizes-para-o-ensino-remoto-e-a-volta-as-aulas-governo-federal-repete-na-educacao-a-tragedia-da-saude-milhoes-de-criancas-ficaram-em-casa-sem-atividades-escolares-e-os-mais-pobres-perderam-a/" target="_blank" class="news-link-btn">
    Acessar Publicação
  </a>
</div>

<div class="news-card">
  <div>
    <div class="news-source-date">17 JUL 2020 | Rede de Políticas Públicas & Sociedade</div>
    
    <a href="https://redepesquisasolidaria.org/boletins/boletim-16/crise-altera-o-perfil-do-trabalho-em-casa-e-do-teletrabalho-desigualdade-digital-reduz-rendimentos-e-rebaixa-atividade-economica/" target="_blank" class="news-title">
      Nota Técnica N° 17: Crise altera o perfil do trabalho em casa e do teletrabalho. Desigualdade digital reduz rendimentos e rebaixa atividade econômica
    </a>
    
    <p class="news-snippet">
     Antes da pandemia, o percentual de pessoas que trabalhavam de casa no Brasil (4,9%) não era muito diferente do que ocorria, por exemplo, nos países da União Europeia (5,4%). Com a pandemia, o trabalho em casa mudou, em volume e em qualidade. O percentual de pessoas que trabalha a partir de suas residências saltou de 4,9%, em 2019, para 10,3% em maio de 2020. Se em 2019 os autônomos eram 88,3% do total das pessoas que trabalhavam em casa, hoje representam menos que 15% do total.
     <strong>Autores:</strong> Rogério Jerônimo Barbosa, Ian Prates, Fábio Senne, Leonardo Lins, Thiago Meireles, et al.
    </p>
  </div>

  <a href="https://redepesquisasolidaria.org/boletins/boletim-16/crise-altera-o-perfil-do-trabalho-em-casa-e-do-teletrabalho-desigualdade-digital-reduz-rendimentos-e-rebaixa-atividade-economica/" target="_blank" class="news-link-btn">
    Acessar Publicação
  </a>
</div>

<div class="news-card">
  <div>
    <div class="news-source-date">03 JUL 2020 | Rede de Políticas Públicas & Sociedade</div>
    
    <a href="https://redepesquisasolidaria.org/boletins/boletim-14/situacao-dramatica-do-desemprego-esta-oculta-nos-indicadores-oficiais-sem-renda-emergencial-de-r-60000-a-pobreza-atingiria-30-da-populacao/" target="_blank" class="news-title">
      Nota Técnica N° 14: Situação dramática do desemprego está oculta nos indicadores oficiais. Sem renda emergencial de R$ 600,00 a pobreza atingiria 30% da população
    </a>
    
    <p class="news-snippet">
     Pela primeira vez na história, o nível de ocupação entre março e abril ficou abaixo de 50%, ou seja, mais pessoas estavam sem trabalho do que trabalhando em todo o país, segundo a PNAD-Covid do IBGE.
     <strong>Autores:</strong> Ian Prates, Rogério Jerônimo Barbosa, Thiago Meireles, et al.
    </p>
  </div>

  <a href="https://redepesquisasolidaria.org/boletins/boletim-14/situacao-dramatica-do-desemprego-esta-oculta-nos-indicadores-oficiais-sem-renda-emergencial-de-r-60000-a-pobreza-atingiria-30-da-populacao/" target="_blank" class="news-link-btn">
    Acessar Publicação
  </a>
</div>

<div class="news-card">
  <div>
    <div class="news-source-date">29 MAI 2020 | Rede de Políticas Públicas & Sociedade</div>
    
    <a href="https://redepesquisasolidaria.org/boletins/boletim-8/auxilio-de-r-60000-precisa-continuar-e-pode-ser-financiado-por-contribuicao-emergencial-sobre-altas-rendas/" target="_blank" class="news-title">
      Nota Técnica N° 8: Auxílio de R$ 600,00 precisa continuar e pode ser financiado por contribuição emergencial sobre altas rendas
    </a>
    
    <p class="news-snippet">
     O prolongamento do distanciamento social desestruturou o mercado de trabalho e afetou o emprego dos trabalhadores e a renda das famílias. O debate público sobre a prorrogação da Renda Básica Emergencial tornou-se inevitável, na sociedade e no Congresso nacional.
     <strong>Autores:</strong> Ian Prates, Rogério Jerônimo Barbosa, Thiago Meireles, et al.
    </p>
  </div>

  <a href="https://redepesquisasolidaria.org/boletins/boletim-8/auxilio-de-r-60000-precisa-continuar-e-pode-ser-financiado-por-contribuicao-emergencial-sobre-altas-rendas/" target="_blank" class="news-link-btn">
    Acessar Publicação
  </a>
</div>

<div class="news-card">
  <div>
    <div class="news-source-date">08 MAI 2020 | Rede de Políticas Públicas & Sociedade</div>
    
    <a href="https://redepesquisasolidaria.org/wp-content/uploads/2020/05/boletim5.pdf" target="_blank" class="news-title">
      Nota Técnica N° 5: Dificuldades com aplicativo e não uso da rede de proteção atual limitam acesso ao auxílio de emergência

    </a>
    
    <p class="news-snippet">
     O governo optou pela implementação tecnológica que apresenta problemas por conta da baixa familiaridade e acesso da população de baixa renda às Tecnologias de Informação e Comunicação (aplicativos, telefones e computadores).
     <strong>Autores:</strong> Rogério Jerônimo Barbosa, Ian Prates, Thiago Meireles, et al.
    </p>
  </div>

  <a href="https://redepesquisasolidaria.org/wp-content/uploads/2020/05/boletim5.pdf" target="_blank" class="news-link-btn">
    Acessar Publicação
  </a>
</div>

<div class="news-card">
  <div>
    <div class="news-source-date">17 ABR 2020 | Rede de Políticas Públicas & Sociedade</div>
    
    <a href="https://redepesquisasolidaria.org/wp-content/uploads/2020/05/boletim2.pdf" target="_blank" class="news-title">
      Nota Técnica N° 2: A vulnerabilidade dos trabalhadores brasileiros na pandemia da Covid-19

    </a>
    
    <p class="news-snippet">
Identificar os segmentos mais frágeis de trabalhadores brasileiros e mensurar o grau de sua vulnerabilidade como procedimento chave para a execução de políticas públicas de qualidade.      <br><br>
      <strong>Autores:</strong> Rogério Jerônimo Barbosa, Ian Prates e Thiago Meireles.
    </p>
  </div>

  <a href="https://redepesquisasolidaria.org/wp-content/uploads/2020/05/boletim2.pdf" target="_blank" class="news-link-btn">
    Acessar Publicação
  </a>
</div>

</div>

  <h2 class="section-title" style="margin-top: 4rem;">Outros Projetos</h2>
  <p>
    Para a lista completa de repositórios, visite meu 
    <a href="https://github.com/thiagomeireles" target="_blank" style="color: #0056b3; font-weight: bold;">GitHub</a>.
  </p>

</div>