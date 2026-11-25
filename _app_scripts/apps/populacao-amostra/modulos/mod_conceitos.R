# =============================================================================
# Script:  mod_conceitos.R (módulo: os quatro conceitos, notação e quiz)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-25
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/apps/populacao-amostra/app.R)
# Saída:
#   - funções mod_conceitos_ui() e mod_conceitos_server()
# =============================================================================

conceitos <- list(
  populacao = list(
    nome = tr("População", "Population"),
    pergunta = tr("O que se quer conhecer", "What we want to know"),
    definicao = tr(
      paste(
        "O conjunto de todos os elementos (pessoas, domicílios, empresas) que têm a característica de interesse.",
        "Pode ser concreta, como os eleitores de 2026, ou hipotética, como todas as peças que uma máquina ainda vai produzir."
      ),
      paste(
        "The total set of subjects of interest (people, households, firms). It can be real, like the voters in a",
        "given election, or conceptual, like all the parts a machine will ever make."
      )
    ),
    exemplo = tr("todos os adultos que moram no Brasil.", "all adults living in Brazil.")
  ),
  amostra = list(
    nome = tr("Amostra", "Sample"),
    pergunta = tr("O que de fato se observa", "What we actually observe"),
    definicao = tr(
      paste(
        "A parte da população que de fato é observada. Para dizer algo confiável sobre a população, a amostra",
        "precisa ser sorteada, e não escolhida por conveniência."
      ),
      paste(
        "The subset of the population on which data are actually collected. To say something reliable about the",
        "population, it has to be drawn at random, not chosen by convenience."
      )
    ),
    exemplo = tr(
      "os cerca de 200 mil domicílios visitados a cada trimestre pela PNAD Contínua.",
      "the roughly 200,000 households visited each quarter by Brazil's continuous household survey (PNAD Contínua)."
    )
  ),
  parametro = list(
    nome = tr("Parâmetro", "Parameter"),
    pergunta = tr("Medida da população", "A measure of the population"),
    definicao = tr(
      paste(
        "Uma medida que resume a população, como a média ou a proporção de todos os elementos. É um número fixo,",
        "mas em geral desconhecido, porque raramente se observa a população inteira."
      ),
      paste(
        "A numerical summary of the population, such as the mean or the proportion over all subjects. It is a fixed",
        "number, but usually unknown, because the whole population is rarely observed."
      )
    ),
    exemplo = tr("a proporção de todos os adultos do país que usam internet.", "the proportion of all adults in the country who use the internet.")
  ),
  estatistica = list(
    nome = tr("Estatística", "Statistic"),
    pergunta = tr("Medida da amostra", "A measure of the sample"),
    definicao = tr(
      "Uma medida calculada com os dados da amostra. Muda de uma amostra para outra e serve para estimar o parâmetro correspondente.",
      "A numerical summary of the sample data. It changes from one sample to another and is used to estimate the corresponding parameter."
    ),
    exemplo = tr(
      "a proporção de usuários de internet entre os entrevistados de uma pesquisa.",
      "the proportion of internet users among the people interviewed in a survey."
    )
  )
)

cartao_conceito <- function(chave) {
  cc <- conceitos[[chave]]
  bslib::card(
    bslib::card_header(
      class = if (chave %in% c("populacao", "parametro")) "bg-primary text-white" else "bg-info text-white",
      cc$nome, shiny::tags$small(class = "ms-2 opacity-75", cc$pergunta)
    ),
    bslib::card_body(
      shiny::p(cc$definicao),
      shiny::p(class = "mb-0", shiny::strong(tr("Exemplo: ", "Example: ")), cc$exemplo)
    )
  )
}

# Símbolos: Morettin e Bussab em pt; Agresti e Finlay em en
tabela_notacao <- function() {
  tabela <- data.frame(
    tr(
      c("Tamanho", "Média", "Mediana", "Variância", "Desvio padrão", "Proporção"),
      c("Size", "Mean", "Median", "Variance", "Standard deviation", "Proportion")
    ),
    c("N", "μ", "Md", "σ²", "σ", tr("p", "π")),
    c("n", tr("X̄ (observada: x̄)", "ȳ"), "md", tr("S² (observada: s²)", "s²"), tr("S (observado: s)", "s"), tr("p̂", "π̂"))
  )
  names(tabela) <- tr(
    c("Medida", "Na população (parâmetro)", "Na amostra (estatística)"),
    c("Measure", "In the population (parameter)", "In the sample (statistic)")
  )
  tabela
}

banco_conceitos <- data.frame(
  afirmacao = tr(
    c(
      "O Censo 2022 contou 203 milhões de pessoas morando no Brasil.",
      "A PNAD Contínua divulgou a taxa de desocupação do último trimestre.",
      "Numa pesquisa eleitoral com 2.000 entrevistas, 41% disseram que votariam no candidato A.",
      "Apuradas todas as urnas, o candidato A teve 43% dos votos válidos.",
      "A pesquisa TIC Domicílios estima a proporção de domicílios com acesso à internet.",
      "A professora calculou a nota média da sua turma, e o interesse é só nessa turma.",
      "A mesma nota média da turma foi usada para falar de todos os alunos da escola.",
      "Uma enquete num portal de notícias teve 500 mil votos, e 70% foram a favor da proposta.",
      "A idade média de todos os servidores ativos, calculada com o cadastro completo da prefeitura.",
      "O desvio padrão da renda de 300 adultos sorteados numa cidade foi de R$ 1.900."
    ),
    c(
      "Brazil's 2022 Census counted 203 million residents.",
      "Brazil's continuous household survey released the unemployment rate for the last quarter.",
      "In a poll of 2,000 voters, 41% said they would vote for candidate A.",
      "Once every ballot was counted, candidate A got 43% of the valid votes.",
      "The ICT Households survey estimates the proportion of Brazilian households with internet access.",
      "A teacher computed the mean score of her class, and she only cares about that class.",
      "The same class mean was used to describe every student in the school.",
      "An online news poll got 500,000 votes, and 70% were in favor of the proposal.",
      "The mean age of all active city employees, computed from the city's complete personnel records.",
      "The standard deviation of income among 300 randomly chosen adults in a city was R$ 1,900."
    )
  ),
  resposta = c(
    "parametro", "estatistica", "estatistica", "parametro", "estatistica",
    "parametro", "estatistica", "estatistica", "parametro", "estatistica"
  ),
  explicacao = tr(
    c(
      "O censo observa a população inteira, então o número descreve a população e não precisa ser estimado.",
      "A taxa vem dos domicílios sorteados e é usada para estimar a taxa de todo o país.",
      "É a porcentagem entre os entrevistados, não entre todos os eleitores.",
      "A apuração cobre todos os votos válidos, ou seja, a população de interesse inteira.",
      "É uma pesquisa amostral: a proporção vem dos domicílios sorteados e tem margem de erro.",
      "Se o interesse é só a turma, ela é a população inteira. Não há o que estimar.",
      "Agora a turma é uma amostra da escola, e uma amostra que não foi sorteada. Generalizar para a escola inteira exige cuidado.",
      paste(
        "É uma estatística de uma amostra voluntária. Meio milhão de votos não corrige o viés: responde quem quer,",
        "e quem escolhe responder tende a pensar diferente de quem não responde."
      ),
      "O cadastro completo cobre todos os servidores, a população de interesse.",
      "É o s de uma amostra. O σ seria o desvio padrão da renda de todos os adultos da cidade."
    ),
    c(
      "A census observes the whole population, so the number describes the population and does not need to be estimated.",
      "The rate comes from the sampled households and is used to estimate the rate for the whole country.",
      "It is the percentage among the people interviewed, not among all voters.",
      "The count covers every valid vote, that is, the whole population of interest.",
      "It is a sample survey: the proportion comes from the sampled households and has a margin of error.",
      "If only that class matters, it is the whole population. There is nothing to estimate.",
      "Now the class is a sample of the school, and one that was not drawn at random. Generalizing to the whole school requires care.",
      paste(
        "It is a statistic from a volunteer sample. Half a million votes do not fix the bias: people choose whether",
        "to respond, and those who do tend to think differently from those who do not."
      ),
      "The complete records cover every employee, the population of interest.",
      "It is the s of a sample. σ would be the standard deviation of income among all adults in the city."
    )
  )
)

opcoes_quiz <- stats::setNames(
  c("parametro", "estatistica"),
  tr(c("Parâmetro", "Estatística"), c("Parameter", "Statistic"))
)

texto_intro_conceitos <- function() {
  tr(
    shiny::tagList(
      shiny::h4("O que se quer conhecer e o que de fato se observa"),
      shiny::p(
        class = "mb-0",
        "Quase toda pergunta estatística é sobre um grupo grande demais para ser observado por inteiro: todos os",
        "adultos do país, todos os domicílios de uma cidade, todas as peças que uma fábrica vai produzir. Esse grupo",
        "é a população, e o que se quer saber dela (uma média, uma proporção) é um parâmetro. O que se tem nas mãos,",
        "no entanto, é só uma parte desse grupo, a amostra, e as contas feitas com ela são estatísticas. Saber de qual",
        "lado está cada número evita confundir o que foi medido com o que se quer conhecer."
      )
    ),
    shiny::tagList(
      shiny::h4("What we want to know and what we actually observe"),
      shiny::p(
        class = "mb-0",
        "Almost every statistical question is about a group too large to observe in full: all adults in a country,",
        "all households in a city, all the parts a factory will ever make. That group is the population, and what we",
        "want to know about it (a mean, a proportion) is a parameter. What we actually have, however, is only part of",
        "that group, the sample, and the numbers computed from it are statistics. Knowing which side each number is on",
        "keeps us from confusing what was measured with what we want to know."
      )
    )
  )
}

nota_notacao <- function() {
  tr(
    paste(
      "Regra geral: letras gregas para parâmetros, latinas para estatísticas. A tabela segue Morettin e Bussab",
      "(seção 10.6): a maiúscula (X̄, S²) indica a estatística como regra, que varia de amostra para amostra; a",
      "minúscula (x̄, s²), o valor observado numa amostra concreta. Agresti e Finlay escrevem ȳ para a média da",
      "amostra e π e π̂ para a proporção. A variância da população divide por N; a da amostra, por n − 1 (a aba",
      "\"Estimador e estimativa\" mostra por quê). Na análise descritiva, porém, Morettin e Bussab dividem por n.",
      "Para a mediana não há letra grega consagrada; o app usa Md e md, como Morettin e Bussab."
    ),
    paste(
      "General rule: Greek letters for parameters, Roman letters for statistics. The table follows Agresti and",
      "Finlay. Morettin and Bussab write x̄ for the sample mean and p and p̂ for the proportion. The population",
      "variance divides by N; the sample variance, s², divides by n − 1 (the \"Estimator and estimate\" tab shows",
      "why). There is no standard Greek letter for the median; the app uses Md and md, as Morettin and Bussab do."
    )
  )
}

mod_conceitos_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::card(bslib::card_body(texto_intro_conceitos())),
    bslib::layout_column_wrap(
      width = "240px", fill = FALSE,
      cartao_conceito("populacao"), cartao_conceito("amostra"),
      cartao_conceito("parametro"), cartao_conceito("estatistica")
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(5, 7)),
      bslib::card(
        bslib::card_header(tr("Como chamar cada medida", "What to call each measure")),
        bslib::card_body(
          tabela_html(tabela_notacao()),
          shiny::p(class = "small text-muted mb-0", nota_notacao())
        )
      ),
      bslib::card(
        bslib::card_header(tr("Quiz: parâmetro ou estatística?", "Quiz: parameter or statistic?")),
        bslib::card_body(
          shiny::uiOutput(ns("pergunta")),
          shiny::radioButtons(ns("resposta"), NULL, choices = opcoes_quiz, selected = character(0), inline = TRUE),
          shiny::div(
            class = "d-flex gap-2 flex-wrap",
            shiny::actionButton(ns("conferir"), tr("Conferir", "Check"), class = "btn-primary", icon = shiny::icon("check")),
            shiny::actionButton(ns("proxima"), tr("Próxima", "Next"), icon = shiny::icon("arrow-right"))
          ),
          shiny::uiOutput(ns("feedback")),
          shiny::p(class = "small text-muted mb-0", shiny::textOutput(ns("placar"), inline = TRUE)),
          shiny::div(
            class = "mt-3",
            shiny::actionButton(ns("ir_amostra"), tr("Ver na prática: sortear uma amostra", "See it in action: draw a sample"), class = "btn-outline-primary btn-sm", icon = shiny::icon("arrow-right"))
          )
        )
      )
    )
  )
}

# `ir_para` recebe o value de uma aba e a abre (definido no app.R)
mod_conceitos_server <- function(id, ir_para = \(aba) NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    total <- nrow(banco_conceitos)
    estado <- shiny::reactiveValues(
      ordem = sample(total), posicao = 1, respondida = FALSE,
      acertou = FALSE, acertos = 0, respondidas = 0
    )

    atual <- shiny::reactive(banco_conceitos[estado$ordem[estado$posicao], ])

    output$pergunta <- shiny::renderUI({
      shiny::tagList(
        shiny::p(class = "text-muted small mb-1", sprintf(tr("Afirmação %d de %d", "Statement %d of %d"), estado$posicao, total)),
        shiny::h5(atual()$afirmacao),
        shiny::p(class = "small mb-1", tr("O número citado é um parâmetro ou uma estatística?", "Is the number quoted a parameter or a statistic?"))
      )
    })

    shiny::observeEvent(input$conferir, {
      shiny::req(input$resposta, !estado$respondida)
      estado$respondida <- TRUE
      estado$acertou <- identical(input$resposta, atual()$resposta)
      estado$respondidas <- estado$respondidas + 1
      estado$acertos <- estado$acertos + estado$acertou
    })

    shiny::observeEvent(input$proxima, {
      if (estado$posicao == total) {
        estado$ordem <- sample(total)
        estado$posicao <- 1
      } else {
        estado$posicao <- estado$posicao + 1
      }
      estado$respondida <- FALSE
      shiny::updateRadioButtons(session, "resposta", selected = character(0))
    })

    output$feedback <- shiny::renderUI({
      if (!estado$respondida) return(NULL)
      certo <- tolower(names(opcoes_quiz)[opcoes_quiz == atual()$resposta])
      shiny::div(
        class = paste("alert mt-3 mb-2", if (estado$acertou) "alert-success" else "alert-warning"),
        shiny::strong(
          if (estado$acertou) tr("Isso mesmo. ", "That's right. ") else sprintf(tr("Não é bem isso: é %s. ", "Not quite: it is a %s. "), certo)
        ),
        atual()$explicacao
      )
    })

    output$placar <- shiny::renderText({
      if (estado$respondidas == 0) return(tr("Escolha uma opção e clique em Conferir.", "Choose an option and click Check."))
      sprintf(tr("Acertos: %d de %d", "Correct: %d of %d"), estado$acertos, estado$respondidas)
    })

    shiny::observeEvent(input$ir_amostra, ir_para("amostra"))
  })
}
