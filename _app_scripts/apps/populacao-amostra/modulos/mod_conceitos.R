# =============================================================================
# Script:  mod_conceitos.R (módulo: os quatro conceitos, notação e quiz)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
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
        "O conjunto de todos os elementos sob investigação (pessoas, domicílios, empresas).",
        "Pode ser concreta, como os eleitores de 2026, ou hipotética, como todas as peças que uma máquina ainda vai produzir."
      ),
      paste(
        "The total set of subjects of interest (people, households, firms). It can be actual, like the voters in a",
        "given election, or conceptual, like all the parts a machine will or could make."
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
      "A pesquisa TIC Domicílios divulgou o percentual de domicílios com acesso à internet.",
      "A professora calculou a nota média da sua turma, e o interesse é só nessa turma.",
      "A mesma nota média da turma foi tomada como a nota média de todos os alunos da escola.",
      "Uma enquete num portal de notícias teve 500 mil votos, e 70% foram a favor da proposta.",
      "A idade média de todos os servidores ativos, calculada com o cadastro completo da prefeitura.",
      "O desvio padrão da renda de 300 adultos sorteados numa cidade foi de R$ 1.900."
    ),
    c(
      "Brazil's 2022 Census counted 203 million residents.",
      "Brazil's continuous household survey (PNAD Contínua) released the unemployment rate for the last quarter.",
      "In a poll of 2,000 voters, 41% said they would vote for candidate A.",
      "Once every ballot was counted, candidate A got 43% of the valid votes cast.",
      "The ICT Households survey reported the percentage of Brazilian households with internet access.",
      "A teacher computed the mean score of her class, and she only cares about that class.",
      "The same class mean was taken as the mean score of all students in the school.",
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
        "e quem escolhe responder tende a ter opinião forte e pensar diferente de quem não responde."
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
        "to respond, and those who do tend to have strong opinions and differ from those who do not."
      ),
      "The complete records cover every employee, the population of interest.",
      "It is the s of a sample. σ would be the standard deviation of income among all adults in the city."
    )
  )
)
banco_conceitos$tipo <- "pe"
banco_conceitos$pergunta <- tr("O número citado é um parâmetro ou uma estatística?", "Is the number quoted a parameter or a statistic?")

# Perguntas sobre o grupo: população ou amostra? (Morettin e Bussab, exemplos
# 10.1 a 10.3 e seção 10.4; Agresti e Finlay, seções 1.2 e 2.2)
banco_grupos <- data.frame(
  afirmacao = tr(
    c(
      "Para estudar os salários dos 500 funcionários de uma empresa, anotaram-se os salários de 36 deles.",
      "Duzentos moradores de uma cidade, sorteados, disseram se apoiam um projeto da prefeitura.",
      "Uma fábrica deixou 100 lâmpadas de um novo tipo acesas até queimarem.",
      "Para saber se os operários de uma fábrica apoiam mudar o horário, entrevistaram-se os 30 primeiros a chegar.",
      "Um pesquisador sorteou 100 dos 30.000 alunos de uma universidade e perguntou quantas horas eles estudam.",
      "Uma prefeitura analisou o cadastro de todos os seus 4.000 servidores."
    ),
    c(
      "To study the salaries of a company's 500 employees, the salaries of 36 of them were recorded.",
      "Two hundred randomly drawn residents of a city said whether they support a city project.",
      "A factory left 100 light bulbs of a new type on until they burned out.",
      "To find out whether a factory's workers support a schedule change, the first 30 to arrive were interviewed.",
      "A researcher drew 100 of a university's 30,000 students and asked how many hours they study.",
      "A city analyzed the records of all its 4,000 employees."
    )
  ),
  pergunta = tr(
    c(
      "Os 36 salários anotados são a população ou uma amostra?",
      "Todos os moradores da cidade, inclusive os contrários ao projeto, são a população ou uma amostra?",
      "Todas as lâmpadas desse tipo que a fábrica já fez ou ainda vai fazer são a população ou uma amostra?",
      "Os 30 entrevistados são a população ou uma amostra?",
      "Os 30.000 alunos da universidade são a população ou uma amostra?",
      "Os 4.000 servidores analisados são a população ou uma amostra?"
    ),
    c(
      "Are the 36 recorded salaries the population or a sample?",
      "Are all the city's residents, including those who oppose the project, the population or a sample?",
      "Are all the bulbs of this type the factory has made or will make the population or a sample?",
      "Are the 30 people interviewed the population or a sample?",
      "Are the university's 30,000 students the population or a sample?",
      "Are the 4,000 employees analyzed the population or a sample?"
    )
  ),
  resposta = c("amostra", "populacao", "populacao", "amostra", "populacao", "populacao"),
  explicacao = tr(
    c(
      "São parte dos 500 salários que se quer conhecer. Os 500 formam a população.",
      "A população é todo mundo sob investigação, e não só quem apoia. Os 200 sorteados são a amostra.",
      "É uma população hipotética: inclui lâmpadas que ainda nem existem. Observá-la inteira, além disso, queimaria todas.",
      "São uma amostra, e uma amostra que não foi sorteada. Quem chega cedo pode pensar diferente de quem chega tarde.",
      "São o grupo sobre o qual se quer concluir algo. Os 100 sorteados são a amostra.",
      "Quando se observa todo mundo, a amostra é a população inteira: é um censo, e não há o que estimar."
    ),
    c(
      "They are part of the 500 salaries we want to know about. The 500 form the population.",
      "The population is everyone under study, not only those in favor. The 200 drawn are the sample.",
      "It is a conceptual population: it includes bulbs that do not exist yet. Observing all of it would also burn them all out.",
      "They are a sample, and one that was not drawn at random. Early arrivals may think differently from late ones.",
      "They are the group we want to draw conclusions about. The 100 drawn are the sample.",
      "When everyone is observed, the sample is the whole population: it is a census, and there is nothing to estimate."
    )
  )
)
banco_grupos$tipo <- "pa"

# Perguntas de notação: o símbolo já diz de que lado está o número
banco_notacao <- data.frame(
  afirmacao = tr(
    c("x̄ = 41 anos", "μ = 43,3 anos", "p̂ = 0,62", "σ² = 3,6 milhões (R$)²", "md = R$ 1.970", "σ = 12 anos"),
    c("ȳ = 41 years", "μ = 43.3 years", "π̂ = 0.62", "σ² = 3.6 million (R$)²", "md = R$ 1,970", "σ = 12 years")
  ),
  resposta = c("estatistica", "parametro", "estatistica", "parametro", "estatistica", "parametro"),
  explicacao = tr(
    c(
      "x̄ é o valor da média amostral X̄ numa amostra concreta.",
      "Letra grega: é a média da população, um número fixo e em geral desconhecido.",
      "O chapéu indica a proporção calculada na amostra. Na população, a proporção é p.",
      "Letra grega: é a variância da população, calculada com divisor N.",
      "Letra minúscula: é a mediana da amostra. A da população é Md.",
      "Letra grega: é o desvio padrão de toda a população em torno de μ."
    ),
    c(
      "ȳ is the sample mean.",
      "A Greek letter: it is the population mean, a fixed and usually unknown number.",
      "The hat marks the proportion computed from the sample. In the population, the proportion is π.",
      "A Greek letter: it is the population variance, computed with divisor N.",
      "Lowercase: it is the sample median. The population median is Md.",
      "A Greek letter: it is the standard deviation of the whole population around μ."
    )
  )
)
banco_notacao$tipo <- "pe"
banco_notacao$pergunta <- tr("O símbolo indica um parâmetro ou uma estatística?", "Does the symbol stand for a parameter or a statistic?")

banco_conceitos <- rbind(banco_conceitos, banco_grupos, banco_notacao)

opcoes_quiz <- stats::setNames(
  c("parametro", "estatistica"),
  tr(c("Parâmetro", "Estatística"), c("Parameter", "Statistic"))
)

opcoes_por_tipo <- list(
  pe = opcoes_quiz,
  pa = stats::setNames(
    c("populacao", "amostra"),
    tr(c("População", "Amostra"), c("Population", "Sample"))
  )
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
      "Regra geral: o parâmetro leva letra grega (μ, σ) e a estatística, letra latina (x̄, s) ou chapéu (p̂). A",
      "proporção é a exceção em Morettin e Bussab, que a tabela segue (seções 10.6 e 11.2): a da população é p,",
      "também uma letra latina, e quem marca a estatística é o chapéu. Nessa notação, a maiúscula (X̄, S²) indica a",
      "estatística como função da amostra, que muda de uma amostra para outra; a minúscula (x̄, s²), o valor observado",
      "numa amostra concreta. Agresti e Finlay escrevem ȳ para a média da amostra e π e π̂ para a proporção. A",
      "variância da população divide por N; a da amostra, por n − 1 (a aba \"Estimador e estimativa\" mostra por",
      "quê). No capítulo 3, de análise descritiva, Morettin e Bussab dividem por n, porque ali não importa se os",
      "dados são população ou amostra. Para a mediana não há letra grega consagrada: o app usa Md para a população",
      "e md para a amostra, como Morettin e Bussab (seção 10.6 e exemplo 11.7)."
    ),
    paste(
      "General rule: parameters get Greek letters (μ, σ, π) and statistics get Roman letters (ȳ, s) or a hat (π̂).",
      "The table follows Agresti and Finlay. Morettin and Bussab write x̄ for the sample mean and p and p̂ for the",
      "proportion, so in their notation it is the hat, not the alphabet, that marks the statistic. The population",
      "variance divides by N; the sample variance, s², divides by n − 1 (the \"Estimator and estimate\" tab shows",
      "why). There is no standard Greek letter for the median, and Agresti and Finlay use no symbol for it; the app",
      "uses Md for the population and md for the sample, as Morettin and Bussab do (section 10.6 and example 11.7)."
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
        bslib::card_header(tr("Quiz: de que lado está cada um?", "Quiz: which side is it on?")),
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
        shiny::p(class = "text-muted small mb-1", sprintf(tr("Pergunta %d de %d", "Question %d of %d"), estado$posicao, total)),
        shiny::h5(atual()$afirmacao),
        shiny::p(class = "small mb-1", atual()$pergunta)
      )
    })

    # Cada tipo de pergunta tem suas opções; trocar de pergunta limpa a resposta
    shiny::observeEvent(atual(), {
      shiny::updateRadioButtons(session, "resposta", choices = opcoes_por_tipo[[atual()$tipo]], selected = character(0), inline = TRUE)
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
    })

    output$feedback <- shiny::renderUI({
      if (!estado$respondida) return(NULL)
      opcoes <- opcoes_por_tipo[[atual()$tipo]]
      certo <- tolower(names(opcoes)[opcoes == atual()$resposta])
      shiny::div(
        class = paste("alert mt-3 mb-2", if (estado$acertou) "alert-success" else "alert-warning"),
        shiny::strong(
          if (estado$acertou) tr("Isso mesmo. ", "That's right. ") else sprintf(tr("Não é bem isso: a resposta é %s. ", "Not quite: the answer is %s. "), certo)
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
