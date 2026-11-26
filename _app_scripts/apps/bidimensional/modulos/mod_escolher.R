# =============================================================================
# Script:  mod_escolher.R (módulo: qual análise usar para cada par de variáveis, com quiz)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-19
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/apps/bidimensional/app.R)
# Saída:
#   - funções mod_escolher_ui() e mod_escolher_server()
# =============================================================================

banco_quiz_pares <- data.frame(
  par = tr(
    c(
      "Escolaridade e salário mensal", "Altura e peso", "Região de moradia e intenção de voto",
      "Horas de sono e nota na prova", "Tipo de escola e aprovação no vestibular (sim ou não)",
      "Turma e nota na prova", "Idade e tempo diário no celular", "Faixa etária e uso de redes sociais (sim ou não)",
      "Número de filhos e renda familiar", "Estado civil e número de filhos",
      "Satisfação com o atendimento (escala de 1 a 5) e agência bancária", "Temperatura do dia e consumo de energia"
    ),
    c(
      "Education level and monthly salary", "Height and weight", "Region of residence and voting intention",
      "Hours of sleep and test score", "School type and university admission (yes or no)",
      "Class and test score", "Age and daily time on the phone", "Age group and social media use (yes or no)",
      "Number of children and household income", "Marital status and number of children",
      "Satisfaction with service (1-to-5 scale) and bank branch", "Daily temperature and energy use"
    )
  ),
  tipo = c(
    "quali_quanti", "quanti_quanti", "quali_quali",
    "quanti_quanti", "quali_quali",
    "quali_quanti", "quanti_quanti", "quali_quali",
    "quanti_quanti", "quali_quanti",
    "quali_quali", "quanti_quanti"
  ),
  explicacao = tr(
    c(
      "A escolaridade forma grupos e o salário é medido em reais: box plots do salário por escolaridade e R².",
      "As duas são medições. Gráfico de dispersão e correlação, com o peso no eixo vertical se a pergunta for prever o peso.",
      "As duas são categorias. Tabela de dupla entrada com a região nas linhas e porcentagens dentro de cada região.",
      "As duas são numéricas. Gráfico de dispersão primeiro, para ver se a relação é linear.",
      "As duas são categorias, numa tabela 2 × 2. A porcentagem de aprovados em cada tipo de escola responde à pergunta.",
      "A turma define grupos e a nota é quantitativa. Com duas turmas, o gráfico q × q também ajuda.",
      "As duas são quantitativas. Gráfico de dispersão e correlação.",
      paste(
        "Faixa etária é ordinal, mas continua sendo categoria, e uso de redes sociais é nominal.",
        "Tabela de dupla entrada, com porcentagens dentro de cada faixa."
      ),
      "O número de filhos é discreto, mas é número, como a renda. Gráfico de dispersão, de preferência com a renda no eixo vertical.",
      "O estado civil forma grupos e o número de filhos é uma contagem. Medidas-resumo dos filhos em cada grupo.",
      paste(
        "Os dois são categorias, e a escala de 1 a 5 é ordinal. Tabela de dupla entrada; tratar a escala como número",
        "é uma decisão de análise, com o pressuposto de distâncias iguais entre as categorias."
      ),
      "As duas são quantitativas, mas a relação é em U: o consumo sobe no frio e no calor. A correlação fica perto de zero e esconde isso."
    ),
    c(
      "Education forms groups and salary is measured in money: box plots of salary by education and R².",
      "Both are measurements. Scatterplot and correlation, with weight on the vertical axis if the question is to predict weight.",
      "Both are categories. Contingency table with region in the rows and percentages within each region.",
      "Both are numeric. Scatterplot first, to see whether the relationship is linear.",
      "Both are categories, in a 2 × 2 table. The share admitted in each school type answers the question.",
      "Class defines groups and the score is quantitative. With two classes, the Q-Q plot also helps.",
      "Both are quantitative. Scatterplot and correlation.",
      paste(
        "Age group is ordinal, but it is still a category, and social media use is nominal.",
        "Contingency table, with percentages within each age group."
      ),
      "Number of children is discrete, but it is a number, as is income. Scatterplot, ideally with income on the vertical axis.",
      "Marital status forms groups and number of children is a count. Summary measures of children in each group.",
      paste(
        "Both are categories, and the 1-to-5 scale is ordinal. Contingency table; treating the scale as a number is an",
        "analysis decision that assumes equal distances between categories."
      ),
      "Both are quantitative, but the relationship is U-shaped: energy use rises in the cold and in the heat. The correlation stays near zero and hides this."
    )
  )
)

quadro_pares <- function() {
  tabela <- data.frame(
    purrr::map_chr(pares_tipo, "nome"),
    purrr::map_chr(pares_tipo, "tabela"),
    purrr::map_chr(pares_tipo, "grafico"),
    purrr::map_chr(pares_tipo, "medida")
  )
  names(tabela) <- tr(c("Par de variáveis", "Tabela", "Gráfico", "Medida de associação"), c("Pair of variables", "Table", "Chart", "Measure of association"))
  tabela_html(tabela)
}

texto_intro_escolher <- function() {
  tr(
    shiny::tagList(
      shiny::h4("Duas variáveis andam juntas?"),
      shiny::p(
        "A análise bidimensional descreve a relação entre duas variáveis observadas nas mesmas unidades: escolaridade",
        "e salário das mesmas pessoas, altura e peso dos mesmos adultos. A ferramenta depende do par de tipos, e por",
        "isso a classificação das variáveis vem antes de tudo. Com duas qualitativas, cruzam-se as categorias numa",
        "tabela; com duas quantitativas, os pares viram pontos num gráfico de dispersão; com uma de cada, a quantitativa",
        "é comparada entre os grupos da qualitativa."
      ),
      shiny::p(
        "Quando uma variável ajuda a explicar a outra, vale nomear os papéis. Agresti e Finlay chamam de resposta a",
        "variável cujo comportamento se quer entender e de explicativa a que define os grupos ou o eixo horizontal.",
        "A escolha dos papéis não prova causa, uma vez que tudo aqui é descritivo, mas orienta a leitura das tabelas",
        "e gráficos."
      ),
      shiny::p(
        class = "mb-0",
        "Treine no quiz, use o assistente para um par de variáveis seu e depois visite cada aba, que traz dados",
        "simulados, gráficos e as medidas de Morettin e Bussab (Estatística Básica, Cap. 4) e Agresti e Finlay",
        "(Statistical Methods for the Social Sciences, Cap. 3)."
      )
    ),
    shiny::tagList(
      shiny::h4("Do two variables go together?"),
      shiny::p(
        "Bivariate analysis describes the relationship between two variables observed on the same units: education",
        "and salary of the same people, height and weight of the same adults. The right tool depends on the pair of",
        "types, which is why classifying the variables comes first. With two categorical variables, the categories are",
        "crossed in a table; with two quantitative ones, the pairs become points on a scatterplot; with one of each, the",
        "quantitative variable is compared across the groups of the categorical one."
      ),
      shiny::p(
        "When one variable helps explain the other, it is worth naming the roles. Agresti and Finlay call the variable",
        "whose behavior you want to understand the response, and the one that defines the groups or the horizontal axis",
        "the explanatory variable. Choosing roles does not prove causation, since everything here is descriptive, but it",
        "guides how tables and charts are read."
      ),
      shiny::p(
        class = "mb-0",
        "Test yourself with the quiz, use the assistant for a pair of variables of your own and then visit each tab,",
        "with simulated data, charts and the measures from Morettin and Bussab (Estatística Básica, ch. 4) and",
        "Agresti and Finlay (Statistical Methods for the Social Sciences, ch. 3)."
      )
    )
  )
}

mod_escolher_ui <- function(id) {
  ns <- shiny::NS(id)
  opcoes_tipo_var <- stats::setNames(c("quali", "quanti"), tr(c("Qualitativa (categorias)", "Quantitativa (números)"), c("Categorical (categories)", "Quantitative (numbers)")))

  shiny::tagList(
    bslib::card(bslib::card_body(texto_intro_escolher())),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header(tr("Quiz: que análise usar?", "Quiz: which analysis?")),
        bslib::card_body(
          shiny::uiOutput(ns("pergunta")),
          shiny::radioButtons(ns("resposta"), NULL, choices = opcoes_par, selected = character(0)),
          shiny::div(
            class = "d-flex gap-2 flex-wrap",
            shiny::actionButton(ns("conferir"), tr("Conferir", "Check"), class = "btn-primary", icon = shiny::icon("check")),
            shiny::actionButton(ns("proxima"), tr("Próximo par", "Next pair"), icon = shiny::icon("arrow-right"))
          ),
          shiny::uiOutput(ns("feedback")),
          shiny::p(class = "text-muted small mb-0 mt-2", shiny::textOutput(ns("placar"), inline = TRUE))
        )
      ),
      bslib::card(
        bslib::card_header(tr("Assistente: classifique o seu par", "Assistant: classify your pair")),
        bslib::card_body(
          shiny::radioButtons(ns("var1"), tr("1. A primeira variável é…", "1. The first variable is…"), choices = opcoes_tipo_var, selected = character(0)),
          shiny::radioButtons(ns("var2"), tr("2. A segunda variável é…", "2. The second variable is…"), choices = opcoes_tipo_var, selected = character(0)),
          shiny::uiOutput(ns("resultado"))
        )
      )
    ),
    bslib::card(
      bslib::card_header(tr("Que tabela, gráfico e medida usar", "Which table, chart and measure to use")),
      bslib::card_body(
        quadro_pares(),
        shiny::p(class = "small text-muted mb-0", tr(
          paste(
            "Variáveis ordinais entram como qualitativas. Variâncias, desvios padrão e covariâncias dividem por n, como",
            "Morettin e Bussab no Cap. 4; Agresti e Finlay usam n − 1. A correlação dá o mesmo valor nos dois casos."
          ),
          paste(
            "Ordinal variables count as categorical. Variances, standard deviations and covariances divide by n, as",
            "Morettin and Bussab do in ch. 4; Agresti and Finlay use n − 1. The correlation is the same either way."
          )
        ))
      )
    )
  )
}

# `ir_para` recebe o nome do par e troca a aba do app (definido no app.R)
mod_escolher_server <- function(id, ir_para = \(par) NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    estado <- shiny::reactiveValues(
      ordem = sample(nrow(banco_quiz_pares)), posicao = 1, respondida = FALSE,
      acertou = FALSE, acertos = 0, respondidas = 0
    )

    atual <- shiny::reactive(banco_quiz_pares[estado$ordem[estado$posicao], ])

    output$pergunta <- shiny::renderUI({
      shiny::tagList(
        shiny::p(
          class = "text-muted small mb-1",
          sprintf(tr("Par %d de %d", "Pair %d of %d"), estado$posicao, nrow(banco_quiz_pares))
        ),
        shiny::h5(atual()$par)
      )
    })

    shiny::observeEvent(input$conferir, {
      shiny::req(input$resposta, !estado$respondida)
      estado$respondida <- TRUE
      estado$acertou <- identical(input$resposta, atual()$tipo)
      estado$respondidas <- estado$respondidas + 1
      estado$acertos <- estado$acertos + estado$acertou
    })

    shiny::observeEvent(input$proxima, {
      if (estado$posicao == nrow(banco_quiz_pares)) {
        estado$ordem <- sample(nrow(banco_quiz_pares))
        estado$posicao <- 1
      } else {
        estado$posicao <- estado$posicao + 1
      }
      estado$respondida <- FALSE
      shiny::updateRadioButtons(session, "resposta", selected = character(0))
    })

    output$feedback <- shiny::renderUI({
      if (!estado$respondida) return(NULL)
      certo <- tolower(pares_tipo[[atual()$tipo]]$nome)
      shiny::div(
        class = paste("alert mt-3 mb-2", if (estado$acertou) "alert-success" else "alert-warning"),
        shiny::strong(
          if (estado$acertou) tr("Isso mesmo. ", "That's right. ") else sprintf(tr("Não é bem isso: é %s. ", "Not quite: it is %s. "), certo)
        ),
        atual()$explicacao
      )
    })

    output$placar <- shiny::renderText({
      if (estado$respondidas == 0) return(tr("Escolha uma opção e clique em Conferir.", "Choose an option and click Check."))
      sprintf(tr("Acertos: %d de %d", "Correct: %d of %d"), estado$acertos, estado$respondidas)
    })

    par_escolhido <- shiny::reactive({
      if (is.null(input$var1) || is.null(input$var2)) return(NULL)
      tipos <- sort(c(input$var1, input$var2))
      if (identical(tipos[1], tipos[2])) paste(tipos, collapse = "_") else "quali_quanti"
    })

    output$resultado <- shiny::renderUI({
      par <- par_escolhido()
      if (is.null(par)) return(NULL)
      p <- pares_tipo[[par]]
      shiny::div(
        class = "alert alert-info mt-2 mb-0",
        shiny::strong(paste0(p$nome, ". ")), p$pergunta,
        shiny::br(), tr("Tabela: ", "Table: "), p$tabela, ".",
        shiny::br(), tr("Gráfico: ", "Chart: "), p$grafico, ".",
        shiny::br(), tr("Medida: ", "Measure: "), p$medida, ".",
        shiny::div(
          class = "mt-2",
          shiny::actionButton(session$ns("ir"), paste(tr("Ver a aba", "Open the tab"), p$aba), class = "btn-primary btn-sm")
        )
      )
    })

    shiny::observeEvent(input$ir, ir_para(par_escolhido()))
  })
}
