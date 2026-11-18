# =============================================================================
# Script:  mod_tipos.R (módulo: visão geral dos tipos de variáveis e quiz)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-18
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/apps/variaveis/app.R)
# Saída:
#   - funções mod_tipos_ui() e mod_tipos_server()
# =============================================================================

opcoes_tipo <- stats::setNames(names(tipos_variavel), purrr::map_chr(tipos_variavel, "nome"))

banco_quiz <- data.frame(
  variavel = tr(
    c(
      "Estado civil", "CEP de residência", "Número da camisa de um jogador", "Tipo sanguíneo", "Religião",
      "Escolaridade (fundamental, médio, superior)", "Grau de concordância numa escala de 1 a 5",
      "Posição de chegada numa corrida (1º, 2º, 3º…)", "Faixa de renda (até 1 salário mínimo, mais de 1 a 3, mais de 3)",
      "Classificação de risco no pronto-socorro", "Faixa etária (0 a 14, 15 a 29, 30 a 59, 60 anos ou mais)",
      "Número de filhos", "Número de acessos a um site em um dia", "Número de disciplinas em que o aluno foi reprovado",
      "Nota numa prova corrigida de meio em meio ponto (0; 0,5; 1… 10)",
      "Altura", "Tempo de deslocamento até o trabalho", "Renda mensal em reais", "Temperatura máxima do dia (°C)",
      "Idade em anos completos"
    ),
    c(
      "Marital status", "ZIP code of residence", "A player's jersey number", "Blood type", "Religious affiliation",
      "Education level (elementary, high school, college)", "Level of agreement on a 1-to-5 scale",
      "Finishing position in a race (1st, 2nd, 3rd…)", "Income bracket (up to 1 monthly minimum wage, over 1 to 3, over 3)",
      "Emergency room triage category", "Age group (0 to 14, 15 to 29, 30 to 59, 60 and over)",
      "Number of children", "Number of visits to a website in one day", "Number of courses a student has failed",
      "Score on a test graded in half points (0, 0.5, 1… 10)",
      "Height", "Commute time to work", "Monthly income in reais", "Daily high temperature (°C)",
      "Age in completed years"
    )
  ),
  tipo = c(
    "nominal", "nominal", "nominal", "nominal", "nominal",
    "ordinal", "ordinal", "ordinal", "ordinal", "ordinal", "ordinal",
    "discreta", "discreta", "discreta", "discreta",
    "continua", "continua", "continua", "continua", "continua"
  ),
  explicacao = tr(
    c(
      "As categorias (solteiro, casado, divorciado…) não têm ordem.",
      "É escrito com números, mas é um rótulo. Somar ou tirar a média de CEPs não tem sentido.",
      "O 10 não é \"maior\" que o 7 em nada que importe: o número só identifica o jogador.",
      "As categorias não têm ordem, e o fator Rh não muda isso.",
      "As categorias (católica, evangélica, sem religião…) não têm ordem natural.",
      "Há uma ordem natural, mas a \"distância\" entre médio e superior não é um número.",
      paste(
        "Os códigos 1 a 5 indicam ordem, mas nada garante que a distância entre 4 e 5 seja igual à distância entre 2 e 3.",
        "Tratar a escala como numérica, com escores, é uma decisão de análise que acrescenta essa suposição."
      ),
      "Indica ordem, mas não distância: o 1º pode ter chegado um segundo ou dez minutos antes do 2º.",
      "A renda em reais é contínua, mas agrupada em faixas ordenadas vira ordinal.",
      "Os níveis vão de não urgente a emergência, numa ordem clara e sem distância definida.",
      "A idade é contínua, mas agrupada em faixas vira ordinal. As faixas têm ordem; o detalhe dentro de cada uma se perde.",
      "É uma contagem: 0, 1, 2… e nunca 1,5.",
      "É uma contagem de eventos, com valores inteiros.",
      "Conta quantas disciplinas, então só assume valores inteiros.",
      paste(
        "Os valores possíveis são separados entre si (não existe nota entre 7 e 7,5), por isso é discreta,",
        "mesmo sem ser inteira. Discreta não quer dizer inteira."
      ),
      "É uma medição: entre 1,70 m e 1,71 m cabem infinitos valores.",
      "Tempo é medido numa escala contínua, mesmo que seja registrado em minutos inteiros.",
      "A rigor, varia de centavo em centavo, mas tem tantos valores possíveis que é tratada como contínua.",
      paste(
        "É uma medição contínua, em escala intervalar: o zero em °C não é ausência de calor.",
        "Por isso não faz sentido dizer que 20 °C é \"o dobro\" de 10 °C."
      ),
      "A idade mede o tempo vivido. Registrar em anos completos arredonda a medida, mas não a transforma em contagem."
    ),
    c(
      "The categories (single, married, divorced…) have no order.",
      "It is written with digits, but it is a label. Adding up or averaging ZIP codes makes no sense.",
      "Number 10 is not \"greater\" than 7 in any way that matters: the number only identifies the player.",
      "The categories have no order, and the Rh factor does not change that.",
      "The categories (Catholic, Protestant, no religion…) have no natural order.",
      "There is a natural order, but the \"distance\" between high school and college is not a number.",
      paste(
        "The codes 1 to 5 show order, but nothing guarantees that the gap between 4 and 5 equals the gap between 2 and 3.",
        "Treating the scale as numeric, with scores, is an analysis choice that adds that assumption."
      ),
      "It shows order, not distance: 1st place may have finished one second or ten minutes ahead of 2nd.",
      "Income in reais is continuous, but grouped into ordered brackets it becomes ordinal.",
      "The levels go from non-urgent to emergency, in a clear order with no defined distance.",
      "Age is continuous, but grouped into ranges it becomes ordinal. The ranges are ordered; the detail within each one is lost.",
      "It is a count: 0, 1, 2… and never 1.5.",
      "It counts events, so it takes whole values.",
      "It counts courses, so it only takes whole values.",
      paste(
        "The possible values are separate from each other (no score falls between 7 and 7.5), so it is discrete",
        "even though the values are not whole numbers. Discrete does not mean whole."
      ),
      "It is a measurement: between 1.70 m and 1.71 m there are infinitely many values.",
      "Time is measured on a continuous scale, even when it is recorded in whole minutes.",
      "Strictly, it moves cent by cent, but it has so many possible values that it is treated as continuous.",
      paste(
        "It is a continuous measurement on an interval scale: zero in °C is not the absence of heat.",
        "That is why it makes no sense to say 20 °C is \"twice\" 10 °C."
      ),
      "Age measures time lived. Recording it in completed years rounds the measurement, but does not turn it into a count."
    )
  )
)

matriz_estatisticas <- function() {
  cautela <- tr("Com cautela", "With caution")
  tabela <- data.frame(
    tr(
      c(
        "Frequências e proporções", "Frequência acumulada", "Moda", "Mediana, quartis e percentis",
        "Mínimo e máximo", "Amplitude e distância interquartil", "Média", "Variância e desvio padrão",
        "Coeficiente de variação", "Assimetria", "Gráfico típico"
      ),
      c(
        "Frequencies and proportions", "Cumulative frequency", "Mode", "Median, quartiles and percentiles",
        "Minimum and maximum", "Range and interquartile range", "Mean", "Variance and standard deviation",
        "Coefficient of variation", "Skewness", "Typical chart"
      )
    ),
    c("✓", "✗", "✓", "✗", "✗", "✗", "✗", "✗", "✗", "✗", tr("Barras (qualquer ordem)", "Bars (any order)")),
    c("✓", "✓", "✓", "✓", tr("✓ (categorias)", "✓ (categories)"), "✗", cautela, cautela, "✗", "✗", tr("Barras na ordem natural", "Bars in natural order")),
    c("✓", "✓", "✓", "✓", "✓", "✓", "✓", "✓", "✓", "✓", tr("Barras por valor", "One bar per value")),
    c(tr("✓ (por classes)", "✓ (by class)"), "✓", tr("Classe modal", "Modal class"), "✓", "✓", "✓", "✓", "✓", "✓*", "✓", tr("Histograma e boxplot", "Histogram and boxplot"))
  )
  names(tabela) <- tr(
    c("Estatística", "Nominal", "Ordinal", "Discreta", "Contínua"),
    c("Statistic", "Nominal", "Ordinal", "Discrete", "Continuous")
  )
  tabela
}

cartao_tipo <- function(tipo) {
  t <- tipos_variavel[[tipo]]
  bslib::card(
    bslib::card_header(class = "bg-primary text-white", t$nome),
    bslib::card_body(
      shiny::p(t$definicao),
      shiny::p(shiny::strong(tr("Exemplos: ", "Examples: ")), t$exemplos),
      shiny::p(shiny::strong(tr("Operações válidas: ", "Valid operations: ")), t$operacoes),
      shiny::p(shiny::strong(tr("Estatísticas: ", "Statistics: ")), t$estatisticas),
      shiny::p(class = "mb-0", shiny::strong(tr("Gráfico: ", "Chart: ")), t$grafico)
    )
  )
}

texto_intro_tipos <- function() {
  tr(
    shiny::tagList(
      shiny::h4("Que tipo de variável é esta?"),
      shiny::p(
        "Antes de calcular qualquer estatística, é preciso saber o que os valores representam. A classificação",
        "depende de duas perguntas: se os valores são categorias ou números e, em seguida, se as categorias têm",
        "ordem ou se os números vêm de uma contagem ou de uma medição. Dessa forma, o tipo define quais resumos",
        "fazem sentido. A média de CEPs pode ser calculada, mas não quer dizer nada, uma vez que o CEP é um rótulo."
      ),
      shiny::p(
        class = "mb-0",
        "Treine no quiz, use o assistente para classificar uma variável sua e, depois, visite a aba",
        "de cada tipo para ver as estatísticas calculadas em dados simulados."
      )
    ),
    shiny::tagList(
      shiny::h4("What type of variable is this?"),
      shiny::p(
        "Before computing any statistic, you need to know what the values represent. Classifying a variable",
        "comes down to two questions: whether the values are categories or numbers and, then, whether the",
        "categories have an order or the numbers come from counting or measuring. The type, in turn, decides which",
        "summaries make sense. You can compute the mean of ZIP codes, but it means nothing, since a ZIP code is a label."
      ),
      shiny::p(
        class = "mb-0",
        "Test yourself with the quiz, use the assistant to classify a variable of your own, and then visit",
        "the tab for each type to see the statistics computed on simulated data."
      )
    )
  )
}

mod_tipos_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::card(bslib::card_body(texto_intro_tipos())),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header(tr("Quiz: classifique a variável", "Quiz: classify the variable")),
        bslib::card_body(
          shiny::uiOutput(ns("pergunta")),
          shiny::radioButtons(ns("resposta"), NULL, choices = opcoes_tipo, selected = character(0)),
          shiny::div(
            class = "d-flex gap-2 flex-wrap",
            shiny::actionButton(ns("conferir"), tr("Conferir", "Check"), class = "btn-primary", icon = shiny::icon("check")),
            shiny::actionButton(ns("proxima"), tr("Próxima variável", "Next variable"), icon = shiny::icon("arrow-right"))
          ),
          shiny::uiOutput(ns("feedback")),
          shiny::p(class = "small text-muted mb-0", shiny::textOutput(ns("placar"), inline = TRUE))
        )
      ),
      bslib::card(
        bslib::card_header(tr("Assistente: classifique a sua variável", "Assistant: classify your own variable")),
        bslib::card_body(
          shiny::radioButtons(
            ns("q1"), tr("1. Os valores são números que medem ou contam alguma coisa?", "1. Are the values numbers that measure or count something?"),
            choices = stats::setNames(
              c("cat", "num"),
              tr(c("Não, são categorias ou rótulos", "Sim, contas com eles fazem sentido"), c("No, they are categories or labels", "Yes, arithmetic with them makes sense"))
            ),
            selected = character(0)
          ),
          shiny::conditionalPanel(
            "input.q1 == 'cat'", ns = ns,
            shiny::radioButtons(
              ns("q2_cat"), tr("2. As categorias têm uma ordem natural?", "2. Do the categories have a natural order?"),
              choices = stats::setNames(
                c("nominal", "ordinal"),
                tr(c("Não, a ordem é arbitrária", "Sim, uma vem antes da outra"), c("No, the order is arbitrary", "Yes, one comes before another"))
              ),
              selected = character(0)
            )
          ),
          shiny::conditionalPanel(
            "input.q1 == 'num'", ns = ns,
            shiny::radioButtons(
              ns("q2_num"), tr("2. Os valores possíveis são separados ou formam um contínuo?", "2. Are the possible values separate or do they form a continuum?"),
              choices = stats::setNames(
                c("discreta", "continua"),
                tr(
                  c("Separados, como numa contagem (0, 1, 2…)", "Contínuos, como numa medição: qualquer valor num intervalo"),
                  c("Separate, as in a count (0, 1, 2…)", "Continuous, as in a measurement: any value within a range")
                )
              ),
              selected = character(0)
            )
          ),
          shiny::uiOutput(ns("resultado"))
        )
      )
    ),
    bslib::layout_column_wrap(
      width = "240px", fill = FALSE,
      cartao_tipo("nominal"), cartao_tipo("ordinal"), cartao_tipo("discreta"), cartao_tipo("continua")
    ),
    bslib::card(
      bslib::card_header(tr("Qual estatística usar com cada tipo", "Which statistic to use with each type")),
      bslib::card_body(
        tabela_html(matriz_estatisticas()),
        shiny::p(
          class = "small text-muted",
          tr(
            paste(
              "* Só na escala de razão, em que o zero significa ausência (altura, renda, tempo). A temperatura em °C",
              "está em escala intervalar: as distâncias têm sentido, mas o zero não, e o coeficiente de variação perde",
              "a interpretação. Na ordinal, média e desvio padrão dos códigos dependem da mesma suposição de",
              "distâncias iguais entre categorias, por isso aparecem com cautela."
            ),
            paste(
              "* Only on a ratio scale, where zero means none of the quantity (height, income, time). Temperature in °C",
              "is on an interval scale: distances are meaningful but zero is not, so the coefficient of variation loses",
              "its interpretation. For ordinal data, the mean and standard deviation of the codes rest on the same",
              "assumption of equal distances between categories, which is why they come with a caution."
            )
          )
        ),
        shiny::p(
          class = "small text-muted mb-0",
          tr(
            paste(
              "A classificação em quatro tipos segue Morettin e Bussab (Estatística Básica, cap. 2) e Agresti e Finlay",
              "(Statistical Methods for the Social Sciences, cap. 2); a divisão entre nominal e ordinal vem das escalas",
              "de medida de Stevens (1946). Variância e desvio padrão usam divisor n − 1, como em Agresti e Finlay.",
              "Morettin e Bussab dividem por n na análise descritiva, e a diferença some à medida que a amostra cresce."
            ),
            paste(
              "The four-way classification follows Agresti and Finlay (Statistical Methods for the Social Sciences,",
              "ch. 2) and Morettin and Bussab (Estatística Básica, ch. 2); the split between nominal and ordinal comes",
              "from Stevens' (1946) levels of measurement. Variance and standard deviation use the n − 1 divisor, as in",
              "Agresti and Finlay. Morettin and Bussab divide by n in descriptive analysis, and the difference fades as",
              "the sample grows."
            )
          )
        )
      )
    )
  )
}

# `ir_para` recebe o nome do tipo e troca a aba do app (definido no app.R)
mod_tipos_server <- function(id, ir_para = \(tipo) NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    estado <- shiny::reactiveValues(
      ordem = sample(nrow(banco_quiz)), posicao = 1, respondida = FALSE,
      acertou = FALSE, acertos = 0, respondidas = 0
    )

    atual <- shiny::reactive(banco_quiz[estado$ordem[estado$posicao], ])

    output$pergunta <- shiny::renderUI({
      shiny::tagList(
        shiny::p(
          class = "text-muted small mb-1",
          sprintf(tr("Variável %d de %d", "Variable %d of %d"), estado$posicao, nrow(banco_quiz))
        ),
        shiny::h5(atual()$variavel)
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
      if (estado$posicao == nrow(banco_quiz)) {
        estado$ordem <- sample(nrow(banco_quiz))
        estado$posicao <- 1
      } else {
        estado$posicao <- estado$posicao + 1
      }
      estado$respondida <- FALSE
      shiny::updateRadioButtons(session, "resposta", selected = character(0))
    })

    output$feedback <- shiny::renderUI({
      if (!estado$respondida) return(NULL)
      certo <- tolower(tipos_variavel[[atual()$tipo]]$nome)
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

    tipo_escolhido <- shiny::reactive({
      if (is.null(input$q1)) return(NULL)
      resposta <- switch(input$q1, cat = input$q2_cat, num = input$q2_num)
      if (length(resposta) == 0) NULL else resposta
    })

    output$resultado <- shiny::renderUI({
      tipo <- tipo_escolhido()
      if (is.null(tipo)) return(NULL)
      t <- tipos_variavel[[tipo]]
      shiny::div(
        class = "alert alert-info mt-2 mb-0",
        shiny::strong(paste0(t$nome, ". ")), t$definicao,
        shiny::br(), tr("Estatísticas: ", "Statistics: "), t$estatisticas, ".",
        shiny::div(
          class = "mt-2",
          shiny::actionButton(session$ns("ir"), paste(tr("Ver a aba", "Open the tab"), t$nome), class = "btn-primary btn-sm")
        )
      )
    })

    shiny::observeEvent(input$ir, ir_para(tipo_escolhido()))
  })
}
