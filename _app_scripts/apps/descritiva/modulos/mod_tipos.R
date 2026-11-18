# =============================================================================
# Script:  mod_tipos.R (módulo: visão geral dos tipos de variáveis e quiz)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-18
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/apps/descritiva/app.R)
# Saída:
#   - funções mod_tipos_ui() e mod_tipos_server()
# =============================================================================

opcoes_tipo <- stats::setNames(names(tipos_variavel), purrr::map_chr(tipos_variavel, "nome"))

banco_quiz <- data.frame(
  variavel = c(
    "Estado civil", "CEP de residência", "Número da camisa de um jogador", "Tipo sanguíneo",
    "Escolaridade (fundamental, médio, superior)", "Grau de concordância numa escala de 1 a 5",
    "Posição de chegada numa corrida (1º, 2º, 3º…)", "Faixa de renda (até 1 salário mínimo, de 1 a 3, mais de 3)",
    "Classificação de risco no pronto-socorro", "Número de filhos", "Acessos a um site em um dia",
    "Disciplinas em que o aluno foi reprovado", "Altura", "Tempo de deslocamento até o trabalho",
    "Renda mensal em reais", "Temperatura máxima do dia (°C)"
  ),
  tipo = c(
    "nominal", "nominal", "nominal", "nominal",
    "ordinal", "ordinal", "ordinal", "ordinal", "ordinal",
    "discreta", "discreta", "discreta",
    "continua", "continua", "continua", "continua"
  ),
  explicacao = c(
    "As categorias (solteiro, casado, divorciado…) não têm ordem.",
    "É escrito com números, mas é um rótulo. Somar ou tirar a média de CEPs não tem sentido.",
    "O 10 não é \"maior\" que o 7 em nada que importe: o número só identifica o jogador.",
    "As categorias não têm ordem, e o fator Rh não muda isso.",
    "Há uma ordem natural, mas a \"distância\" entre médio e superior não é um número.",
    "Os códigos 1 a 5 indicam ordem, mas nada garante que a distância entre 4 e 5 seja igual à distância entre 2 e 3.",
    "Indica ordem, mas não distância: o 1º pode ter chegado um segundo ou dez minutos antes do 2º.",
    "A renda em reais é contínua, mas agrupada em faixas ordenadas vira ordinal.",
    "Os níveis vão de não urgente a emergência, numa ordem clara e sem distância definida.",
    "É uma contagem: 0, 1, 2… e nunca 1,5.",
    "É uma contagem de eventos, com valores inteiros.",
    "Conta quantas disciplinas, então só assume valores inteiros.",
    "É uma medição: entre 1,70 m e 1,71 m cabem infinitos valores.",
    "Tempo é medido numa escala contínua, mesmo que seja registrado em minutos inteiros.",
    "Varia de centavo em centavo, mas tem tantos valores possíveis que é tratada como contínua.",
    "É uma medição contínua. Repare que o zero em °C não é ausência de calor: por isso não faz sentido dizer que 20 °C é \"o dobro\" de 10 °C."
  )
)

matriz_estatisticas <- function() {
  data.frame(
    "Estatística" = c(
      "Frequências e proporções", "Frequência acumulada", "Moda", "Mediana, quartis e percentis",
      "Mínimo e máximo", "Amplitude e intervalo interquartil", "Média", "Variância e desvio padrão",
      "Coeficiente de variação", "Assimetria", "Gráfico típico"
    ),
    "Nominal" = c("✓", "✗", "✓", "✗", "✗", "✗", "✗", "✗", "✗", "✗", "Barras (qualquer ordem)"),
    "Ordinal" = c("✓", "✓", "✓", "✓", "✓ (categorias)", "✗", "Com cautela", "✗", "✗", "✗", "Barras na ordem natural"),
    "Discreta" = c("✓", "✓", "✓", "✓", "✓", "✓", "✓", "✓", "✓", "✓", "Barras por valor"),
    "Contínua" = c("✓ (por classes)", "✓", "Classe modal", "✓", "✓", "✓", "✓", "✓", "✓*", "✓", "Histograma e boxplot"),
    check.names = FALSE
  )
}

cartao_tipo <- function(tipo) {
  t <- tipos_variavel[[tipo]]
  bslib::card(
    bslib::card_header(class = "bg-primary text-white", t$nome),
    bslib::card_body(
      shiny::p(t$definicao),
      shiny::p(shiny::strong("Exemplos: "), t$exemplos),
      shiny::p(shiny::strong("Operações válidas: "), t$operacoes),
      shiny::p(shiny::strong("Estatísticas: "), t$estatisticas),
      shiny::p(class = "mb-0", shiny::strong("Gráfico: "), t$grafico)
    )
  )
}

mod_tipos_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::card(
      bslib::card_body(
        shiny::h4("Que tipo de variável é esta?"),
        shiny::p(
          "Antes de calcular qualquer estatística, é preciso saber o que os valores representam.",
          "Categorias ou números? Com ordem ou sem ordem? Contagem ou medição? A resposta define quais",
          "resumos fazem sentido. Uma média de CEPs pode ser calculada, mas não quer dizer nada."
        ),
        shiny::p(
          class = "mb-0",
          "Teste seu olho no quiz, use o assistente para classificar uma variável sua e, depois, visite a aba",
          "de cada tipo para ver as estatísticas em ação."
        )
      )
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header("Quiz: classifique a variável"),
        bslib::card_body(
          shiny::uiOutput(ns("pergunta")),
          shiny::radioButtons(ns("resposta"), NULL, choices = opcoes_tipo, selected = character(0)),
          shiny::div(
            class = "d-flex gap-2 flex-wrap",
            shiny::actionButton(ns("conferir"), "Conferir", class = "btn-primary", icon = shiny::icon("check")),
            shiny::actionButton(ns("proxima"), "Próxima variável", icon = shiny::icon("arrow-right"))
          ),
          shiny::uiOutput(ns("feedback")),
          shiny::p(class = "small text-muted mb-0", shiny::textOutput(ns("placar"), inline = TRUE))
        )
      ),
      bslib::card(
        bslib::card_header("Assistente: classifique a sua variável"),
        bslib::card_body(
          shiny::radioButtons(
            ns("q1"), "1. Os valores são números que medem ou contam alguma coisa?",
            choices = c("Não, são categorias ou rótulos" = "cat", "Sim, contas com eles fazem sentido" = "num"),
            selected = character(0)
          ),
          shiny::conditionalPanel(
            "input.q1 == 'cat'", ns = ns,
            shiny::radioButtons(
              ns("q2_cat"), "2. As categorias têm uma ordem natural?",
              choices = c("Não, a ordem é arbitrária" = "nominal", "Sim, uma vem antes da outra" = "ordinal"),
              selected = character(0)
            )
          ),
          shiny::conditionalPanel(
            "input.q1 == 'num'", ns = ns,
            shiny::radioButtons(
              ns("q2_num"), "2. Os valores vêm de uma contagem ou de uma medição?",
              choices = c(
                "Contagem: só inteiros (0, 1, 2…)" = "discreta",
                "Medição: qualquer valor num intervalo" = "continua"
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
      bslib::card_header("Qual estatística usar com cada tipo"),
      bslib::card_body(
        tabela_html(matriz_estatisticas()),
        shiny::p(
          class = "small text-muted mb-0",
          "* Só quando o zero da escala significa ausência (altura, renda, tempo). Para temperatura em °C, o",
          "coeficiente de variação não tem interpretação. A divisão entre nominal e ordinal vem das escalas de",
          "medida de Stevens (1946); entre as quantitativas, a separação aqui é entre contagens e medições."
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
        shiny::p(class = "text-muted small mb-1", sprintf("Variável %d de %d", estado$posicao, nrow(banco_quiz))),
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
      certo <- tipos_variavel[[atual()$tipo]]$nome
      shiny::div(
        class = paste("alert mt-3 mb-2", if (estado$acertou) "alert-success" else "alert-warning"),
        shiny::strong(if (estado$acertou) "Isso mesmo. " else paste0("Não é bem isso: é ", tolower(certo), ". ")),
        atual()$explicacao
      )
    })

    output$placar <- shiny::renderText({
      if (estado$respondidas == 0) return("Escolha uma opção e clique em Conferir.")
      sprintf("Acertos: %d de %d", estado$acertos, estado$respondidas)
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
        shiny::br(), "Estatísticas: ", t$estatisticas, ".",
        shiny::div(
          class = "mt-2",
          shiny::actionButton(session$ns("ir"), paste("Ver a aba", t$nome), class = "btn-primary btn-sm")
        )
      )
    })

    shiny::observeEvent(input$ir, ir_para(tipo_escolhido()))
  })
}
