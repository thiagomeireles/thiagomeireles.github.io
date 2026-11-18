# =============================================================================
# Script:  mod_normal.R (módulo: distribuições Normal padrão Z e t de Student)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-18
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/apps/probabilidade/app.R)
# Saída:
#   - funções mod_normal_ui() e mod_normal_server()
# =============================================================================

dens_dist <- function(x, dist, gl) {
  if (dist == "z") stats::dnorm(x) else stats::dt(x, df = gl)
}

acum_dist <- function(q, dist, gl) {
  if (dist == "z") stats::pnorm(q) else stats::pt(q, df = gl)
}

quant_dist <- function(p, dist, gl) {
  if (dist == "z") stats::qnorm(p) else stats::qt(p, df = gl)
}

# Intervalos (lista de pares inf/sup) que formam a área pedida
intervalos_area <- function(tipo, a, b) {
  switch(
    tipo,
    inferior = list(c(-Inf, a)),
    superior = list(c(a, Inf)),
    entre    = list(sort(c(a, b))),
    bicaudal = list(c(-Inf, -abs(a)), c(abs(a), Inf))
  )
}

prob_area <- function(intervalos, dist, gl) {
  intervalos |>
    purrr::map_dbl(\(l) acum_dist(l[2], dist, gl) - acum_dist(l[1], dist, gl)) |>
    sum()
}

dentro_area <- function(x, intervalos) {
  intervalos |>
    purrr::map(\(l) x >= l[1] & x <= l[2]) |>
    purrr::reduce(`|`)
}

rotulo_prob <- function(tipo, a, b) {
  switch(
    tipo,
    inferior = sprintf("P(X ≤ %s)", fmt_num(a, 2)),
    superior = sprintf("P(X ≥ %s)", fmt_num(a, 2)),
    entre    = sprintf("P(%s ≤ X ≤ %s)", fmt_num(min(a, b), 2), fmt_num(max(a, b), 2)),
    bicaudal = sprintf("P(|X| ≥ %s)", fmt_num(abs(a), 2))
  )
}

texto_normal <- function() {
  tr(
    shiny::withMathJax(
      shiny::p(
        "A normal padrão (Z) tem média 0 e desvio padrão 1. Qualquer variável normal",
        "\\(X\\) com média \\(\\mu\\) e desvio padrão \\(\\sigma\\) pode ser levada a ela pela padronização",
        "\\(Z = \\dfrac{X - \\mu}{\\sigma}\\). Por isso uma única tabela (ou uma única curva) serve para",
        "calcular probabilidades de qualquer normal."
      ),
      shiny::p(
        "A t de Student aparece quando o desvio padrão da população não é conhecido e precisa ser",
        "estimado pela amostra: \\(t = \\dfrac{\\bar{X} - \\mu}{s / \\sqrt{n}}\\), com \\(n - 1\\) graus de liberdade.",
        "As caudas da t são mais pesadas que as da Z, o que traduz a incerteza extra de estimar \\(\\sigma\\).",
        "Conforme os graus de liberdade crescem, a t se aproxima da Z."
      ),
      shiny::p(
        shiny::strong("Experimente:"),
        "escolha a t com 1 grau de liberdade e compare o valor crítico de 95% com o da Z (1,96).",
        "Depois aumente os graus de liberdade e veja a diferença diminuir. Com 30 gl ela já é pequena."
      )
    ),
    shiny::withMathJax(
      shiny::p(
        "The standard normal (Z) has mean 0 and standard deviation 1. Any normal variable",
        "\\(X\\) with mean \\(\\mu\\) and standard deviation \\(\\sigma\\) can be brought to it by standardizing,",
        "\\(Z = \\dfrac{X - \\mu}{\\sigma}\\). That is why a single table (or a single curve) is enough to",
        "compute probabilities for any normal."
      ),
      shiny::p(
        "Student's t appears when the population standard deviation is unknown and has to be estimated",
        "from the sample: \\(t = \\dfrac{\\bar{X} - \\mu}{s / \\sqrt{n}}\\), with \\(n - 1\\) degrees of freedom.",
        "The tails of t are heavier than those of Z, reflecting the extra uncertainty of estimating \\(\\sigma\\).",
        "As the degrees of freedom grow, t approaches Z."
      ),
      shiny::p(
        shiny::strong("Try it:"),
        "choose t with 1 degree of freedom and compare its 95% critical value with Z's (1.96).",
        "Then increase the degrees of freedom and watch the gap shrink. With 30 df it is already small."
      )
    )
  )
}

mod_normal_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::radioButtons(
        ns("dist"), tr("Distribuição", "Distribution"),
        choices = stats::setNames(c("z", "t"), tr(c("Normal padrão (Z)", "t de Student"), c("Standard normal (Z)", "Student's t")))
      ),
      shiny::conditionalPanel(
        condition = "input.dist == 't'", ns = ns,
        shiny::sliderInput(ns("gl"), tr("Graus de liberdade (gl)", "Degrees of freedom (df)"), min = 1, max = 60, value = 5, step = 1),
        shiny::checkboxInput(ns("comparar"), tr("Sobrepor a curva Z", "Overlay the Z curve"), value = TRUE)
      ),
      shiny::selectInput(
        ns("tipo"), tr("Probabilidade a calcular", "Probability to compute"),
        choices = stats::setNames(
          c("inferior", "superior", "entre", "bicaudal"),
          c("P(X ≤ a)", "P(X ≥ a)", "P(a ≤ X ≤ b)", tr("P(|X| ≥ a), bicaudal", "P(|X| ≥ a), two-tailed"))
        )
      ),
      shiny::numericInput(ns("a"), "a", value = 1.96, step = 0.01),
      shiny::conditionalPanel(
        condition = "input.tipo == 'entre'", ns = ns,
        shiny::numericInput(ns("b"), "b", value = -1.96, step = 0.01)
      ),
      shiny::hr(),
      shiny::sliderInput(ns("nivel"), tr("Nível de confiança (%)", "Confidence level (%)"), min = 80, max = 99.9, value = 95, step = 0.1)
    ),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(
        title = tr("Probabilidade da área destacada", "Probability of the shaded area"),
        value = shiny::textOutput(ns("prob")),
        shiny::textOutput(ns("prob_desc")),
        theme = "primary"
      ),
      bslib::value_box(
        title = tr("Valor crítico bicaudal", "Two-tailed critical value"),
        value = shiny::textOutput(ns("critico")),
        shiny::textOutput(ns("critico_desc")),
        theme = "info"
      )
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_body(saida_grafico(ns("grafico"), height = "420px"))
    ),
    bslib::card(
      bslib::card_header(rotulo_entenda()),
      bslib::card_body(texto_normal())
    )
  )
}

mod_normal_server <- function(id, modo = shiny::reactive("light")) {
  shiny::moduleServer(id, function(input, output, session) {

    parametros <- shiny::reactive({
      shiny::req(is.finite(input$a))
      b <- if (input$tipo == "entre") input$b else NA
      if (input$tipo == "entre") shiny::req(is.finite(b))

      intervalos <- intervalos_area(input$tipo, input$a, b)
      list(
        dist       = input$dist,
        gl         = input$gl,
        intervalos = intervalos,
        prob       = prob_area(intervalos, input$dist, input$gl),
        rotulo     = rotulo_prob(input$tipo, input$a, b)
      )
    })

    output$prob      <- shiny::renderText(fmt_num(parametros()$prob, 4))
    output$prob_desc <- shiny::renderText(parametros()$rotulo)

    output$critico <- shiny::renderText({
      p_sup <- 1 - (1 - input$nivel / 100) / 2
      paste0("± ", fmt_num(quant_dist(p_sup, input$dist, input$gl), 3))
    })

    output$critico_desc <- shiny::renderText({
      p_sup  <- 1 - (1 - input$nivel / 100) / 2
      base   <- sprintf(tr("Para %s%% de confiança", "For %s%% confidence"), fmt_num(input$nivel, 1))
      if (input$dist == "z") return(base)
      sprintf(tr("%s. Com Z seria ± %s", "%s. With Z it would be ± %s"), base, fmt_num(stats::qnorm(p_sup), 3))
    })

    output$grafico <- render_grafico({
      p <- parametros()
      pal <- paleta_modo(modo())
      x <- seq(-5, 5, length.out = 2001)
      y <- dens_dist(x, p$dist, p$gl)
      dados <- data.frame(x = x, y = y, area = ifelse(dentro_area(x, p$intervalos), y, 0))

      titulo <- if (p$dist == "z") {
        tr("Normal padrão: Z ~ N(0, 1)", "Standard normal: Z ~ N(0, 1)")
      } else {
        sprintf(tr("t de Student com %d gl", "Student's t with %d df"), p$gl)
      }
      comparar <- p$dist == "t" && isTRUE(input$comparar)

      grafico <- ggplot2::ggplot(dados, ggplot2::aes(x = x)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = 0, ymax = area), fill = cores_app$destaque, alpha = 0.45) +
        ggplot2::geom_line(ggplot2::aes(y = y), linewidth = 1.1, colour = pal$linha)

      if (comparar) {
        grafico <- grafico +
          ggplot2::geom_line(
            data = data.frame(x = x, z = stats::dnorm(x)),
            ggplot2::aes(y = z),
            linetype = "dashed", linewidth = 0.9, colour = cores_app$referencia
          )
      }

      legenda_area <- tr("Área azul: probabilidade calculada", "Blue area: computed probability")

      grafico +
        ggplot2::scale_x_continuous(breaks = -5:5) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title    = titulo,
          subtitle = sprintf("%s = %s", p$rotulo, fmt_num(p$prob, 4)),
          x = tr("Valor", "Value"), y = tr("Densidade", "Density"),
          caption  = if (comparar) paste(legenda_area, tr("| Tracejado vermelho: normal padrão Z", "| Red dashed: standard normal Z")) else legenda_area
        ) +
        tema_grafico(modo())
    })
  })
}
