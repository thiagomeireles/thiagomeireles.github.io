# =============================================================================
# Script:  mod_normal.R (módulo: distribuições Normal N(μ, σ²), Z e t de Student)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-25
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/apps/probabilidade/app.R)
# Saída:
#   - funções mod_normal_ui() e mod_normal_server()
# =============================================================================

dens_dist <- function(x, dist, gl, mu = 0, sigma = 1) {
  if (dist == "normal_geral") return(stats::dnorm(x, mean = mu, sd = sigma))
  if (dist == "z") stats::dnorm(x) else stats::dt(x, df = gl)
}

acum_dist <- function(q, dist, gl, mu = 0, sigma = 1) {
  if (dist == "normal_geral") return(stats::pnorm(q, mean = mu, sd = sigma))
  if (dist == "z") stats::pnorm(q) else stats::pt(q, df = gl)
}

quant_dist <- function(p, dist, gl, mu = 0, sigma = 1) {
  if (dist == "normal_geral") return(stats::qnorm(p, mean = mu, sd = sigma))
  if (dist == "z") stats::qnorm(p) else stats::qt(p, df = gl)
}

# Intervalos (lista de pares inf/sup) que formam a área pedida
intervalos_area <- function(tipo, a, b, centro = 0) {
  afast <- abs(a - centro)
  switch(
    tipo,
    inferior = list(c(-Inf, a)),
    superior = list(c(a, Inf)),
    entre    = list(sort(c(a, b))),
    bicaudal = list(c(-Inf, centro - afast), c(centro + afast, Inf))
  )
}

prob_area <- function(intervalos, dist, gl, mu = 0, sigma = 1) {
  intervalos |>
    purrr::map_dbl(\(l) acum_dist(l[2], dist, gl, mu, sigma) - acum_dist(l[1], dist, gl, mu, sigma)) |>
    sum()
}

dentro_area <- function(x, intervalos) {
  intervalos |>
    purrr::map(\(l) x >= l[1] & x <= l[2]) |>
    purrr::reduce(`|`)
}

rotulo_prob <- function(tipo, a, b, centro = 0) {
  afast <- abs(a - centro)
  switch(
    tipo,
    inferior = sprintf("P(X ≤ %s)", fmt_num(a, 2)),
    superior = sprintf("P(X ≥ %s)", fmt_num(a, 2)),
    entre    = sprintf("P(%s ≤ X ≤ %s)", fmt_num(min(a, b), 2), fmt_num(max(a, b), 2)),
    bicaudal = if (centro == 0) {
      sprintf("P(|X| ≥ %s)", fmt_num(afast, 2))
    } else {
      sprintf("P(|X − μ| ≥ %s)", fmt_num(afast, 2))
    }
  )
}

texto_normal <- function() {
  tr(
    shiny::withMathJax(
      shiny::p(
        "Numa variável aleatória contínua, probabilidades correspondem a áreas sob a curva de densidade \\(f(x)\\):",
        "a área total vale 1 e a probabilidade de qualquer ponto isolado é zero, \\(P(X = a) = 0\\) (Morettin e",
        "Bussab, seção 7.1). A família normal \\(N(\\mu, \\sigma^2)\\) forma uma família de locação-escala (Casella e",
        "Berger, seção 3.5): \\(\\mu\\) desloca o eixo de simetria sem mudar a forma, e \\(\\sigma\\) abre ou estreita",
        "a curva. Por decorrência disso, toda variável \\(X \\sim N(\\mu, \\sigma^2)\\) se reduz à normal padrão",
        "\\(Z = (X - \\mu)/\\sigma \\sim N(0, 1)\\), preservando exatamente a área entre pontos correspondentes."
      ),
      shiny::p(
        "A distribuição \\(t\\) de Student entra em cena na inferência quando \\(\\sigma\\) é desconhecido e precisa",
        "ser estimado pelo desvio padrão amostral \\(S\\): a estatística \\(T = (\\bar{X} - \\mu)/(S/\\sqrt{n})\\)",
        "segue uma \\(t\\) com \\(\\nu = n - 1\\) graus de liberdade (Morettin e Bussab, seção 10.9; Agresti e Finlay,",
        "cap. 5). Como estimar \\(\\sigma\\) acrescenta incerteza em amostras pequenas, a \\(t\\) espalha mais massa",
        "nas caudas do que a \\(Z\\), exigindo valores críticos maiores para o mesmo nível de confiança. À medida que",
        "\\(\\nu\\) cresce, \\(S\\) se estabiliza perto de \\(\\sigma\\) e a curva \\(t\\) converge para a normal padrão."
      ),
      shiny::p(
        shiny::strong("Experimente:"),
        "na normal geral \\(N(\\mu, \\sigma^2)\\), mude \\(\\mu\\) e \\(\\sigma\\) mantendo \\(a = \\mu + 1{,}96\\sigma\\)",
        "para confirmar que a área à direita permanece em 2,5%. Depois passe para a \\(t\\) com 1 ou 2 graus de",
        "liberdade e veja quanto o valor crítico de 95% excede 1,96 antes de se aproximar de \\(Z\\) perto de 30 gl."
      )
    ),
    shiny::withMathJax(
      shiny::p(
        "For a continuous random variable, probabilities are areas under the density curve \\(f(x)\\): the total area",
        "equals 1 and the probability of any single point is zero, \\(P(X = a) = 0\\) (Morettin and Bussab, section",
        "7.1). The normal family \\(N(\\mu, \\sigma^2)\\) is a location-scale family (Casella and Berger, section 3.5):",
        "\\(\\mu\\) shifts the center of symmetry without altering the shape, and \\(\\sigma\\) stretches or",
        "compresses the spread. Consequently, any \\(X \\sim N(\\mu, \\sigma^2)\\) reduces to the standard normal",
        "\\(Z = (X - \\mu)/\\sigma \\sim N(0, 1)\\), preserving the exact area between corresponding cutoffs."
      ),
      shiny::p(
        "Student's \\(t\\) distribution arises in inference when \\(\\sigma\\) is unknown and has to be estimated by",
        "the sample standard deviation \\(s\\): the ratio \\(t = (\\bar{Y} - \\mu)/(s/\\sqrt{n})\\) follows a \\(t\\)",
        "distribution with \\(\\nu = n - 1\\) degrees of freedom (Agresti and Finlay, ch. 5; Morettin and Bussab,",
        "section 10.9). Because estimating \\(\\sigma\\) adds uncertainty in small samples, \\(t\\) puts more mass in",
        "its tails than \\(Z\\), requiring wider critical values for the same confidence level. As \\(\\nu\\) grows,",
        "\\(s\\) settles near \\(\\sigma\\) and the \\(t\\) curve converges to the standard normal."
      ),
      shiny::p(
        shiny::strong("Try it:"),
        "under the general normal \\(N(\\mu, \\sigma^2)\\), change \\(\\mu\\) and \\(\\sigma\\) while setting",
        "\\(a = \\mu + 1.96\\sigma\\) to see the upper-tail area stay at 2.5%. Then switch to \\(t\\) with 1 or 2",
        "degrees of freedom and see how far the 95% critical value exceeds 1.96 before approaching \\(Z\\) past 30 df."
      )
    )
  )
}

cuidados_normal <- function() {
  tr(
    c(
      "Desigualdades < e ≤" = "em distribuições contínuas, P(X < a) e P(X ≤ a) são idênticas, porque a linha vertical em X = a tem área zero.",
      "Variância σ² contra desvio padrão σ" = paste(
        "Morettin e Bussab escrevem N(μ, σ²) com a variância no segundo argumento, mas pnorm() e qnorm() no R",
        "recebem o desvio padrão σ = √σ². Confundir os dois é um dos erros mais comuns em código."
      ),
      "Momentos da t com poucos graus de liberdade" = paste(
        "com gl = 1 a distribuição t coincide com a Cauchy padrão e não possui média nem variância; com gl = 2 a média",
        "é 0, mas a variância é infinita; para gl ≥ 3, Var(T) = gl / (gl − 2)."
      ),
      "Densidade não é probabilidade" = "f(x) é altura (densidade por unidade de x) e pode passar de 1 quando σ é pequeno; só a área ∫ f(x) dx está entre 0 e 1."
    ),
    c(
      "Strict vs. weak inequalities" = "for continuous distributions, P(X < a) and P(X ≤ a) are identical because a single vertical line at X = a has zero area.",
      "Variance σ² vs. standard deviation σ" = paste(
        "textbooks write N(μ, σ²) with the variance as the second parameter, whereas R's pnorm() and qnorm() take",
        "the standard deviation σ = √σ². Mixing them up is one of the most frequent coding mistakes."
      ),
      "Moments of t for small df" = paste(
        "at df = 1 Student's t is the standard Cauchy and has neither mean nor variance; at df = 2 the mean is 0",
        "but variance is infinite; for df ≥ 3, Var(T) = df / (df − 2)."
      ),
      "Density is not probability" = "f(x) is height per unit of x and can exceed 1 when σ is small; only the integrated area ∫ f(x) dx lies between 0 and 1."
    )
  )
}

cartao_cuidados_prob <- function(titulo, itens) {
  bslib::card(
    bslib::card_header(titulo),
    bslib::card_body(
      shiny::tags$ul(
        class = "mb-0",
        purrr::imap(itens, \(texto, nome) shiny::tags$li(shiny::strong(paste0(nome, ": ")), texto))
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
        choices = stats::setNames(
          c("z", "normal_geral", "t"),
          tr(
            c("Normal padrão Z ~ N(0, 1)", "Normal geral X ~ N(μ, σ²)", "t de Student (ν gl)"),
            c("Standard normal Z ~ N(0, 1)", "General normal X ~ N(μ, σ²)", "Student's t (ν df)")
          )
        )
      ),
      shiny::conditionalPanel(
        condition = "input.dist == 'normal_geral'", ns = ns,
        shiny::numericInput(ns("mu"), tr("Média (μ)", "Mean (μ)"), value = 100, step = 1),
        shiny::numericInput(ns("sigma"), tr("Desvio padrão (σ > 0)", "Standard deviation (σ > 0)"), value = 15, min = 0.1, step = 0.5)
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
          c("P(X ≤ a)", "P(X ≥ a)", "P(a ≤ X ≤ b)", tr("Bicaudal simétrica", "Symmetric two-tailed"))
        )
      ),
      shiny::numericInput(ns("a"), "a", value = 1.96, step = 0.05),
      shiny::conditionalPanel(
        condition = "input.tipo == 'entre'", ns = ns,
        shiny::numericInput(ns("b"), "b", value = -1.96, step = 0.05)
      ),
      shiny::hr(),
      shiny::sliderInput(ns("nivel"), tr("Nível de confiança central (%)", "Central confidence level (%)"), min = 80, max = 99.9, value = 95, step = 0.1)
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
        title = tr("Corte central de confiança", "Central confidence cutoff"),
        value = shiny::textOutput(ns("critico")),
        shiny::textOutput(ns("critico_desc")),
        theme = "info"
      ),
      bslib::value_box(
        title = tr("Equivalente padronizado (z de a)", "Standardized equivalent (z of a)"),
        value = shiny::textOutput(ns("z_equiv")),
        shiny::textOutput(ns("momentos_desc")),
        theme = NULL
      )
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_body(shiny::plotOutput(ns("grafico"), height = "420px"))
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
      bslib::card(
        bslib::card_header(rotulo_entenda()),
        bslib::card_body(texto_normal())
      ),
      cartao_cuidados_prob(tr("Cuidados", "Watch out"), cuidados_normal())
    )
  )
}

mod_normal_server <- function(id, modo = shiny::reactive("light")) {
  shiny::moduleServer(id, function(input, output, session) {

    # Ajusta os pontos iniciais a e b ao trocar entre escala padrão (0, 1) e N(μ, σ²)
    shiny::observeEvent(input$dist, {
      if (identical(input$dist, "normal_geral")) {
        mu <- if (is.finite(input$mu)) input$mu else 100
        sg <- if (is.finite(input$sigma) && input$sigma > 0) input$sigma else 15
        shiny::updateNumericInput(session, "a", value = round(mu + 1.96 * sg, 2))
        shiny::updateNumericInput(session, "b", value = round(mu - 1.96 * sg, 2))
      } else {
        shiny::updateNumericInput(session, "a", value = 1.96)
        shiny::updateNumericInput(session, "b", value = -1.96)
      }
    }, ignoreInit = TRUE)

    parametros <- shiny::reactive({
      shiny::req(is.finite(input$a))
      mu <- if (identical(input$dist, "normal_geral")) {
        shiny::req(is.finite(input$mu))
        input$mu
      } else {
        0
      }
      sigma <- if (identical(input$dist, "normal_geral")) {
        shiny::req(is.finite(input$sigma), input$sigma > 0)
        input$sigma
      } else {
        1
      }
      b <- if (input$tipo == "entre") input$b else NA
      if (input$tipo == "entre") shiny::req(is.finite(b))

      intervalos <- intervalos_area(input$tipo, input$a, b, centro = mu)
      list(
        dist       = input$dist,
        gl         = input$gl,
        mu         = mu,
        sigma      = sigma,
        intervalos = intervalos,
        prob       = prob_area(intervalos, input$dist, input$gl, mu, sigma),
        rotulo     = rotulo_prob(input$tipo, input$a, b, centro = mu)
      )
    })

    output$prob      <- shiny::renderText(fmt_num(parametros()$prob, 4))
    output$prob_desc <- shiny::renderText(parametros()$rotulo)

    output$critico <- shiny::renderText({
      p <- parametros()
      p_sup <- 1 - (1 - input$nivel / 100) / 2
      q_sup <- quant_dist(p_sup, p$dist, p$gl, p$mu, p$sigma)
      if (p$dist == "normal_geral") {
        q_inf <- quant_dist(1 - p_sup, p$dist, p$gl, p$mu, p$sigma)
        sprintf("[%s; %s]", fmt_num(q_inf, 2), fmt_num(q_sup, 2))
      } else {
        paste0("± ", fmt_num(q_sup, 3))
      }
    })

    output$critico_desc <- shiny::renderText({
      p <- parametros()
      p_sup <- 1 - (1 - input$nivel / 100) / 2
      base <- sprintf(tr("Para %s%% de confiança", "For %s%% confidence"), fmt_num(input$nivel, 1))
      if (p$dist == "z") return(base)
      if (p$dist == "normal_geral") {
        return(sprintf(tr("%s (μ ± %s σ)", "%s (μ ± %s σ)"), base, fmt_num(stats::qnorm(p_sup), 3)))
      }
      sprintf(tr("%s. Com Z seria ± %s", "%s. With Z it would be ± %s"), base, fmt_num(stats::qnorm(p_sup), 3))
    })

    output$z_equiv <- shiny::renderText({
      p <- parametros()
      if (p$dist == "t") {
        return(sprintf("t = %s", fmt_num(input$a, 2)))
      }
      z <- (input$a - p$mu) / p$sigma
      sprintf("z = %s", fmt_num(z, 2))
    })

    output$momentos_desc <- shiny::renderText({
      p <- parametros()
      if (p$dist == "t") {
        if (p$gl == 1) return(tr("gl = 1 (Cauchy): sem E(T) nem Var(T)", "df = 1 (Cauchy): no E(T) or Var(T)"))
        if (p$gl == 2) return(tr("E(T) = 0 | Var(T) = ∞", "E(T) = 0 | Var(T) = ∞"))
        return(sprintf("E(T) = 0 | Var(T) = %s", fmt_num(p$gl / (p$gl - 2), 3)))
      }
      sprintf("E(X) = %s | Var(X) = %s", fmt_num(p$mu, 2), fmt_num(p$sigma^2, 2))
    })

    output$grafico <- shiny::renderPlot({
      p <- parametros()
      pal <- paleta_modo(modo())
      amplitude <- if (p$dist == "normal_geral") 4.5 * p$sigma else 5
      x <- seq(p$mu - amplitude, p$mu + amplitude, length.out = 2001)
      y <- dens_dist(x, p$dist, p$gl, p$mu, p$sigma)
      dados <- data.frame(x = x, y = y, area = ifelse(dentro_area(x, p$intervalos), y, 0))

      titulo <- switch(
        p$dist,
        z = tr("Normal padrão: Z ~ N(0, 1)", "Standard normal: Z ~ N(0, 1)"),
        normal_geral = sprintf(
          tr("Normal geral: X ~ N(%s; %s²)", "General normal: X ~ N(%s, %s²)"),
          fmt_num(p$mu, 1), fmt_num(p$sigma, 1)
        ),
        sprintf(tr("t de Student com %d gl", "Student's t with %d df"), p$gl)
      )
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
        ggplot2::scale_x_continuous(labels = rotulos_eixo()) +
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
