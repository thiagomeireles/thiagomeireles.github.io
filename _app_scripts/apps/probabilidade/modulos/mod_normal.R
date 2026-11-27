# =============================================================================
# Script:  mod_normal.R (módulo: distribuições Normal N(μ, σ²), Z e t de Student)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-27
#
# Entrada:
#   - funções de modulos/dados.R (carregado por _app_scripts/apps/probabilidade/app.R)
# Saída:
#   - funções mod_normal_ui() e mod_normal_server()
# =============================================================================

texto_normal <- function() {
  tr(
    shiny::withMathJax(
      shiny::p(
        "Numa variável aleatória contínua, probabilidades são áreas sob a curva de densidade \\(f(x)\\): a área",
        "total vale 1 e a probabilidade de um ponto isolado é zero, \\(P(X = a) = 0\\) (Morettin e Bussab, seção",
        "7.1). A normal \\(N(\\mu, \\sigma^2)\\) (seção 7.4) é uma família de locação-escala (Casella e Berger, seção",
        "3.5): \\(\\mu\\) desloca o centro de simetria sem mudar a forma, e \\(\\sigma\\) abre ou estreita a curva.",
        "Por isso, toda \\(X \\sim N(\\mu, \\sigma^2)\\) se reduz à normal padrão",
        "\\(Z = (X - \\mu)/\\sigma \\sim N(0, 1)\\), e a área entre dois cortes de \\(X\\) é a mesma área entre os",
        "escores \\(z\\) correspondentes (Agresti e Finlay, seção 4.3)."
      ),
      shiny::p(
        "A \\(t\\) de Student (Morettin e Bussab, seção 7.7.3) entra na inferência quando \\(\\sigma\\) é",
        "desconhecido e precisa ser estimado pelo desvio padrão amostral \\(S\\). Numa população normal,",
        "\\(T = (\\bar{X} - \\mu)/(S/\\sqrt{n})\\) segue uma \\(t\\) com \\(\\nu = n - 1\\) graus de liberdade",
        "(Morettin e Bussab, seção 12.10; Agresti e Finlay, seção 5.3). Estimar \\(\\sigma\\) acrescenta incerteza,",
        "que é grande em amostras pequenas; por isso a \\(t\\) põe mais massa nas caudas do que a \\(Z\\) e pede",
        "valores críticos maiores para o mesmo nível de confiança. Com \\(\\nu\\) grande, \\(S\\) fica perto de",
        "\\(\\sigma\\) e a \\(t\\) se aproxima da normal padrão."
      ),
      shiny::p(
        shiny::strong("Experimente:"),
        "na normal geral, escolha \\(P(X \\geq a)\\) e mude \\(\\mu\\) e \\(\\sigma\\). O corte \\(a\\) se move junto e",
        "mantém \\(z = 1{,}96\\), então a área à direita continua em 2,5%. Depois passe para a \\(t\\) com 1 ou 2",
        "graus de liberdade e compare, no terceiro quadro, a área da \\(t\\) com a da \\(Z\\) nos mesmos cortes. Com",
        "poucos graus de liberdade, o valor crítico de 95% fica bem acima de 1,96, e com 30 gl ele ainda é 2,04."
      )
    ),
    shiny::withMathJax(
      shiny::p(
        "For a continuous random variable, probabilities are areas under the density curve \\(f(x)\\): the total",
        "area is 1 and the probability of any single point is zero, \\(P(X = a) = 0\\) (Morettin and Bussab,",
        "section 7.1). The normal \\(N(\\mu, \\sigma^2)\\) (section 7.4) is a location-scale family (Casella and",
        "Berger, section 3.5): \\(\\mu\\) shifts the center of symmetry without changing the shape, and \\(\\sigma\\)",
        "stretches or compresses the curve. That is why any \\(X \\sim N(\\mu, \\sigma^2)\\) reduces to the standard",
        "normal \\(Z = (X - \\mu)/\\sigma \\sim N(0, 1)\\), and the area between two cutoffs of \\(X\\) equals the area",
        "between the matching \\(z\\)-scores (Agresti and Finlay, section 4.3)."
      ),
      shiny::p(
        "Student's \\(t\\) (Morettin and Bussab, section 7.7.3) comes into inference when \\(\\sigma\\) is unknown and",
        "has to be estimated by the sample standard deviation \\(s\\). In a normal population,",
        "\\(t = (\\bar{y} - \\mu)/(s/\\sqrt{n})\\) follows a \\(t\\) distribution with \\(\\nu = n - 1\\) degrees of",
        "freedom (Agresti and Finlay, section 5.3; Morettin and Bussab, section 12.10). Estimating \\(\\sigma\\) adds",
        "uncertainty, which is large in small samples, so \\(t\\) puts more mass in its tails than \\(Z\\) and calls",
        "for larger critical values at the same confidence level. As \\(\\nu\\) grows, \\(s\\) settles near",
        "\\(\\sigma\\) and \\(t\\) approaches the standard normal."
      ),
      shiny::p(
        shiny::strong("Try it:"),
        "under the general normal, pick \\(P(X \\geq a)\\) and change \\(\\mu\\) and \\(\\sigma\\). The cutoff \\(a\\)",
        "moves along and keeps \\(z = 1.96\\), so the upper-tail area stays at 2.5%. Then switch to \\(t\\) with 1 or",
        "2 degrees of freedom and compare, in the third box, the area under \\(t\\) with the area under \\(Z\\) for",
        "the same cutoffs. With few degrees of freedom, the 95% critical value sits well above 1.96, and at 30 df it",
        "is still 2.04."
      )
    )
  )
}

cuidados_normal <- function() {
  tr(
    c(
      "Desigualdades < e ≤" = "em distribuições contínuas, P(X < a) e P(X ≤ a) são iguais, porque a linha vertical em X = a tem área zero.",
      "Variância σ² contra desvio padrão σ" = paste(
        "Morettin e Bussab escrevem N(μ, σ²) com a variância no segundo argumento, mas pnorm() e qnorm() no R",
        "recebem o desvio padrão σ = √σ². Se você passar a variância no lugar do desvio padrão, o R calcula sem",
        "reclamar e devolve uma probabilidade errada."
      ),
      "Momentos da t com poucos graus de liberdade" = paste(
        "com gl = 1 a t coincide com a Cauchy padrão e não tem média nem variância; com gl = 2 a média",
        "é 0, mas a variância é infinita; para gl ≥ 3, Var(T) = gl / (gl − 2) (Casella e Berger, eq. 5.3.7)."
      ),
      "Densidade não é probabilidade" = "f(x) é altura (densidade por unidade de x) e pode passar de 1 quando σ é pequeno; só a área ∫ f(x) dx fica entre 0 e 1."
    ),
    c(
      "Strict vs. weak inequalities" = "for continuous distributions, P(X < a) and P(X ≤ a) are equal because the vertical line at X = a has zero area.",
      "Variance σ² vs. standard deviation σ" = paste(
        "Morettin and Bussab write N(μ, σ²) with the variance as the second parameter, but R's pnorm() and qnorm()",
        "take the standard deviation σ = √σ². If you pass the variance instead of the standard deviation, R runs",
        "without complaint and returns the wrong probability."
      ),
      "Moments of t for small df" = paste(
        "at df = 1 Student's t is the standard Cauchy and has neither mean nor variance; at df = 2 the mean is 0",
        "but the variance is infinite; for df ≥ 3, Var(T) = df / (df − 2) (Casella and Berger, eq. 5.3.7)."
      ),
      "Density is not probability" = "f(x) is height per unit of x and can exceed 1 when σ is small; only the area ∫ f(x) dx lies between 0 and 1."
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
        shiny::numericInput(ns("sigma"), tr("Desvio padrão (σ > 0)", "Standard deviation (σ > 0)"), value = 15, min = 0.1, step = 0.5),
        shiny::helpText(tr(
          "Quando você muda μ ou σ, os cortes a e b se movem junto e mantêm o mesmo z.",
          "When you change μ or σ, cutoffs a and b move along and keep the same z."
        ))
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
        title = shiny::textOutput(ns("titulo_terceiro")),
        value = shiny::textOutput(ns("terceiro")),
        shiny::textOutput(ns("terceiro_desc")),
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
      cartao_cuidados(tr("Cuidados", "Watch out"), cuidados_normal())
    )
  )
}

mod_normal_server <- function(id, modo = shiny::reactive("light")) {
  shiny::moduleServer(id, function(input, output, session) {

    # Escores z dos cortes a e b na normal geral. Ao mudar μ ou σ, os cortes
    # são recalculados a partir deles, e a área destacada não muda.
    z_cortes <- shiny::reactiveVal(c(a = 1.96, b = -1.96))

    escala_valida <- function() {
      is.numeric(input$mu) && is.finite(input$mu) && is.numeric(input$sigma) && is.finite(input$sigma) && input$sigma > 0
    }

    # Ajusta os pontos iniciais a e b ao trocar entre escala padrão (0, 1) e N(μ, σ²)
    shiny::observeEvent(input$dist, {
      if (identical(input$dist, "normal_geral")) {
        mu <- if (is.numeric(input$mu) && is.finite(input$mu)) input$mu else 100
        sg <- if (is.numeric(input$sigma) && is.finite(input$sigma) && input$sigma > 0) input$sigma else 15
        z_cortes(c(a = 1.96, b = -1.96))
        shiny::updateNumericInput(session, "a", value = round(mu + 1.96 * sg, 4))
        shiny::updateNumericInput(session, "b", value = round(mu - 1.96 * sg, 4))
      } else {
        shiny::updateNumericInput(session, "a", value = 1.96)
        shiny::updateNumericInput(session, "b", value = -1.96)
      }
    }, ignoreInit = TRUE)

    # Quem digita a ou b na normal geral muda o z guardado
    shiny::observeEvent(list(input$a, input$b), {
      if (!identical(input$dist, "normal_geral") || !escala_valida()) return()
      z <- z_cortes()
      if (is.numeric(input$a) && is.finite(input$a)) z[["a"]] <- (input$a - input$mu) / input$sigma
      if (is.numeric(input$b) && is.finite(input$b)) z[["b"]] <- (input$b - input$mu) / input$sigma
      z_cortes(z)
    }, ignoreInit = TRUE)

    # Quem muda μ ou σ leva os cortes junto, com o mesmo z
    shiny::observeEvent(list(input$mu, input$sigma), {
      if (!identical(input$dist, "normal_geral") || !escala_valida()) return()
      z <- z_cortes()
      shiny::updateNumericInput(session, "a", value = round(input$mu + z[["a"]] * input$sigma, 4))
      shiny::updateNumericInput(session, "b", value = round(input$mu + z[["b"]] * input$sigma, 4))
    }, ignoreInit = TRUE)

    parametros <- shiny::reactive({
      geral <- identical(input$dist, "normal_geral")
      shiny::validate(
        shiny::need(!geral || (is.numeric(input$mu) && is.finite(input$mu)), tr("Informe a média μ.", "Enter the mean μ.")),
        shiny::need(
          !geral || (is.numeric(input$sigma) && is.finite(input$sigma) && input$sigma > 0),
          tr("O desvio padrão σ precisa ser maior que zero.", "The standard deviation σ must be greater than zero.")
        ),
        shiny::need(is.numeric(input$a) && is.finite(input$a), tr("Informe o corte a.", "Enter the cutoff a.")),
        shiny::need(
          input$tipo != "entre" || (is.numeric(input$b) && is.finite(input$b)),
          tr("Informe o corte b.", "Enter the cutoff b.")
        )
      )
      mu <- if (geral) input$mu else 0
      sigma <- if (geral) input$sigma else 1
      b <- if (input$tipo == "entre") input$b else NA

      intervalos <- intervalos_area(input$tipo, input$a, b, centro = mu)
      # Mesmos cortes em escala z, para comparar a área da t com a da normal padrão
      intervalos_z <- purrr::map(intervalos, \(l) (l - mu) / sigma)
      list(
        dist       = input$dist,
        gl         = input$gl,
        mu         = mu,
        sigma      = sigma,
        intervalos = intervalos,
        prob       = prob_area(intervalos, input$dist, input$gl, mu, sigma),
        prob_z     = prob_area(intervalos_z, "z", input$gl),
        rotulo     = rotulo_prob(input$tipo, input$a, b, centro = mu, letra = letra_dist(input$dist)),
        momentos   = texto_momentos(input$dist, input$gl, mu, sigma)
      )
    })

    output$prob      <- shiny::renderText(fmt_num(parametros()$prob, 4))
    output$prob_desc <- shiny::renderText(parametros()$rotulo)

    output$critico <- shiny::renderText({
      p <- parametros()
      p_sup <- 1 - (1 - input$nivel / 100) / 2
      q_sup <- quant_dist(p_sup, p$dist, p$gl, p$mu, p$sigma)
      if (p$dist == "normal_geral") {
        fmt_intervalo(quant_dist(1 - p_sup, p$dist, p$gl, p$mu, p$sigma), q_sup)
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
        return(sprintf("%s (μ ± %s σ)", base, fmt_num(stats::qnorm(p_sup), 3)))
      }
      sprintf(tr("%s. Com Z seria ± %s", "%s. With Z it would be ± %s"), base, fmt_num(stats::qnorm(p_sup), 3))
    })

    # Terceiro quadro: escore z do corte a (Z e normal geral) ou a mesma área sob a Z (t)
    output$titulo_terceiro <- shiny::renderText({
      if (identical(input$dist, "t")) {
        tr("Área sob a Z nos mesmos cortes", "Area under Z for the same cutoffs")
      } else {
        tr("Escore z do corte a", "z-score of cutoff a")
      }
    })

    output$terceiro <- shiny::renderText({
      p <- parametros()
      if (p$dist == "t") return(fmt_num(p$prob_z, 4))
      sprintf("z = %s", fmt_num((input$a - p$mu) / p$sigma, 2))
    })

    output$terceiro_desc <- shiny::renderText({
      p <- parametros()
      if (p$dist != "t") return(p$momentos)
      dif <- p$prob - p$prob_z
      sprintf(tr("Diferença t − Z: %s%s", "Difference t − Z: %s%s"), if (dif >= 0) "+" else "−", fmt_num(abs(dif), 4))
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
          format(p$mu, decimal.mark = tr(",", "."), big.mark = tr(".", ",")),
          format(p$sigma, decimal.mark = tr(",", "."), big.mark = tr(".", ","))
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
          subtitle = sprintf("%s = %s | %s", p$rotulo, fmt_num(p$prob, 4), p$momentos),
          x = tr("Valor", "Value"), y = tr("Densidade", "Density"),
          caption  = if (comparar) paste(legenda_area, tr("| Tracejado vermelho: normal padrão Z", "| Red dashed: standard normal Z")) else legenda_area
        ) +
        tema_grafico(modo())
    })
  })
}
