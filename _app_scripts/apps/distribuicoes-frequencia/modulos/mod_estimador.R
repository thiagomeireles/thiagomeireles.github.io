# =============================================================================
# Script:  mod_estimador.R (módulo: histograma como estimador da densidade, MISE e KDE)
# Autor:  Thiago Meireles
# Atualizado: 2026-11-25
#
# Entrada:
#   - nenhuma (carregado por _app_scripts/apps/distribuicoes-frequencia/app.R)
# Saída:
#   - funções mod_estimador_ui() e mod_estimador_server()
# =============================================================================

# A uniforme fica de fora: as descontinuidades nas bordas tornam ∫f'² infinita
# e a fórmula assintótica de h* não se aplica
modelos_estimador <- modelos_densidade[c("normal", "assimetrica", "bimodal")]

# Quebras de largura h ancoradas em `ancora`, cobrindo o intervalo [de; ate]
quebras_ancoradas <- function(ancora, h, de, ate) {
  inicio <- ancora + h * floor((de - ancora) / h)
  fim <- ancora + h * (floor((ate - ancora) / h) + 1)
  seq(inicio, fim, by = h)
}

# Histograma como estimador: f̂(x) = n_j / (n h) para x na classe j
# (Yuksel e Aydede, seção 8.1.1). Devolve as quebras e as alturas.
estimar_histograma <- function(x, h, ancora) {
  quebras <- quebras_ancoradas(ancora, h, min(x), max(x))
  contagem <- tabulate(findInterval(x, quebras), nbins = length(quebras) - 1)
  list(quebras = quebras, altura = contagem / (length(x) * h), p = contagem / length(x))
}

avaliar_histograma <- function(est, x) {
  j <- findInterval(x, est$quebras)
  dentro <- j >= 1 & j < length(est$quebras)
  ifelse(dentro, est$altura[pmax(1, pmin(j, length(est$altura)))], 0)
}

# ISE de uma estimativa: ∫(f̂ − f)², numa grade que cobre o suporte e os dados
ise <- function(f_chapeu, modelo, limites) {
  xs <- seq(limites[1], limites[2], length.out = 4000)
  integrar(xs, (f_chapeu(xs) - modelo$f(xs))^2)
}

# MISE exato do histograma com classes fixas: n_j ~ binomial(n, p_j), então
# E f̂(x) = p_j / h e Var f̂(x) = p_j (1 − p_j) / (n h²). Integrando:
# variância integrada = (1 − Σ p_j²) / (n h); viés² = ∫(p_j / h − f(x))² dx.
mise_histograma <- function(modelo, n, h) {
  s <- modelo$suporte
  quebras <- quebras_ancoradas(s[1], h, s[1], s[2])
  p <- diff(modelo$F(quebras))
  xs <- seq(quebras[1], utils::tail(quebras, 1), length.out = 4000)
  esperado <- p[pmin(findInterval(xs, quebras), length(p))] / h
  vies2 <- integrar(xs, (esperado - modelo$f(xs))^2)
  variancia <- (1 - sum(p^2)) / (n * h)
  c(vies2 = vies2, variancia = variancia, mise = vies2 + variancia)
}

# Validação cruzada do histograma (risco estimado, Rudemo 1982):
# J(h) = 2 / ((n − 1) h) − (n + 1) / ((n − 1) h) · Σ p̂_j². Estima MISE − ∫f².
cv_histograma <- function(x, h, ancora) {
  n <- length(x)
  p <- estimar_histograma(x, h, ancora)$p
  2 / ((n - 1) * h) - (n + 1) / ((n - 1) * h) * sum(p^2)
}

# ∫f'² por diferenças finitas, usada na largura ótima assintótica
# h* = (6 / ∫f'²)^(1/3) n^(−1/3) (Scott, 1979)
integral_derivada2 <- function(modelo) {
  xs <- seq(modelo$suporte[1], modelo$suporte[2], length.out = 20001)
  derivada <- diff(modelo$f(xs)) / diff(xs)
  sum(derivada^2 * diff(xs))
}

grade_h <- function(modelo) {
  amplitude <- diff(modelo$suporte)
  exp(seq(log(amplitude / 200), log(amplitude / 4), length.out = 80))
}

texto_estimador <- function() {
  tr(
    shiny::withMathJax(
      shiny::p(
        "Quando os dados são uma amostra de uma população com densidade \\(f\\), o histograma com",
        "densidade deixa de ser só um desenho e passa a ser um estimador de \\(f\\). Para \\(x\\) na classe",
        "\\(j\\), de largura \\(h\\),",
        "\\(\\hat f(x) = \\frac{1}{nh}\\sum_{i=1}^{n} \\mathbf{1}(x_i \\in \\text{classe } j) = \\frac{n_j}{nh}\\)",
        "(Yuksel e Aydede, seção 8.1.1). É a mesma altura \\(f_i/\\Delta_i\\) da aba Histograma, agora lida como",
        "estimativa de uma curva que existe na população."
      ),
      shiny::p(
        "A largura \\(h\\) controla a troca entre viés e variância. A contagem \\(n_j\\) segue uma binomial",
        "\\((n, p_j)\\), com \\(p_j\\) a probabilidade da classe. Daí \\(E\\,\\hat f(x) = p_j/h\\), a média de \\(f\\)",
        "na classe, e \\(\\text{Var}\\,\\hat f(x) = p_j(1-p_j)/(nh^2)\\). Classes largas achatam picos e vales (viés",
        "alto); classes estreitas têm poucas observações cada e oscilam de amostra para amostra (variância alta). O",
        "erro quadrático integrado médio, \\(\\text{MISE}(h) = E\\int(\\hat f - f)^2\\), soma os dois e é o critério",
        "para escolher \\(h\\)."
      ),
      shiny::p(
        "Para \\(h\\) pequeno e \\(n\\) grande, \\(\\text{MISE}(h) \\approx \\frac{1}{nh} + \\frac{h^2}{12}\\int f'(x)^2dx\\),",
        "minimizado em \\(h^* = (6/\\int f'^2)^{1/3}\\, n^{-1/3}\\). Na normal, isso dá \\(3{,}49\\,\\sigma\\, n^{-1/3}\\)",
        "(regra de Scott). Na prática \\(f\\) é desconhecida, e a escolha vem da validação cruzada, que estima o",
        "risco só com os dados: \\(J(h) = \\frac{2}{(n-1)h} - \\frac{n+1}{(n-1)h}\\sum_j \\hat p_j^2\\). Como \\(J\\)",
        "estima \\(\\text{MISE} - \\int f^2\\), o gráfico soma \\(\\int f^2\\) (conhecida aqui, porque a população é",
        "simulada) só para pôr as duas curvas na mesma escala; o mínimo não muda."
      ),
      shiny::p(
        "O histograma tem dois defeitos: depende da origem das classes e dá o mesmo peso a todos os pontos da",
        "classe, perto ou longe de \\(x\\). O estimador de kernel (KDE) troca a classe fixa por uma janela centrada",
        "em cada \\(x\\) e pesos que caem com a distância: \\(\\hat f(x) = \\frac{1}{nh}\\sum K\\big(\\frac{x - x_i}{h}\\big)\\).",
        "A curva fica suave, e o MISE ótimo cai mais depressa com \\(n\\) (ordem \\(n^{-4/5}\\), contra \\(n^{-2/3}\\) do",
        "histograma). A banda \\(h\\), porém, continua determinando o resultado. A regra de Silverman do livro,",
        "\\(h = 1{,}06\\,\\hat\\sigma\\, n^{-1/5}\\) (seção 8.2.6), é ótima para a normal. Na bimodal, o desvio",
        "padrão alto, que vem da distância entre os picos, faz a regra alisar demais, e a KDE pode perder para um",
        "histograma bem escolhido. A validação cruzada, o método que o livro prefere, tende a corrigir isso."
      ),
      shiny::p(
        shiny::strong("Experimente:"),
        "na bimodal, leve \\(h\\) ao máximo e veja o viés apagar os dois picos; leve ao mínimo e veja o ruído. Depois",
        "use os botões para pular para o \\(h\\) ótimo e para o da validação cruzada, e sorteie de novo: o ótimo",
        "fica parado, a validação cruzada oscila em torno dele."
      )
    ),
    shiny::withMathJax(
      shiny::p(
        "When the data are a sample from a population with density \\(f\\), the density histogram is no longer just",
        "a picture and becomes an estimator of \\(f\\). For \\(x\\) in bin \\(j\\), of width \\(h\\),",
        "\\(\\hat f(x) = \\frac{1}{nh}\\sum_{i=1}^{n} \\mathbf{1}(x_i \\in \\text{bin } j) = \\frac{n_j}{nh}\\)",
        "(Yuksel and Aydede, section 8.1.1). It is the same height \\(f_i/\\Delta_i\\) as in the Histogram tab, now",
        "read as an estimate of a curve that exists in the population."
      ),
      shiny::p(
        "The width \\(h\\) controls the trade-off between bias and variance. The count \\(n_j\\) is binomial",
        "\\((n, p_j)\\), with \\(p_j\\) the probability of the bin. Hence \\(E\\,\\hat f(x) = p_j/h\\), the average of",
        "\\(f\\) over the bin, and \\(\\text{Var}\\,\\hat f(x) = p_j(1-p_j)/(nh^2)\\). Wide bins flatten peaks and",
        "valleys (high bias); narrow bins hold few observations each and jump around from sample to sample (high",
        "variance). The mean integrated squared error, \\(\\text{MISE}(h) = E\\int(\\hat f - f)^2\\), adds the two",
        "and is the criterion for choosing \\(h\\)."
      ),
      shiny::p(
        "For small \\(h\\) and large \\(n\\), \\(\\text{MISE}(h) \\approx \\frac{1}{nh} + \\frac{h^2}{12}\\int f'(x)^2dx\\),",
        "minimized at \\(h^* = (6/\\int f'^2)^{1/3}\\, n^{-1/3}\\). For the normal, this gives",
        "\\(3.49\\,\\sigma\\, n^{-1/3}\\) (Scott's rule). In practice \\(f\\) is unknown, and the choice comes from",
        "cross-validation, which estimates the risk from the data alone:",
        "\\(J(h) = \\frac{2}{(n-1)h} - \\frac{n+1}{(n-1)h}\\sum_j \\hat p_j^2\\). Since \\(J\\) estimates",
        "\\(\\text{MISE} - \\int f^2\\), the plot adds \\(\\int f^2\\) (known here, because the population is simulated)",
        "only to put both curves on the same scale; the minimum does not move."
      ),
      shiny::p(
        "The histogram has two flaws: it depends on where the bins start, and it gives the same weight to every point",
        "in the bin, near or far from \\(x\\). The kernel density estimator (KDE) replaces the fixed bin with a window",
        "centered at each \\(x\\) and weights that fall with distance: \\(\\hat f(x) = \\frac{1}{nh}\\sum K\\big(\\frac{x - x_i}{h}\\big)\\).",
        "The curve is smooth, and the optimal MISE falls faster with \\(n\\) (order \\(n^{-4/5}\\), against \\(n^{-2/3}\\)",
        "for the histogram). The bandwidth \\(h\\), however, still drives the result. The book's Silverman rule,",
        "\\(h = 1.06\\,\\hat\\sigma\\, n^{-1/5}\\) (section 8.2.6), is optimal for the normal. For the bimodal model,",
        "the large standard deviation, driven by the distance between the peaks, makes the rule oversmooth, and the",
        "KDE can lose to a well-chosen histogram. Cross-validation, the method the book prefers, tends to fix that."
      ),
      shiny::p(
        shiny::strong("Try it:"),
        "for the bimodal model, push \\(h\\) to the maximum and watch the bias erase both peaks; push it to the",
        "minimum and watch the noise. Then use the buttons to jump to the optimal \\(h\\) and to the cross-validated",
        "one, and draw again: the optimum stays put, cross-validation wobbles around it."
      )
    )
  )
}

mod_estimador_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::selectInput(
        ns("modelo"), tr("População (densidade f conhecida)", "Population (known density f)"),
        choices = stats::setNames(names(modelos_estimador), purrr::map_chr(modelos_estimador, "rotulo")),
        selected = "bimodal"
      ),
      shiny::sliderInput(ns("n"), tr("Tamanho da amostra (n)", "Sample size (n)"), min = 50, max = 5000, value = 500, step = 50, sep = sep_milhar()),
      shiny::uiOutput(ns("controle_h")),
      shiny::div(
        class = "d-grid gap-2 mb-3",
        shiny::actionButton(ns("usar_otimo"), tr("Usar o h que minimiza o MISE", "Use the h that minimizes MISE"), class = "btn-outline-primary btn-sm"),
        shiny::actionButton(ns("usar_cv"), tr("Usar o h da validação cruzada", "Use the cross-validated h"), class = "btn-outline-primary btn-sm")
      ),
      shiny::checkboxInput(ns("kde"), tr("Sobrepor a KDE gaussiana", "Overlay the Gaussian KDE"), value = FALSE),
      shiny::radioButtons(
        ns("banda"), tr("Banda da KDE", "KDE bandwidth"),
        choices = stats::setNames(c("silverman", "cv"), tr(c("Silverman: 1,06 s n^(−1/5)", "Validação cruzada"), c("Silverman: 1.06 s n^(−1/5)", "Cross-validation")))
      ),
      shiny::actionButton(ns("sortear"), tr("Sortear nova amostra", "Draw a new sample"), class = "btn-primary", icon = shiny::icon("dice"))
    ),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(title = tr("ISE do histograma nesta amostra", "Histogram ISE in this sample"), value = shiny::textOutput(ns("ise_hist")), shiny::textOutput(ns("ise_hist_desc")), theme = "primary"),
      bslib::value_box(title = tr("h que minimiza o MISE", "h that minimizes MISE"), value = shiny::textOutput(ns("h_otimo")), shiny::textOutput(ns("h_otimo_desc")), theme = "info"),
      bslib::value_box(title = tr("h da validação cruzada", "Cross-validated h"), value = shiny::textOutput(ns("h_cv")), shiny::textOutput(ns("h_cv_desc")), theme = "info"),
      bslib::value_box(title = tr("ISE da KDE nesta amostra", "KDE ISE in this sample"), value = shiny::textOutput(ns("ise_kde")), shiny::textOutput(ns("ise_kde_desc")), theme = NULL)
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico_estimativa"), height = "420px"))),
      bslib::card(full_screen = TRUE, bslib::card_body(shiny::plotOutput(ns("grafico_risco"), height = "420px")))
    ),
    bslib::card(bslib::card_header(rotulo_entenda()), bslib::card_body(texto_estimador()))
  )
}

mod_estimador_server <- function(id, modo = shiny::reactive("light")) {
  shiny::moduleServer(id, function(input, output, session) {

    modelo <- shiny::reactive(modelos_estimador[[input$modelo]])
    ancora <- shiny::reactive(modelo()$suporte[1])

    output$controle_h <- shiny::renderUI({
      hs <- grade_h(modelo())
      shiny::sliderInput(
        session$ns("h"), tr("Largura das classes (h)", "Bin width (h)"),
        min = signif(min(hs), 2), max = signif(max(hs), 2),
        value = signif(stats::median(hs), 2), step = signif(min(hs) / 5, 1)
      )
    })

    amostra <- shiny::reactive({
      input$sortear
      modelo()$gerar(input$n)
    })

    # Curva exata de risco: não depende da amostra, só do modelo e de n
    curva_mise <- shiny::reactive({
      m <- modelo()
      hs <- grade_h(m)
      riscos <- purrr::map(hs, \(h) mise_histograma(m, input$n, h))
      data.frame(
        h = hs,
        vies2 = purrr::map_dbl(riscos, "vies2"),
        variancia = purrr::map_dbl(riscos, "variancia"),
        mise = purrr::map_dbl(riscos, "mise")
      )
    })

    integral_f2 <- shiny::reactive({
      xs <- seq(modelo()$suporte[1], modelo()$suporte[2], length.out = 4000)
      integrar(xs, modelo()$f(xs)^2)
    })

    curva_cv <- shiny::reactive({
      hs <- grade_h(modelo())
      x <- amostra()
      data.frame(h = hs, risco = purrr::map_dbl(hs, \(h) cv_histograma(x, h, ancora())) + integral_f2())
    })

    h_otimo <- shiny::reactive({
      cm <- curva_mise()
      stats::optimize(\(h) mise_histograma(modelo(), input$n, h)[["mise"]], range(cm$h))$minimum
    })
    h_assintotico <- shiny::reactive((6 / integral_derivada2(modelo()))^(1 / 3) * input$n^(-1 / 3))
    h_cv <- shiny::reactive({
      cv <- curva_cv()
      cv$h[which.min(cv$risco)]
    })

    limites <- shiny::reactive(range(c(modelo()$suporte, amostra())))
    estimativa <- shiny::reactive({
      shiny::req(input$h)
      estimar_histograma(amostra(), input$h, ancora())
    })
    # Banda da KDE: regra de Silverman do livro ou validação cruzada não
    # viesada (stats::bw.ucv), que minimiza uma estimativa do MISE
    banda_kde <- shiny::reactive({
      x <- amostra()
      if (identical(input$banda, "cv")) {
        suppressWarnings(stats::bw.ucv(x))
      } else {
        1.06 * stats::sd(x) * length(x)^(-1 / 5)
      }
    })
    nome_kde <- shiny::reactive(if (identical(input$banda, "cv")) tr("KDE (validação cruzada)", "KDE (cross-validation)") else "KDE (Silverman)")
    kde <- shiny::reactive({
      d <- stats::density(amostra(), bw = banda_kde(), kernel = "gaussian", from = limites()[1], to = limites()[2], n = 1024)
      stats::approxfun(d$x, d$y, rule = 2)
    })

    shiny::observeEvent(input$usar_otimo, shiny::updateSliderInput(session, "h", value = h_otimo()))
    shiny::observeEvent(input$usar_cv, shiny::updateSliderInput(session, "h", value = h_cv()))

    output$ise_hist <- shiny::renderText(fmt_num(ise(\(x) avaliar_histograma(estimativa(), x), modelo(), limites()), 4))
    output$ise_hist_desc <- shiny::renderText({
      shiny::req(input$h)
      sprintf(tr("∫(f̂ − f)² com h = %s. MISE nesse h: %s", "∫(f̂ − f)² with h = %s. MISE at this h: %s"),
              fmt_num(input$h, 3), fmt_num(mise_histograma(modelo(), input$n, input$h)[["mise"]], 4))
    })
    output$h_otimo <- shiny::renderText(fmt_num(h_otimo(), 3))
    output$h_otimo_desc <- shiny::renderText(sprintf(
      tr("Fórmula assintótica (6/∫f′²)^(1/3) n^(−1/3) = %s", "Asymptotic formula (6/∫f′²)^(1/3) n^(−1/3) = %s"),
      fmt_num(h_assintotico(), 3)
    ))
    output$h_cv <- shiny::renderText(fmt_num(h_cv(), 3))
    output$h_cv_desc <- shiny::renderText(sprintf(
      tr("Só com os dados. Regra de Scott 3,49 s n^(−1/3) = %s", "From the data alone. Scott's rule 3.49 s n^(−1/3) = %s"),
      fmt_num(3.49 * stats::sd(amostra()) * input$n^(-1 / 3), 3)
    ))
    output$ise_kde <- shiny::renderText(fmt_num(ise(kde(), modelo(), limites()), 4))
    output$ise_kde_desc <- shiny::renderText(sprintf(
      tr("%s, banda = %s", "%s, bandwidth = %s"),
      nome_kde(), fmt_num(banda_kde(), 3)
    ))

    output$grafico_estimativa <- shiny::renderPlot({
      pal <- paleta_modo(modo())
      est <- estimativa()
      m <- modelo()
      classes <- data.frame(inicio = utils::head(est$quebras, -1), fim = utils::tail(est$quebras, -1), altura = est$altura)
      xs <- seq(limites()[1], limites()[2], length.out = 600)
      curvas <- data.frame(x = xs, y = m$f(xs), curva = tr("Densidade verdadeira f", "True density f"))
      if (isTRUE(input$kde)) {
        curvas <- rbind(curvas, data.frame(x = xs, y = kde()(xs), curva = nome_kde()))
      }
      cores <- stats::setNames(c(cores_app$referencia, pal$linha), c(tr("Densidade verdadeira f", "True density f"), nome_kde()))

      ggplot2::ggplot() +
        ggplot2::geom_rect(data = classes, ggplot2::aes(xmin = inicio, xmax = fim, ymin = 0, ymax = altura), fill = cores_app$destaque, colour = pal$fundo, alpha = 0.6) +
        ggplot2::geom_line(data = curvas, ggplot2::aes(x = x, y = y, colour = curva, linetype = curva), linewidth = 1.1) +
        ggplot2::scale_colour_manual(values = cores, name = NULL) +
        ggplot2::scale_linetype_manual(values = stats::setNames(c("solid", "dashed"), names(cores)), name = NULL) +
        ggplot2::scale_x_continuous(labels = rotulos_eixo()) +
        ggplot2::scale_y_continuous(labels = rotulos_eixo()) +
        ggplot2::labs(
          title = tr("O histograma estima a densidade", "The histogram estimates the density"),
          subtitle = sprintf(tr("%s | n = %s | h = %s", "%s | n = %s | h = %s"), m$rotulo, fmt_num(input$n, 0), fmt_num(input$h, 3)),
          x = "x", y = tr("Densidade", "Density"),
          caption = tr("Barras: f̂(x) = n_j / (n h)", "Bars: f̂(x) = n_j / (n h)")
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(legend.position = "top", legend.justification = "left")
    })

    output$grafico_risco <- shiny::renderPlot({
      pal <- paleta_modo(modo())
      cm <- curva_mise()
      cv <- curva_cv()
      nomes <- tr(
        c("MISE = viés² + variância", "Viés² integrado", "Variância integrada", "Validação cruzada nesta amostra"),
        c("MISE = bias² + variance", "Integrated bias²", "Integrated variance", "Cross-validation, this sample")
      )
      curvas <- rbind(
        data.frame(h = cm$h, risco = cm$mise, curva = nomes[1]),
        data.frame(h = cm$h, risco = cm$vies2, curva = nomes[2]),
        data.frame(h = cm$h, risco = cm$variancia, curva = nomes[3]),
        data.frame(h = cv$h, risco = pmax(cv$risco, 1e-6), curva = nomes[4])
      )
      curvas$curva <- factor(curvas$curva, levels = nomes)
      teto <- max(cm$mise[cm$h >= h_otimo() / 4]) * 1.6
      piso <- min(cm$mise) / 50

      ggplot2::ggplot(curvas, ggplot2::aes(x = h, y = risco, colour = curva, linetype = curva)) +
        ggplot2::geom_vline(xintercept = input$h, colour = pal$secundario, linewidth = 0.8) +
        ggplot2::geom_vline(xintercept = h_otimo(), colour = cores_app$referencia, linetype = "dotted", linewidth = 0.9) +
        ggplot2::geom_line(linewidth = 1.1) +
        ggplot2::scale_colour_manual(values = c(cores_app$referencia, "#6f42c1", "#20c997", cores_app$destaque), name = NULL) +
        ggplot2::scale_linetype_manual(values = c("solid", "dashed", "dashed", "solid"), name = NULL) +
        ggplot2::scale_x_log10(labels = rotulos_eixo()) +
        ggplot2::scale_y_log10(labels = rotulos_eixo()) +
        ggplot2::coord_cartesian(ylim = c(piso, teto)) +
        ggplot2::guides(colour = ggplot2::guide_legend(ncol = 2)) +
        ggplot2::labs(
          title = tr("O risco em função de h", "Risk as a function of h"),
          subtitle = sprintf(tr("Curvas exatas para n = %s; escalas logarítmicas", "Exact curves for n = %s; log scales"), fmt_num(input$n, 0)),
          x = tr("Largura das classes h", "Bin width h"), y = tr("Erro quadrático integrado", "Integrated squared error"),
          caption = tr(
            "Vertical cinza: h escolhido | Pontilhada vermelha: h ótimo\nValidação cruzada somada a ∫f² para ficar na escala do MISE",
            "Grey vertical: chosen h | Red dotted: optimal h\nCross-validation plus ∫f², to sit on the MISE scale"
          )
        ) +
        tema_grafico(modo()) +
        ggplot2::theme(legend.position = "top", legend.justification = "left")
    })
  })
}
