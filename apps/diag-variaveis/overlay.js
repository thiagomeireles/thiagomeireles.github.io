// =============================================================================
// Script:  overlay.js (diagnóstico do app real no iPhone; temporário)
// Autor:  Thiago Meireles
// Atualizado: 2026-11-19
//
// Entrada:
//   - carregado no <head> de apps/diag-variaveis/index.html, antes do Shinylive
// Saída:
//   - painel fixo na parte de baixo da tela com o registro, em ordem, de:
//     mensagens página <-> service worker, iframe do app (src, carga, texto),
//     estado do controller a cada 2 s, console e erros
// Remover junto com apps/diag-variaveis/ quando o 404 do iOS estiver resolvido.
// =============================================================================
(function () {
  var t0 = performance.now();
  var linhas = [];
  var painel = null;

  function agora() { return ((performance.now() - t0) / 1000).toFixed(1).padStart(5, " "); }

  function texto(valor) {
    try {
      if (valor instanceof Error) return valor.message;
      if (typeof valor === "object") return JSON.stringify(valor).slice(0, 160);
      return String(valor);
    } catch (e) { return String(valor); }
  }

  function registrar(tipo, mensagem) {
    var linha = agora() + "s " + tipo + ": " + String(mensagem).replace(/\s+/g, " ").slice(0, 220);
    linhas.push(linha);
    if (painel) {
      var div = document.createElement("div");
      div.textContent = linha;
      if (/FALHA|erro|404|serviceworkerStart|sem controller/i.test(linha)) div.style.color = "#c00";
      painel.appendChild(div);
      painel.scrollTop = painel.scrollHeight;
    }
  }

  function criarPainel() {
    painel = document.createElement("div");
    painel.id = "diag-painel";
    painel.setAttribute("style", [
      "position:fixed", "left:0", "right:0", "bottom:0", "max-height:45vh", "overflow:auto",
      "background:rgba(255,255,255,0.96)", "border-top:2px solid #3459e6", "z-index:2147483647",
      "font:11px/1.35 ui-monospace,Menlo,monospace", "color:#111", "padding:6px 8px",
      "white-space:pre-wrap", "word-break:break-all"
    ].join(";"));
    linhas.forEach(function (l) {
      var div = document.createElement("div");
      div.textContent = l;
      painel.appendChild(div);
    });
    document.body.appendChild(painel);
  }
  if (document.body) criarPainel(); else document.addEventListener("DOMContentLoaded", criarPainel);

  registrar("início", navigator.userAgent.replace(/.*OS ([\d_]+).*Version\/([\d.]+).*/, "iOS-UA $1 | Safari $2"));

  ["log", "warn", "error"].forEach(function (metodo) {
    var original = console[metodo];
    console[metodo] = function () {
      var msg = Array.prototype.map.call(arguments, texto).join(" ");
      if (!/^preload echo:/.test(msg)) registrar("console." + metodo, msg);
      return original.apply(console, arguments);
    };
  });
  window.addEventListener("error", function (e) { registrar("erro", e.message + " @ " + (e.filename || "").split("/").pop() + ":" + e.lineno); });
  window.addEventListener("unhandledrejection", function (e) { registrar("rejeição", texto(e.reason)); });

  if (!navigator.serviceWorker) {
    registrar("FALHA", "navigator.serviceWorker indisponível");
    return;
  }

  // Mensagens da página para o service worker (configureProxyPath etc.)
  var postOriginal = ServiceWorker.prototype.postMessage;
  ServiceWorker.prototype.postMessage = function (mensagem) {
    registrar("página→SW", (mensagem && mensagem.type) + (mensagem && mensagem.path ? " " + mensagem.path : "") + " (SW " + this.state + ")");
    return postOriginal.apply(this, arguments);
  };

  // Mensagens do service worker para a página (serviceworkerStart = SW foi reiniciado)
  navigator.serviceWorker.addEventListener("message", function (e) {
    registrar("SW→página", e.data && e.data.type);
  });
  navigator.serviceWorker.addEventListener("controllerchange", function () {
    registrar("controllerchange", navigator.serviceWorker.controller && navigator.serviceWorker.controller.state);
  });

  // iframe do app: quando o src muda e o que carrega nele
  function vigiarIframe(quadro) {
    if (quadro.__diag) return;
    quadro.__diag = true;
    registrar("iframe", "criado, src=" + (quadro.getAttribute("src") || "(vazio)"));
    new MutationObserver(function () {
      registrar("iframe", "src → " + quadro.getAttribute("src") + " | controller " + (navigator.serviceWorker.controller ? navigator.serviceWorker.controller.state : "nenhum"));
    }).observe(quadro, { attributes: true, attributeFilter: ["src"] });
    quadro.addEventListener("load", function () {
      try {
        var doc = quadro.contentDocument;
        var corpo = doc && doc.body ? doc.body.innerText : "";
        var falhou = /File not found|404/.test(corpo) || /404/.test(doc.title || "");
        registrar(falhou ? "FALHA iframe" : "iframe carregou", (doc ? doc.location.pathname : "?") + " | título: " + (doc && doc.title) + " | " + corpo.slice(0, 90));
      } catch (e) {
        registrar("iframe carregou", "sem acesso: " + e.message);
      }
    });
  }
  new MutationObserver(function () {
    document.querySelectorAll("iframe").forEach(vigiarIframe);
  }).observe(document.documentElement, { childList: true, subtree: true });

  // Estado do controller a cada 2 s (por 3 minutos)
  var ultimoEstado = "";
  var rodadas = 0;
  var timer = setInterval(function () {
    rodadas++;
    var c = navigator.serviceWorker.controller;
    navigator.serviceWorker.getRegistration().then(function (r) {
      var estado = "controller " + (c ? c.state : "nenhum") + " | registro " + (r ? r.scope.replace(location.origin, "") + " active=" + (r.active && r.active.state) + (r.waiting ? " waiting" : "") + (r.installing ? " installing" : "") : "nenhum");
      if (estado !== ultimoEstado) { registrar("estado", estado); ultimoEstado = estado; }
    });
    if (rodadas >= 90) clearInterval(timer);
  }, 2000);
})();
