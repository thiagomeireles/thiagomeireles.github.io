// Desativa o service worker do Shinylive, usado pelos apps até 2026-11-19.
// Autor: Thiago Meireles | Atualizado: 2026-11-19
// O navegador de quem já abriu os apps antigos busca este arquivo para atualizar o
// service worker; esta versão apaga o cache do webR (~80 MB) e se desregistra.
// Pode ser removido do site depois de alguns meses.
self.addEventListener("install", () => self.skipWaiting());

self.addEventListener("activate", (event) => {
  event.waitUntil((async () => {
    const chaves = await caches.keys();
    await Promise.all(chaves.map((chave) => caches.delete(chave)));
    await self.registration.unregister();
  })());
});
