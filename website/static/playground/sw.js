// Makes the playground cross-origin isolated. GitHub Pages cannot send the
// headers that do it, so this service worker adds them to the playground's
// responses. Isolation is what lets a page share memory with its workers
// (SharedArrayBuffer), which the playground needs to pause a program while it
// waits for typed input, to run `diverge` tasks on threads, and to run the
// language server. Its scope is /playground/ only; the rest of the site is
// unaffected. Nothing is cached: every request still goes to the network.

self.addEventListener("install", () => self.skipWaiting());
self.addEventListener("activate", (event) => event.waitUntil(self.clients.claim()));

self.addEventListener("fetch", (event) => {
  const request = event.request;
  // A Chrome quirk: these cannot be fetched from a service worker.
  if (request.cache === "only-if-cached" && request.mode !== "same-origin") return;
  if (new URL(request.url).origin !== self.location.origin) return;
  event.respondWith(
    fetch(request).then((response) => {
      if (response.status === 0 || response.type === "opaqueredirect") return response;
      const headers = new Headers(response.headers);
      headers.set("Cross-Origin-Embedder-Policy", "require-corp");
      headers.set("Cross-Origin-Opener-Policy", "same-origin");
      return new Response(response.body, { status: response.status, statusText: response.statusText, headers });
    }),
  );
});
