package org.example.urlshortener;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.InetSocketAddress;
import java.net.URI;
import java.net.URL;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Level;
import java.util.logging.Logger;

public class SimpleProxy {
    private static final Logger LOG = Logger.getLogger(SimpleProxy.class.getName());

    public static HttpServer createProxyServer(int port, List<String> backends) throws IOException {
        HttpServer server = HttpServer.create(new InetSocketAddress(port), 0);
        server.createContext("/", new ProxyHandler(backends));
        server.setExecutor(null);
        return server;
    }

    public static void main(String[] args) throws Exception {
        HttpServer server = createProxyServer(8080, List.of("https://example.com/backend1", "https://example.com/backend2"));
        server.start();
        LOG.info("Proxy server started on port 8080");
    }

    public static class ProxyHandler implements HttpHandler {
        private static final Logger HANDLER_LOG = Logger.getLogger(ProxyHandler.class.getName());
        private final List<String> backends;
        private final AtomicInteger next = new AtomicInteger(0);

        public ProxyHandler(List<String> backends) {
            if (backends == null || backends.isEmpty()) {
                throw new IllegalArgumentException("Backends list cannot be null or empty");
            }
            this.backends = List.copyOf(backends);
            HANDLER_LOG.info("ProxyHandler initialized with backends: " + backends);
        }

        public String getNextBackend() {
            int index = Math.abs(next.getAndIncrement() % backends.size());
            return backends.get(index);
        }

        @Override
        public void handle(HttpExchange exchange) {
            String backend = null;
            String targetUrl = null;
            try {
                backend = getNextBackend();
                targetUrl = backend + exchange.getRequestURI().toString();
                HANDLER_LOG.info("Proxying request " + exchange.getRequestMethod() + " " + exchange.getRequestURI() + " to " + backend);

                URL url = URI.create(targetUrl).toURL();
                HttpURLConnection conn = (HttpURLConnection) url.openConnection();
                conn.setRequestMethod(exchange.getRequestMethod());

                // Forward request headers
                exchange.getRequestHeaders().forEach((k, values) -> {
                    values.forEach(val -> conn.addRequestProperty(k, val));
                });

                // Forward request body for POST/PUT
                if ("POST".equalsIgnoreCase(exchange.getRequestMethod()) ||
                        "PUT".equalsIgnoreCase(exchange.getRequestMethod())) {
                    conn.setDoOutput(true);
                    try (InputStream reqIn = exchange.getRequestBody();
                         OutputStream out = conn.getOutputStream()) {
                        reqIn.transferTo(out);
                    }
                }

                int responseCode = conn.getResponseCode();
                exchange.sendResponseHeaders(responseCode, 0);

                InputStream responseStream = (responseCode >= 400) ? conn.getErrorStream() : conn.getInputStream();
                if (responseStream != null) {
                    try (OutputStream out = exchange.getResponseBody()) {
                        responseStream.transferTo(out);
                    }
                }
            } catch (Exception ex) {
                HANDLER_LOG.log(Level.SEVERE, "Proxy error for backend " + backend + ", url " + targetUrl, ex);
                try {
                    exchange.sendResponseHeaders(502, 0);
                    try (OutputStream out = exchange.getResponseBody()) {
                        out.write(("Proxy error: " + ex.getMessage()).getBytes());
                    }
                } catch (Exception ignored) {
                }
            } finally {
                exchange.close();
            }
        }
    }
}
