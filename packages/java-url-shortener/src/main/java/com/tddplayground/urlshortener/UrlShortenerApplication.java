package com.tddplayground.urlshortener;

import com.sun.net.httpserver.HttpServer;
import com.tddplayground.urlshortener.controller.UrlController;
import com.tddplayground.urlshortener.repository.UrlRepository;
import com.tddplayground.urlshortener.service.UrlShortenerService;

import java.io.IOException;
import java.net.InetSocketAddress;

public class UrlShortenerApplication {

    private static final int DEFAULT_PORT = 8080;
    private HttpServer server;
    private final int port;

    public UrlShortenerApplication() {
        this(DEFAULT_PORT);
    }

    public UrlShortenerApplication(int port) {
        this.port = port;
    }

    public void start() throws IOException {
        // Initialize components
        UrlRepository repository = new UrlRepository();
        UrlShortenerService service = new UrlShortenerService(repository);
        UrlController controller = new UrlController(service);

        // Create HTTP server
        server = HttpServer.create(new InetSocketAddress(port), 0);

        // Register endpoints
        server.createContext("/api/urls", exchange -> {
            String path = exchange.getRequestURI().getPath();
            if (path.equals("/api/urls")) {
                controller.createShortUrlHandler().handle(exchange);
            } else if (path.startsWith("/api/urls/")) {
                controller.getUrlInfoHandler().handle(exchange);
            } else {
                exchange.sendResponseHeaders(404, -1);
                exchange.close();
            }
        });

        // Redirect handler for short codes
        server.createContext("/", exchange -> {
            String path = exchange.getRequestURI().getPath();
            if (path.equals("/") || path.startsWith("/api")) {
                exchange.sendResponseHeaders(404, -1);
                exchange.close();
            } else {
                controller.redirectHandler().handle(exchange);
            }
        });

        // Start server
        server.setExecutor(null);
        server.start();

        System.out.println("URL Shortener started on port " + port);
        System.out.println("Access the API at: http://localhost:" + port);
    }

    public void stop() {
        if (server != null) {
            server.stop(0);
            System.out.println("URL Shortener stopped");
        }
    }

    public int getPort() {
        return port;
    }

    public static void main(String[] args) {
        try {
            int port = DEFAULT_PORT;
            if (args.length > 0) {
                try {
                    port = Integer.parseInt(args[0]);
                } catch (NumberFormatException e) {
                    System.err.println("Invalid port number. Using default port: " + DEFAULT_PORT);
                }
            }

            UrlShortenerApplication app = new UrlShortenerApplication(port);
            app.start();

            // Add shutdown hook
            Runtime.getRuntime().addShutdownHook(new Thread(app::stop));

        } catch (IOException e) {
            System.err.println("Failed to start server: " + e.getMessage());
            System.exit(1);
        }
    }
}
