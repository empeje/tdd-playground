package org.example.urlshortener;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;

import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;

public class ApiServerExampleForward {
    public static void main(String[] args) throws IOException {
        HttpServer server = HttpServer.create(new InetSocketAddress(8080), 0);
        server.createContext("/api", new PermanentRedirectHandler("https://www.example.com/new-location"));
        server.setExecutor(null);
        server.start();
        System.out.println("Listening on port 8080...");
    }

    public static class PermanentRedirectHandler implements HttpHandler {
        private final String destinationUrl;

        public PermanentRedirectHandler(String destinationUrl) {
            this.destinationUrl = destinationUrl;
        }

        public String getDestinationUrl() {
            return destinationUrl;
        }

        @Override
        public void handle(HttpExchange exchange) throws IOException {
            if ("GET".equalsIgnoreCase(exchange.getRequestMethod())) {
                exchange.getResponseHeaders().set("Location", destinationUrl);
                exchange.sendResponseHeaders(301, -1);
                exchange.close();
            } else {
                String response = "Method Not Allowed";
                exchange.sendResponseHeaders(405, response.length());
                OutputStream os = exchange.getResponseBody();
                os.write(response.getBytes());
                os.close();
            }
        }
    }
}
