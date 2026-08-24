package org.example.urlshortener;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;

public class ApiServerExample {
    public static void main(String[] args) throws IOException {
        HttpServer server = HttpServer.create(new InetSocketAddress(8080), 0);
        server.createContext("/api", new MyHandler());
        server.setExecutor(null);
        server.start();
        System.out.println("Listening on port 8080...");
    }

    public static class MyHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            String method = exchange.getRequestMethod();
            String response;
            if ("GET".equalsIgnoreCase(method)) {
                response = "Received GET request!";
                exchange.sendResponseHeaders(200, response.length());
            } else if ("POST".equalsIgnoreCase(method)) {
                InputStream is = exchange.getRequestBody();
                byte[] data = is.readAllBytes();
                String body = new String(data);
                response = "Received POST request with body: " + body;
                exchange.sendResponseHeaders(200, response.length());
            } else {
                response = "Method Not Allowed";
                exchange.sendResponseHeaders(405, response.length());
            }
            OutputStream os = exchange.getResponseBody();
            os.write(response.getBytes());
            os.close();
        }
    }
}
