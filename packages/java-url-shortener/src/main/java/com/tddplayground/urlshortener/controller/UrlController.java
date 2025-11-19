package com.tddplayground.urlshortener.controller;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonSyntaxException;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;
import com.tddplayground.urlshortener.dto.CreateShortUrlRequest;
import com.tddplayground.urlshortener.dto.ShortUrlResponse;
import com.tddplayground.urlshortener.dto.UrlInfoResponse;
import com.tddplayground.urlshortener.exception.UrlNotFoundException;
import com.tddplayground.urlshortener.model.UrlMapping;
import com.tddplayground.urlshortener.service.UrlShortenerService;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.Map;

public class UrlController {

    private final UrlShortenerService urlShortenerService;
    private final Gson gson;

    public UrlController(UrlShortenerService urlShortenerService) {
        this.urlShortenerService = urlShortenerService;
        this.gson = new GsonBuilder()
                .registerTypeAdapter(LocalDateTime.class, new LocalDateTimeAdapter())
                .create();
    }

    // LocalDateTime adapter for Gson
    private static class LocalDateTimeAdapter extends TypeAdapter<LocalDateTime> {
        private static final DateTimeFormatter formatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME;

        @Override
        public void write(JsonWriter out, LocalDateTime value) throws IOException {
            if (value == null) {
                out.nullValue();
            } else {
                out.value(value.format(formatter));
            }
        }

        @Override
        public LocalDateTime read(JsonReader in) throws IOException {
            String dateString = in.nextString();
            return LocalDateTime.parse(dateString, formatter);
        }
    }

    public HttpHandler createShortUrlHandler() {
        return exchange -> {
            if (!"POST".equals(exchange.getRequestMethod())) {
                sendMethodNotAllowed(exchange);
                return;
            }

            try {
                String body = readRequestBody(exchange);
                CreateShortUrlRequest request = gson.fromJson(body, CreateShortUrlRequest.class);

                if (request == null || !request.isValid()) {
                    sendBadRequest(exchange, "Invalid URL. URL must start with http:// or https:// and cannot exceed 2048 characters");
                    return;
                }

                UrlMapping urlMapping = urlShortenerService.createShortUrl(request.getLongUrl());
                String baseUrl = getBaseUrl(exchange);
                String shortUrl = baseUrl + "/" + urlMapping.getShortCode();

                ShortUrlResponse response = new ShortUrlResponse(
                        urlMapping.getShortCode(),
                        shortUrl,
                        urlMapping.getLongUrl()
                );

                sendJsonResponse(exchange, 201, response);
            } catch (JsonSyntaxException e) {
                sendBadRequest(exchange, "Invalid JSON format");
            } catch (Exception e) {
                sendInternalError(exchange, e.getMessage());
            }
        };
    }

    public HttpHandler redirectHandler() {
        return exchange -> {
            if (!"GET".equals(exchange.getRequestMethod())) {
                sendMethodNotAllowed(exchange);
                return;
            }

            try {
                String path = exchange.getRequestURI().getPath();
                String shortCode = path.substring(1); // Remove leading '/'

                if (shortCode.isEmpty() || shortCode.contains("/")) {
                    sendNotFound(exchange, "Invalid short code");
                    return;
                }

                String originalUrl = urlShortenerService.getOriginalUrl(shortCode);

                exchange.getResponseHeaders().set("Location", originalUrl);
                exchange.sendResponseHeaders(302, -1);
                exchange.close();
            } catch (UrlNotFoundException e) {
                sendNotFound(exchange, e.getMessage());
            } catch (Exception e) {
                sendInternalError(exchange, e.getMessage());
            }
        };
    }

    public HttpHandler getUrlInfoHandler() {
        return exchange -> {
            if (!"GET".equals(exchange.getRequestMethod())) {
                sendMethodNotAllowed(exchange);
                return;
            }

            try {
                String path = exchange.getRequestURI().getPath();
                String shortCode = path.substring("/api/urls/".length());

                if (shortCode.isEmpty()) {
                    sendBadRequest(exchange, "Short code is required");
                    return;
                }

                UrlMapping urlMapping = urlShortenerService.getUrlInfo(shortCode);

                UrlInfoResponse response = new UrlInfoResponse(
                        urlMapping.getShortCode(),
                        urlMapping.getLongUrl(),
                        urlMapping.getCreatedAt(),
                        urlMapping.getAccessCount()
                );

                sendJsonResponse(exchange, 200, response);
            } catch (UrlNotFoundException e) {
                sendNotFound(exchange, e.getMessage());
            } catch (Exception e) {
                sendInternalError(exchange, e.getMessage());
            }
        };
    }

    private String readRequestBody(HttpExchange exchange) throws IOException {
        InputStream inputStream = exchange.getRequestBody();
        return new String(inputStream.readAllBytes(), StandardCharsets.UTF_8);
    }

    private String getBaseUrl(HttpExchange exchange) {
        String protocol = exchange.getProtocol().toLowerCase().contains("https") ? "https" : "http";
        String host = exchange.getRequestHeaders().getFirst("Host");
        if (host == null) {
            host = exchange.getLocalAddress().getHostString() + ":" + exchange.getLocalAddress().getPort();
        }
        return protocol + "://" + host;
    }

    private void sendJsonResponse(HttpExchange exchange, int statusCode, Object response) throws IOException {
        String json = gson.toJson(response);
        byte[] bytes = json.getBytes(StandardCharsets.UTF_8);
        
        exchange.getResponseHeaders().set("Content-Type", "application/json");
        exchange.sendResponseHeaders(statusCode, bytes.length);
        
        OutputStream os = exchange.getResponseBody();
        os.write(bytes);
        os.close();
    }

    private void sendBadRequest(HttpExchange exchange, String message) throws IOException {
        Map<String, String> error = new HashMap<>();
        error.put("error", "Bad Request");
        error.put("message", message);
        sendJsonResponse(exchange, 400, error);
    }

    private void sendNotFound(HttpExchange exchange, String message) throws IOException {
        Map<String, String> error = new HashMap<>();
        error.put("error", "Not Found");
        error.put("message", message);
        sendJsonResponse(exchange, 404, error);
    }

    private void sendMethodNotAllowed(HttpExchange exchange) throws IOException {
        Map<String, String> error = new HashMap<>();
        error.put("error", "Method Not Allowed");
        error.put("message", "HTTP method not supported for this endpoint");
        sendJsonResponse(exchange, 405, error);
    }

    private void sendInternalError(HttpExchange exchange, String message) throws IOException {
        Map<String, String> error = new HashMap<>();
        error.put("error", "Internal Server Error");
        error.put("message", message);
        sendJsonResponse(exchange, 500, error);
    }
}
