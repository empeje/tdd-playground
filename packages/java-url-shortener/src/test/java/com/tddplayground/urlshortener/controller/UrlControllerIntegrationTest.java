package com.tddplayground.urlshortener.controller;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;
import com.tddplayground.urlshortener.UrlShortenerApplication;
import com.tddplayground.urlshortener.dto.CreateShortUrlRequest;
import com.tddplayground.urlshortener.dto.ShortUrlResponse;
import com.tddplayground.urlshortener.dto.UrlInfoResponse;
import org.junit.jupiter.api.*;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

import static org.junit.jupiter.api.Assertions.*;

class UrlControllerIntegrationTest {

    private static UrlShortenerApplication app;
    private static int port;
    private static String baseUrl;
    private HttpClient client;
    private Gson gson;

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

    @BeforeAll
    static void startServer() throws IOException {
        port = 8081; // Use different port for tests
        app = new UrlShortenerApplication(port);
        app.start();
        baseUrl = "http://localhost:" + port;
        
        // Give server time to start
        try {
            Thread.sleep(500);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }

    @AfterAll
    static void stopServer() {
        if (app != null) {
            app.stop();
        }
    }

    @BeforeEach
    void setUp() {
        client = HttpClient.newHttpClient();
        gson = new GsonBuilder()
                .registerTypeAdapter(LocalDateTime.class, new LocalDateTimeAdapter())
                .create();
    }

    @Test
    void shouldCreateShortUrlSuccessfully() throws Exception {
        // Given
        CreateShortUrlRequest request = new CreateShortUrlRequest("https://www.example.com/very/long/url");
        String requestBody = gson.toJson(request);

        HttpRequest httpRequest = HttpRequest.newBuilder()
                .uri(URI.create(baseUrl + "/api/urls"))
                .header("Content-Type", "application/json")
                .POST(HttpRequest.BodyPublishers.ofString(requestBody))
                .build();

        // When
        HttpResponse<String> response = client.send(httpRequest, HttpResponse.BodyHandlers.ofString());

        // Then
        assertEquals(201, response.statusCode());
        ShortUrlResponse shortUrlResponse = gson.fromJson(response.body(), ShortUrlResponse.class);
        assertNotNull(shortUrlResponse.getShortCode());
        assertEquals(6, shortUrlResponse.getShortCode().length());
        assertTrue(shortUrlResponse.getShortUrl().contains(shortUrlResponse.getShortCode()));
        assertEquals("https://www.example.com/very/long/url", shortUrlResponse.getLongUrl());
    }

    @Test
    void shouldReturnBadRequestForInvalidUrl() throws Exception {
        // Given
        CreateShortUrlRequest request = new CreateShortUrlRequest("not-a-valid-url");
        String requestBody = gson.toJson(request);

        HttpRequest httpRequest = HttpRequest.newBuilder()
                .uri(URI.create(baseUrl + "/api/urls"))
                .header("Content-Type", "application/json")
                .POST(HttpRequest.BodyPublishers.ofString(requestBody))
                .build();

        // When
        HttpResponse<String> response = client.send(httpRequest, HttpResponse.BodyHandlers.ofString());

        // Then
        assertEquals(400, response.statusCode());
        assertTrue(response.body().contains("error"));
    }

    @Test
    void shouldReturnBadRequestForBlankUrl() throws Exception {
        // Given
        CreateShortUrlRequest request = new CreateShortUrlRequest("");
        String requestBody = gson.toJson(request);

        HttpRequest httpRequest = HttpRequest.newBuilder()
                .uri(URI.create(baseUrl + "/api/urls"))
                .header("Content-Type", "application/json")
                .POST(HttpRequest.BodyPublishers.ofString(requestBody))
                .build();

        // When
        HttpResponse<String> response = client.send(httpRequest, HttpResponse.BodyHandlers.ofString());

        // Then
        assertEquals(400, response.statusCode());
    }

    @Test
    void shouldRedirectToOriginalUrl() throws Exception {
        // Given - create a URL mapping
        CreateShortUrlRequest createRequest = new CreateShortUrlRequest("https://www.google.com");
        String requestBody = gson.toJson(createRequest);

        HttpRequest createHttpRequest = HttpRequest.newBuilder()
                .uri(URI.create(baseUrl + "/api/urls"))
                .header("Content-Type", "application/json")
                .POST(HttpRequest.BodyPublishers.ofString(requestBody))
                .build();

        HttpResponse<String> createResponse = client.send(createHttpRequest, HttpResponse.BodyHandlers.ofString());
        ShortUrlResponse shortUrlResponse = gson.fromJson(createResponse.body(), ShortUrlResponse.class);

        // When - access the short URL (don't follow redirects)
        HttpClient noRedirectClient = HttpClient.newBuilder()
                .followRedirects(HttpClient.Redirect.NEVER)
                .build();

        HttpRequest redirectRequest = HttpRequest.newBuilder()
                .uri(URI.create(baseUrl + "/" + shortUrlResponse.getShortCode()))
                .GET()
                .build();

        HttpResponse<String> redirectResponse = noRedirectClient.send(redirectRequest, HttpResponse.BodyHandlers.ofString());

        // Then
        assertEquals(302, redirectResponse.statusCode());
        assertTrue(redirectResponse.headers().firstValue("Location").isPresent());
        assertEquals("https://www.google.com", redirectResponse.headers().firstValue("Location").get());
    }

    @Test
    void shouldReturnNotFoundForNonExistentShortCode() throws Exception {
        // Given
        HttpClient noRedirectClient = HttpClient.newBuilder()
                .followRedirects(HttpClient.Redirect.NEVER)
                .build();

        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create(baseUrl + "/notexist"))
                .GET()
                .build();

        // When
        HttpResponse<String> response = noRedirectClient.send(request, HttpResponse.BodyHandlers.ofString());

        // Then
        assertEquals(404, response.statusCode());
        assertTrue(response.body().contains("Not Found"));
    }

    @Test
    void shouldGetUrlInfoSuccessfully() throws Exception {
        // Given - create a URL mapping
        CreateShortUrlRequest createRequest = new CreateShortUrlRequest("https://info.example.com");
        String requestBody = gson.toJson(createRequest);

        HttpRequest createHttpRequest = HttpRequest.newBuilder()
                .uri(URI.create(baseUrl + "/api/urls"))
                .header("Content-Type", "application/json")
                .POST(HttpRequest.BodyPublishers.ofString(requestBody))
                .build();

        HttpResponse<String> createResponse = client.send(createHttpRequest, HttpResponse.BodyHandlers.ofString());
        ShortUrlResponse shortUrlResponse = gson.fromJson(createResponse.body(), ShortUrlResponse.class);

        // When - get URL info
        HttpRequest infoRequest = HttpRequest.newBuilder()
                .uri(URI.create(baseUrl + "/api/urls/" + shortUrlResponse.getShortCode()))
                .GET()
                .build();

        HttpResponse<String> infoResponse = client.send(infoRequest, HttpResponse.BodyHandlers.ofString());

        // Then
        assertEquals(200, infoResponse.statusCode());
        UrlInfoResponse urlInfoResponse = gson.fromJson(infoResponse.body(), UrlInfoResponse.class);
        assertEquals(shortUrlResponse.getShortCode(), urlInfoResponse.getShortCode());
        assertEquals("https://info.example.com", urlInfoResponse.getLongUrl());
        assertEquals(0, urlInfoResponse.getAccessCount());
        assertNotNull(urlInfoResponse.getCreatedAt());
    }

    @Test
    void shouldReturnNotFoundForNonExistentUrlInfo() throws Exception {
        // Given
        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create(baseUrl + "/api/urls/noinfo"))
                .GET()
                .build();

        // When
        HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());

        // Then
        assertEquals(404, response.statusCode());
        assertTrue(response.body().contains("Not Found"));
    }

    @Test
    void shouldIncrementAccessCountOnRedirect() throws Exception {
        // Given - create a URL mapping
        CreateShortUrlRequest createRequest = new CreateShortUrlRequest("https://counter.com");
        String requestBody = gson.toJson(createRequest);

        HttpRequest createHttpRequest = HttpRequest.newBuilder()
                .uri(URI.create(baseUrl + "/api/urls"))
                .header("Content-Type", "application/json")
                .POST(HttpRequest.BodyPublishers.ofString(requestBody))
                .build();

        HttpResponse<String> createResponse = client.send(createHttpRequest, HttpResponse.BodyHandlers.ofString());
        ShortUrlResponse shortUrlResponse = gson.fromJson(createResponse.body(), ShortUrlResponse.class);

        // When - access the short URL
        HttpClient noRedirectClient = HttpClient.newBuilder()
                .followRedirects(HttpClient.Redirect.NEVER)
                .build();

        HttpRequest redirectRequest = HttpRequest.newBuilder()
                .uri(URI.create(baseUrl + "/" + shortUrlResponse.getShortCode()))
                .GET()
                .build();

        noRedirectClient.send(redirectRequest, HttpResponse.BodyHandlers.ofString());

        // Then - check access count
        HttpRequest infoRequest = HttpRequest.newBuilder()
                .uri(URI.create(baseUrl + "/api/urls/" + shortUrlResponse.getShortCode()))
                .GET()
                .build();

        HttpResponse<String> infoResponse = client.send(infoRequest, HttpResponse.BodyHandlers.ofString());
        UrlInfoResponse urlInfoResponse = gson.fromJson(infoResponse.body(), UrlInfoResponse.class);
        assertEquals(1, urlInfoResponse.getAccessCount());
    }
}
