package com.tddplayground.urlshortener.controller;

import com.tddplayground.urlshortener.dto.CreateShortUrlRequest;
import com.tddplayground.urlshortener.model.UrlMapping;
import com.tddplayground.urlshortener.repository.UrlRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.server.LocalServerPort;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;

import io.restassured.RestAssured;
import io.restassured.http.ContentType;

import static io.restassured.RestAssured.given;
import static org.hamcrest.Matchers.*;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
class UrlControllerIntegrationTest {

    @LocalServerPort
    private int port;

    @Autowired
    private UrlRepository urlRepository;

    @BeforeEach
    void setUp() {
        RestAssured.port = port;
        urlRepository.deleteAll();
    }

    @Test
    void shouldCreateShortUrlSuccessfully() {
        CreateShortUrlRequest request = new CreateShortUrlRequest("https://www.example.com/very/long/url");

        given()
            .contentType(ContentType.JSON)
            .body(request)
        .when()
            .post("/api/urls")
        .then()
            .statusCode(HttpStatus.CREATED.value())
            .body("shortCode", notNullValue())
            .body("shortCode", hasLength(6))
            .body("shortUrl", containsString("/"))
            .body("longUrl", equalTo("https://www.example.com/very/long/url"));
    }

    @Test
    void shouldReturnBadRequestForInvalidUrl() {
        CreateShortUrlRequest request = new CreateShortUrlRequest("not-a-valid-url");

        given()
            .contentType(ContentType.JSON)
            .body(request)
        .when()
            .post("/api/urls")
        .then()
            .statusCode(HttpStatus.BAD_REQUEST.value());
    }

    @Test
    void shouldReturnBadRequestForBlankUrl() {
        CreateShortUrlRequest request = new CreateShortUrlRequest("");

        given()
            .contentType(ContentType.JSON)
            .body(request)
        .when()
            .post("/api/urls")
        .then()
            .statusCode(HttpStatus.BAD_REQUEST.value());
    }

    @Test
    void shouldRedirectToOriginalUrl() {
        // Given - create a URL mapping
        UrlMapping urlMapping = new UrlMapping("redir1", "https://www.google.com");
        urlRepository.save(urlMapping);

        // When & Then
        given()
            .redirects().follow(false)
        .when()
            .get("/redir1")
        .then()
            .statusCode(HttpStatus.FOUND.value())
            .header("Location", equalTo("https://www.google.com"));
    }

    @Test
    void shouldReturnNotFoundForNonExistentShortCode() {
        given()
            .redirects().follow(false)
        .when()
            .get("/notexist")
        .then()
            .statusCode(HttpStatus.NOT_FOUND.value())
            .body("error", equalTo("Not Found"))
            .body("message", containsString("notexist"));
    }

    @Test
    void shouldGetUrlInfoSuccessfully() {
        // Given - create a URL mapping
        UrlMapping urlMapping = new UrlMapping("info99", "https://info.example.com");
        urlMapping.setAccessCount(10);
        urlRepository.save(urlMapping);

        // When & Then
        given()
            .contentType(ContentType.JSON)
        .when()
            .get("/api/urls/info99")
        .then()
            .statusCode(HttpStatus.OK.value())
            .body("shortCode", equalTo("info99"))
            .body("longUrl", equalTo("https://info.example.com"))
            .body("accessCount", equalTo(10))
            .body("createdAt", notNullValue());
    }

    @Test
    void shouldReturnNotFoundForNonExistentUrlInfo() {
        given()
            .contentType(ContentType.JSON)
        .when()
            .get("/api/urls/noinfo")
        .then()
            .statusCode(HttpStatus.NOT_FOUND.value())
            .body("error", equalTo("Not Found"));
    }

    @Test
    void shouldIncrementAccessCountOnRedirect() {
        // Given
        UrlMapping urlMapping = new UrlMapping("count1", "https://counter.com");
        urlRepository.save(urlMapping);

        // When - access the short URL
        given()
            .redirects().follow(false)
        .when()
            .get("/count1")
        .then()
            .statusCode(HttpStatus.FOUND.value());

        // Then - check access count
        given()
            .contentType(ContentType.JSON)
        .when()
            .get("/api/urls/count1")
        .then()
            .statusCode(HttpStatus.OK.value())
            .body("accessCount", equalTo(1));
    }

    @Test
    void shouldHandleMultipleRedirectsWithCorrectCount() {
        // Given
        UrlMapping urlMapping = new UrlMapping("multi1", "https://multiple.com");
        urlRepository.save(urlMapping);

        // When - access multiple times
        for (int i = 0; i < 3; i++) {
            given()
                .redirects().follow(false)
            .when()
                .get("/multi1")
            .then()
                .statusCode(HttpStatus.FOUND.value());
        }

        // Then
        given()
            .contentType(ContentType.JSON)
        .when()
            .get("/api/urls/multi1")
        .then()
            .statusCode(HttpStatus.OK.value())
            .body("accessCount", equalTo(3));
    }
}
