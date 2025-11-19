package com.tddplayground.urlshortener.service;

import com.tddplayground.urlshortener.exception.UrlNotFoundException;
import com.tddplayground.urlshortener.model.UrlMapping;
import com.tddplayground.urlshortener.repository.UrlRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class UrlShortenerServiceTest {

    private UrlRepository urlRepository;
    private UrlShortenerService urlShortenerService;

    @BeforeEach
    void setUp() {
        urlRepository = new UrlRepository();
        urlShortenerService = new UrlShortenerService(urlRepository);
    }

    @Test
    void shouldCreateShortUrlSuccessfully() {
        // Given
        String longUrl = "https://www.example.com/very/long/url";

        // When
        UrlMapping result = urlShortenerService.createShortUrl(longUrl);

        // Then
        assertNotNull(result);
        assertEquals(longUrl, result.getLongUrl());
        assertNotNull(result.getShortCode());
        assertEquals(6, result.getShortCode().length());
    }

    @Test
    void shouldGenerateDifferentShortCodes() {
        // Given
        String longUrl1 = "https://www.example.com/url1";
        String longUrl2 = "https://www.example.com/url2";

        // When
        UrlMapping result1 = urlShortenerService.createShortUrl(longUrl1);
        UrlMapping result2 = urlShortenerService.createShortUrl(longUrl2);

        // Then
        assertNotEquals(result1.getShortCode(), result2.getShortCode());
    }

    @Test
    void shouldGetOriginalUrlSuccessfully() {
        // Given
        String shortCode = "abc123";
        UrlMapping urlMapping = new UrlMapping(shortCode, "https://example.com");
        urlMapping.setId(1L);
        urlRepository.save(urlMapping);

        // When
        String originalUrl = urlShortenerService.getOriginalUrl(shortCode);

        // Then
        assertEquals("https://example.com", originalUrl);
    }

    @Test
    void shouldIncrementAccessCountWhenGettingOriginalUrl() {
        // Given
        String shortCode = "test123";
        UrlMapping urlMapping = new UrlMapping(shortCode, "https://test.com");
        urlMapping.setId(1L);
        urlRepository.save(urlMapping);

        // When
        urlShortenerService.getOriginalUrl(shortCode);

        // Then
        UrlMapping updated = urlRepository.findByShortCode(shortCode).orElseThrow();
        assertEquals(1, updated.getAccessCount());
    }

    @Test
    void shouldThrowExceptionWhenShortCodeNotFound() {
        // Given
        String shortCode = "notfound";

        // When & Then
        UrlNotFoundException exception = assertThrows(
                UrlNotFoundException.class,
                () -> urlShortenerService.getOriginalUrl(shortCode)
        );
        assertTrue(exception.getMessage().contains(shortCode));
    }

    @Test
    void shouldGetUrlInfoSuccessfully() {
        // Given
        String shortCode = "info123";
        UrlMapping urlMapping = new UrlMapping(shortCode, "https://info.com");
        urlMapping.setId(1L);
        urlMapping.setAccessCount(5);
        urlRepository.save(urlMapping);

        // When
        UrlMapping result = urlShortenerService.getUrlInfo(shortCode);

        // Then
        assertNotNull(result);
        assertEquals(shortCode, result.getShortCode());
        assertEquals("https://info.com", result.getLongUrl());
        assertEquals(5, result.getAccessCount());
    }

    @Test
    void shouldThrowExceptionWhenGettingInfoForNonExistentShortCode() {
        // Given
        String shortCode = "notfound";

        // When & Then
        UrlNotFoundException exception = assertThrows(
                UrlNotFoundException.class,
                () -> urlShortenerService.getUrlInfo(shortCode)
        );
        assertTrue(exception.getMessage().contains(shortCode));
    }

    @Test
    void shouldNotIncrementAccessCountWhenGettingInfo() {
        // Given
        String shortCode = "nocount";
        UrlMapping urlMapping = new UrlMapping(shortCode, "https://nocount.com");
        urlMapping.setId(1L);
        urlRepository.save(urlMapping);

        // When
        urlShortenerService.getUrlInfo(shortCode);

        // Then
        UrlMapping result = urlRepository.findByShortCode(shortCode).orElseThrow();
        assertEquals(0, result.getAccessCount());
    }
}
