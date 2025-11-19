package com.tddplayground.urlshortener.repository;

import com.tddplayground.urlshortener.model.UrlMapping;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;

class UrlRepositoryTest {

    private UrlRepository urlRepository;

    @BeforeEach
    void setUp() {
        urlRepository = new UrlRepository();
    }

    @Test
    void shouldSaveAndFindUrlMapping() {
        // Given
        UrlMapping urlMapping = new UrlMapping("abc123", "https://example.com");

        // When
        UrlMapping saved = urlRepository.save(urlMapping);

        // Then
        assertNotNull(saved.getId());
        assertEquals("abc123", saved.getShortCode());
        assertEquals("https://example.com", saved.getLongUrl());
        assertNotNull(saved.getCreatedAt());
        assertEquals(0, saved.getAccessCount());
    }

    @Test
    void shouldFindUrlMappingByShortCode() {
        // Given
        UrlMapping urlMapping = new UrlMapping("xyz789", "https://test.com");
        urlRepository.save(urlMapping);

        // When
        Optional<UrlMapping> found = urlRepository.findByShortCode("xyz789");

        // Then
        assertTrue(found.isPresent());
        assertEquals("https://test.com", found.get().getLongUrl());
    }

    @Test
    void shouldReturnEmptyWhenShortCodeNotFound() {
        // When
        Optional<UrlMapping> found = urlRepository.findByShortCode("notfound");

        // Then
        assertFalse(found.isPresent());
    }

    @Test
    void shouldReturnTrueWhenShortCodeExists() {
        // Given
        UrlMapping urlMapping = new UrlMapping("exist123", "https://exists.com");
        urlRepository.save(urlMapping);

        // When
        boolean exists = urlRepository.existsByShortCode("exist123");

        // Then
        assertTrue(exists);
    }

    @Test
    void shouldReturnFalseWhenShortCodeDoesNotExist() {
        // When
        boolean exists = urlRepository.existsByShortCode("notexist");

        // Then
        assertFalse(exists);
    }

    @Test
    void shouldIncrementAccessCount() {
        // Given
        UrlMapping urlMapping = new UrlMapping("count123", "https://count.com");
        UrlMapping saved = urlRepository.save(urlMapping);

        // When
        saved.incrementAccessCount();
        saved.incrementAccessCount();
        urlRepository.save(saved);

        // Then
        Optional<UrlMapping> found = urlRepository.findByShortCode("count123");
        assertTrue(found.isPresent());
        assertEquals(2, found.get().getAccessCount());
    }

    @Test
    void shouldGenerateUniqueIds() {
        // Given
        UrlMapping mapping1 = new UrlMapping("code1", "https://url1.com");
        UrlMapping mapping2 = new UrlMapping("code2", "https://url2.com");

        // When
        UrlMapping saved1 = urlRepository.save(mapping1);
        UrlMapping saved2 = urlRepository.save(mapping2);

        // Then
        assertNotNull(saved1.getId());
        assertNotNull(saved2.getId());
        assertNotEquals(saved1.getId(), saved2.getId());
    }

    @Test
    void shouldCountMappings() {
        // Given
        urlRepository.save(new UrlMapping("code1", "https://url1.com"));
        urlRepository.save(new UrlMapping("code2", "https://url2.com"));

        // When & Then
        assertEquals(2, urlRepository.count());
    }

    @Test
    void shouldDeleteAll() {
        // Given
        urlRepository.save(new UrlMapping("code1", "https://url1.com"));
        urlRepository.save(new UrlMapping("code2", "https://url2.com"));

        // When
        urlRepository.deleteAll();

        // Then
        assertEquals(0, urlRepository.count());
    }
}
