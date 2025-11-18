package com.tddplayground.urlshortener.repository;

import com.tddplayground.urlshortener.model.UrlMapping;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

@DataJpaTest
class UrlRepositoryTest {

    @Autowired
    private UrlRepository urlRepository;

    @Test
    void shouldSaveAndFindUrlMapping() {
        // Given
        UrlMapping urlMapping = new UrlMapping("abc123", "https://example.com");

        // When
        UrlMapping saved = urlRepository.save(urlMapping);

        // Then
        assertThat(saved.getId()).isNotNull();
        assertThat(saved.getShortCode()).isEqualTo("abc123");
        assertThat(saved.getLongUrl()).isEqualTo("https://example.com");
        assertThat(saved.getCreatedAt()).isNotNull();
        assertThat(saved.getAccessCount()).isEqualTo(0);
    }

    @Test
    void shouldFindUrlMappingByShortCode() {
        // Given
        UrlMapping urlMapping = new UrlMapping("xyz789", "https://test.com");
        urlRepository.save(urlMapping);

        // When
        Optional<UrlMapping> found = urlRepository.findByShortCode("xyz789");

        // Then
        assertThat(found).isPresent();
        assertThat(found.get().getLongUrl()).isEqualTo("https://test.com");
    }

    @Test
    void shouldReturnEmptyWhenShortCodeNotFound() {
        // When
        Optional<UrlMapping> found = urlRepository.findByShortCode("notfound");

        // Then
        assertThat(found).isEmpty();
    }

    @Test
    void shouldReturnTrueWhenShortCodeExists() {
        // Given
        UrlMapping urlMapping = new UrlMapping("exist123", "https://exists.com");
        urlRepository.save(urlMapping);

        // When
        boolean exists = urlRepository.existsByShortCode("exist123");

        // Then
        assertThat(exists).isTrue();
    }

    @Test
    void shouldReturnFalseWhenShortCodeDoesNotExist() {
        // When
        boolean exists = urlRepository.existsByShortCode("notexist");

        // Then
        assertThat(exists).isFalse();
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
        assertThat(found).isPresent();
        assertThat(found.get().getAccessCount()).isEqualTo(2);
    }
}
