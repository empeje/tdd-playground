package com.tddplayground.urlshortener.service;

import com.tddplayground.urlshortener.exception.UrlNotFoundException;
import com.tddplayground.urlshortener.model.UrlMapping;
import com.tddplayground.urlshortener.repository.UrlRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class UrlShortenerServiceTest {

    @Mock
    private UrlRepository urlRepository;

    @InjectMocks
    private UrlShortenerService urlShortenerService;

    @BeforeEach
    void setUp() {
        // Reset mocks before each test
        reset(urlRepository);
    }

    @Test
    void shouldCreateShortUrlSuccessfully() {
        // Given
        String longUrl = "https://www.example.com/very/long/url";
        when(urlRepository.existsByShortCode(anyString())).thenReturn(false);
        when(urlRepository.save(any(UrlMapping.class))).thenAnswer(invocation -> {
            UrlMapping mapping = invocation.getArgument(0);
            mapping.setId(1L);
            return mapping;
        });

        // When
        UrlMapping result = urlShortenerService.createShortUrl(longUrl);

        // Then
        assertThat(result).isNotNull();
        assertThat(result.getLongUrl()).isEqualTo(longUrl);
        assertThat(result.getShortCode()).isNotNull();
        assertThat(result.getShortCode()).hasSize(6);
        verify(urlRepository).save(any(UrlMapping.class));
    }

    @Test
    void shouldGenerateUniqueShortCode() {
        // Given
        String longUrl = "https://www.example.com/test";
        when(urlRepository.existsByShortCode(anyString()))
                .thenReturn(true)  // First attempt: code exists
                .thenReturn(false); // Second attempt: code is unique
        when(urlRepository.save(any(UrlMapping.class))).thenAnswer(invocation -> invocation.getArgument(0));

        // When
        UrlMapping result = urlShortenerService.createShortUrl(longUrl);

        // Then
        assertThat(result).isNotNull();
        verify(urlRepository, atLeast(2)).existsByShortCode(anyString());
    }

    @Test
    void shouldGetOriginalUrlSuccessfully() {
        // Given
        String shortCode = "abc123";
        UrlMapping urlMapping = new UrlMapping(shortCode, "https://example.com");
        urlMapping.setId(1L);
        when(urlRepository.findByShortCode(shortCode)).thenReturn(Optional.of(urlMapping));
        when(urlRepository.save(any(UrlMapping.class))).thenAnswer(invocation -> invocation.getArgument(0));

        // When
        String originalUrl = urlShortenerService.getOriginalUrl(shortCode);

        // Then
        assertThat(originalUrl).isEqualTo("https://example.com");
        verify(urlRepository).findByShortCode(shortCode);
        verify(urlRepository).save(any(UrlMapping.class));
    }

    @Test
    void shouldIncrementAccessCountWhenGettingOriginalUrl() {
        // Given
        String shortCode = "test123";
        UrlMapping urlMapping = new UrlMapping(shortCode, "https://test.com");
        urlMapping.setId(1L);
        when(urlRepository.findByShortCode(shortCode)).thenReturn(Optional.of(urlMapping));
        when(urlRepository.save(any(UrlMapping.class))).thenAnswer(invocation -> invocation.getArgument(0));

        // When
        urlShortenerService.getOriginalUrl(shortCode);

        // Then
        verify(urlRepository).save(argThat(mapping -> 
            mapping.getAccessCount() == 1
        ));
    }

    @Test
    void shouldThrowExceptionWhenShortCodeNotFound() {
        // Given
        String shortCode = "notfound";
        when(urlRepository.findByShortCode(shortCode)).thenReturn(Optional.empty());

        // When & Then
        assertThatThrownBy(() -> urlShortenerService.getOriginalUrl(shortCode))
                .isInstanceOf(UrlNotFoundException.class)
                .hasMessageContaining(shortCode);
    }

    @Test
    void shouldGetUrlInfoSuccessfully() {
        // Given
        String shortCode = "info123";
        UrlMapping urlMapping = new UrlMapping(shortCode, "https://info.com");
        urlMapping.setId(1L);
        urlMapping.setAccessCount(5);
        when(urlRepository.findByShortCode(shortCode)).thenReturn(Optional.of(urlMapping));

        // When
        UrlMapping result = urlShortenerService.getUrlInfo(shortCode);

        // Then
        assertThat(result).isNotNull();
        assertThat(result.getShortCode()).isEqualTo(shortCode);
        assertThat(result.getLongUrl()).isEqualTo("https://info.com");
        assertThat(result.getAccessCount()).isEqualTo(5);
        verify(urlRepository).findByShortCode(shortCode);
        verify(urlRepository, never()).save(any(UrlMapping.class));
    }

    @Test
    void shouldThrowExceptionWhenGettingInfoForNonExistentShortCode() {
        // Given
        String shortCode = "notfound";
        when(urlRepository.findByShortCode(shortCode)).thenReturn(Optional.empty());

        // When & Then
        assertThatThrownBy(() -> urlShortenerService.getUrlInfo(shortCode))
                .isInstanceOf(UrlNotFoundException.class)
                .hasMessageContaining(shortCode);
    }
}
