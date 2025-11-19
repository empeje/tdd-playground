package com.tddplayground.urlshortener.service;

import com.tddplayground.urlshortener.exception.ShortCodeGenerationException;
import com.tddplayground.urlshortener.exception.UrlNotFoundException;
import com.tddplayground.urlshortener.model.UrlMapping;
import com.tddplayground.urlshortener.repository.UrlRepository;

import java.security.SecureRandom;

public class UrlShortenerService {

    private static final String BASE62_CHARS = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
    private static final int SHORT_CODE_LENGTH = 6;
    private static final int MAX_RETRY_ATTEMPTS = 5;
    private final SecureRandom random = new SecureRandom();

    private final UrlRepository urlRepository;

    public UrlShortenerService(UrlRepository urlRepository) {
        this.urlRepository = urlRepository;
    }

    public UrlMapping createShortUrl(String longUrl) {
        String shortCode = generateUniqueShortCode();
        UrlMapping urlMapping = new UrlMapping(shortCode, longUrl);
        return urlRepository.save(urlMapping);
    }

    public String getOriginalUrl(String shortCode) {
        UrlMapping urlMapping = urlRepository.findByShortCode(shortCode)
                .orElseThrow(() -> new UrlNotFoundException(shortCode));
        
        urlMapping.incrementAccessCount();
        urlRepository.save(urlMapping);
        
        return urlMapping.getLongUrl();
    }

    public UrlMapping getUrlInfo(String shortCode) {
        return urlRepository.findByShortCode(shortCode)
                .orElseThrow(() -> new UrlNotFoundException(shortCode));
    }

    private String generateUniqueShortCode() {
        for (int attempt = 0; attempt < MAX_RETRY_ATTEMPTS; attempt++) {
            String shortCode = generateShortCode();
            if (!urlRepository.existsByShortCode(shortCode)) {
                return shortCode;
            }
        }
        throw new ShortCodeGenerationException("Failed to generate unique short code after " + MAX_RETRY_ATTEMPTS + " attempts");
    }

    private String generateShortCode() {
        StringBuilder shortCode = new StringBuilder(SHORT_CODE_LENGTH);
        for (int i = 0; i < SHORT_CODE_LENGTH; i++) {
            int index = random.nextInt(BASE62_CHARS.length());
            shortCode.append(BASE62_CHARS.charAt(index));
        }
        return shortCode.toString();
    }
}
