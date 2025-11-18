package com.tddplayground.urlshortener.controller;

import com.tddplayground.urlshortener.dto.CreateShortUrlRequest;
import com.tddplayground.urlshortener.dto.ShortUrlResponse;
import com.tddplayground.urlshortener.dto.UrlInfoResponse;
import com.tddplayground.urlshortener.model.UrlMapping;
import com.tddplayground.urlshortener.service.UrlShortenerService;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.view.RedirectView;

import jakarta.servlet.http.HttpServletRequest;

@RestController
public class UrlController {

    private final UrlShortenerService urlShortenerService;

    public UrlController(UrlShortenerService urlShortenerService) {
        this.urlShortenerService = urlShortenerService;
    }

    @PostMapping("/api/urls")
    public ResponseEntity<ShortUrlResponse> createShortUrl(
            @Valid @RequestBody CreateShortUrlRequest request,
            HttpServletRequest httpRequest) {
        
        UrlMapping urlMapping = urlShortenerService.createShortUrl(request.getLongUrl());
        
        String baseUrl = getBaseUrl(httpRequest);
        String shortUrl = baseUrl + "/" + urlMapping.getShortCode();
        
        ShortUrlResponse response = new ShortUrlResponse(
                urlMapping.getShortCode(),
                shortUrl,
                urlMapping.getLongUrl()
        );
        
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @GetMapping("/{shortCode}")
    public RedirectView redirect(@PathVariable String shortCode) {
        String originalUrl = urlShortenerService.getOriginalUrl(shortCode);
        RedirectView redirectView = new RedirectView();
        redirectView.setUrl(originalUrl);
        redirectView.setStatusCode(HttpStatus.FOUND);
        return redirectView;
    }

    @GetMapping("/api/urls/{shortCode}")
    public ResponseEntity<UrlInfoResponse> getUrlInfo(@PathVariable String shortCode) {
        UrlMapping urlMapping = urlShortenerService.getUrlInfo(shortCode);
        
        UrlInfoResponse response = new UrlInfoResponse(
                urlMapping.getShortCode(),
                urlMapping.getLongUrl(),
                urlMapping.getCreatedAt(),
                urlMapping.getAccessCount()
        );
        
        return ResponseEntity.ok(response);
    }

    private String getBaseUrl(HttpServletRequest request) {
        String scheme = request.getScheme();
        String serverName = request.getServerName();
        int serverPort = request.getServerPort();
        
        String baseUrl = scheme + "://" + serverName;
        if ((scheme.equals("http") && serverPort != 80) || (scheme.equals("https") && serverPort != 443)) {
            baseUrl += ":" + serverPort;
        }
        
        return baseUrl;
    }
}
