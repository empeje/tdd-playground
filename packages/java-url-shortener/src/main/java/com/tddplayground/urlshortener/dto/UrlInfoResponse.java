package com.tddplayground.urlshortener.dto;

import java.time.LocalDateTime;

public class UrlInfoResponse {

    private String shortCode;
    private String longUrl;
    private LocalDateTime createdAt;
    private int accessCount;

    public UrlInfoResponse() {
    }

    public UrlInfoResponse(String shortCode, String longUrl, LocalDateTime createdAt, int accessCount) {
        this.shortCode = shortCode;
        this.longUrl = longUrl;
        this.createdAt = createdAt;
        this.accessCount = accessCount;
    }

    public String getShortCode() {
        return shortCode;
    }

    public void setShortCode(String shortCode) {
        this.shortCode = shortCode;
    }

    public String getLongUrl() {
        return longUrl;
    }

    public void setLongUrl(String longUrl) {
        this.longUrl = longUrl;
    }

    public LocalDateTime getCreatedAt() {
        return createdAt;
    }

    public void setCreatedAt(LocalDateTime createdAt) {
        this.createdAt = createdAt;
    }

    public int getAccessCount() {
        return accessCount;
    }

    public void setAccessCount(int accessCount) {
        this.accessCount = accessCount;
    }

    @Override
    public String toString() {
        return "UrlInfoResponse{" +
                "shortCode='" + shortCode + '\'' +
                ", longUrl='" + longUrl + '\'' +
                ", createdAt=" + createdAt +
                ", accessCount=" + accessCount +
                '}';
    }
}
