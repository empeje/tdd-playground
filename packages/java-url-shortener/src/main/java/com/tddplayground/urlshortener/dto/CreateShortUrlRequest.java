package com.tddplayground.urlshortener.dto;

public class CreateShortUrlRequest {

    private String longUrl;

    public CreateShortUrlRequest() {
    }

    public CreateShortUrlRequest(String longUrl) {
        this.longUrl = longUrl;
    }

    public String getLongUrl() {
        return longUrl;
    }

    public void setLongUrl(String longUrl) {
        this.longUrl = longUrl;
    }

    public boolean isValid() {
        return longUrl != null && 
               !longUrl.isBlank() && 
               longUrl.length() <= 2048 &&
               (longUrl.startsWith("http://") || longUrl.startsWith("https://"));
    }

    @Override
    public String toString() {
        return "CreateShortUrlRequest{" +
                "longUrl='" + longUrl + '\'' +
                '}';
    }
}
