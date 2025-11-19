package com.tddplayground.urlshortener.repository;

import com.tddplayground.urlshortener.model.UrlMapping;

import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;

public class UrlRepository {

    private final Map<String, UrlMapping> storage = new ConcurrentHashMap<>();
    private final AtomicLong idGenerator = new AtomicLong(1);

    public UrlMapping save(UrlMapping urlMapping) {
        if (urlMapping.getId() == null) {
            urlMapping.setId(idGenerator.getAndIncrement());
        }
        storage.put(urlMapping.getShortCode(), urlMapping);
        return urlMapping;
    }

    public Optional<UrlMapping> findByShortCode(String shortCode) {
        return Optional.ofNullable(storage.get(shortCode));
    }

    public boolean existsByShortCode(String shortCode) {
        return storage.containsKey(shortCode);
    }

    public void deleteAll() {
        storage.clear();
    }

    public long count() {
        return storage.size();
    }
}
