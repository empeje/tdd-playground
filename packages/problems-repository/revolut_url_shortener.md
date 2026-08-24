# Revolut URL Shortener System

## Problem Description

Design and implement the core components of a URL shortener service, including ID-to-slug encoding, HTTP API routing, reverse proxying, and permanent redirects.

### Functional Requirements

1. **Shorten URL**: Users input an original URL and optional custom slug, receiving a shortened URL (e.g. `revolut.me/abc`).
2. **Deterministic & Unique Encoding**: Convert auto-incrementing numeric IDs into compact Base62 alphanumeric slugs (`[0-9a-zA-Z]`).
3. **Redirection**: Visiting `revolut.me/{slug}` issues an HTTP `301 Moved Permanently` redirecting to the target URL.
4. **Proxy & Load Balancing**: Forward traffic across multiple backend instances using round-robin distribution.

### Component Breakdown

#### 1. Base62 Encoder & Decoder
- Base character set: `0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ` (length 62).
- `encode(long number) -> String`: Converts numeric ID to Base62 string.
- `decode(String input) -> long`: Converts Base62 string back to numeric ID.
- Rejects invalid characters with `IllegalArgumentException`.

#### 2. HTTP Redirection Handler (`PermanentRedirectHandler`)
- Employs Java's built-in `HttpServer` / `HttpHandler`.
- Sets header `Location: {destination}` and responds with HTTP status `301`.

#### 3. Reverse Proxy Handler (`ProxyHandler`)
- Distributes requests across a list of backend host URLs using round-robin indexing.
- Forwards HTTP method, request headers, and request body payloads.
- Handles downstream errors returning `502 Bad Gateway`.

#### 4. CRUD API (`CrudApiExample`)
- Integrates in-memory storage (H2 database or ConcurrentMap) to manage user and URL entities.
- Supports GET, POST, PUT, DELETE operations with standard status codes (200, 201, 400, 404, 405).
