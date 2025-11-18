# URL Shortener REST API

A Test-Driven Development (TDD) implementation of a URL shortener service with REST API endpoints.

## Overview

This is a REST API implementation of a URL shortener service built using Spring Boot and following TDD practices. The service allows users to create shortened URLs and redirect to the original URLs.

## Features

- **Create Short URL**: Generate a short URL from a long URL
- **Retrieve Original URL**: Get the original URL from a short code
- **Redirect**: Automatically redirect from short URL to original URL
- **URL Validation**: Validate URLs before shortening
- **In-Memory Storage**: Uses H2 database for simple persistence

## Architecture

### System Components

The URL shortener follows a layered architecture:

1. **Controller Layer**: REST API endpoints
2. **Service Layer**: Business logic and URL shortening algorithm
3. **Repository Layer**: Data persistence
4. **Model Layer**: Domain entities
5. **DTO Layer**: Data transfer objects for API requests/responses

### System Design

```mermaid
graph TB
    Client[Client/Browser]
    Controller[URL Controller]
    Service[URL Shortener Service]
    Repository[URL Repository]
    Database[(H2 Database)]
    
    Client -->|HTTP Request| Controller
    Controller -->|Validate & Process| Service
    Service -->|Generate Short Code| Service
    Service -->|Persist URL Mapping| Repository
    Repository -->|CRUD Operations| Database
    Controller -->|HTTP Response| Client
    
    style Client fill:#e1f5ff
    style Controller fill:#fff4e1
    style Service fill:#ffe1e1
    style Repository fill:#e1ffe1
    style Database fill:#f0e1ff
```

### Sequence Diagram - Create Short URL

```mermaid
sequenceDiagram
    participant Client
    participant Controller
    participant Service
    participant Repository
    participant Database
    
    Client->>Controller: POST /api/urls {longUrl}
    Controller->>Controller: Validate Request
    Controller->>Service: createShortUrl(longUrl)
    Service->>Service: Generate Short Code
    Service->>Repository: save(urlMapping)
    Repository->>Database: INSERT url_mapping
    Database-->>Repository: Saved Entity
    Repository-->>Service: UrlMapping
    Service-->>Controller: ShortUrlResponse
    Controller-->>Client: 201 Created {shortCode, shortUrl}
```

### Sequence Diagram - Redirect to Original URL

```mermaid
sequenceDiagram
    participant Client
    participant Controller
    participant Service
    participant Repository
    participant Database
    
    Client->>Controller: GET /{shortCode}
    Controller->>Service: getOriginalUrl(shortCode)
    Service->>Repository: findByShortCode(shortCode)
    Repository->>Database: SELECT FROM url_mapping
    Database-->>Repository: UrlMapping
    Repository-->>Service: UrlMapping
    Service->>Service: Update Access Count
    Service->>Repository: save(updatedMapping)
    Repository->>Database: UPDATE url_mapping
    Service-->>Controller: Original URL
    Controller-->>Client: 302 Redirect to Original URL
```

### Class Diagram

```mermaid
classDiagram
    class UrlController {
        -UrlShortenerService service
        +createShortUrl(request) ResponseEntity
        +redirect(shortCode) ResponseEntity
        +getUrlInfo(shortCode) ResponseEntity
    }
    
    class UrlShortenerService {
        -UrlRepository repository
        +createShortUrl(longUrl) UrlMapping
        +getOriginalUrl(shortCode) String
        +getUrlInfo(shortCode) UrlMapping
        -generateShortCode() String
    }
    
    class UrlRepository {
        <<interface>>
        +findByShortCode(shortCode) Optional~UrlMapping~
        +existsByShortCode(shortCode) boolean
    }
    
    class UrlMapping {
        -Long id
        -String longUrl
        -String shortCode
        -LocalDateTime createdAt
        -int accessCount
    }
    
    class CreateShortUrlRequest {
        -String longUrl
    }
    
    class ShortUrlResponse {
        -String shortCode
        -String shortUrl
        -String longUrl
    }
    
    UrlController --> UrlShortenerService
    UrlShortenerService --> UrlRepository
    UrlRepository --> UrlMapping
    UrlController --> CreateShortUrlRequest
    UrlController --> ShortUrlResponse
```

## API Endpoints

### 1. Create Short URL

**Endpoint**: `POST /api/urls`

**Request Body**:
```json
{
  "longUrl": "https://www.example.com/very/long/url/path"
}
```

**Response**: `201 Created`
```json
{
  "shortCode": "abc123",
  "shortUrl": "http://localhost:8080/abc123",
  "longUrl": "https://www.example.com/very/long/url/path"
}
```

### 2. Redirect to Original URL

**Endpoint**: `GET /{shortCode}`

**Response**: `302 Found`
- Redirects to the original URL

### 3. Get URL Information

**Endpoint**: `GET /api/urls/{shortCode}`

**Response**: `200 OK`
```json
{
  "shortCode": "abc123",
  "longUrl": "https://www.example.com/very/long/url/path",
  "createdAt": "2024-01-15T10:30:00",
  "accessCount": 42
}
```

## Technology Stack

- **Java 17**: Programming language
- **Spring Boot 3.2.0**: Framework for building the REST API
- **Spring Data JPA**: Data persistence
- **H2 Database**: In-memory database
- **Maven**: Build tool
- **JUnit 5**: Testing framework
- **REST Assured**: API testing

## Prerequisites

- Java 17 or higher
- Maven 3.6+

## Setup and Installation

1. Navigate to the project directory:
```bash
cd packages/java-url-shortener
```

2. Build the project:
```bash
mvn clean install
```

3. Run the application:
```bash
mvn spring-boot:run
```

The API will be available at `http://localhost:8080`

## Running Tests

Run all tests:
```bash
mvn test
```

Run tests with coverage:
```bash
mvn test jacoco:report
```

## Usage Examples

### Using cURL

Create a short URL:
```bash
curl -X POST http://localhost:8080/api/urls \
  -H "Content-Type: application/json" \
  -d '{"longUrl": "https://www.example.com/very/long/url"}'
```

Access a short URL (redirect):
```bash
curl -L http://localhost:8080/abc123
```

Get URL information:
```bash
curl http://localhost:8080/api/urls/abc123
```

## TDD Approach

This project was built following Test-Driven Development principles:

1. **Red**: Write failing tests first
2. **Green**: Implement minimum code to pass tests
3. **Refactor**: Improve code while keeping tests passing

### Test Coverage

- Unit tests for service layer logic
- Unit tests for repository operations
- Integration tests for REST API endpoints
- Edge case and error handling tests

## Design Decisions

### Short Code Generation

- Uses a random alphanumeric string (6 characters)
- Collision detection with retry mechanism
- Base62 encoding for URL-safe characters

### Storage

- H2 in-memory database for simplicity
- Can be easily switched to PostgreSQL/MySQL for production
- JPA entities for database abstraction

### Error Handling

- Custom exceptions for different error scenarios
- Global exception handler for consistent error responses
- Proper HTTP status codes

## Future Enhancements

- Custom short codes (vanity URLs)
- URL expiration
- Analytics and tracking
- User authentication
- Rate limiting
- Custom domain support
- Database migration to persistent storage

## License

MIT License - Part of the TDD Playground repository
