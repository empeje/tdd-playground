# java-exercise

A collection of Test-Driven Development (TDD) algorithmic problems and low-level system design exercises implemented in Java 21 with Maven and JUnit.

## Problems

### Revolut Practice Problems
- **In-Memory Load Balancer**:
  - `org.example.loadbalancer.Instance`: Server instance record (address, name).
  - `org.example.loadbalancer.InstanceRegistration`: Load balancer handling instance registration, max 10 instances constraint, duplicate check, and random routing via `RandomGenerator`.
- **URL Shortener & Backend Services**:
  - `org.example.urlshortener.Base62`: Base62 encoder and decoder for URL identifier compression.
  - `org.example.urlshortener.SimpleProxy`: Round-robin HTTP reverse proxy handler with header/body forwarding.
  - `org.example.urlshortener.CrudApiExample`: In-memory H2 database with embedded HTTP server for user/entity management.
  - `org.example.urlshortener.ApiServerExampleForward`: HTTP 301 Permanent Redirect Handler.

## Prerequisites

- Java 21 or later
- Maven 3.8+

## Running Tests

```bash
# Using Maven directly
cd packages/java-exercise && mvn test

# Using Nx from root
pnpm dlx nx test java-exercise --no-cloud
```
