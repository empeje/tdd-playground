# Revolut Technical Interview Questions

This document outlines key practical coding challenges and system architecture questions encountered in Revolut's technical interviews.

## Core Problem Categories

### 1. In-Memory Load Balancer
- **Problem**: [Revolut Load Balancer](revolut_load_balancer.md)
- **Domain**: Distributed Systems / Object-Oriented Design
- **Description**: Implement an in-memory load balancer component capable of registering backend server instances with strict constraints (unique address constraint, maximum 10 instances) and routing client requests randomly or via round-robin distribution.

### 2. URL Shortener Service & Low-Level Design
- **Problem**: [Revolut URL Shortener](revolut_url_shortener.md)
- **Domain**: Low-Level System Design & Backend API
- **Description**: Build an end-to-end URL shortener system:
  - Base62 ID encoding and decoding algorithm
  - In-memory database persistence for URL/slug mappings
  - Embedded HTTP server supporting 301 Moved Permanently redirects
  - HTTP reverse proxy with round-robin traffic routing

## Architectural & Coding Expectations

- **TDD & Clean Code**: Clean separation of concerns, test coverage for edge cases (e.g. limit overflow, duplicate keys, malformed inputs).
- **Concurrency & Resilience**: Thread-safety considerations, handling server downstream errors (e.g. 502 Bad Gateway proxy responses).
- **Extensibility**: Designing interfaces and domain objects for easy substitution (e.g. pluggable random generators, mockable registries).
