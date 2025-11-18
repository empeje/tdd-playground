# Java Exercise - Load Balancer Service Registry

A Test-Driven Development (TDD) implementation of a Load Balancer service registry with REST API support.

## Overview

This package contains a simple load balancer implementation that provides a REST API for service registration and discovery. Services can register themselves with the load balancer, and clients can query for available service instances.

## Features

- **Service Registration**: Register service instances with unique IDs
- **Service Discovery**: Query services by ID or name
- **Service Deregistration**: Remove services from the registry
- **Health Tracking**: Track service health status
- **REST API**: Complete RESTful API for all operations
- **Thread-Safe**: Concurrent-safe service registry
- **Validation**: Input validation with Jakarta Bean Validation

## Prerequisites

- Java 17 or later
- Maven 3.6 or later

## Project Structure

```
java-exercise/
├── src/
│   ├── main/
│   │   ├── java/
│   │   │   └── com/tddplayground/loadbalancer/
│   │   │       ├── LoadBalancerApplication.java
│   │   │       ├── controller/
│   │   │       │   └── LoadBalancerController.java
│   │   │       ├── service/
│   │   │       │   └── ServiceRegistry.java
│   │   │       └── model/
│   │   │           ├── ServiceInstance.java
│   │   │           └── RegisterServiceRequest.java
│   │   └── resources/
│   │       └── application.properties
│   └── test/
│       └── java/
│           └── com/tddplayground/loadbalancer/
│               ├── controller/
│               │   └── LoadBalancerControllerTest.java
│               └── service/
│                   └── ServiceRegistryTest.java
├── pom.xml
├── DESIGN.md
└── README.md
```

## Getting Started

### Build the Project

```bash
mvn clean install
```

### Run Tests

```bash
mvn test
```

### Run the Application

```bash
mvn spring-boot:run
```

The application will start on `http://localhost:8080`.

## API Documentation

### Base URL
```
http://localhost:8080/api/loadbalancer
```

### Endpoints

#### 1. Register a Service

**Request:**
```bash
curl -X POST http://localhost:8080/api/loadbalancer/services \
  -H "Content-Type: application/json" \
  -d '{
    "serviceId": "service-1",
    "serviceName": "user-service",
    "host": "localhost",
    "port": 8081,
    "protocol": "http",
    "healthCheckPath": "/health"
  }'
```

**Response:** (201 Created)
```json
{
  "serviceId": "service-1",
  "serviceName": "user-service",
  "host": "localhost",
  "port": 8081,
  "protocol": "http",
  "healthCheckPath": "/health",
  "healthy": true,
  "registeredAt": "2025-11-18T19:30:00Z",
  "lastHealthCheck": null,
  "url": "http://localhost:8081"
}
```

#### 2. Get All Services

**Request:**
```bash
curl http://localhost:8080/api/loadbalancer/services
```

**Response:** (200 OK)
```json
[
  {
    "serviceId": "service-1",
    "serviceName": "user-service",
    ...
  }
]
```

#### 3. Get Service by ID

**Request:**
```bash
curl http://localhost:8080/api/loadbalancer/services/service-1
```

**Response:** (200 OK)
```json
{
  "serviceId": "service-1",
  "serviceName": "user-service",
  ...
}
```

#### 4. Get Services by Name

**Request:**
```bash
curl http://localhost:8080/api/loadbalancer/services/by-name/user-service
```

**Response:** (200 OK)
```json
[
  {
    "serviceId": "service-1",
    "serviceName": "user-service",
    ...
  },
  {
    "serviceId": "service-2",
    "serviceName": "user-service",
    ...
  }
]
```

#### 5. Get Healthy Services by Name

**Request:**
```bash
curl http://localhost:8080/api/loadbalancer/services/by-name/user-service/healthy
```

**Response:** (200 OK)
```json
[
  {
    "serviceId": "service-1",
    "serviceName": "user-service",
    "healthy": true,
    ...
  }
]
```

#### 6. Deregister a Service

**Request:**
```bash
curl -X DELETE http://localhost:8080/api/loadbalancer/services/service-1
```

**Response:** (200 OK)
```json
{
  "serviceId": "service-1",
  "serviceName": "user-service",
  ...
}
```

## Testing

The project includes comprehensive tests:

### Unit Tests
- `ServiceRegistryTest`: Tests the service registry business logic
  - Service registration and validation
  - Service deregistration
  - Service retrieval and filtering

### Integration Tests
- `LoadBalancerControllerTest`: Tests the REST API
  - HTTP request/response handling
  - Validation and error handling
  - End-to-end workflows

### Running Tests

```bash
# Run all tests
mvn test

# Run specific test class
mvn test -Dtest=ServiceRegistryTest

# Run with coverage
mvn test jacoco:report
```

## Design Documentation

For detailed design documentation, architecture diagrams, and implementation details, see [DESIGN.md](DESIGN.md).

The design document includes:
- Architecture overview
- Component descriptions
- API specifications
- Mermaid diagrams (architecture, sequence, data flow, class)
- Implementation details
- Future enhancements

## Technology Stack

- **Spring Boot 3.2.0**: Application framework
- **Spring Web**: REST API support
- **Spring Boot Validation**: Request validation
- **JUnit 5**: Testing framework
- **Maven**: Build tool
- **Java 17**: Programming language

## Error Handling

The API returns appropriate HTTP status codes:

- `200 OK`: Successful operation
- `201 Created`: Service successfully registered
- `400 Bad Request`: Invalid request data
- `404 Not Found`: Service not found
- `409 Conflict`: Duplicate service ID

Error responses include descriptive messages:
```json
{
  "error": "Service with ID service-1 is already registered"
}
```

## Contributing

This is a TDD exercise project. When adding new features:

1. Write tests first (Red)
2. Implement the minimum code to pass tests (Green)
3. Refactor while keeping tests passing (Refactor)

## License

MIT License
