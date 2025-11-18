# Java Exercise - Complete Load Balancer Implementation

A Test-Driven Development (TDD) implementation of a complete Load Balancer with service registry, REST API, and request proxying capabilities.

## Overview

This package contains a production-ready load balancer implementation that provides:
- Service registration and discovery via REST API
- Actual load balancing with configurable strategies
- Request proxying to backend services
- Health-based instance selection

## Features

- **Service Registration**: Register service instances with unique IDs
- **Service Discovery**: Query services by ID or name
- **Service Deregistration**: Remove services from the registry
- **Health Tracking**: Track service health status
- **Load Balancing Strategies**: Pluggable algorithms (Round Robin, Random)
- **Request Proxying**: Forward HTTP requests to backend services
- **Strategy Management**: Change load balancing strategy at runtime
- **REST API**: Complete RESTful API for all operations
- **Thread-Safe**: Concurrent-safe registry and strategies
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
│   │   │       │   ├── LoadBalancerController.java
│   │   │       │   ├── ProxyController.java
│   │   │       │   └── StrategyController.java
│   │   │       ├── service/
│   │   │       │   ├── ServiceRegistry.java
│   │   │       │   └── LoadBalancerService.java
│   │   │       ├── strategy/
│   │   │       │   ├── LoadBalancingStrategy.java
│   │   │       │   ├── RoundRobinStrategy.java
│   │   │       │   └── RandomStrategy.java
│   │   │       └── model/
│   │   │           ├── ServiceInstance.java
│   │   │           └── RegisterServiceRequest.java
│   │   └── resources/
│   │       └── application.properties
│   └── test/
│       └── java/
│           └── com/tddplayground/loadbalancer/
│               ├── controller/
│               │   ├── LoadBalancerControllerTest.java
│               │   └── StrategyControllerTest.java
│               ├── service/
│               │   ├── ServiceRegistryTest.java
│               │   └── LoadBalancerServiceTest.java
│               └── strategy/
│                   ├── RoundRobinStrategyTest.java
│                   └── RandomStrategyTest.java
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

### Load Balancing Strategy API

#### 7. Get Current Strategy

**Request:**
```bash
curl http://localhost:8080/api/loadbalancer/strategy
```

**Response:** (200 OK)
```json
{
  "strategy": "ROUND_ROBIN"
}
```

#### 8. Set Load Balancing Strategy

**Request:**
```bash
curl -X PUT http://localhost:8080/api/loadbalancer/strategy \
  -H "Content-Type: application/json" \
  -d '{"strategy": "RANDOM"}'
```

**Response:** (200 OK)
```json
{
  "strategy": "RANDOM",
  "message": "Strategy updated successfully"
}
```

#### 9. Get Available Strategies

**Request:**
```bash
curl http://localhost:8080/api/loadbalancer/strategy/available
```

**Response:** (200 OK)
```json
{
  "strategies": ["RANDOM", "ROUND_ROBIN"],
  "current": "ROUND_ROBIN"
}
```

### Request Proxying

#### 10. Proxy Request to Backend Service

The load balancer can proxy requests to registered backend services:

**Request:**
```bash
curl http://localhost:8080/proxy/user-service/api/users
```

This will:
1. Select a healthy instance of "user-service" using the configured load balancing strategy
2. Forward the GET request to the selected instance
3. Return the response from the backend service

You can also proxy POST, PUT, DELETE, and PATCH requests:

```bash
# POST request
curl -X POST http://localhost:8080/proxy/user-service/api/users \
  -H "Content-Type: application/json" \
  -d '{"name": "John Doe"}'

# PUT request
curl -X PUT http://localhost:8080/proxy/user-service/api/users/1 \
  -H "Content-Type: application/json" \
  -d '{"name": "Jane Doe"}'

# DELETE request
curl -X DELETE http://localhost:8080/proxy/user-service/api/users/1
```

## Testing

The project includes comprehensive tests (48 total):

### Unit Tests
- `ServiceRegistryTest` (11 tests): Tests the service registry business logic
  - Service registration and validation
  - Service deregistration
  - Service retrieval and filtering

- `LoadBalancerServiceTest` (9 tests): Tests load balancing logic
  - Strategy management
  - Instance selection
  - Health-based filtering

- `RoundRobinStrategyTest` (6 tests): Tests round-robin algorithm
  - Even distribution
  - Edge cases

- `RandomStrategyTest` (6 tests): Tests random selection
  - Random distribution
  - Edge cases

### Integration Tests
- `LoadBalancerControllerTest` (10 tests): Tests the service management REST API
  - HTTP request/response handling
  - Validation and error handling
  - End-to-end workflows

- `StrategyControllerTest` (6 tests): Tests strategy management REST API
  - Strategy retrieval and switching
  - Available strategies listing
  - Error handling

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
# Run all tests (48 tests)
mvn test

# Run specific test class
mvn test -Dtest=RoundRobinStrategyTest

# Run with coverage
mvn test jacoco:report
```

## Load Balancing in Action

Here's a complete example of using the load balancer:

### 1. Start the application
```bash
mvn spring-boot:run
```

### 2. Register multiple instances of a service
```bash
# Register instance 1
curl -X POST http://localhost:8080/api/loadbalancer/services \
  -H "Content-Type: application/json" \
  -d '{
    "serviceId": "user-1",
    "serviceName": "user-service",
    "host": "localhost",
    "port": 8081
  }'

# Register instance 2
curl -X POST http://localhost:8080/api/loadbalancer/services \
  -H "Content-Type: application/json" \
  -d '{
    "serviceId": "user-2",
    "serviceName": "user-service",
    "host": "localhost",
    "port": 8082
  }'
```

### 3. Check current strategy (default is ROUND_ROBIN)
```bash
curl http://localhost:8080/api/loadbalancer/strategy
```

### 4. Make requests - they will be distributed across instances
```bash
# First request goes to instance 1
curl http://localhost:8080/proxy/user-service/api/users

# Second request goes to instance 2
curl http://localhost:8080/proxy/user-service/api/users

# Third request goes back to instance 1
curl http://localhost:8080/proxy/user-service/api/users
```

### 5. Change strategy to RANDOM
```bash
curl -X PUT http://localhost:8080/api/loadbalancer/strategy \
  -H "Content-Type: application/json" \
  -d '{"strategy": "RANDOM"}'
```

Now requests will be randomly distributed!

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
