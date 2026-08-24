# Revolut Load Balancer

## Problem Description

Design and implement an in-memory load balancer system in Java that manages backend server instances and balances incoming traffic.

### Requirements

1. **Instance Registration**:
   - Register a backend instance identified by a unique network address (e.g., FQDN or IP address) and name.
   - Each address must be unique. Attempting to register an already registered address must throw a `RuntimeException` or custom exception.
   - The load balancer must accept a maximum of 10 registered addresses. Exceeding this capacity must throw a `RuntimeException` with message `"Address can't be more than 10, remove if you want to add more"`.

2. **Lookup & Retrieval**:
   - `getInstance(String address)`: Retrieve a specific instance by its registered address.
   - `getInstance()`: Develop an algorithm that, when invoking `get()` multiple times, returns one backend instance chosen randomly between all currently registered instances.
   - When no instances are registered, handle the empty condition safely.

3. **Design & Testability**:
   - Use dependency injection for the random number generator (`RandomGenerator` / `Random`) so test suites can deterministically verify selection algorithms.
   - Maintain immutable instance models (`record Instance(String address, String name)`).

### Example Usage

```java
InstanceRegistration loadBalancer = new InstanceRegistration(new Random());

// Register instances
loadBalancer.registerInstance(new Instance("srv1.revolut.com", "revolut-app-1"));
loadBalancer.registerInstance(new Instance("srv2.revolut.com", "revolut-app-2"));

// Direct lookup
Instance inst = loadBalancer.getInstance("srv1.revolut.com");

// Random load-balanced selection
Instance selected = loadBalancer.getInstance();
```
