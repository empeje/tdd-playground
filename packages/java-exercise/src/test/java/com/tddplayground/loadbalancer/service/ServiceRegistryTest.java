package com.tddplayground.loadbalancer.service;

import com.tddplayground.loadbalancer.model.ServiceInstance;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for ServiceRegistry.
 */
class ServiceRegistryTest {

    private ServiceRegistry serviceRegistry;

    @BeforeEach
    void setUp() {
        serviceRegistry = new ServiceRegistry();
    }

    @Test
    void shouldRegisterServiceSuccessfully() {
        // Given
        ServiceInstance service = new ServiceInstance("service-1", "user-service", "localhost", 8081);

        // When
        ServiceInstance registered = serviceRegistry.registerService(service);

        // Then
        assertNotNull(registered);
        assertEquals("service-1", registered.getServiceId());
        assertEquals("user-service", registered.getServiceName());
        assertEquals("localhost", registered.getHost());
        assertEquals(8081, registered.getPort());
        assertEquals(1, serviceRegistry.getServiceCount());
    }

    @Test
    void shouldThrowExceptionWhenRegisteringDuplicateService() {
        // Given
        ServiceInstance service1 = new ServiceInstance("service-1", "user-service", "localhost", 8081);
        serviceRegistry.registerService(service1);

        ServiceInstance service2 = new ServiceInstance("service-1", "user-service", "localhost", 8082);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            serviceRegistry.registerService(service2);
        });

        assertTrue(exception.getMessage().contains("already registered"));
        assertEquals(1, serviceRegistry.getServiceCount());
    }

    @Test
    void shouldThrowExceptionWhenRegisteringNullService() {
        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            serviceRegistry.registerService(null);
        });

        assertTrue(exception.getMessage().contains("cannot be null"));
    }

    @Test
    void shouldDeregisterServiceSuccessfully() {
        // Given
        ServiceInstance service = new ServiceInstance("service-1", "user-service", "localhost", 8081);
        serviceRegistry.registerService(service);

        // When
        ServiceInstance deregistered = serviceRegistry.deregisterService("service-1");

        // Then
        assertNotNull(deregistered);
        assertEquals("service-1", deregistered.getServiceId());
        assertEquals(0, serviceRegistry.getServiceCount());
    }

    @Test
    void shouldReturnNullWhenDeregisteringNonExistentService() {
        // When
        ServiceInstance deregistered = serviceRegistry.deregisterService("non-existent");

        // Then
        assertNull(deregistered);
    }

    @Test
    void shouldGetServiceById() {
        // Given
        ServiceInstance service = new ServiceInstance("service-1", "user-service", "localhost", 8081);
        serviceRegistry.registerService(service);

        // When
        ServiceInstance retrieved = serviceRegistry.getService("service-1");

        // Then
        assertNotNull(retrieved);
        assertEquals("service-1", retrieved.getServiceId());
    }

    @Test
    void shouldReturnNullWhenGettingNonExistentService() {
        // When
        ServiceInstance retrieved = serviceRegistry.getService("non-existent");

        // Then
        assertNull(retrieved);
    }

    @Test
    void shouldGetAllServices() {
        // Given
        serviceRegistry.registerService(new ServiceInstance("service-1", "user-service", "localhost", 8081));
        serviceRegistry.registerService(new ServiceInstance("service-2", "order-service", "localhost", 8082));
        serviceRegistry.registerService(new ServiceInstance("service-3", "user-service", "localhost", 8083));

        // When
        List<ServiceInstance> allServices = serviceRegistry.getAllServices();

        // Then
        assertEquals(3, allServices.size());
    }

    @Test
    void shouldGetServicesByName() {
        // Given
        serviceRegistry.registerService(new ServiceInstance("service-1", "user-service", "localhost", 8081));
        serviceRegistry.registerService(new ServiceInstance("service-2", "order-service", "localhost", 8082));
        serviceRegistry.registerService(new ServiceInstance("service-3", "user-service", "localhost", 8083));

        // When
        List<ServiceInstance> userServices = serviceRegistry.getServicesByName("user-service");

        // Then
        assertEquals(2, userServices.size());
        assertTrue(userServices.stream().allMatch(s -> s.getServiceName().equals("user-service")));
    }

    @Test
    void shouldGetHealthyServicesByName() {
        // Given
        ServiceInstance service1 = new ServiceInstance("service-1", "user-service", "localhost", 8081);
        ServiceInstance service2 = new ServiceInstance("service-2", "user-service", "localhost", 8082);
        service2.setHealthy(false);
        ServiceInstance service3 = new ServiceInstance("service-3", "user-service", "localhost", 8083);

        serviceRegistry.registerService(service1);
        serviceRegistry.registerService(service2);
        serviceRegistry.registerService(service3);

        // When
        List<ServiceInstance> healthyServices = serviceRegistry.getHealthyServicesByName("user-service");

        // Then
        assertEquals(2, healthyServices.size());
        assertTrue(healthyServices.stream().allMatch(ServiceInstance::isHealthy));
    }

    @Test
    void shouldClearAllServices() {
        // Given
        serviceRegistry.registerService(new ServiceInstance("service-1", "user-service", "localhost", 8081));
        serviceRegistry.registerService(new ServiceInstance("service-2", "order-service", "localhost", 8082));

        // When
        serviceRegistry.clear();

        // Then
        assertEquals(0, serviceRegistry.getServiceCount());
    }
}
