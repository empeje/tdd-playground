package com.tddplayground.loadbalancer.service;

import com.tddplayground.loadbalancer.model.ServiceInstance;
import com.tddplayground.loadbalancer.strategy.LoadBalancingStrategy;
import com.tddplayground.loadbalancer.strategy.RoundRobinStrategy;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for LoadBalancerService.
 */
class LoadBalancerServiceTest {

    private ServiceRegistry serviceRegistry;
    private LoadBalancerService loadBalancerService;

    @BeforeEach
    void setUp() {
        serviceRegistry = new ServiceRegistry();
        loadBalancerService = new LoadBalancerService(serviceRegistry, new RoundRobinStrategy());
    }

    @Test
    void shouldUseRoundRobinStrategyByDefault() {
        assertEquals("ROUND_ROBIN", loadBalancerService.getCurrentStrategyName());
    }

    @Test
    void shouldRegisterCustomStrategy() {
        LoadBalancingStrategy customStrategy = new LoadBalancingStrategy() {
            @Override
            public ServiceInstance selectInstance(List<ServiceInstance> instances) {
                return instances.isEmpty() ? null : instances.get(0);
            }

            @Override
            public String getStrategyName() {
                return "CUSTOM";
            }
        };

        loadBalancerService.registerStrategy(customStrategy);

        Map<String, LoadBalancingStrategy> strategies = loadBalancerService.getAvailableStrategies();
        assertTrue(strategies.containsKey("CUSTOM"));
    }

    @Test
    void shouldSetStrategy() {
        LoadBalancingStrategy customStrategy = new LoadBalancingStrategy() {
            @Override
            public ServiceInstance selectInstance(List<ServiceInstance> instances) {
                return instances.isEmpty() ? null : instances.get(0);
            }

            @Override
            public String getStrategyName() {
                return "CUSTOM";
            }
        };

        loadBalancerService.registerStrategy(customStrategy);
        loadBalancerService.setStrategy("CUSTOM");

        assertEquals("CUSTOM", loadBalancerService.getCurrentStrategyName());
    }

    @Test
    void shouldThrowExceptionForUnknownStrategy() {
        assertThrows(IllegalArgumentException.class, () -> {
            loadBalancerService.setStrategy("UNKNOWN");
        });
    }

    @Test
    void shouldSelectInstanceUsingCurrentStrategy() {
        // Register services
        serviceRegistry.registerService(new ServiceInstance("service-1", "user-service", "localhost", 8081));
        serviceRegistry.registerService(new ServiceInstance("service-2", "user-service", "localhost", 8082));

        // Select instance
        ServiceInstance selected = loadBalancerService.selectInstance("user-service");

        assertNotNull(selected);
        assertTrue(selected.getServiceId().equals("service-1") || selected.getServiceId().equals("service-2"));
    }

    @Test
    void shouldReturnNullWhenNoHealthyInstancesAvailable() {
        ServiceInstance selected = loadBalancerService.selectInstance("non-existent-service");
        assertNull(selected);
    }

    @Test
    void shouldSelectOnlyHealthyInstances() {
        // Register healthy and unhealthy services
        ServiceInstance healthy = new ServiceInstance("service-1", "user-service", "localhost", 8081);
        ServiceInstance unhealthy = new ServiceInstance("service-2", "user-service", "localhost", 8082);
        unhealthy.setHealthy(false);

        serviceRegistry.registerService(healthy);
        serviceRegistry.registerService(unhealthy);

        // Select instance - should only get the healthy one
        ServiceInstance selected = loadBalancerService.selectInstance("user-service");

        assertNotNull(selected);
        assertEquals("service-1", selected.getServiceId());
    }

    @Test
    void shouldDistributeRequestsUsingRoundRobin() {
        // Register multiple services
        serviceRegistry.registerService(new ServiceInstance("service-1", "user-service", "localhost", 8081));
        serviceRegistry.registerService(new ServiceInstance("service-2", "user-service", "localhost", 8082));
        serviceRegistry.registerService(new ServiceInstance("service-3", "user-service", "localhost", 8083));

        // Select instances in round-robin fashion
        ServiceInstance first = loadBalancerService.selectInstance("user-service");
        ServiceInstance second = loadBalancerService.selectInstance("user-service");
        ServiceInstance third = loadBalancerService.selectInstance("user-service");
        ServiceInstance fourth = loadBalancerService.selectInstance("user-service");

        // Verify round-robin behavior
        assertNotNull(first);
        assertNotNull(second);
        assertNotNull(third);
        assertNotNull(fourth);

        // Fourth should be same as first (cycling)
        assertEquals(first.getServiceId(), fourth.getServiceId());
    }

    @Test
    void shouldGetAvailableStrategies() {
        Map<String, LoadBalancingStrategy> strategies = loadBalancerService.getAvailableStrategies();
        
        assertNotNull(strategies);
        assertTrue(strategies.containsKey("ROUND_ROBIN"));
    }
}
