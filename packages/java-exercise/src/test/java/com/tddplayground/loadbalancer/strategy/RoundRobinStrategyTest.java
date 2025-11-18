package com.tddplayground.loadbalancer.strategy;

import com.tddplayground.loadbalancer.model.ServiceInstance;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for RoundRobinStrategy.
 */
class RoundRobinStrategyTest {

    private RoundRobinStrategy strategy;
    private List<ServiceInstance> instances;

    @BeforeEach
    void setUp() {
        strategy = new RoundRobinStrategy();
        instances = Arrays.asList(
                new ServiceInstance("service-1", "user-service", "localhost", 8081),
                new ServiceInstance("service-2", "user-service", "localhost", 8082),
                new ServiceInstance("service-3", "user-service", "localhost", 8083)
        );
    }

    @Test
    void shouldReturnNullForEmptyList() {
        ServiceInstance selected = strategy.selectInstance(new ArrayList<>());
        assertNull(selected);
    }

    @Test
    void shouldReturnNullForNullList() {
        ServiceInstance selected = strategy.selectInstance(null);
        assertNull(selected);
    }

    @Test
    void shouldReturnSingleInstanceWhenOnlyOneAvailable() {
        List<ServiceInstance> singleInstance = List.of(instances.get(0));
        
        ServiceInstance selected = strategy.selectInstance(singleInstance);
        
        assertEquals("service-1", selected.getServiceId());
    }

    @Test
    void shouldDistributeRequestsInRoundRobinFashion() {
        // First request should go to first instance
        ServiceInstance first = strategy.selectInstance(instances);
        assertEquals("service-1", first.getServiceId());

        // Second request should go to second instance
        ServiceInstance second = strategy.selectInstance(instances);
        assertEquals("service-2", second.getServiceId());

        // Third request should go to third instance
        ServiceInstance third = strategy.selectInstance(instances);
        assertEquals("service-3", third.getServiceId());

        // Fourth request should go back to first instance
        ServiceInstance fourth = strategy.selectInstance(instances);
        assertEquals("service-1", fourth.getServiceId());
    }

    @Test
    void shouldHandleMultipleRounds() {
        for (int i = 0; i < 10; i++) {
            ServiceInstance selected = strategy.selectInstance(instances);
            assertNotNull(selected);
            
            // Verify it cycles through instances
            String expectedId = "service-" + (i % 3 + 1);
            assertEquals(expectedId, selected.getServiceId());
        }
    }

    @Test
    void shouldReturnCorrectStrategyName() {
        assertEquals("ROUND_ROBIN", strategy.getStrategyName());
    }
}
