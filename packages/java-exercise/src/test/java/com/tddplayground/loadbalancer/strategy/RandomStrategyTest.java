package com.tddplayground.loadbalancer.strategy;

import com.tddplayground.loadbalancer.model.ServiceInstance;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for RandomStrategy.
 */
class RandomStrategyTest {

    private RandomStrategy strategy;
    private List<ServiceInstance> instances;

    @BeforeEach
    void setUp() {
        strategy = new RandomStrategy();
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
    void shouldSelectFromAvailableInstances() {
        ServiceInstance selected = strategy.selectInstance(instances);
        
        assertNotNull(selected);
        assertTrue(instances.contains(selected), "Selected instance should be from the available instances");
    }

    @Test
    void shouldEventuallySelectAllInstances() {
        // Over multiple selections, all instances should be selected at least once
        Set<String> selectedIds = new HashSet<>();
        
        for (int i = 0; i < 100; i++) {
            ServiceInstance selected = strategy.selectInstance(instances);
            selectedIds.add(selected.getServiceId());
        }
        
        // With 100 random selections from 3 instances, we should hit all of them
        assertEquals(3, selectedIds.size(), "All instances should be selected at least once");
        assertTrue(selectedIds.contains("service-1"));
        assertTrue(selectedIds.contains("service-2"));
        assertTrue(selectedIds.contains("service-3"));
    }

    @Test
    void shouldReturnCorrectStrategyName() {
        assertEquals("RANDOM", strategy.getStrategyName());
    }
}
