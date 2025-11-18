package com.tddplayground.loadbalancer.strategy;

import com.tddplayground.loadbalancer.model.ServiceInstance;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Round-robin load balancing strategy.
 * Distributes requests evenly across all available instances.
 */
@Component
public class RoundRobinStrategy implements LoadBalancingStrategy {
    
    private final AtomicInteger counter = new AtomicInteger(0);
    
    @Override
    public ServiceInstance selectInstance(List<ServiceInstance> instances) {
        if (instances == null || instances.isEmpty()) {
            return null;
        }
        
        int index = counter.getAndIncrement() % instances.size();
        return instances.get(index);
    }
    
    @Override
    public String getStrategyName() {
        return "ROUND_ROBIN";
    }
}
