package com.tddplayground.loadbalancer.strategy;

import com.tddplayground.loadbalancer.model.ServiceInstance;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Random;

/**
 * Random load balancing strategy.
 * Randomly selects an instance from the available instances.
 */
@Component
public class RandomStrategy implements LoadBalancingStrategy {
    
    private final Random random = new Random();
    
    @Override
    public ServiceInstance selectInstance(List<ServiceInstance> instances) {
        if (instances == null || instances.isEmpty()) {
            return null;
        }
        
        int index = random.nextInt(instances.size());
        return instances.get(index);
    }
    
    @Override
    public String getStrategyName() {
        return "RANDOM";
    }
}
