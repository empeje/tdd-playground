package com.tddplayground.loadbalancer.strategy;

import com.tddplayground.loadbalancer.model.ServiceInstance;

import java.util.List;

/**
 * Strategy interface for load balancing algorithms.
 */
public interface LoadBalancingStrategy {
    
    /**
     * Select a service instance from the list of available instances.
     *
     * @param instances list of available service instances
     * @return selected service instance, or null if no instances available
     */
    ServiceInstance selectInstance(List<ServiceInstance> instances);
    
    /**
     * Get the name of the load balancing strategy.
     *
     * @return strategy name
     */
    String getStrategyName();
}
