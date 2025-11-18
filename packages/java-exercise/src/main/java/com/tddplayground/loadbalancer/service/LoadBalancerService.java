package com.tddplayground.loadbalancer.service;

import com.tddplayground.loadbalancer.model.ServiceInstance;
import com.tddplayground.loadbalancer.strategy.LoadBalancingStrategy;
import com.tddplayground.loadbalancer.strategy.RoundRobinStrategy;
import org.springframework.stereotype.Service;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Load Balancer service that distributes requests across service instances
 * using configurable load balancing strategies.
 */
@Service
public class LoadBalancerService {

    private final ServiceRegistry serviceRegistry;
    private final Map<String, LoadBalancingStrategy> strategies = new HashMap<>();
    private LoadBalancingStrategy currentStrategy;

    public LoadBalancerService(ServiceRegistry serviceRegistry, 
                               RoundRobinStrategy roundRobinStrategy) {
        this.serviceRegistry = serviceRegistry;
        this.strategies.put("ROUND_ROBIN", roundRobinStrategy);
        this.currentStrategy = roundRobinStrategy; // Default strategy
    }

    /**
     * Register a load balancing strategy.
     *
     * @param strategy the strategy to register
     */
    public void registerStrategy(LoadBalancingStrategy strategy) {
        strategies.put(strategy.getStrategyName(), strategy);
    }

    /**
     * Set the current load balancing strategy.
     *
     * @param strategyName name of the strategy to use
     * @throws IllegalArgumentException if strategy not found
     */
    public void setStrategy(String strategyName) {
        LoadBalancingStrategy strategy = strategies.get(strategyName);
        if (strategy == null) {
            throw new IllegalArgumentException("Strategy not found: " + strategyName);
        }
        this.currentStrategy = strategy;
    }

    /**
     * Get the current strategy name.
     *
     * @return current strategy name
     */
    public String getCurrentStrategyName() {
        return currentStrategy.getStrategyName();
    }

    /**
     * Select a service instance for a given service name using the current strategy.
     *
     * @param serviceName the name of the service
     * @return selected service instance, or null if none available
     */
    public ServiceInstance selectInstance(String serviceName) {
        List<ServiceInstance> healthyInstances = serviceRegistry.getHealthyServicesByName(serviceName);
        return currentStrategy.selectInstance(healthyInstances);
    }

    /**
     * Get all available strategies.
     *
     * @return map of strategy names to strategies
     */
    public Map<String, LoadBalancingStrategy> getAvailableStrategies() {
        return new HashMap<>(strategies);
    }
}
