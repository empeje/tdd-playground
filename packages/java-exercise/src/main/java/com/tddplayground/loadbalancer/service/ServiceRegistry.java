package com.tddplayground.loadbalancer.service;

import com.tddplayground.loadbalancer.model.ServiceInstance;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * Service Registry that manages registered service instances.
 * This is the core component of the load balancer.
 */
@Service
public class ServiceRegistry {

    private final Map<String, ServiceInstance> serviceInstances = new ConcurrentHashMap<>();

    /**
     * Register a new service instance.
     *
     * @param serviceInstance the service instance to register
     * @return the registered service instance
     * @throws IllegalArgumentException if a service with the same ID already exists
     */
    public ServiceInstance registerService(ServiceInstance serviceInstance) {
        if (serviceInstance == null) {
            throw new IllegalArgumentException("Service instance cannot be null");
        }

        if (serviceInstances.containsKey(serviceInstance.getServiceId())) {
            throw new IllegalArgumentException("Service with ID " + serviceInstance.getServiceId() + " is already registered");
        }

        serviceInstances.put(serviceInstance.getServiceId(), serviceInstance);
        return serviceInstance;
    }

    /**
     * Deregister a service instance by its ID.
     *
     * @param serviceId the ID of the service to deregister
     * @return the deregistered service instance, or null if not found
     */
    public ServiceInstance deregisterService(String serviceId) {
        return serviceInstances.remove(serviceId);
    }

    /**
     * Get a service instance by its ID.
     *
     * @param serviceId the ID of the service
     * @return the service instance, or null if not found
     */
    public ServiceInstance getService(String serviceId) {
        return serviceInstances.get(serviceId);
    }

    /**
     * Get all registered service instances.
     *
     * @return a list of all registered service instances
     */
    public List<ServiceInstance> getAllServices() {
        return new ArrayList<>(serviceInstances.values());
    }

    /**
     * Get all service instances for a specific service name.
     *
     * @param serviceName the name of the service
     * @return a list of service instances with the given name
     */
    public List<ServiceInstance> getServicesByName(String serviceName) {
        return serviceInstances.values().stream()
                .filter(instance -> instance.getServiceName().equals(serviceName))
                .collect(Collectors.toList());
    }

    /**
     * Get all healthy service instances for a specific service name.
     *
     * @param serviceName the name of the service
     * @return a list of healthy service instances with the given name
     */
    public List<ServiceInstance> getHealthyServicesByName(String serviceName) {
        return serviceInstances.values().stream()
                .filter(instance -> instance.getServiceName().equals(serviceName))
                .filter(ServiceInstance::isHealthy)
                .collect(Collectors.toList());
    }

    /**
     * Get the count of registered services.
     *
     * @return the number of registered services
     */
    public int getServiceCount() {
        return serviceInstances.size();
    }

    /**
     * Clear all registered services.
     */
    public void clear() {
        serviceInstances.clear();
    }
}
