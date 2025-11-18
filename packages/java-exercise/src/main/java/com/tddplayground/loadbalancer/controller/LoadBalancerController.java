package com.tddplayground.loadbalancer.controller;

import com.tddplayground.loadbalancer.model.RegisterServiceRequest;
import com.tddplayground.loadbalancer.model.ServiceInstance;
import com.tddplayground.loadbalancer.service.ServiceRegistry;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * REST Controller for the Load Balancer service registry.
 * Provides endpoints for managing service instances.
 */
@RestController
@RequestMapping("/api/loadbalancer")
public class LoadBalancerController {

    private final ServiceRegistry serviceRegistry;

    public LoadBalancerController(ServiceRegistry serviceRegistry) {
        this.serviceRegistry = serviceRegistry;
    }

    /**
     * Register a new service instance.
     *
     * @param request the registration request
     * @return the registered service instance
     */
    @PostMapping("/services")
    public ResponseEntity<?> registerService(@Valid @RequestBody RegisterServiceRequest request) {
        try {
            ServiceInstance serviceInstance = new ServiceInstance(
                    request.getServiceId(),
                    request.getServiceName(),
                    request.getHost(),
                    request.getPort()
            );
            serviceInstance.setProtocol(request.getProtocol());
            serviceInstance.setHealthCheckPath(request.getHealthCheckPath());

            ServiceInstance registered = serviceRegistry.registerService(serviceInstance);
            return ResponseEntity.status(HttpStatus.CREATED).body(registered);
        } catch (IllegalArgumentException e) {
            Map<String, String> error = new HashMap<>();
            error.put("error", e.getMessage());
            return ResponseEntity.status(HttpStatus.CONFLICT).body(error);
        }
    }

    /**
     * Deregister a service instance.
     *
     * @param serviceId the ID of the service to deregister
     * @return response indicating success or failure
     */
    @DeleteMapping("/services/{serviceId}")
    public ResponseEntity<?> deregisterService(@PathVariable String serviceId) {
        ServiceInstance deregistered = serviceRegistry.deregisterService(serviceId);
        if (deregistered == null) {
            Map<String, String> error = new HashMap<>();
            error.put("error", "Service with ID " + serviceId + " not found");
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(error);
        }
        return ResponseEntity.ok(deregistered);
    }

    /**
     * Get a service instance by its ID.
     *
     * @param serviceId the ID of the service
     * @return the service instance
     */
    @GetMapping("/services/{serviceId}")
    public ResponseEntity<?> getService(@PathVariable String serviceId) {
        ServiceInstance service = serviceRegistry.getService(serviceId);
        if (service == null) {
            Map<String, String> error = new HashMap<>();
            error.put("error", "Service with ID " + serviceId + " not found");
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(error);
        }
        return ResponseEntity.ok(service);
    }

    /**
     * Get all registered service instances.
     *
     * @return a list of all service instances
     */
    @GetMapping("/services")
    public ResponseEntity<List<ServiceInstance>> getAllServices() {
        return ResponseEntity.ok(serviceRegistry.getAllServices());
    }

    /**
     * Get all service instances for a specific service name.
     *
     * @param serviceName the name of the service
     * @return a list of service instances
     */
    @GetMapping("/services/by-name/{serviceName}")
    public ResponseEntity<List<ServiceInstance>> getServicesByName(@PathVariable String serviceName) {
        return ResponseEntity.ok(serviceRegistry.getServicesByName(serviceName));
    }

    /**
     * Get all healthy service instances for a specific service name.
     *
     * @param serviceName the name of the service
     * @return a list of healthy service instances
     */
    @GetMapping("/services/by-name/{serviceName}/healthy")
    public ResponseEntity<List<ServiceInstance>> getHealthyServicesByName(@PathVariable String serviceName) {
        return ResponseEntity.ok(serviceRegistry.getHealthyServicesByName(serviceName));
    }
}
