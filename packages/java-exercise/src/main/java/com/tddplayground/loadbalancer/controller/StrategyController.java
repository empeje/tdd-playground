package com.tddplayground.loadbalancer.controller;

import com.tddplayground.loadbalancer.service.LoadBalancerService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Controller for managing load balancing strategies.
 */
@RestController
@RequestMapping("/api/loadbalancer/strategy")
public class StrategyController {

    private final LoadBalancerService loadBalancerService;

    public StrategyController(LoadBalancerService loadBalancerService) {
        this.loadBalancerService = loadBalancerService;
    }

    /**
     * Get the current load balancing strategy.
     *
     * @return current strategy name
     */
    @GetMapping
    public ResponseEntity<Map<String, String>> getCurrentStrategy() {
        Map<String, String> response = new HashMap<>();
        response.put("strategy", loadBalancerService.getCurrentStrategyName());
        return ResponseEntity.ok(response);
    }

    /**
     * Set the load balancing strategy.
     *
     * @param request request containing strategy name
     * @return response with the updated strategy
     */
    @PutMapping
    public ResponseEntity<?> setStrategy(@RequestBody Map<String, String> request) {
        String strategyName = request.get("strategy");
        
        if (strategyName == null || strategyName.trim().isEmpty()) {
            Map<String, String> error = new HashMap<>();
            error.put("error", "Strategy name is required");
            return ResponseEntity.badRequest().body(error);
        }

        try {
            loadBalancerService.setStrategy(strategyName);
            Map<String, String> response = new HashMap<>();
            response.put("strategy", strategyName);
            response.put("message", "Strategy updated successfully");
            return ResponseEntity.ok(response);
        } catch (IllegalArgumentException e) {
            Map<String, String> error = new HashMap<>();
            error.put("error", e.getMessage());
            return ResponseEntity.badRequest().body(error);
        }
    }

    /**
     * Get all available load balancing strategies.
     *
     * @return list of available strategies
     */
    @GetMapping("/available")
    public ResponseEntity<Map<String, Object>> getAvailableStrategies() {
        List<String> strategies = loadBalancerService.getAvailableStrategies()
                .keySet()
                .stream()
                .sorted()
                .toList();
        
        Map<String, Object> response = new HashMap<>();
        response.put("strategies", strategies);
        response.put("current", loadBalancerService.getCurrentStrategyName());
        return ResponseEntity.ok(response);
    }
}
