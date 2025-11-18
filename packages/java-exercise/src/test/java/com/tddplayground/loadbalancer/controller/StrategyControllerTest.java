package com.tddplayground.loadbalancer.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.tddplayground.loadbalancer.service.LoadBalancerService;
import com.tddplayground.loadbalancer.strategy.RoundRobinStrategy;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.MediaType;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.web.servlet.MockMvc;

import java.util.HashMap;
import java.util.Map;

import static org.hamcrest.Matchers.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

/**
 * Integration tests for StrategyController.
 */
@WebMvcTest(StrategyController.class)
@ContextConfiguration(classes = {StrategyController.class, StrategyControllerTest.TestConfig.class})
class StrategyControllerTest {

    @Configuration
    static class TestConfig {
        @Bean
        public LoadBalancerService loadBalancerService() {
            return new LoadBalancerService(
                    new com.tddplayground.loadbalancer.service.ServiceRegistry(),
                    new RoundRobinStrategy()
            );
        }
    }

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private LoadBalancerService loadBalancerService;

    @Autowired
    private ObjectMapper objectMapper;

    @BeforeEach
    void setUp() {
        // Reset to default strategy
        loadBalancerService.setStrategy("ROUND_ROBIN");
    }

    @Test
    void shouldGetCurrentStrategy() throws Exception {
        mockMvc.perform(get("/api/loadbalancer/strategy"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.strategy").value("ROUND_ROBIN"));
    }

    @Test
    void shouldSetStrategy() throws Exception {
        Map<String, String> request = new HashMap<>();
        request.put("strategy", "ROUND_ROBIN");

        mockMvc.perform(put("/api/loadbalancer/strategy")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(request)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.strategy").value("ROUND_ROBIN"))
                .andExpect(jsonPath("$.message").value("Strategy updated successfully"));
    }

    @Test
    void shouldReturnBadRequestForInvalidStrategy() throws Exception {
        Map<String, String> request = new HashMap<>();
        request.put("strategy", "INVALID_STRATEGY");

        mockMvc.perform(put("/api/loadbalancer/strategy")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(request)))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.error").value(containsString("Strategy not found")));
    }

    @Test
    void shouldReturnBadRequestForMissingStrategy() throws Exception {
        Map<String, String> request = new HashMap<>();

        mockMvc.perform(put("/api/loadbalancer/strategy")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(request)))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.error").value("Strategy name is required"));
    }

    @Test
    void shouldReturnBadRequestForEmptyStrategy() throws Exception {
        Map<String, String> request = new HashMap<>();
        request.put("strategy", "");

        mockMvc.perform(put("/api/loadbalancer/strategy")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(request)))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.error").value("Strategy name is required"));
    }

    @Test
    void shouldGetAvailableStrategies() throws Exception {
        mockMvc.perform(get("/api/loadbalancer/strategy/available"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.strategies").isArray())
                .andExpect(jsonPath("$.strategies", hasItem("ROUND_ROBIN")))
                .andExpect(jsonPath("$.current").value("ROUND_ROBIN"));
    }
}
