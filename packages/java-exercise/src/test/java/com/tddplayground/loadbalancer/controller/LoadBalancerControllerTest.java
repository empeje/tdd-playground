package com.tddplayground.loadbalancer.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.tddplayground.loadbalancer.model.RegisterServiceRequest;
import com.tddplayground.loadbalancer.service.ServiceRegistry;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.MediaType;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.web.servlet.MockMvc;

import static org.hamcrest.Matchers.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

/**
 * Integration tests for LoadBalancerController.
 */
@WebMvcTest(LoadBalancerController.class)
@ContextConfiguration(classes = {LoadBalancerController.class, LoadBalancerControllerTest.TestConfig.class})
class LoadBalancerControllerTest {

    @Configuration
    static class TestConfig {
        @Bean
        public ServiceRegistry serviceRegistry() {
            return new ServiceRegistry();
        }
    }

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ServiceRegistry serviceRegistry;

    @Autowired
    private ObjectMapper objectMapper;

    @BeforeEach
    void setUp() {
        serviceRegistry.clear();
    }

    @Test
    void shouldRegisterServiceSuccessfully() throws Exception {
        // Given
        RegisterServiceRequest request = new RegisterServiceRequest(
                "service-1",
                "user-service",
                "localhost",
                8081
        );

        // When & Then
        mockMvc.perform(post("/api/loadbalancer/services")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(request)))
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.serviceId").value("service-1"))
                .andExpect(jsonPath("$.serviceName").value("user-service"))
                .andExpect(jsonPath("$.host").value("localhost"))
                .andExpect(jsonPath("$.port").value(8081))
                .andExpect(jsonPath("$.protocol").value("http"))
                .andExpect(jsonPath("$.healthy").value(true));
    }

    @Test
    void shouldReturnConflictWhenRegisteringDuplicateService() throws Exception {
        // Given
        RegisterServiceRequest request = new RegisterServiceRequest(
                "service-1",
                "user-service",
                "localhost",
                8081
        );

        // Register first time
        mockMvc.perform(post("/api/loadbalancer/services")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)));

        // When & Then - Try to register again
        mockMvc.perform(post("/api/loadbalancer/services")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(request)))
                .andExpect(status().isConflict())
                .andExpect(jsonPath("$.error").value(containsString("already registered")));
    }

    @Test
    void shouldReturnBadRequestWhenRegisteringInvalidService() throws Exception {
        // Given - Missing required fields
        String invalidRequest = "{}";

        // When & Then
        mockMvc.perform(post("/api/loadbalancer/services")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(invalidRequest))
                .andExpect(status().isBadRequest());
    }

    @Test
    void shouldDeregisterServiceSuccessfully() throws Exception {
        // Given - Register a service first
        RegisterServiceRequest request = new RegisterServiceRequest(
                "service-1",
                "user-service",
                "localhost",
                8081
        );
        mockMvc.perform(post("/api/loadbalancer/services")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)));

        // When & Then
        mockMvc.perform(delete("/api/loadbalancer/services/service-1"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.serviceId").value("service-1"));
    }

    @Test
    void shouldReturnNotFoundWhenDeregisteringNonExistentService() throws Exception {
        // When & Then
        mockMvc.perform(delete("/api/loadbalancer/services/non-existent"))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.error").value(containsString("not found")));
    }

    @Test
    void shouldGetServiceById() throws Exception {
        // Given - Register a service first
        RegisterServiceRequest request = new RegisterServiceRequest(
                "service-1",
                "user-service",
                "localhost",
                8081
        );
        mockMvc.perform(post("/api/loadbalancer/services")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)));

        // When & Then
        mockMvc.perform(get("/api/loadbalancer/services/service-1"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.serviceId").value("service-1"))
                .andExpect(jsonPath("$.serviceName").value("user-service"));
    }

    @Test
    void shouldReturnNotFoundWhenGettingNonExistentService() throws Exception {
        // When & Then
        mockMvc.perform(get("/api/loadbalancer/services/non-existent"))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.error").value(containsString("not found")));
    }

    @Test
    void shouldGetAllServices() throws Exception {
        // Given - Register multiple services
        RegisterServiceRequest request1 = new RegisterServiceRequest("service-1", "user-service", "localhost", 8081);
        RegisterServiceRequest request2 = new RegisterServiceRequest("service-2", "order-service", "localhost", 8082);

        mockMvc.perform(post("/api/loadbalancer/services")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request1)));
        mockMvc.perform(post("/api/loadbalancer/services")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request2)));

        // When & Then
        mockMvc.perform(get("/api/loadbalancer/services"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$", hasSize(2)))
                .andExpect(jsonPath("$[*].serviceId", containsInAnyOrder("service-1", "service-2")));
    }

    @Test
    void shouldGetServicesByName() throws Exception {
        // Given - Register multiple services with same name
        RegisterServiceRequest request1 = new RegisterServiceRequest("service-1", "user-service", "localhost", 8081);
        RegisterServiceRequest request2 = new RegisterServiceRequest("service-2", "user-service", "localhost", 8082);
        RegisterServiceRequest request3 = new RegisterServiceRequest("service-3", "order-service", "localhost", 8083);

        mockMvc.perform(post("/api/loadbalancer/services")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request1)));
        mockMvc.perform(post("/api/loadbalancer/services")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request2)));
        mockMvc.perform(post("/api/loadbalancer/services")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request3)));

        // When & Then
        mockMvc.perform(get("/api/loadbalancer/services/by-name/user-service"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$", hasSize(2)))
                .andExpect(jsonPath("$[*].serviceName", everyItem(is("user-service"))));
    }

    @Test
    void shouldGetHealthyServicesByName() throws Exception {
        // Given - Register services
        RegisterServiceRequest request1 = new RegisterServiceRequest("service-1", "user-service", "localhost", 8081);
        RegisterServiceRequest request2 = new RegisterServiceRequest("service-2", "user-service", "localhost", 8082);

        mockMvc.perform(post("/api/loadbalancer/services")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request1)));
        mockMvc.perform(post("/api/loadbalancer/services")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request2)));

        // When & Then
        mockMvc.perform(get("/api/loadbalancer/services/by-name/user-service/healthy"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$", hasSize(2)))
                .andExpect(jsonPath("$[*].healthy", everyItem(is(true))));
    }
}
