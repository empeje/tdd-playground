package com.tddplayground.loadbalancer.controller;

import com.tddplayground.loadbalancer.model.ServiceInstance;
import com.tddplayground.loadbalancer.service.LoadBalancerService;
import jakarta.servlet.http.HttpServletRequest;
import org.springframework.http.*;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.HttpServerErrorException;
import org.springframework.web.client.RestTemplate;

import java.net.URI;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;

/**
 * Proxy controller that forwards requests to backend services
 * using the configured load balancing strategy.
 */
@RestController
@RequestMapping("/proxy")
public class ProxyController {

    private final LoadBalancerService loadBalancerService;
    private final RestTemplate restTemplate;

    public ProxyController(LoadBalancerService loadBalancerService) {
        this.loadBalancerService = loadBalancerService;
        this.restTemplate = new RestTemplate();
    }

    /**
     * Proxy all requests to the specified service.
     *
     * @param serviceName the name of the service to proxy to
     * @param request the HTTP request
     * @return the response from the backend service
     */
    @RequestMapping(value = "/{serviceName}/**", method = {
            RequestMethod.GET, RequestMethod.POST, RequestMethod.PUT,
            RequestMethod.DELETE, RequestMethod.PATCH
    })
    public ResponseEntity<?> proxyRequest(
            @PathVariable String serviceName,
            HttpServletRequest request,
            @RequestBody(required = false) String body) {

        // Select a service instance using load balancing
        ServiceInstance instance = loadBalancerService.selectInstance(serviceName);
        
        if (instance == null) {
            Map<String, String> error = new HashMap<>();
            error.put("error", "No healthy instances available for service: " + serviceName);
            return ResponseEntity.status(HttpStatus.SERVICE_UNAVAILABLE).body(error);
        }

        try {
            // Build the target URL
            String path = extractPath(request, serviceName);
            String targetUrl = instance.getUrl() + path;
            
            if (request.getQueryString() != null) {
                targetUrl += "?" + request.getQueryString();
            }

            // Copy headers
            HttpHeaders headers = new HttpHeaders();
            Enumeration<String> headerNames = request.getHeaderNames();
            while (headerNames.hasMoreElements()) {
                String headerName = headerNames.nextElement();
                // Skip host and content-length headers
                if (!headerName.equalsIgnoreCase("host") && 
                    !headerName.equalsIgnoreCase("content-length")) {
                    headers.add(headerName, request.getHeader(headerName));
                }
            }

            // Create request entity
            HttpEntity<String> requestEntity = new HttpEntity<>(body, headers);

            // Forward the request
            ResponseEntity<String> response = restTemplate.exchange(
                    URI.create(targetUrl),
                    HttpMethod.valueOf(request.getMethod()),
                    requestEntity,
                    String.class
            );

            return ResponseEntity.status(response.getStatusCode())
                    .headers(response.getHeaders())
                    .body(response.getBody());

        } catch (HttpClientErrorException | HttpServerErrorException e) {
            return ResponseEntity.status(e.getStatusCode())
                    .body(e.getResponseBodyAsString());
        } catch (Exception e) {
            Map<String, String> error = new HashMap<>();
            error.put("error", "Error forwarding request: " + e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_GATEWAY).body(error);
        }
    }

    /**
     * Extract the path after /proxy/{serviceName}
     */
    private String extractPath(HttpServletRequest request, String serviceName) {
        String requestPath = request.getRequestURI();
        String prefix = "/proxy/" + serviceName;
        
        if (requestPath.startsWith(prefix)) {
            String path = requestPath.substring(prefix.length());
            return path.isEmpty() ? "/" : path;
        }
        
        return "/";
    }
}
