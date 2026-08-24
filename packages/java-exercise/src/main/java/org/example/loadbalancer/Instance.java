package org.example.loadbalancer;

/**
 * Represents a backend server instance.
 *
 * @param address Unique network identifier (FQDN or IP address)
 * @param name Descriptive instance name
 */
public record Instance(String address, String name) {
    public Instance {
        if (address == null || address.isBlank()) {
            throw new IllegalArgumentException("Instance address cannot be null or blank");
        }
        if (name == null || name.isBlank()) {
            throw new IllegalArgumentException("Instance name cannot be null or blank");
        }
    }
}
