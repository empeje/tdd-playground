package org.example.loadbalancer;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.random.RandomGenerator;

public class InstanceRegistration {
    public static final int MAX_CAPACITY = 10;

    private final Map<String, Instance> instanceStorage = new LinkedHashMap<>();
    private final RandomGenerator randomGenerator;

    public InstanceRegistration(RandomGenerator randomGenerator) {
        if (randomGenerator == null) {
            throw new IllegalArgumentException("RandomGenerator cannot be null");
        }
        this.randomGenerator = randomGenerator;
    }

    public void registerInstance(Instance instance) throws RuntimeException {
        if (instance == null) {
            throw new IllegalArgumentException("Instance cannot be null");
        }
        if (instanceStorage.size() >= MAX_CAPACITY) {
            throw new RuntimeException("Address can't be more than 10, remove if you want to add more");
        }
        if (instanceStorage.containsKey(instance.address())) {
            throw new RuntimeException("Address can't be the same");
        }
        instanceStorage.put(instance.address(), instance);
    }

    public Instance getInstance(String address) {
        return instanceStorage.get(address);
    }

    public Instance getInstance() {
        if (instanceStorage.isEmpty()) {
            return null;
        }
        List<String> addresses = new ArrayList<>(instanceStorage.keySet());
        int randomIndex = randomGenerator.nextInt(addresses.size());
        String selectedAddress = addresses.get(randomIndex);
        return instanceStorage.get(selectedAddress);
    }

    public boolean removeInstance(String address) {
        return instanceStorage.remove(address) != null;
    }

    public int size() {
        return instanceStorage.size();
    }

    public List<Instance> getAllInstances() {
        return Collections.unmodifiableList(new ArrayList<>(instanceStorage.values()));
    }

    public void showAllInstances() {
        System.out.println(getAllInstances());
    }
}
