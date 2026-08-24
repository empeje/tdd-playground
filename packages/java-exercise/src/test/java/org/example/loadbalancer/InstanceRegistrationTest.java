package org.example.loadbalancer;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Random;
import java.util.random.RandomGenerator;

import static org.junit.jupiter.api.Assertions.*;

public class InstanceRegistrationTest {

    private InstanceRegistration instanceRegistration;

    @BeforeEach
    public void setUp() {
        instanceRegistration = new InstanceRegistration(new Random(42));
    }

    @Test
    public void test_instanceRegistrationAndLookup() {
        Instance inst1 = new Instance("revolut-website1.revolut.com", "revolut1");
        Instance inst2 = new Instance("revolut-website2.revolut.com", "revolut2");

        instanceRegistration.registerInstance(inst1);
        instanceRegistration.registerInstance(inst2);

        assertEquals(inst1, instanceRegistration.getInstance("revolut-website1.revolut.com"));
        assertEquals(inst2, instanceRegistration.getInstance("revolut-website2.revolut.com"));
        assertEquals(2, instanceRegistration.size());
    }

    @Test
    public void test_addressShouldBeUniqueRejectIfAlreadyRegistered() {
        instanceRegistration.registerInstance(new Instance("revolut-website1.revolut.com", "revolut1"));

        RuntimeException exception = assertThrows(RuntimeException.class, () -> {
            instanceRegistration.registerInstance(new Instance("revolut-website1.revolut.com", "revolut1-duplicate"));
        });
        assertEquals("Address can't be the same", exception.getMessage());
    }

    @Test
    public void test_addressShouldNotBeMoreThan10() {
        for (int i = 1; i <= 10; i++) {
            instanceRegistration.registerInstance(new Instance("revolut-website" + i + ".revolut.com", "revolut" + i));
        }

        assertEquals(10, instanceRegistration.size());

        RuntimeException exception = assertThrows(RuntimeException.class, () -> {
            instanceRegistration.registerInstance(new Instance("revolut-website11.revolut.com", "revolut11"));
        });
        assertEquals("Address can't be more than 10, remove if you want to add more", exception.getMessage());
    }

    @Test
    public void test_shouldPickAddressRandomlyFromLoadBalancer() {
        // Deterministic mock RandomGenerator returning sequence: 0, 2, 1
        class MockRandomGenerator implements RandomGenerator {
            private int callCount = 0;
            private final int[] sequence = {0, 2, 1};

            @Override
            public int nextInt(int bound) {
                int val = sequence[callCount % sequence.length];
                callCount++;
                return val % bound;
            }

            @Override
            public long nextLong() {
                return 0;
            }
        }

        InstanceRegistration lb = new InstanceRegistration(new MockRandomGenerator());
        Instance inst1 = new Instance("revolut-website1.revolut.com", "revolut1");
        Instance inst2 = new Instance("revolut-website2.revolut.com", "revolut2");
        Instance inst3 = new Instance("revolut-website3.revolut.com", "revolut3");

        lb.registerInstance(inst1);
        lb.registerInstance(inst2);
        lb.registerInstance(inst3);

        assertEquals(inst1, lb.getInstance());
        assertEquals(inst3, lb.getInstance());
        assertEquals(inst2, lb.getInstance());
    }

    @Test
    public void test_emptyLoadBalancerReturnsNull() {
        assertNull(instanceRegistration.getInstance());
        assertNull(instanceRegistration.getInstance("non-existent"));
    }

    @Test
    public void test_removeInstance() {
        Instance inst = new Instance("revolut-website1.revolut.com", "revolut1");
        instanceRegistration.registerInstance(inst);
        assertEquals(1, instanceRegistration.size());

        assertTrue(instanceRegistration.removeInstance("revolut-website1.revolut.com"));
        assertEquals(0, instanceRegistration.size());
        assertNull(instanceRegistration.getInstance("revolut-website1.revolut.com"));
    }
}
