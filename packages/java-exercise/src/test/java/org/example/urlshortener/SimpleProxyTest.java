package org.example.urlshortener;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

public class SimpleProxyTest {

    @Test
    public void test_proxyHandlerRoundRobin() {
        List<String> backends = List.of("http://backend1:8080", "http://backend2:8080", "http://backend3:8080");
        SimpleProxy.ProxyHandler handler = new SimpleProxy.ProxyHandler(backends);

        assertEquals("http://backend1:8080", handler.getNextBackend());
        assertEquals("http://backend2:8080", handler.getNextBackend());
        assertEquals("http://backend3:8080", handler.getNextBackend());
        assertEquals("http://backend1:8080", handler.getNextBackend());
    }

    @Test
    public void test_emptyBackendsThrows() {
        assertThrows(IllegalArgumentException.class, () -> new SimpleProxy.ProxyHandler(List.of()));
        assertThrows(IllegalArgumentException.class, () -> new SimpleProxy.ProxyHandler(null));
    }
}
