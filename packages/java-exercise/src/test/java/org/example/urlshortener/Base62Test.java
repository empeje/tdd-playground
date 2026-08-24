package org.example.urlshortener;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class Base62Test {

    @Test
    public void test_encodeZero() {
        assertEquals("0", Base62.encode(0));
    }

    @Test
    public void test_encodePositiveNumbers() {
        assertEquals("1", Base62.encode(1));
        assertEquals("Z", Base62.encode(61));
        assertEquals("10", Base62.encode(62));
        assertEquals("g8", Base62.encode(1000));
    }

    @Test
    public void test_decodeBase62() {
        assertEquals(0L, Base62.decode("0"));
        assertEquals(1L, Base62.decode("1"));
        assertEquals(61L, Base62.decode("Z"));
        assertEquals(62L, Base62.decode("10"));
        assertEquals(1000L, Base62.decode("g8"));
    }

    @Test
    public void test_roundTrip() {
        long[] testValues = {0L, 1L, 42L, 999999L, 123456789012345L};
        for (long val : testValues) {
            String encoded = Base62.encode(val);
            long decoded = Base62.decode(encoded);
            assertEquals(val, decoded, "Failed round-trip for value " + val);
        }
    }

    @Test
    public void test_negativeNumberThrows() {
        assertThrows(IllegalArgumentException.class, () -> Base62.encode(-1));
    }

    @Test
    public void test_invalidCharactersThrow() {
        assertThrows(IllegalArgumentException.class, () -> Base62.decode("invalid-char!"));
        assertThrows(IllegalArgumentException.class, () -> Base62.decode(null));
        assertThrows(IllegalArgumentException.class, () -> Base62.decode(""));
    }
}
