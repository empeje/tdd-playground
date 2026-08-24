package org.example.urlshortener;

public class Base62 {
    private static final String CHARACTERS = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
    private static final int BASE = CHARACTERS.length();

    /**
     * Encode a non-negative long number to a Base62 string.
     */
    public static String encode(long number) {
        if (number < 0) {
            throw new IllegalArgumentException("Number must be non-negative");
        }
        if (number == 0) {
            return "0";
        }
        StringBuilder sb = new StringBuilder();
        while (number > 0) {
            int remainder = (int) (number % BASE);
            sb.append(CHARACTERS.charAt(remainder));
            number /= BASE;
        }
        return sb.reverse().toString();
    }

    /**
     * Decode a Base62 string back to a long.
     */
    public static long decode(String input) {
        if (input == null || input.isEmpty()) {
            throw new IllegalArgumentException("Input string cannot be null or empty");
        }
        long result = 0;
        for (int i = 0; i < input.length(); i++) {
            int value = CHARACTERS.indexOf(input.charAt(i));
            if (value == -1) {
                throw new IllegalArgumentException("Invalid character for Base62: " + input.charAt(i));
            }
            result = result * BASE + value;
        }
        return result;
    }
}
