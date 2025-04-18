# Booking.com Closest Hotels

## Problem Description

You are tasked with implementing a system to find the K closest hotels from a given starting point for Booking.com's hotel search feature. The system should efficiently process hotel locations and return the nearest hotels based on geographical distance.

### Input Format

The system receives:
1. A list of hotels with their coordinates and metadata
2. A starting point (latitude, longitude)
3. An integer K representing the number of closest hotels to find

```json
{
  "hotels": [
    {
      "hotel_id": "H1",
      "name": "Grand Hotel",
      "latitude": 52.3676,
      "longitude": 4.9041,
      "rating": 4.5,
      "price_per_night": 150.00,
      "amenities": ["wifi", "pool", "gym"]
    },
    {
      "hotel_id": "H2",
      "name": "Seaside Resort",
      "latitude": 52.3702,
      "longitude": 4.8952,
      "rating": 4.2,
      "price_per_night": 200.00,
      "amenities": ["wifi", "beach", "restaurant"]
    }
    // ... more hotels
  ],
  "start_point": {
    "latitude": 52.3670,
    "longitude": 4.9010
  },
  "k": 5
}
```

### Requirements

1. Implement a function that returns the K closest hotels to the starting point, sorted by distance.
2. The solution should be efficient for large datasets (millions of hotels).
3. Consider additional factors that might affect hotel selection:
   - Hotel rating
   - Price per night
   - Available amenities
   - Current availability

4. Handle edge cases:
   - Multiple hotels at the same distance
   - Invalid coordinates
   - Empty hotel list
   - K larger than the number of available hotels
   - Hotels with missing or invalid data

### Additional Scenarios to Consider

1. How would you handle real-time updates to hotel availability?
2. What if users want to filter results by price range or amenities?
3. How would you implement caching for frequently searched locations?
4. How would you handle different distance metrics (e.g., walking distance vs. straight-line distance)?
5. What if you need to consider traffic conditions or transportation options?

### Technical Requirements

1. Write code that efficiently finds the K closest hotels
2. Explain the runtime complexity of your solution
3. Suggest optimizations for handling large datasets
4. Consider how to make the system scalable for future requirements

### Example Output

```json
{
  "closest_hotels": [
    {
      "hotel_id": "H1",
      "name": "Grand Hotel",
      "distance": 0.3,  // in kilometers
      "rating": 4.5,
      "price_per_night": 150.00,
      "amenities": ["wifi", "pool", "gym"]
    },
    {
      "hotel_id": "H2",
      "name": "Seaside Resort",
      "distance": 0.5,
      "rating": 4.2,
      "price_per_night": 200.00,
      "amenities": ["wifi", "beach", "restaurant"]
    }
    // ... more hotels up to K
  ],
  "search_metadata": {
    "total_hotels_considered": 1000,
    "search_time_ms": 45,
    "radius_covered_km": 2.5
  }
}
```

### Evaluation Criteria

1. Algorithm efficiency and correctness
2. Code quality and organization
3. Handling of edge cases
4. Runtime complexity analysis
5. Scalability considerations
6. Additional features implemented
7. Problem-solving approach

### Notes

- The Earth's curvature should be considered when calculating distances
- The solution should be optimized for both time and space complexity
- Consider using appropriate data structures for efficient nearest neighbor searches
- The system should be able to handle frequent updates to hotel data
- Think about how to make the solution maintainable and extensible

### Source

This problem is based on a real Booking.com interview experience:
https://leetcode.com/discuss/post/1395715/bookingcom-backend-developer-amsterdam-j-cm4f/ 