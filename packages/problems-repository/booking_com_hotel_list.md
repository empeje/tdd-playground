# Booking.com Hotel List Problem

## Problem Description

Given a dataset of hotel rooms availability across different dates, find valid booking combinations that satisfy user requirements.

### Input Data Structure

The input consists of two parts:

1. Hotel Data Map:
```json
{
    "176": [
        {
            "price": 120,
            "features": ["breakfast", "refundable"],
            "availability": 5
        }
    ],
    "177": [
        {
            "price": 130,
            "features": ["breakfast"],
            "availability": 1
        },
        {
            "price": 140,
            "features": ["breakfast", "refundable", "wifi"],
            "availability": 3
        }
    ],
    "178": [
        {
            "price": 130,
            "features": ["breakfast"],
            "availability": 2
        },
        {
            "price": 140,
            "features": ["breakfast", "refundable", "wifi"],
            "availability": 10
        }
    ]
}
```

2. User Query:
```json
{
    "checkin": 176,
    "checkout": 178,
    "features": ["breakfast"],
    "rooms": 1
}
```

### Problem Breakdown

1. The hotel data map shows:
   - Keys are dates (176, 177, 178)
   - Each date has an array of room types
   - Each room type has:
     - price (per night)
     - features (array of amenities)
     - availability (number of rooms available)

2. The user query asks for:
   - A stay from date 176 to 178 (3 nights)
   - Rooms must have breakfast
   - Need 1 room for the entire stay

3. The output shows possible combinations where:
   - Each combination is valid for all nights
   - Total price is sum of all nights
   - Features are the common features across all nights
   - Availability is the minimum availability across all nights

### Solution Approach

1. **Find Valid Room Combinations**:
   - For each date between checkin and checkout
   - Filter rooms that have required features
   - Generate all possible combinations

2. **Calculate Total Price**:
   - Sum up prices for each night in the combination

3. **Calculate Common Features**:
   - Find intersection of features across all nights

4. **Calculate Availability**:
   - Find minimum availability across all nights

### Example Solutions

1. [Python Implementation](../python-exercise/python_exercise/hotel_list.py)
   - Type-safe implementation using TypedDict
   - Efficient use of itertools.product
   - Comprehensive test suite
   - Modular design with helper functions

2. TypeScript Solution:
```typescript
interface RoomType {
    price: number;
    features: string[];
    availability: number;
}

interface HotelData {
    [date: string]: RoomType[];
}

interface UserQuery {
    checkin: number;
    checkout: number;
    features: string[];
    rooms: number;
}

function findValidBookings(
    hotelData: HotelData, 
    query: UserQuery
): RoomType[] {
    // Get all dates in the range
    const dates = [];
    for (let d = query.checkin; d <= query.checkout; d++) {
        dates.push(d.toString());
    }
    
    // Filter rooms with required features for each date
    const validRoomsPerDate = dates.map(date => 
        hotelData[date].filter(room => 
            query.features.every(f => room.features.includes(f))
        )
    );
    
    // Find valid combinations
    const combinations = findCombinations(validRoomsPerDate);
    
    // Calculate results
    return combinations.map(combo => ({
        price: combo.reduce((sum, room) => sum + room.price, 0),
        features: findCommonFeatures(combo),
        availability: Math.min(...combo.map(room => room.availability))
    }));
}

// Helper functions would be needed for:
// - findCombinations
// - findCommonFeatures
```

### Time Complexity

- Let n be the number of dates
- Let m be the average number of room types per date
- Let f be the number of features

The time complexity would be O(m^n) in worst case because we need to check all possible combinations.

### Space Complexity

O(m^n) to store all possible combinations in worst case.

### Interview Tips

1. **Clarifying Questions to Ask**:
   - Can dates be non-consecutive?
   - How to handle unavailable dates?
   - Should we consider check-out date in price calculation?
   - How to handle feature matching (exact vs partial)?

2. **Edge Cases to Consider**:
   - No available rooms
   - No rooms with required features
   - Dates not in sequence
   - Invalid date ranges

3. **Optimization Ideas**:
   - Early filtering of invalid rooms
   - Caching common feature calculations
   - Using bit manipulation for feature matching
   - Pruning invalid combinations early

## Follow-up Questions

1. How would you modify the solution if:
   - Dates can be non-consecutive?
   - Multiple room types can be combined?
   - Prices vary by occupancy?
   - Need to consider cancellation policies?

2. System Design Considerations:
   - How to handle concurrent bookings?
   - How to scale for millions of hotels?
   - How to handle price updates?
   - How to implement caching? 