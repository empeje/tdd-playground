from typing import Dict, List, TypedDict
from itertools import product


class RoomType(TypedDict):
    price: int
    features: List[str]
    availability: int


class UserQuery(TypedDict):
    checkin: int
    checkout: int
    features: List[str]
    rooms: int


def find_common_features(rooms: List[RoomType]) -> List[str]:
    """Find common features across all rooms."""
    if not rooms:
        return []
    common = set(rooms[0]['features'])
    for room in rooms[1:]:
        common.intersection_update(room['features'])
    return list(common)


def find_combinations(rooms_per_date: List[List[RoomType]]) -> List[List[RoomType]]:
    """Generate all possible combinations of rooms across dates."""
    if not rooms_per_date:
        return []
    # Use itertools.product to generate all possible combinations
    return list(product(*rooms_per_date))


def find_valid_bookings(
    hotel_data: Dict[str, List[RoomType]],
    query: UserQuery
) -> List[RoomType]:
    """
    Find valid booking combinations that satisfy the query requirements.
    
    Args:
        hotel_data: Dictionary mapping dates to list of available rooms
        query: User requirements including checkin/checkout dates and features
        
    Returns:
        List of valid booking combinations with total price and common features
    """
    # Get all dates in the range
    dates = [str(d) for d in range(query['checkin'], query['checkout'] + 1)]
    
    # Validate dates exist in hotel data
    if not all(date in hotel_data for date in dates):
        return []
    
    # Filter rooms with required features for each date
    valid_rooms_per_date = []
    for date in dates:
        valid_rooms = [
            room for room in hotel_data[date]
            if all(f in room['features'] for f in query['features'])
            and room['availability'] >= query['rooms']
        ]
        if not valid_rooms:  # If any date has no valid rooms, return empty result
            return []
        valid_rooms_per_date.append(valid_rooms)
    
    # Find all valid combinations
    combinations = find_combinations(valid_rooms_per_date)
    
    # Calculate results for each combination
    results = []
    for combo in combinations:
        total_price = sum(room['price'] for room in combo)
        common_features = find_common_features(combo)
        min_availability = min(room['availability'] for room in combo)
        
        results.append({
            'price': total_price,
            'features': common_features,
            'availability': min_availability
        })
    
    return results 