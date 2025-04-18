from dataclasses import dataclass
from typing import List, Dict, Optional, Set, Tuple, Any, NoReturn
import math
from collections import defaultdict


@dataclass
class Point:
    latitude: float
    longitude: float


@dataclass
class Hotel:
    hotel_id: str
    name: str
    latitude: float
    longitude: float
    rating: float
    price_per_night: float
    amenities: List[str]


class ClosestHotelsFinder:
    def __init__(self) -> None:
        self.hotels: List[Hotel] = []
        self._rtree = None  # Placeholder for R-tree implementation

    def add_hotel(self, hotel_data: Dict) -> None:
        """Add a hotel to the finder."""
        try:
            hotel = Hotel(**hotel_data)
            self._validate_coordinates(hotel.latitude, hotel.longitude)
            self.hotels.append(hotel)
        except (TypeError, ValueError) as e:
            raise ValueError(f"Invalid hotel data: {str(e)}")

    def _validate_coordinates(self, latitude: float, longitude: float) -> None:
        """Validate that coordinates are within valid ranges."""
        if not (-90 <= latitude <= 90):
            raise ValueError(f"Invalid latitude: {latitude}")
        if not (-180 <= longitude <= 180):
            raise ValueError(f"Invalid longitude: {longitude}")

    def _calculate_distance(self, point1: Point, point2: Point) -> float:
        """Calculate the Haversine distance between two points in kilometers."""
        # Earth's radius in kilometers
        R = 6371.0

        lat1, lon1 = math.radians(point1.latitude), math.radians(point1.longitude)
        lat2, lon2 = math.radians(point2.latitude), math.radians(point2.longitude)

        dlat = lat2 - lat1
        dlon = lon2 - lon1

        a = math.sin(dlat / 2)**2 + math.cos(lat1) * math.cos(lat2) * math.sin(dlon / 2)**2
        c = 2 * math.atan2(math.sqrt(a), math.sqrt(1 - a))

        return R * c

    def find_closest_hotels(
        self,
        start_point: Dict[str, float],
        k: int,
        filters: Optional[Dict] = None
    ) -> Dict[str, Any]:
        """Find the K closest hotels to the starting point."""
        if not self.hotels:
            return {
                "closest_hotels": [],
                "search_metadata": {
                    "total_hotels_considered": 0,
                    "search_time_ms": 0,
                    "radius_covered_km": 0
                }
            }

        try:
            start = Point(**start_point)
            self._validate_coordinates(start.latitude, start.longitude)
        except (TypeError, ValueError) as e:
            raise ValueError(f"Invalid start point: {str(e)}")

        if k <= 0:
            raise ValueError("K must be a positive integer")

        # Apply filters if provided
        filtered_hotels = self._apply_filters(self.hotels, filters)

        # Calculate distances and sort
        hotels_with_distances = []
        for hotel in filtered_hotels:
            hotel_point = Point(latitude=hotel.latitude, longitude=hotel.longitude)
            distance = self._calculate_distance(start, hotel_point)
            hotels_with_distances.append((hotel, distance))

        # Sort by distance and take top K
        hotels_with_distances.sort(key=lambda x: x[1])
        closest_hotels = hotels_with_distances[:k]

        # Prepare response
        result = {
            "closest_hotels": [
                {
                    "hotel_id": hotel.hotel_id,
                    "name": hotel.name,
                    "distance": round(distance, 2),
                    "rating": hotel.rating,
                    "price_per_night": hotel.price_per_night,
                    "amenities": hotel.amenities
                }
                for hotel, distance in closest_hotels
            ],
            "search_metadata": {
                "total_hotels_considered": len(filtered_hotels),
                "search_time_ms": 0,  # Would be implemented with actual timing
                "radius_covered_km": round(closest_hotels[-1][1], 2) if closest_hotels else 0
            }
        }

        return result

    def _apply_filters(self, hotels: List[Hotel], filters: Optional[Dict]) -> List[Hotel]:
        """Apply filters to the hotel list."""
        if not filters:
            return hotels

        filtered_hotels = hotels.copy()

        if "min_rating" in filters:
            filtered_hotels = [h for h in filtered_hotels if h.rating >= filters["min_rating"]]
        
        if "max_price" in filters:
            filtered_hotels = [h for h in filtered_hotels if h.price_per_night <= filters["max_price"]]
        
        if "amenities" in filters:
            required_amenities = set(filters["amenities"])
            filtered_hotels = [
                h for h in filtered_hotels 
                if required_amenities.issubset(set(h.amenities))
            ]

        return filtered_hotels

    def load_hotels_from_file(self, file_path: str) -> None:
        """Load hotels from a JSON file."""
        import json
        try:
            with open(file_path, 'r') as f:
                data = json.load(f)
                for hotel_data in data.get("hotels", []):
                    self.add_hotel(hotel_data)
        except (FileNotFoundError, json.JSONDecodeError) as e:
            raise Exception(f"Error loading hotels: {str(e)}")