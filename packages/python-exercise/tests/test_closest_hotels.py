import pytest
from python_exercise.closest_hotels import ClosestHotelsFinder, Hotel, Point


@pytest.fixture
def finder():
    return ClosestHotelsFinder()


@pytest.fixture
def sample_hotel():
    return {
        "hotel_id": "H1",
        "name": "Grand Hotel",
        "latitude": 52.3676,
        "longitude": 4.9041,
        "rating": 4.5,
        "price_per_night": 150.00,
        "amenities": ["wifi", "pool", "gym"]
    }


@pytest.fixture
def sample_start_point():
    return {
        "latitude": 52.3670,
        "longitude": 4.9010
    }


def test_add_hotel(finder, sample_hotel):
    finder.add_hotel(sample_hotel)
    assert len(finder.hotels) == 1
    assert finder.hotels[0].hotel_id == "H1"
    assert finder.hotels[0].name == "Grand Hotel"


def test_invalid_coordinates(finder):
    invalid_hotel = {
        "hotel_id": "H1",
        "name": "Invalid Hotel",
        "latitude": 200,  # Invalid latitude
        "longitude": 4.9041,
        "rating": 4.5,
        "price_per_night": 150.00,
        "amenities": ["wifi"]
    }
    
    with pytest.raises(ValueError):
        finder.add_hotel(invalid_hotel)


def test_find_closest_hotels_empty(finder, sample_start_point):
    result = finder.find_closest_hotels(sample_start_point, k=5)
    assert result["closest_hotels"] == []
    assert result["search_metadata"]["total_hotels_considered"] == 0


def test_find_closest_hotels_single(finder, sample_hotel, sample_start_point):
    finder.add_hotel(sample_hotel)
    result = finder.find_closest_hotels(sample_start_point, k=5)
    
    assert len(result["closest_hotels"]) == 1
    assert result["closest_hotels"][0]["hotel_id"] == "H1"
    assert result["search_metadata"]["total_hotels_considered"] == 1


def test_find_closest_hotels_multiple(finder, sample_hotel, sample_start_point):
    # Add first hotel
    finder.add_hotel(sample_hotel)
    
    # Add second hotel (further away)
    hotel2 = sample_hotel.copy()
    hotel2.update({
        "hotel_id": "H2",
        "name": "Seaside Resort",
        "latitude": 52.3702,
        "longitude": 4.8952
    })
    finder.add_hotel(hotel2)
    
    result = finder.find_closest_hotels(sample_start_point, k=2)
    
    assert len(result["closest_hotels"]) == 2
    assert result["closest_hotels"][0]["hotel_id"] == "H1"  # Closer hotel
    assert result["closest_hotels"][1]["hotel_id"] == "H2"  # Further hotel


def test_find_closest_hotels_with_filters(finder, sample_hotel, sample_start_point):
    # Add first hotel
    finder.add_hotel(sample_hotel)
    
    # Add second hotel with different rating and price
    hotel2 = sample_hotel.copy()
    hotel2.update({
        "hotel_id": "H2",
        "name": "Luxury Hotel",
        "rating": 4.8,
        "price_per_night": 300.00
    })
    finder.add_hotel(hotel2)
    
    filters = {
        "min_rating": 4.6,
        "max_price": 250.00
    }
    
    result = finder.find_closest_hotels(sample_start_point, k=5, filters=filters)
    assert len(result["closest_hotels"]) == 0  # No hotels match the filters


def test_find_closest_hotels_with_amenities_filter(finder, sample_hotel, sample_start_point):
    # Add first hotel
    finder.add_hotel(sample_hotel)
    
    # Add second hotel with different amenities
    hotel2 = sample_hotel.copy()
    hotel2.update({
        "hotel_id": "H2",
        "name": "Beach Resort",
        "amenities": ["wifi", "beach", "restaurant"]
    })
    finder.add_hotel(hotel2)
    
    filters = {
        "amenities": ["pool", "gym"]
    }
    
    result = finder.find_closest_hotels(sample_start_point, k=5, filters=filters)
    assert len(result["closest_hotels"]) == 1
    assert result["closest_hotels"][0]["hotel_id"] == "H1"


def test_invalid_k_value(finder, sample_hotel, sample_start_point):
    finder.add_hotel(sample_hotel)
    with pytest.raises(ValueError):
        finder.find_closest_hotels(sample_start_point, k=0)


def test_load_hotels_from_file(tmp_path, finder):
    # Create a temporary JSON file
    import json
    hotels_data = {
        "hotels": [
            {
                "hotel_id": "H1",
                "name": "Grand Hotel",
                "latitude": 52.3676,
                "longitude": 4.9041,
                "rating": 4.5,
                "price_per_night": 150.00,
                "amenities": ["wifi", "pool", "gym"]
            }
        ]
    }
    
    file_path = tmp_path / "hotels.json"
    with open(file_path, 'w') as f:
        json.dump(hotels_data, f)
    
    finder.load_hotels_from_file(str(file_path))
    assert len(finder.hotels) == 1
    assert finder.hotels[0].hotel_id == "H1"


def test_same_distance_hotels(finder, sample_hotel, sample_start_point):
    # Add two hotels at the same distance
    finder.add_hotel(sample_hotel)
    
    hotel2 = sample_hotel.copy()
    hotel2.update({
        "hotel_id": "H2",
        "name": "Twin Hotel",
        "latitude": 52.3676,  # Same latitude
        "longitude": 4.9041   # Same longitude
    })
    finder.add_hotel(hotel2)
    
    result = finder.find_closest_hotels(sample_start_point, k=2)
    assert len(result["closest_hotels"]) == 2
    # Both hotels should be included since they're at the same distance 