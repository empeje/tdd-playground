import pytest
from python_exercise.hotel_list import find_valid_bookings, find_common_features, find_combinations


@pytest.fixture
def sample_hotel_data():
    return {
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


def test_find_common_features():
    rooms = [
        {"features": ["breakfast", "wifi", "refundable"]},
        {"features": ["breakfast", "pool", "refundable"]},
        {"features": ["breakfast", "refundable", "parking"]}
    ]
    assert set(find_common_features(rooms)) == {"breakfast", "refundable"}


def test_find_combinations():
    rooms_per_date = [
        [{"price": 100}, {"price": 120}],
        [{"price": 110}, {"price": 130}]
    ]
    combinations = find_combinations(rooms_per_date)
    assert len(combinations) == 4  # 2 options per day = 2 * 2 combinations


def test_find_valid_bookings_basic(sample_hotel_data):
    query = {
        "checkin": 176,
        "checkout": 178,
        "features": ["breakfast"],
        "rooms": 1
    }
    result = find_valid_bookings(sample_hotel_data, query)
    assert len(result) > 0
    for booking in result:
        assert "breakfast" in booking["features"]
        assert booking["availability"] >= 1


def test_find_valid_bookings_no_availability(sample_hotel_data):
    query = {
        "checkin": 176,
        "checkout": 178,
        "features": ["breakfast"],
        "rooms": 10  # More rooms than available
    }
    result = find_valid_bookings(sample_hotel_data, query)
    assert len(result) == 0


def test_find_valid_bookings_invalid_dates(sample_hotel_data):
    query = {
        "checkin": 175,  # Date not in dataset
        "checkout": 178,
        "features": ["breakfast"],
        "rooms": 1
    }
    result = find_valid_bookings(sample_hotel_data, query)
    assert len(result) == 0


def test_find_valid_bookings_specific_features(sample_hotel_data):
    query = {
        "checkin": 176,
        "checkout": 178,
        "features": ["breakfast", "refundable"],
        "rooms": 1
    }
    result = find_valid_bookings(sample_hotel_data, query)
    assert len(result) > 0
    for booking in result:
        assert all(f in booking["features"] for f in ["breakfast", "refundable"])


def test_find_valid_bookings_price_calculation(sample_hotel_data):
    query = {
        "checkin": 176,
        "checkout": 176,  # Single night
        "features": ["breakfast"],
        "rooms": 1
    }
    result = find_valid_bookings(sample_hotel_data, query)
    assert len(result) == 1
    assert result[0]["price"] == 120  # Price for single night in room 176 