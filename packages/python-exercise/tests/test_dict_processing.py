from python_exercise.dict_processing import aggregate_revenue


def test_aggregate_revenue_standard_case():
    sales_data = {
        "regions": {
            "north": [
                {"id": 1, "product": "Widget", "revenue": 100},
                {"id": 2, "product": "Gadget", "revenue": 150},
            ],
            "south": [
                {"id": 3, "product": "Widget", "revenue": 200},
                {"id": 4, "product": "Sprocket", "revenue": 300},
            ],
            "west": [
                {"id": 5, "product": "Gadget", "revenue": 100},
                {"id": 6, "product": "Widget", "revenue": 50},
            ],
        }
    }

    result = aggregate_revenue(sales_data)
    assert result == {
        "Widget": 350,
        "Gadget": 250,
        "Sprocket": 300,
    }


def test_aggregate_revenue_empty_or_invalid():
    assert aggregate_revenue({}) == {}
    assert aggregate_revenue({"regions": {}}) == {}
    assert aggregate_revenue({"regions": {"empty_region": []}}) == {}
    assert aggregate_revenue(None) == {}  # type: ignore


def test_aggregate_revenue_missing_fields():
    data = {
        "regions": {
            "north": [
                {"id": 1},  # missing product and revenue
                {"product": "Widget"},  # missing revenue (defaults to 0)
                {"product": "Widget", "revenue": "invalid"},  # invalid revenue ignored
                {"product": "Widget", "revenue": 50},
            ]
        }
    }
    result = aggregate_revenue(data)
    assert result == {"Widget": 50}
