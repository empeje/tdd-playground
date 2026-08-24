from python_exercise.sanitize_json import sanitize_json


def test_sanitize_json_nested_structure():
    input_data = {
        "user": {
            "id": 1,
            "name": "John",
            "password": "hashed_pw",
            "metadata": {"last_login": "2024-01-01", "internal_id": "999"},
        },
        "logs": [{"action": "login", "auth_token": "abc-123"}, {"action": "view"}],
    }

    sensitive = ["password", "auth_token", "internal_id"]

    result = sanitize_json(input_data, sensitive)

    expected = {
        "user": {
            "id": 1,
            "name": "John",
            "metadata": {"last_login": "2024-01-01"},
        },
        "logs": [{"action": "login"}, {"action": "view"}],
    }

    assert result == expected


def test_sanitize_json_no_sensitive_keys():
    data = {"a": 1, "b": {"c": 2}}
    assert sanitize_json(data, ["x", "y"]) == {"a": 1, "b": {"c": 2}}


def test_sanitize_json_empty_inputs():
    assert sanitize_json({}, ["pwd"]) == {}
    assert sanitize_json([], ["pwd"]) == []
    assert sanitize_json("primitive", ["pwd"]) == "primitive"
