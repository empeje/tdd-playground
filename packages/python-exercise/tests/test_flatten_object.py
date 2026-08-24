from python_exercise.flatten_object import flatten_object


def test_flatten_object_standard():
    input_obj = {
        "user": {
            "id": 1,
            "name": "John Doe",
            "contacts": [
                {"type": "email", "value": "john@example.com"},
                {"type": "phone", "value": "123-456-7890"},
            ],
        },
        "active": True,
    }

    result = flatten_object(input_obj)

    expected = {
        "user.id": 1,
        "user.name": "John Doe",
        "user.contacts.0.type": "email",
        "user.contacts.0.value": "john@example.com",
        "user.contacts.1.type": "phone",
        "user.contacts.1.value": "123-456-7890",
        "active": True,
    }

    assert result == expected


def test_flatten_object_custom_separator():
    data = {"a": {"b": 1, "c": [2, 3]}}
    result = flatten_object(data, sep="/")
    assert result == {
        "a/b": 1,
        "a/c/0": 2,
        "a/c/1": 3,
    }


def test_flatten_object_empty_and_primitives():
    assert flatten_object({}) == {}
    assert flatten_object(None) == {}  # type: ignore
    assert flatten_object(42, "num") == {"num": 42}
