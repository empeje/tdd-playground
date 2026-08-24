"""Recursively sanitize JSON data by removing sensitive keys."""

from typing import Any, Container


def sanitize_json(data: Any, sensitive_keys: Container[str]) -> Any:
    """Recursively removes occurrences of sensitive keys from JSON structures."""
    if isinstance(data, dict):
        keys_to_delete = [k for k in data if k in sensitive_keys]
        for k in keys_to_delete:
            del data[k]

        for _k, v in list(data.items()):
            sanitize_json(v, sensitive_keys)

    elif isinstance(data, list):
        for item in data:
            sanitize_json(item, sensitive_keys)

    return data
