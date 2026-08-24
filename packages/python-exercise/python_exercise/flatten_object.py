"""Flatten nested dictionary and list structures with dot notation."""

from typing import Any, Dict


def flatten_object(
    obj: Any, parent_key: str = "", sep: str = "."
) -> Dict[str, Any]:
    """Flatten a nested dictionary and list structure into a flat dictionary.

    Keys are joined using `sep` (default '.'). List indices are converted to string.
    """
    if not isinstance(obj, dict):
        return {} if obj is None else {parent_key: obj} if parent_key else {}

    items: Dict[str, Any] = {}

    for k, v in obj.items():
        new_key = f"{parent_key}{sep}{k}" if parent_key else str(k)
        if isinstance(v, dict):
            items.update(flatten_object(v, new_key, sep=sep))
        elif isinstance(v, list):
            for i, val in enumerate(v):
                list_key = f"{new_key}{sep}{i}"
                if isinstance(val, (dict, list)):
                    if isinstance(val, dict):
                        items.update(flatten_object(val, list_key, sep=sep))
                    else:
                        items.update(
                            flatten_object({str(i): val}, new_key, sep=sep)
                        )
                else:
                    items[list_key] = val
        else:
            items[new_key] = v

    return items
