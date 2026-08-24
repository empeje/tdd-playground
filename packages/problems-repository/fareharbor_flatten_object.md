# FareHarbor Flatten Nested Object

## Problem Description

Given a nested object consisting of dictionaries, lists, and scalar values, flatten the structure into a single-level dictionary where keys represent the hierarchical path joined with a separator (defaulting to dot `.` notation).

### Input Format

- `obj`: Nested dictionary structure.
- `parent_key` (optional, default `""`): Prefix path for nested keys.
- `sep` (optional, default `"."`): Key path separator.

```json
{
  "user": {
    "id": 1,
    "name": "John Doe",
    "contacts": [
      {"type": "email", "value": "john@example.com"},
      {"type": "phone", "value": "123-456-7890"}
    ]
  },
  "active": true
}
```

### Expected Output

```json
{
  "user.id": 1,
  "user.name": "John Doe",
  "user.contacts.0.type": "email",
  "user.contacts.0.value": "john@example.com",
  "user.contacts.1.type": "phone",
  "user.contacts.1.value": "123-456-7890",
  "active": true
}
```

### Requirements

1. Traverse nested dictionaries recursively, prepending the parent key.
2. Flatten list indices as string integers (`"0"`, `"1"`).
3. Support custom separators (such as `"/"` or `"_"`).
4. Correctly preserve primitive values (strings, numbers, booleans, `None`).
