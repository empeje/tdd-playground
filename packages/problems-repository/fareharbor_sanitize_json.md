# FareHarbor Sanitize JSON Data

## Problem Description

Given a nested JSON-like structure (combinations of dictionaries, lists, and primitives) and a list of sensitive keys (such as `"password"`, `"ssn"`, `"auth_token"`, `"credit_card"`), recursively sanitize the data by removing all occurrences of those keys at any nesting level.

### Input Format

- `data`: Arbitrarily nested dictionary or list structure.
- `sensitive_keys`: List or set of string keys to strip out.

```json
{
  "user": {
    "id": 1,
    "name": "Admin",
    "password": "secret_password",
    "profiles": [
      {"id": 10, "ssn": "000-00-0000"},
      {"id": 11, "name": "Public"}
    ]
  },
  "logs": [
    {"action": "login", "auth_token": "abc-123"},
    {"action": "view"}
  ]
}
```

Sensitive keys: `["password", "ssn", "auth_token"]`

### Expected Output

```json
{
  "user": {
    "id": 1,
    "name": "Admin",
    "profiles": [
      {"id": 10},
      {"id": 11, "name": "Public"}
    ]
  },
  "logs": [
    {"action": "login"},
    {"action": "view"}
  ]
}
```

### Requirements

1. Recursively traverse nested dictionaries and lists.
2. Avoid dictionary mutation runtime errors (`dictionary changed size during iteration`) by collecting keys before deleting.
3. Return the sanitized data structure.
