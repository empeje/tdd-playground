# FareHarbor Aggregate Revenue from Nested Dict

## Problem Description

Given a complex nested data structure representing sales across multiple regions, aggregate the total revenue generated per product across all regions.

### Input Format

A dictionary where keys represent region names, and values are lists of sales records:

```json
{
  "regions": {
    "north": [
      {"id": 1, "product": "Widget", "revenue": 100},
      {"id": 2, "product": "Gadget", "revenue": 150}
    ],
    "south": [
      {"id": 3, "product": "Widget", "revenue": 200},
      {"id": 4, "product": "Sprocket", "revenue": 300}
    ]
  }
}
```

### Expected Output

A flat dictionary mapping each unique product to its total accumulated revenue:

```json
{
  "Widget": 300,
  "Gadget": 150,
  "Sprocket": 300
}
```

### Requirements

1. Traverse all regions and collect sales entries.
2. Sum up revenues for each unique product identifier/name.
3. Handle missing `regions` key, empty region lists, and missing `revenue` fields gracefully (defaulting to 0).
4. Maintain `O(N)` time complexity where `N` is the total number of individual sale items across all regions.
