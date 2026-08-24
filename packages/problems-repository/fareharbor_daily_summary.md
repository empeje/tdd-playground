# FareHarbor Daily Summary Service

## Problem Description

Design a domain service for FareHarbor's booking platform that computes and aggregates daily business metrics. Given a specific calendar date, the service returns a summary consisting of the total number of bookings and the total revenue generated.

### Input Format

- `target_date`: A `date` object representing the requested business day.

### Expected Output

A `DailySummary` domain model with:
- `date`: The target date
- `total_bookings`: Total count of confirmed reservations (integer)
- `revenue`: Total revenue in USD (float / decimal)

### Requirements

1. Encapsulate business data within a well-defined `DailySummary` model / dataclass.
2. Provide a `FareHarborService` class with a `get_daily_summary(target_date: date) -> DailySummary` method.
3. Allow extending the service with custom data providers or booking repositories.
4. Correctly calculate total bookings and revenue when given booking lists.

### Example

```python
from datetime import date
from python_exercise.fareharbor_daily import FareHarborService, Booking

service = FareHarborService()
summary = service.get_daily_summary(date(2026, 8, 24))

print(summary.total_bookings)  # e.g., 42
print(summary.revenue)         # e.g., 1250.50
```
