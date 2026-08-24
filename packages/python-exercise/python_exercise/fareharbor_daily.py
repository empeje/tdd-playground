from dataclasses import dataclass
from datetime import date
from typing import List, Optional


@dataclass
class Booking:
    booking_id: str
    booking_date: date
    amount: float
    customer_name: str


@dataclass
class DailySummary:
    date: date
    total_bookings: int
    revenue: float


class FareHarborService:
    """Domain service for FareHarbor booking calculations and daily summaries."""

    def __init__(self, bookings: Optional[List[Booking]] = None):
        self._bookings = bookings if bookings is not None else []

    def add_booking(self, booking: Booking) -> None:
        self._bookings.append(booking)

    def get_daily_summary(self, target_date: date) -> DailySummary:
        """Compute the summary of bookings and total revenue for a given date."""
        matching_bookings = [
            b for b in self._bookings if b.booking_date == target_date
        ]

        if not matching_bookings:
            return DailySummary(
                date=target_date,
                total_bookings=0,
                revenue=0.0,
            )

        total_bookings = len(matching_bookings)
        revenue = sum(b.amount for b in matching_bookings)

        return DailySummary(
            date=target_date,
            total_bookings=total_bookings,
            revenue=round(revenue, 2),
        )
