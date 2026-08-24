from datetime import date
import pytest
from python_exercise.fareharbor_daily import (
    Booking,
    DailySummary,
    FareHarborService,
)


@pytest.fixture
def sample_service():
    service = FareHarborService()
    service.add_booking(
        Booking(
            booking_id="B001",
            booking_date=date(2026, 8, 24),
            amount=150.50,
            customer_name="Alice",
        )
    )
    service.add_booking(
        Booking(
            booking_id="B002",
            booking_date=date(2026, 8, 24),
            amount=249.50,
            customer_name="Bob",
        )
    )
    service.add_booking(
        Booking(
            booking_id="B003",
            booking_date=date(2026, 8, 25),
            amount=100.00,
            customer_name="Charlie",
        )
    )
    return service


def test_empty_service_summary():
    service = FareHarborService()
    summary = service.get_daily_summary(date(2026, 8, 24))
    assert isinstance(summary, DailySummary)
    assert summary.date == date(2026, 8, 24)
    assert summary.total_bookings == 0
    assert summary.revenue == 0.0


def test_daily_summary_aggregation(sample_service):
    summary_day1 = sample_service.get_daily_summary(date(2026, 8, 24))
    assert summary_day1.date == date(2026, 8, 24)
    assert summary_day1.total_bookings == 2
    assert summary_day1.revenue == 400.00

    summary_day2 = sample_service.get_daily_summary(date(2026, 8, 25))
    assert summary_day2.total_bookings == 1
    assert summary_day2.revenue == 100.00

    summary_empty_day = sample_service.get_daily_summary(date(2026, 8, 26))
    assert summary_empty_day.total_bookings == 0
    assert summary_empty_day.revenue == 0.0


def test_service_constructor_with_list():
    bookings = [
        Booking("B10", date(2026, 1, 1), 50.25, "David"),
        Booking("B11", date(2026, 1, 1), 49.75, "Eve"),
    ]
    service = FareHarborService(bookings)
    summary = service.get_daily_summary(date(2026, 1, 1))
    assert summary.total_bookings == 2
    assert summary.revenue == 100.00
