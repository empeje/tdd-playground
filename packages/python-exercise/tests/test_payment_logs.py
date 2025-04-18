import pytest
from datetime import datetime
from python_exercise.payment_logs import PaymentLogsProcessor, PaymentLog, BookingLog


@pytest.fixture
def processor():
    return PaymentLogsProcessor()


@pytest.fixture
def sample_payment_log():
    return {
        "payment_id": "PAY123",
        "amount": 150.00,
        "currency": "EUR",
        "status": "SUCCESS",
        "timestamp": "2024-04-18T10:30:00Z",
        "payment_method": "CREDIT_CARD",
        "booking_id": "BOOK456"
    }


@pytest.fixture
def sample_booking_log():
    return {
        "booking_id": "BOOK456",
        "property_id": "PROP789",
        "check_in_date": "2024-05-01",
        "check_out_date": "2024-05-05",
        "guest_id": "GUEST123",
        "status": "CONFIRMED",
        "timestamp": "2024-04-18T10:29:00Z"
    }


def test_add_payment_log(processor, sample_payment_log):
    processor.add_payment_log(sample_payment_log)
    assert len(processor.payments) == 1
    assert processor.payments[0].payment_id == "PAY123"
    assert processor.payments[0].amount == 150.00


def test_add_booking_log(processor, sample_booking_log):
    processor.add_booking_log(sample_booking_log)
    assert len(processor.bookings) == 1
    assert processor.bookings[0].booking_id == "BOOK456"
    assert processor.bookings[0].property_id == "PROP789"
    assert processor.booking_durations["BOOK456"] == 4  # 5 days - 1 day = 4 nights


def test_generate_report_empty(processor):
    report = processor.generate_report()
    assert report == {}


def test_generate_report_with_data(processor, sample_payment_log, sample_booking_log):
    processor.add_payment_log(sample_payment_log)
    processor.add_booking_log(sample_booking_log)
    
    report = processor.generate_report()
    
    assert report["summary"]["total_successful_payments"] == 1
    assert report["summary"]["total_revenue"]["EUR"] == 150.00
    assert report["summary"]["payment_success_rate"] == 1.0
    assert report["summary"]["popular_payment_methods"]["CREDIT_CARD"] == 1
    
    assert report["booking_metrics"]["bookings_per_property"]["PROP789"] == 1
    assert report["booking_metrics"]["average_booking_duration"] == 4
    assert report["booking_metrics"]["conversion_rate"] == 1.0


def test_multiple_payments_same_booking(processor, sample_payment_log, sample_booking_log):
    # Add two payments for the same booking
    processor.add_payment_log(sample_payment_log)
    second_payment = sample_payment_log.copy()
    second_payment["payment_id"] = "PAY124"
    processor.add_payment_log(second_payment)
    
    processor.add_booking_log(sample_booking_log)
    
    report = processor.generate_report()
    assert report["anomalies"]["suspicious_payments"] == 1


def test_failed_payments(processor, sample_payment_log, sample_booking_log):
    # Add a failed payment
    failed_payment = sample_payment_log.copy()
    failed_payment["status"] = "FAILED"
    processor.add_payment_log(failed_payment)
    
    processor.add_booking_log(sample_booking_log)
    
    report = processor.generate_report()
    assert report["summary"]["total_successful_payments"] == 0
    assert report["summary"]["payment_success_rate"] == 0.0
    assert report["anomalies"]["failed_conversions"] == 1


def test_multiple_currencies(processor, sample_payment_log, sample_booking_log):
    # Add payments in different currencies
    processor.add_payment_log(sample_payment_log)
    
    usd_payment = sample_payment_log.copy()
    usd_payment["currency"] = "USD"
    usd_payment["amount"] = 200.00
    processor.add_payment_log(usd_payment)
    
    processor.add_booking_log(sample_booking_log)
    
    report = processor.generate_report()
    assert report["summary"]["total_revenue"]["EUR"] == 150.00
    assert report["summary"]["total_revenue"]["USD"] == 200.00


def test_process_logs_from_file(tmp_path, processor):
    # Create temporary JSON files
    payment_logs = [{
        "payment_id": "PAY123",
        "amount": 150.00,
        "currency": "EUR",
        "status": "SUCCESS",
        "timestamp": "2024-04-18T10:30:00Z",
        "payment_method": "CREDIT_CARD",
        "booking_id": "BOOK456"
    }]
    
    booking_logs = [{
        "booking_id": "BOOK456",
        "property_id": "PROP789",
        "check_in_date": "2024-05-01",
        "check_out_date": "2024-05-05",
        "guest_id": "GUEST123",
        "status": "CONFIRMED",
        "timestamp": "2024-04-18T10:29:00Z"
    }]
    
    payment_file = tmp_path / "payments.json"
    booking_file = tmp_path / "bookings.json"
    
    import json
    with open(payment_file, 'w') as f:
        json.dump(payment_logs, f)
    with open(booking_file, 'w') as f:
        json.dump(booking_logs, f)
    
    report = processor.process_logs_from_file(str(payment_file), str(booking_file))
    assert report["summary"]["total_successful_payments"] == 1
    assert report["booking_metrics"]["bookings_per_property"]["PROP789"] == 1 