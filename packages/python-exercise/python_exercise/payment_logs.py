from dataclasses import dataclass
from datetime import datetime
from typing import Dict, List, Optional
import json
from collections import defaultdict


@dataclass
class PaymentLog:
    payment_id: str
    amount: float
    currency: str
    status: str
    timestamp: str
    payment_method: str
    booking_id: str


@dataclass
class BookingLog:
    booking_id: str
    property_id: str
    check_in_date: str
    check_out_date: str
    guest_id: str
    status: str
    timestamp: str


class PaymentLogsProcessor:
    def __init__(self):
        self.payments: List[PaymentLog] = []
        self.bookings: List[BookingLog] = []
        self.booking_durations: Dict[str, int] = {}

    def add_payment_log(self, payment_data: Dict) -> None:
        """Add a payment log to the processor."""
        payment = PaymentLog(**payment_data)
        self.payments.append(payment)

    def add_booking_log(self, booking_data: Dict) -> None:
        """Add a booking log to the processor."""
        booking = BookingLog(**booking_data)
        self.bookings.append(booking)
        
        # Calculate booking duration
        check_in = datetime.strptime(booking.check_in_date, "%Y-%m-%d")
        check_out = datetime.strptime(booking.check_out_date, "%Y-%m-%d")
        duration = (check_out - check_in).days
        self.booking_durations[booking.booking_id] = duration

    def generate_report(self) -> Dict:
        """Generate a comprehensive report from the processed logs."""
        if not self.payments and not self.bookings:
            return {}

        # Payment metrics
        successful_payments = [p for p in self.payments if p.status == "SUCCESS"]
        total_successful = len(successful_payments)
        
        revenue_by_currency = defaultdict(float)
        for payment in successful_payments:
            revenue_by_currency[payment.currency] += payment.amount
            
        payment_methods = defaultdict(int)
        for payment in self.payments:
            payment_methods[payment.payment_method] += 1
            
        # Booking metrics
        bookings_per_property = defaultdict(int)
        for booking in self.bookings:
            bookings_per_property[booking.property_id] += 1
            
        # Calculate conversion rate
        successful_bookings = len([b for b in self.bookings if b.status == "CONFIRMED"])
        total_bookings = len(self.bookings)
        conversion_rate = successful_bookings / total_bookings if total_bookings > 0 else 0
        
        # Calculate average booking duration
        total_duration = sum(self.booking_durations.values())
        avg_duration = total_duration / len(self.booking_durations) if self.booking_durations else 0

        return {
            "summary": {
                "total_successful_payments": total_successful,
                "total_revenue": dict(revenue_by_currency),
                "average_payment_amount": sum(p.amount for p in successful_payments) / total_successful if total_successful > 0 else 0,
                "payment_success_rate": total_successful / len(self.payments) if self.payments else 0,
                "popular_payment_methods": dict(payment_methods)
            },
            "booking_metrics": {
                "bookings_per_property": dict(bookings_per_property),
                "average_booking_duration": avg_duration,
                "conversion_rate": conversion_rate
            },
            "anomalies": {
                "suspicious_payments": self._detect_suspicious_payments(),
                "failed_conversions": len(self.payments) - total_successful
            }
        }

    def _detect_suspicious_payments(self) -> int:
        """Detect potentially suspicious payment patterns."""
        suspicious_count = 0
        
        # Group payments by booking_id
        payments_by_booking = defaultdict(list)
        for payment in self.payments:
            payments_by_booking[payment.booking_id].append(payment)
            
        # Check for multiple payments for the same booking
        for booking_id, payments in payments_by_booking.items():
            if len(payments) > 1:
                suspicious_count += 1
                
        return suspicious_count

    def process_logs_from_file(self, payment_logs_path: str, booking_logs_path: str) -> Dict:
        """Process logs from JSON files and generate a report."""
        try:
            with open(payment_logs_path, 'r') as f:
                payment_logs = json.load(f)
            with open(booking_logs_path, 'r') as f:
                booking_logs = json.load(f)
                
            for payment in payment_logs:
                self.add_payment_log(payment)
            for booking in booking_logs:
                self.add_booking_log(booking)
                
            return self.generate_report()
        except (FileNotFoundError, json.JSONDecodeError) as e:
            raise Exception(f"Error processing logs: {str(e)}") 