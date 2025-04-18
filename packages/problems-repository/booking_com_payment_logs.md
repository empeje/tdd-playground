# Booking.com Payment Logs Processing

## Problem Description

You are tasked with creating a system to process payment and booking logs for Booking.com's payment operations team. The logs are provided in JSON format and contain information about payments and bookings made on the platform.

### Input Format

The system receives two types of logs:

1. Payment Logs:
```json
{
  "payment_id": "PAY123",
  "amount": 150.00,
  "currency": "EUR",
  "status": "SUCCESS",
  "timestamp": "2024-04-18T10:30:00Z",
  "payment_method": "CREDIT_CARD",
  "booking_id": "BOOK456"
}
```

2. Booking Logs:
```json
{
  "booking_id": "BOOK456",
  "property_id": "PROP789",
  "check_in_date": "2024-05-01",
  "check_out_date": "2024-05-05",
  "guest_id": "GUEST123",
  "status": "CONFIRMED",
  "timestamp": "2024-04-18T10:29:00Z"
}
```

### Requirements

1. Process the logs and generate a comprehensive report that includes:
   - Total number of successful payments
   - Total revenue by currency
   - Average payment amount
   - Payment success rate
   - Most popular payment methods
   - Number of bookings per property
   - Average booking duration
   - Payment-to-booking conversion rate

2. Handle edge cases:
   - Failed payments
   - Cancelled bookings
   - Refunded payments
   - Multiple payments for the same booking
   - Missing or malformed data

3. The report should be generated in a format that is easy to read and analyze by the payment operations team.

### Additional Scenarios to Consider

1. How would you handle real-time processing of logs?
2. What if the logs are received out of order?
3. How would you detect and report suspicious payment patterns?
4. How would you handle currency conversion for international payments?
5. What metrics would you add to detect potential fraud?

### Technical Requirements

1. Write code that processes the logs and generates the report
2. Explain the runtime complexity of your solution
3. Suggest optimizations for handling large volumes of logs
4. Consider how to make the system scalable for future requirements

### Evaluation Criteria

1. Code quality and organization
2. Handling of edge cases
3. Runtime complexity analysis
4. Scalability considerations
5. Clarity of the generated report
6. Ability to ask clarifying questions
7. Problem-solving approach

### Example Output

```json
{
  "summary": {
    "total_successful_payments": 1000,
    "total_revenue": {
      "EUR": 150000.00,
      "USD": 120000.00
    },
    "average_payment_amount": 135.00,
    "payment_success_rate": 0.98,
    "popular_payment_methods": {
      "CREDIT_CARD": 800,
      "PAYPAL": 150,
      "BANK_TRANSFER": 50
    }
  },
  "booking_metrics": {
    "bookings_per_property": {
      "PROP789": 50,
      "PROP123": 45
    },
    "average_booking_duration": 3.5,
    "conversion_rate": 0.95
  },
  "anomalies": {
    "suspicious_payments": 5,
    "failed_conversions": 20
  }
}
```

### Source

This problem is based on a real Booking.com interview experience:
https://leetcode.com/discuss/post/2086386/bookingcom-sde-2-amsterdam-2022-offer-by-n7tu/ 