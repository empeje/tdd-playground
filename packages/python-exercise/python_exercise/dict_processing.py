"""Module for aggregating sales data from nested region-product records."""

from typing import Any, Dict


def aggregate_revenue(data: Dict[str, Any]) -> Dict[str, float]:
    """Summarize total revenue per product across all regions.

    Given a nested dictionary structure with regions containing lists of
    sales records, aggregate the total revenue for each unique product.
    """
    if not isinstance(data, dict):
        return {}

    product_revenue: Dict[str, float] = {}

    for _region_name, sales_list in data.get("regions", {}).items():
        if not isinstance(sales_list, list):
            continue
        for sale in sales_list:
            if not isinstance(sale, dict):
                continue
            product = sale.get("product")
            if not product:
                continue
            revenue = sale.get("revenue", 0)
            if not isinstance(revenue, (int, float)):
                continue

            product_revenue[product] = product_revenue.get(product, 0) + revenue

    return product_revenue
