# Example 102: Inventory Management - Movements, Reservations, Lots

## Overview

This example demonstrates a complete **inventory management system** with multi-warehouse stock levels, stock movements (receive, ship, transfer), lot/batch tracking with expiry dates, stock reservations, low-stock alerts, and stock valuation.

## Features Demonstrated

### 1. Multi-Warehouse Stock Levels
- Track quantity and reserved stock per product per warehouse
- Available stock = quantity - reserved
- Reorder levels and max stock thresholds

### 2. Stock Movements
- Receive: incoming stock from suppliers
- Ship: outgoing stock for orders
- Transfer: move stock between warehouses
- All movements logged with timestamp and actor

### 3. Stock Reservations
- Reserve stock for pending orders (reduces available, not quantity)
- Availability check before reservation
- Fulfill reservation (converts to shipment)
- Cancel reservation (releases reserved stock)

### 4. Lot/Batch Tracking
- Track lots with manufacture and expiry dates
- Supplier information per lot
- Expiring lot detection with configurable threshold

### 5. Low-Stock Alerts
- Compare available stock against reorder level
- Per-warehouse and total stock alerts
- Visual indicators for low stock products

### 6. Movement History
- Full audit trail of all stock movements
- Signed quantities (+receive, -ship)
- Lot and reference tracking per movement

### 7. Stock Valuation
- Unit price per product
- Total value per product (quantity x price)
- Grand total inventory value

### 8. Statistics
- Movement counts by type with net quantities
- Lot and reservation counts
- Movement type distribution

## Database Schema

```
+------------------+     +------------------+     +------------------+
| products         |     | stock_levels     |     | stock_movements  |
+------------------+     +------------------+     +------------------+
| id (PK)          |<--->| product_id (FK)  |     | id (PK, AUTO)    |
| name             |     | warehouse        |     | product_id (FK)  |
| sku (UNIQUE)     |     | quantity         |     | warehouse        |
| category         |     | reserved         |     | movement_type    |
| unit_price       |     | PK(prod,wh)      |     | quantity (+/-)   |
| reorder_level    |     +------------------+     | lot_number       |
| max_stock        |                               | reference        |
+------------------+     +------------------+     | moved_at         |
                          | lots             |     | moved_by         |
+------------------+     +------------------+     +------------------+
| reservations     |     | lot_number (PK)  |
+------------------+     | product_id (FK)  |
| id (PK)          |     | warehouse        |
| product_id (FK)  |     | quantity         |
| warehouse        |     | manufactured_at  |
| quantity         |     | expires_at       |
| order_ref        |     | received_at      |
| status           |     | supplier         |
| reserved_at      |     +------------------+
| expires_at       |
+------------------+
```

## Compilation

```bash
cd 102_InventoryManagement
lazbuild InventoryManagement.lpi
./InventoryManagement
```

## Sample Output

```
3. Stock Reservations
   Reserve 25x Widget A for ORD-5010: SUCCESS
   Reserve 15x Motor D for ORD-5012:  FAILED (only 8 available)

5. Low-Stock Alerts
   ALERT: Motor D (MTR-D-004) at warehouse-A: available=4, reorder_level=5
   Motor D total=4 reserved=0 reorder=5 ** LOW **

6. Movement History
   2024-02-01 receive      +50 lot=LOT-2024-001
   2024-02-02 ship         -20 ref=ORD-5002
   2024-02-03 transfer_out -30 ref=to:warehouse-B

7. Stock Valuation
   Grand total inventory value: $8437.50
```

## Related Examples

- **101_WorkflowEngine** - Business process workflows
- **103_InvoiceSystem** - Financial transactions
- **104_ChatMessaging** - Real-time data patterns

## Best Practices

1. **Available vs Quantity**: Always use (quantity - reserved) for availability checks
2. **Movement log**: Record every stock change as an immutable movement
3. **Lot expiry**: Check expiry dates regularly, alert before expiration
4. **Reservation timeout**: Set expiry on reservations to prevent indefinite holds
5. **Transfer = two movements**: Record transfer_out and transfer_in atomically
6. **Signed quantities**: Positive for incoming, negative for outgoing
7. **Reorder alerts**: Trigger alerts when available drops below reorder_level
