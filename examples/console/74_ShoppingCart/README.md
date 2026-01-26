# Example 74: Shopping Cart with Stock Management

## Overview

This example demonstrates a **complete e-commerce shopping cart system** with real-time inventory management, stock reservation, discount codes, and order processing. It showcases transactional integrity patterns essential for production e-commerce applications.

## Features Demonstrated

### 1. Product Catalog & Inventory
- Product management with SKU, pricing, and categories
- Real-time stock tracking (available vs. reserved)
- Automatic low-stock alerts and reorder suggestions
- Inventory transaction logging for audit trails

### 2. Shopping Cart Operations
- Cart creation with session tracking
- Add/update/remove items with stock validation
- Stock reservation on cart add (prevents overselling)
- Stock release on item removal or cart abandonment

### 3. Discount Code System
- Multiple discount types: percentage, fixed amount, free shipping
- Validation rules: minimum order, expiration, usage limits
- Automatic usage tracking

### 4. Checkout Process
- Transactional order creation (all-or-nothing)
- Stock transfer from reserved to sold
- Tax and shipping calculation
- Loyalty points accumulation

### 5. Cart Abandonment
- Automatic expiration of inactive carts
- Stock release for abandoned carts
- Recovery potential tracking

## Database Schema

```
+------------+     +-----------+     +-------------+
| products   |     | inventory |     | customers   |
+------------+     +-----------+     +-------------+
| id         |<--->| product_id|     | id          |
| sku        |     | available |     | email       |
| name       |     | reserved  |     | tier        |
| price      |     | reorder   |     | points      |
+------------+     +-----------+     +-------------+
      |                                     |
      |                                     |
      v                                     v
+------------+     +-------------+    +-----------+
| cart_items |<--->| carts       |<-->| orders    |
+------------+     +-------------+    +-----------+
| product_id |     | customer_id |    | customer  |
| quantity   |     | session_id  |    | total     |
| unit_price |     | status      |    | status    |
+------------+     | expires_at  |    | discount  |
                   +-------------+    +-----------+
```

## Key Patterns

### Stock Reservation Pattern
```sql
-- Reserve stock when adding to cart
UPDATE inventory
SET quantity_reserved = quantity_reserved + ?
WHERE product_id = ?;

-- Check available (not reserved) stock
SELECT quantity_available - quantity_reserved
FROM inventory WHERE product_id = ?;
```

### Transactional Checkout
```sql
BEGIN TRANSACTION;
  -- Create order
  INSERT INTO orders (...) VALUES (...);

  -- Copy cart items to order items
  INSERT INTO order_items SELECT ... FROM cart_items;

  -- Update inventory (sold = available - quantity)
  UPDATE inventory SET
    quantity_available = quantity_available - sold_qty,
    quantity_reserved = quantity_reserved - sold_qty;

  -- Mark cart as converted
  UPDATE carts SET status = 'converted';
COMMIT;
```

### Cart Expiration
```sql
-- Find abandoned carts
SELECT * FROM carts
WHERE status = 'active'
  AND expires_at < datetime('now');

-- Release reserved stock
UPDATE inventory SET quantity_reserved = quantity_reserved -
  (SELECT SUM(ci.quantity) FROM cart_items ci
   WHERE ci.cart_id IN (SELECT id FROM abandoned_carts)
   AND ci.product_id = inventory.product_id);

-- Mark as abandoned
UPDATE carts SET status = 'abandoned'
WHERE status = 'active' AND expires_at < datetime('now');
```

## Demonstration Sections

1. **Schema Creation** - Complete e-commerce database structure
2. **Sample Data** - Products, inventory, customers, discount codes
3. **Cart Operations** - Add items with stock validation
4. **Stock Reservation** - View reserved vs. available stock
5. **Discount Validation** - Test various discount code scenarios
6. **Checkout Process** - Complete order with transaction safety
7. **Inventory Updates** - Stock levels after orders
8. **Cart Abandonment** - Handle expired carts
9. **Order Analytics** - Sales reports and customer loyalty
10. **Transaction Log** - Audit trail for inventory changes

## Business Rules Implemented

| Rule | Implementation |
|------|----------------|
| Prevent overselling | Stock checked and reserved on cart add |
| Price lock | Unit price captured when item added to cart |
| Discount validation | Check expiry, min amount, usage limits |
| Transaction safety | Order creation uses BEGIN/COMMIT |
| Audit trail | All inventory changes logged |
| Customer loyalty | Points earned on purchase |

## Compilation

```bash
cd 74_ShoppingCart
lazbuild ShoppingCart.lpi
./ShoppingCart
```

## Sample Output

```
3. Shopping Cart Operations
   =========================

   Adding items to cart:
     + Pro Laptop 15" (qty: 1)
     + Wireless Headphones (qty: 2)

   Attempting to add Smart Watch (qty: 5) - only 3 in stock:
     ERROR: Insufficient stock. Available: 3, Requested: 5

4. Stock Reservation Status
   =========================

   Product              | Available | Reserved | Free  | Status
   ---------------------|-----------|----------|-------|----------
   Smart Watch          |         3 |        2 |     1 | LOW STOCK
   Pro Laptop 15"       |        25 |        1 |    24 | OK
```

## Related Examples

- **73_ManyToManyAdvanced** - Complex relationship patterns
- **75_UserAuthentication** - Secure user sessions
- **79_BookingReservation** - Similar reservation patterns
- **13_Transactions** - Transaction handling

## Production Considerations

1. **Concurrency**: Use row-level locking or optimistic concurrency for high-traffic
2. **Stock Sync**: Consider eventual consistency for distributed inventory
3. **Price Changes**: Decide policy for items already in cart
4. **Cart Persistence**: Implement guest-to-user cart merging
5. **Payment Integration**: Add payment status tracking
