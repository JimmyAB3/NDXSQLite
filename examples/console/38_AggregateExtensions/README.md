# Example 38: Aggregate Functions and Extensions

This example demonstrates advanced aggregate functions including GROUP_CONCAT, JSON aggregates, and window functions.

## What you'll learn

- GROUP_CONCAT for string aggregation
- JSON aggregate functions (json_group_array, json_group_object)
- Window functions with OVER clause
- Aggregate with FILTER
- ROW_NUMBER, RANK, DENSE_RANK
- LAG, LEAD, FIRST_VALUE, LAST_VALUE

## Key concepts

### GROUP_CONCAT

```sql
-- Basic concatenation
SELECT customer, GROUP_CONCAT(product) AS products
FROM orders
GROUP BY customer;

-- With custom separator
SELECT customer, GROUP_CONCAT(product, ' | ') AS products
FROM orders
GROUP BY customer;

-- With DISTINCT
SELECT customer, GROUP_CONCAT(DISTINCT category) AS categories
FROM orders
GROUP BY customer;
```

### JSON Aggregation

```sql
-- Array of values
SELECT customer, JSON_GROUP_ARRAY(product) AS products
FROM orders
GROUP BY customer;

-- Array of objects
SELECT customer,
       JSON_GROUP_ARRAY(
         JSON_OBJECT('product', product, 'qty', quantity)
       ) AS orders
FROM orders
GROUP BY customer;

-- Object from key-value pairs
SELECT JSON_GROUP_OBJECT(customer, total) AS totals
FROM (
  SELECT customer, SUM(amount) AS total
  FROM orders
  GROUP BY customer
);
```

### FILTER clause

```sql
SELECT customer,
       COUNT(*) AS total,
       COUNT(*) FILTER (WHERE category = 'Electronics') AS electronics,
       SUM(price) FILTER (WHERE category = 'Books') AS books_spent
FROM orders
GROUP BY customer;
```

### Window Functions

```sql
-- Running total and rank
SELECT name, salary,
       SUM(salary) OVER (ORDER BY salary DESC) AS running_total,
       RANK() OVER (ORDER BY salary DESC) AS salary_rank
FROM employees;

-- Partition by department
SELECT name, department, salary,
       AVG(salary) OVER (PARTITION BY department) AS dept_avg,
       salary - AVG(salary) OVER (PARTITION BY department) AS diff
FROM employees;
```

### Ranking Functions

```sql
SELECT name, department, salary,
       ROW_NUMBER() OVER (PARTITION BY department ORDER BY salary DESC) AS row_num,
       RANK() OVER (PARTITION BY department ORDER BY salary DESC) AS rank,
       DENSE_RANK() OVER (PARTITION BY department ORDER BY salary DESC) AS dense_rank
FROM employees;
```

### LAG and LEAD

```sql
SELECT id, product,
       LAG(product, 1, '(first)') OVER (ORDER BY id) AS prev_product,
       LEAD(product, 1, '(last)') OVER (ORDER BY id) AS next_product
FROM orders;
```

### FIRST_VALUE and LAST_VALUE

```sql
SELECT name, department, salary,
       FIRST_VALUE(name) OVER (
         PARTITION BY department ORDER BY salary DESC
       ) AS top_earner,
       LAST_VALUE(name) OVER (
         PARTITION BY department ORDER BY salary DESC
         RANGE BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING
       ) AS lowest_earner
FROM employees;
```

## Function summary

| Function | Description |
|----------|-------------|
| GROUP_CONCAT | Concatenate strings with separator |
| JSON_GROUP_ARRAY | Aggregate into JSON array |
| JSON_GROUP_OBJECT | Aggregate key-value pairs into JSON object |
| SUM/AVG/COUNT OVER | Window aggregate functions |
| ROW_NUMBER | Unique row number in partition |
| RANK | Rank with gaps for ties |
| DENSE_RANK | Rank without gaps |
| LAG | Value from previous row |
| LEAD | Value from next row |
| FIRST_VALUE | First value in window |
| LAST_VALUE | Last value in window |
| NTILE | Divide into buckets |

## Window frame options

```sql
-- Default frame
OVER (ORDER BY col)

-- Full partition
OVER (PARTITION BY dept ORDER BY salary
      RANGE BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING)

-- Moving average (3 rows)
OVER (ORDER BY date
      ROWS BETWEEN 1 PRECEDING AND 1 FOLLOWING)
```

## Building

```bash
lazbuild AggregateExtensions.lpi
```

## Running

```bash
./AggregateExtensions      # Linux/macOS
AggregateExtensions.exe    # Windows
```

## Cross-Platform

This example works on Windows, Linux, and macOS.
