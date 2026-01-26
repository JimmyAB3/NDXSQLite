# Example 134: Test Fixtures - Loading, Integrity, Teardown, Factories

## Overview

This example demonstrates a **Test Data Management System** with fixture loading, referential integrity verification, FK-safe teardown, factory patterns, reproducible seed data, fixture isolation, and composite fixture layering. It simulates a testing framework's data setup/teardown lifecycle.

## Features Demonstrated

### 1. Schema with FK Relationships
- 5 tables with multi-level FK dependencies
- Dependency graph: departments -> users/projects -> tasks -> comments
- Explicit loading order and reverse teardown order
- PRAGMA foreign_keys enforcement

### 2. Minimal Fixture Loading
- 1 record per table (smallest valid dataset)
- Satisfies all FK constraints
- FK chain verification: comment -> task -> project -> department
- Fixture registry tracking

### 3. Teardown (FK-safe cleanup)
- Reverse FK order deletion (comments first, departments last)
- Before/after record count verification
- No FK violation errors
- Clean slate for next test

### 4. Full Fixture Loading
- 3 departments, 6 users, 4 projects, 10 tasks, 8 comments
- Realistic cross-department relationships
- Multiple task statuses and priorities
- Fixture registry with descriptions

### 5. Referential Integrity Verification
- 6 FK constraint checks (all VALID)
- users.department_id -> departments.id
- projects.department_id -> departments.id
- tasks.project_id -> projects.id
- tasks.assigned_to -> users.id
- comments.task_id -> tasks.id
- comments.user_id -> users.id

### 6. Factory Pattern
- Programmatic user generation with prefix and department
- 8 factory users created (5 engineering + 3 design)
- Predictable naming: prefix_NNN@factory.test
- Respects FK constraints automatically

### 7. Reproducible Seed Data
- Deterministic task generation with seed=42
- Same seed always produces identical assignments
- Distributes across projects, users, and statuses
- Configurable hours estimates

### 8. Fixture Isolation
- Fresh teardown + load between tests
- Test1 (minimal): 1 task
- Test2 (full): 10 tasks
- Test3 (minimal): 1 task (no bleed-through)
- Proven isolation between test runs

### 9. Composite Fixtures
- Layered loading: base + factory + seed
- Full fixture as base layer
- Factory additions for specific scenarios
- 14 tasks from combined layers

### 10. Data Validation
- 7 checks all passing
- Email format, positive hours, project coverage
- Budget validation, orphan detection
- Status enum verification, username uniqueness

### 11. Fixture Statistics
- Task distribution by status (done/in_progress/todo)
- Workload per user (tasks and hours)
- Department summary (users, projects, tasks)

### 12. Final Teardown Verification
- Complete cleanup of all tables
- Zero records remaining
- Successful teardown confirmation

## Architecture

```
+------------------+     +------------------+     +------------------+
| Fixture Registry |     | Loading Engine   |     | Validation       |
+------------------+     +------------------+     +------------------+
| Named fixtures   |---->| FK-order loading |---->| Constraint checks|
| Table mapping    |     | Minimal/Full     |     | Data quality     |
| Record counts    |     | Factory pattern  |     | Orphan detection |
+------------------+     +------------------+     +------------------+
        |                         |                         |
        v                         v                         v
+------------------+     +------------------+     +------------------+
| Teardown Engine  |     | Seed Generator   |     | Isolation Manager|
+------------------+     +------------------+     +------------------+
| Reverse FK order |     | Deterministic    |     | Per-test setup   |
| Clean verification|    | Reproducible     |     | No bleed-through |
| Zero-records check|    | Configurable     |     | Fresh state      |
+------------------+     +------------------+     +------------------+
```

## Database Schema

```
+------------------+     +------------------+
| departments      |     | fixture_registry |
+------------------+     +------------------+
| id (PK)          |     | fixture_name     |
| name (UQ)        |     | table_name       |
| budget           |     | record_count     |
+--------+---------+     | description      |
         |               +------------------+
    +----+----+
    |         |
+---v---+  +--v-----------+
| users |  | projects     |
+-------+  +--------------+
| id (PK)|  | id (PK)      |
| username| | name         |
| email  |  | department_id|-->departments
| dept_id|->departments   | status       |
| role   |  | priority     |
+---+----+  +------+-------+
    |              |
    +------+-------+
           |
     +-----v------+
     | tasks      |
     +------------+
     | id (PK)    |
     | title      |
     | project_id |-->projects
     | assigned_to|-->users
     | status     |
     | hours_est  |
     +-----+------+
           |
     +-----v------+
     | comments   |
     +------------+
     | id (PK)    |
     | task_id    |-->tasks
     | user_id    |-->users
     | content    |
     | created_at |
     +------------+
```

## Fixture Types

| Type | Records | Use Case |
|------|---------|----------|
| Minimal | 5 (1/table) | Unit tests needing FK satisfaction |
| Full | 31 (realistic) | Integration tests with varied data |
| Factory | N (generated) | Stress tests, bulk scenarios |
| Seed | N (deterministic) | Reproducible test runs |
| Composite | Combined layers | Complex integration scenarios |

## Loading/Teardown Order

```
LOADING (FK dependencies first):
  1. departments  (no FK)
  2. users        (FK: departments)
  3. projects     (FK: departments)
  4. tasks        (FK: projects, users)
  5. comments     (FK: tasks, users)

TEARDOWN (reverse order):
  1. comments     (depends on tasks, users)
  2. tasks        (depends on projects, users)
  3. projects     (depends on departments)
  4. users        (depends on departments)
  5. departments  (no dependencies)
```

## Compilation

```bash
cd 134_TestFixtures
lazbuild TestFixtures.lpi
./TestFixtures
```

## Sample Output

```
2. Minimal: 1 record per table, FK chain verified
3. Teardown: all tables empty (reverse FK order)
5. Integrity: 6/6 FK constraints VALID
6. Factory: 8 users generated (eng_001-005, des_001-003)
7. Seed: 5 deterministic tasks across 5 users
8. Isolation: Test1=1, Test2=10, Test3=1 (no bleed)
10. Validation: 7/7 checks PASS
12. Final: 0 records remaining, teardown SUCCESSFUL
```

## Related Examples

- **132_DatabaseDiff** - Schema comparison between databases
- **133_DataSnapshot** - Point-in-time snapshots

## Best Practices

1. **FK-aware ordering**: Always load parents before children, delete children before parents
2. **Minimal fixtures**: Use smallest valid dataset for unit tests
3. **Full fixtures**: Use realistic data for integration tests
4. **Factory pattern**: Generate bulk data programmatically with predictable naming
5. **Seed reproducibility**: Same seed = same data, every time
6. **Isolation**: Teardown completely between tests, no shared state
7. **Validation**: Verify data quality after loading (emails, enums, FKs)
8. **Composite layering**: Build complex scenarios from simple building blocks
