# Example 73: Advanced Many-to-Many Relationships

## Overview

This example demonstrates **advanced many-to-many (M:N) relationships** with rich junction tables that contain metadata such as roles, dates, ordering, and state information. Unlike simple M:N relationships that only link two entities, these patterns store meaningful data about the relationship itself.

## Beyond Simple Junction Tables

A simple many-to-many just links IDs:

```sql
-- Simple M:N (limited)
CREATE TABLE project_members (
    project_id INTEGER,
    user_id INTEGER,
    PRIMARY KEY (project_id, user_id)
);
```

Advanced M:N stores relationship metadata:

```sql
-- Advanced M:N (rich)
CREATE TABLE project_members (
    project_id INTEGER,
    user_id INTEGER,
    role TEXT,              -- 'owner', 'admin', 'member'
    joined_at TEXT,         -- When they joined
    invited_by INTEGER,     -- Who invited them
    permissions TEXT,       -- JSON permissions
    PRIMARY KEY (project_id, user_id)
);
```

## Demonstration Sections

### 1. Role-Based Team Membership
Projects with team members who have different roles (owner, admin, contributor, viewer) and role-specific permissions.

### 2. Self-Referential Many-to-Many (Social Graph)
Users following other users, with metadata like when the follow started and notification preferences.

```sql
CREATE TABLE follows (
    follower_id INTEGER,
    followed_id INTEGER,
    created_at TEXT,
    notifications_enabled INTEGER DEFAULT 1,
    PRIMARY KEY (follower_id, followed_id)
);
```

### 3. Ordered Relationships (Playlists)
Playlist tracks with position ordering, allowing reordering without changing IDs.

```sql
CREATE TABLE playlist_tracks (
    playlist_id INTEGER,
    track_id INTEGER,
    position INTEGER,        -- Order in playlist
    added_at TEXT,
    added_by INTEGER
);
```

### 4. State Machine Relationships (Course Enrollments)
Course enrollments with status progression (enrolled -> in_progress -> completed).

```sql
CREATE TABLE enrollments (
    student_id INTEGER,
    course_id INTEGER,
    status TEXT CHECK (status IN ('enrolled', 'in_progress', 'completed', 'dropped')),
    enrolled_at TEXT,
    started_at TEXT,
    completed_at TEXT,
    grade REAL
);
```

### 5. Temporal Relationships (Employee Positions)
Historical tracking of employee positions with valid_from and valid_to dates.

```sql
CREATE TABLE employee_positions (
    employee_id INTEGER,
    position_id INTEGER,
    department_id INTEGER,
    valid_from TEXT,
    valid_to TEXT,           -- NULL means current
    salary REAL
);
```

### 6. Weighted Relationships (Skills/Ratings)
User skills with proficiency levels and endorsements.

```sql
CREATE TABLE user_skills (
    user_id INTEGER,
    skill_id INTEGER,
    proficiency_level INTEGER,  -- 1-10
    years_experience REAL,
    endorsed_by_count INTEGER
);
```

### 7. Hierarchical Relationships
Bill of Materials (BOM) pattern - products composed of other products with quantities.

```sql
CREATE TABLE product_components (
    parent_product_id INTEGER,
    child_product_id INTEGER,
    quantity INTEGER,
    is_optional INTEGER DEFAULT 0
);
```

### 8. Versioned Relationships
Document collaborators with version tracking and change history.

## Key Patterns

### Ordering Pattern
```sql
-- Get playlist in order
SELECT t.* FROM tracks t
JOIN playlist_tracks pt ON t.id = pt.track_id
WHERE pt.playlist_id = 1
ORDER BY pt.position;

-- Move track to new position
UPDATE playlist_tracks
SET position = position + 1
WHERE playlist_id = 1 AND position >= 3;

UPDATE playlist_tracks
SET position = 3
WHERE playlist_id = 1 AND track_id = 5;
```

### Temporal Pattern (SCD Type 2)
```sql
-- Get current position
SELECT * FROM employee_positions
WHERE employee_id = 1 AND valid_to IS NULL;

-- Get position at specific date
SELECT * FROM employee_positions
WHERE employee_id = 1
  AND valid_from <= '2024-06-15'
  AND (valid_to IS NULL OR valid_to > '2024-06-15');

-- Promote employee (close old, create new)
UPDATE employee_positions
SET valid_to = date('now')
WHERE employee_id = 1 AND valid_to IS NULL;

INSERT INTO employee_positions (employee_id, position_id, valid_from, salary)
VALUES (1, 2, date('now'), 75000);
```

### Self-Referential Pattern
```sql
-- Get all followers of a user
SELECT u.* FROM users u
JOIN follows f ON u.id = f.follower_id
WHERE f.followed_id = 1;

-- Get mutual follows (friends)
SELECT u.* FROM users u
JOIN follows f1 ON u.id = f1.followed_id
JOIN follows f2 ON u.id = f2.follower_id
WHERE f1.follower_id = 1 AND f2.followed_id = 1;

-- Suggest users to follow (followers of my followers)
SELECT DISTINCT u.* FROM users u
JOIN follows f1 ON u.id = f1.followed_id
JOIN follows f2 ON f1.follower_id = f2.followed_id
WHERE f2.follower_id = 1
  AND u.id != 1
  AND u.id NOT IN (SELECT followed_id FROM follows WHERE follower_id = 1);
```

### State Machine Pattern
```sql
-- Progress enrollment
UPDATE enrollments
SET status = 'in_progress', started_at = datetime('now')
WHERE student_id = 1 AND course_id = 1 AND status = 'enrolled';

-- Complete with grade
UPDATE enrollments
SET status = 'completed', completed_at = datetime('now'), grade = 95.5
WHERE student_id = 1 AND course_id = 1 AND status = 'in_progress';

-- Get completion rate
SELECT
    COUNT(*) as total,
    SUM(CASE WHEN status = 'completed' THEN 1 ELSE 0 END) as completed,
    AVG(CASE WHEN status = 'completed' THEN grade END) as avg_grade
FROM enrollments
WHERE course_id = 1;
```

## Schema Diagram

```
+--------+     +------------------+     +----------+
| users  |     | project_members  |     | projects |
+--------+     +------------------+     +----------+
| id     |<--->| user_id          |<--->| id       |
| name   |     | project_id       |     | name     |
+--------+     | role             |     +----------+
               | joined_at        |
               | permissions      |
               +------------------+

+--------+     +------------+
| users  |     | follows    |
+--------+     +------------+
| id     |<-+->| follower_id|
| name   |  +->| followed_id|
+--------+     | created_at |
               +------------+
```

## Indexing Strategies

```sql
-- For role-based queries
CREATE INDEX idx_members_role ON project_members(project_id, role);

-- For temporal queries
CREATE INDEX idx_positions_temporal
ON employee_positions(employee_id, valid_from, valid_to);

-- For ordered relationships
CREATE INDEX idx_playlist_order ON playlist_tracks(playlist_id, position);

-- For self-referential
CREATE INDEX idx_follows_follower ON follows(follower_id);
CREATE INDEX idx_follows_followed ON follows(followed_id);
```

## Compilation

```bash
cd 73_ManyToManyAdvanced
lazbuild ManyToManyAdvanced.lpi
./ManyToManyAdvanced
```

## Related Examples

- **71_EAVPattern** - Flexible attribute storage pattern
- **72_PolymorphicAssociations** - Single table, multiple parent types
- **54_ConstraintsValidation** - Database constraints
- **13_Transactions** - Transaction handling for complex operations

## References

- [Slowly Changing Dimensions](https://en.wikipedia.org/wiki/Slowly_changing_dimension)
- [Graph Database Patterns](https://neo4j.com/developer/data-modeling/)
- [Bill of Materials Pattern](https://en.wikipedia.org/wiki/Bill_of_materials)
