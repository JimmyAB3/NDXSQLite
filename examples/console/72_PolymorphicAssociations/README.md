# Example 72: Polymorphic Associations

## Overview

This example demonstrates **polymorphic associations**, a design pattern where a single table can belong to multiple parent tables of different types. This is commonly used for features like comments, tags, attachments, or activity logs that apply to many different entity types.

## What are Polymorphic Associations?

A polymorphic association allows one table to reference rows in multiple other tables. For example, a `comments` table might store comments for blog posts, photos, and videos all in one place.

```sql
-- Polymorphic comments table
CREATE TABLE comments (
    id INTEGER PRIMARY KEY,
    commentable_type TEXT,    -- 'Post', 'Photo', 'Video'
    commentable_id INTEGER,   -- ID of the parent record
    content TEXT,
    created_at TEXT
);
```

The `commentable_type` field indicates which table the `commentable_id` refers to.

## Comparison with Traditional Approach

**Traditional (separate tables):**
```sql
CREATE TABLE post_comments (post_id INTEGER, content TEXT);
CREATE TABLE photo_comments (photo_id INTEGER, content TEXT);
CREATE TABLE video_comments (video_id INTEGER, content TEXT);
```

**Polymorphic (unified table):**
```sql
CREATE TABLE comments (
    commentable_type TEXT,  -- discriminator column
    commentable_id INTEGER, -- foreign key
    content TEXT
);
```

## Demonstration Sections

### 1. Schema Design
Creates the base entity tables (posts, photos, videos) and polymorphic association tables (comments, tags, attachments).

### 2. CHECK Constraints for Type Safety
Uses CHECK constraints to ensure only valid entity types are stored:
```sql
CHECK (commentable_type IN ('Post', 'Photo', 'Video'))
```

### 3. Adding Comments to Different Entities
Shows how to add comments to posts, photos, and videos using the same table.

### 4. Tagging System
Implements a polymorphic tagging system where any entity can be tagged.

### 5. File Attachments
Creates a unified attachments table for storing files associated with any entity.

### 6. Querying Comments for an Entity
Retrieves all comments for a specific post, photo, or video.

### 7. Finding All Entities with a Tag
Finds all posts, photos, and videos that have a specific tag.

### 8. Activity Log Pattern
Creates a comprehensive activity log that tracks actions across all entity types.

### 9. Type-Safe Views
Creates views that present polymorphic data in a type-safe manner:
```sql
CREATE VIEW post_comments AS
SELECT c.* FROM comments c WHERE c.commentable_type = 'Post';
```

## Schema Design

```
+--------+     +-----------+     +--------+
| posts  |     | comments  |     | photos |
+--------+     +-----------+     +--------+
| id     |<-+  | id        |  +->| id     |
| title  |  |  | type      |  |  | url    |
| body   |  +--| parent_id |--+  +--------+
+--------+     | content   |  |
               +-----------+  |  +--------+
                              |  | videos |
                              |  +--------+
                              +->| id     |
                                 | title  |
                                 +--------+
```

## Key SQL Techniques

```sql
-- Insert a polymorphic comment
INSERT INTO comments (commentable_type, commentable_id, content)
VALUES ('Post', 1, 'Great article!');

-- Get all comments for a post
SELECT * FROM comments
WHERE commentable_type = 'Post' AND commentable_id = 1;

-- Count comments per entity type
SELECT commentable_type, COUNT(*) as comment_count
FROM comments
GROUP BY commentable_type;

-- Find entities with a specific tag (using UNION)
SELECT 'Post' as type, p.id, p.title as name
FROM posts p
JOIN taggings t ON t.taggable_type = 'Post' AND t.taggable_id = p.id
JOIN tags ON tags.id = t.tag_id
WHERE tags.name = 'featured'

UNION ALL

SELECT 'Photo' as type, ph.id, ph.title as name
FROM photos ph
JOIN taggings t ON t.taggable_type = 'Photo' AND t.taggable_id = ph.id
JOIN tags ON tags.id = t.tag_id
WHERE tags.name = 'featured';
```

## Advantages and Disadvantages

**Advantages:**
- Single table for common functionality (comments, tags, etc.)
- Easy to add new entity types without schema changes
- Simplified queries when working across all types
- Reduced code duplication in application layer

**Disadvantages:**
- No referential integrity at database level
- Queries can be more complex
- Cannot use traditional foreign key constraints
- Index performance considerations

## Enforcing Integrity

Since SQLite cannot enforce foreign keys to multiple tables, use these strategies:

1. **CHECK constraints** to validate allowed types
2. **Triggers** to verify the referenced row exists
3. **Application-level validation**

```sql
-- Trigger to validate polymorphic reference
CREATE TRIGGER validate_comment_parent
BEFORE INSERT ON comments
BEGIN
    SELECT CASE
        WHEN NEW.commentable_type = 'Post'
             AND NOT EXISTS (SELECT 1 FROM posts WHERE id = NEW.commentable_id)
        THEN RAISE(ABORT, 'Referenced post does not exist')
        -- ... similar checks for other types
    END;
END;
```

## Compilation

```bash
cd 72_PolymorphicAssociations
lazbuild PolymorphicAssociations.lpi
./PolymorphicAssociations
```

## Related Examples

- **71_EAVPattern** - Another flexible data modeling pattern
- **73_ManyToManyAdvanced** - Complex relationship modeling
- **54_ConstraintsValidation** - Database constraints and validation
- **13_Transactions** - Transaction handling for data integrity

## References

- [Martin Fowler: Single Table Inheritance](https://martinfowler.com/eaaCatalog/singleTableInheritance.html)
- [Rails Polymorphic Associations](https://guides.rubyonrails.org/association_basics.html#polymorphic-associations)
