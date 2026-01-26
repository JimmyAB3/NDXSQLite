# Example 76: Flexible Tagging System (Folksonomy)

## Overview

This example demonstrates a **comprehensive tagging system** (folksonomy) with tag creation, content tagging, tag clouds, related tags discovery, tag hierarchies, and user-specific tag preferences. It showcases patterns essential for content organization and discovery in modern applications.

## Features Demonstrated

### 1. Tag Management
- Tag creation with normalization (lowercase)
- Display name preservation (original case)
- URL-friendly slug generation
- Tag descriptions and colors
- Featured tag marking

### 2. Tag Relationships
- Tag synonyms for better search
- Tag hierarchy (parent-child)
- Related tags based on co-occurrence
- Tag suggestions based on usage

### 3. Content Tagging
- Many-to-many content-tag relationships
- Relevance scoring per tag assignment
- User attribution for tags (folksonomy)
- Multiple tags per content item

### 4. Tag Cloud Generation
- Popularity-weighted tag clouds
- Normalized weight calculation (1-5 scale)
- Usage count tracking
- Featured tag highlighting

### 5. Tag-Based Discovery
- Content search by single tag
- Content search by multiple tags (AND/OR)
- Related content discovery
- Tag-based recommendations

## Database Schema

```
+------------+     +---------------+     +------------------+
| tags       |<--->| tag_synonyms  |     | tag_hierarchy    |
+------------+     +---------------+     +------------------+
| id         |     | tag_id        |     | parent_tag_id    |
| name       |     | synonym       |     | child_tag_id     |
| display_name|    +---------------+     +------------------+
| slug       |             |                    |
| usage_count|             |                    |
+------------+             |                    |
      |                    |                    |
      v                    v                    v
+------------+     +---------------+     +------------------+
| content    |<--->| content_tags  |<--->| related_tags     |
+------------+     +---------------+     +------------------+
| id         |     | content_id    |     | tag1_id          |
| title      |     | tag_id        |     | tag2_id          |
| author     |     | tagged_by     |     | co_occurrence    |
| view_count |     | relevance     |     +------------------+
+------------+     +---------------+
                         |
                         v
               +-------------------+
               | user_tag_prefs    |
               +-------------------+
               | user_name         |
               | tag_id            |
               | preference_score  |
               +-------------------+
```

## Key Patterns

### Tag Normalization
```pascal
function NormalizeTagName(const Name: string): string;
begin
  Result := LowerCase(Trim(Name));
  while Pos('  ', Result) > 0 do
    Result := StringReplace(Result, '  ', ' ', [rfReplaceAll]);
end;

function CreateSlug(const Name: string): string;
// Convert "Machine Learning" -> "machine-learning"
```

### Tag Cloud Weight Calculation
```pascal
// Normalize usage count to 1-5 scale
if MaxCount > MinCount then
  Weight := 1 + 4 * (Count - MinCount) / (MaxCount - MinCount)
else
  Weight := 3;
```

### Related Tags by Co-occurrence
```sql
-- Track which tags are used together
INSERT INTO related_tags (tag1_id, tag2_id, co_occurrence_count)
VALUES (?, ?, 1)
ON CONFLICT(tag1_id, tag2_id) DO UPDATE SET
  co_occurrence_count = co_occurrence_count + 1;
```

### Content Discovery with Multiple Tags
```sql
-- Find content with both "Travel" AND "Photography"
SELECT * FROM content WHERE id IN (
  SELECT content_id FROM content_tags
  WHERE tag_id = (SELECT id FROM tags WHERE name = 'travel')
  INTERSECT
  SELECT content_id FROM content_tags
  WHERE tag_id = (SELECT id FROM tags WHERE name = 'photography')
);
```

## Demonstration Sections

1. **Schema Creation** - Tagging database structure
2. **Sample Data** - Tags, content, and relationships
3. **Tag Cloud** - Popularity-weighted tag display
4. **Tag Hierarchy** - Parent-child relationships
5. **Content Discovery** - Find content by tags
6. **Related Tags** - Co-occurrence analysis
7. **Tag Suggestions** - Smart tag recommendations
8. **User Preferences** - Personal tagging patterns
9. **Tag Statistics** - Usage analytics

## Tag Features Implemented

| Feature | Implementation |
|---------|----------------|
| Normalization | Lowercase, trimmed, single spaces |
| Slugs | URL-friendly identifiers |
| Synonyms | Alternative names for search |
| Hierarchy | Parent-child relationships |
| Co-occurrence | Track tag pairs used together |
| User prefs | Personal tag scoring |

## Compilation

```bash
cd 76_TaggingSystem
lazbuild TaggingSystem.lpi
./TaggingSystem
```

## Sample Output

```
3. Tag Cloud Generation
   ======================

   Tag Cloud (weight 1-5):
   Tag Name                | Uses | Weight | Featured
   ------------------------|------|--------|----------
   Technology              |    4 | *****  | Yes
   Travel                  |    4 | *****  | Yes
   Python                  |    3 | ****   |
   Photography             |    3 | ****   | Yes
   Machine Learning        |    2 | **     | Yes

6. Related Tags
   ==============

   Tags frequently used together:
   Tag 1            | Tag 2            | Co-occurs
   -----------------|------------------|----------
   Photography      | Travel           | 3 times
   Technology       | Python           | 2 times
   Technology       | Machine Learning | 2 times

7. Tag Suggestions
   =================

   If you used "Python" and "Web Development", you might also use:
     - Technology (used 4 times)
     - Machine Learning (used 2 times)
     - Database (used 2 times)
```

## Related Examples

- **73_ManyToManyAdvanced** - Complex relationship patterns
- **75_UserAuthentication** - User identification for tagging
- **77_RatingReviews** - User-generated content
- **78_NotificationSystem** - Tag subscription alerts

## Production Considerations

1. **Tag Merging**: Implement tag consolidation for similar tags
2. **Tag Limits**: Set maximum tags per content item
3. **Moderation**: Add tag approval workflow
4. **Caching**: Cache popular tag queries
5. **Search Integration**: Full-text search with tag boosting
6. **Tag Autocomplete**: Prefix-based tag suggestions

## Use Cases

- **Blog/CMS**: Article categorization
- **E-commerce**: Product tagging
- **Photo Gallery**: Image organization
- **Social Media**: Content discovery
- **Knowledge Base**: Document classification
- **Bookmarking**: URL organization
