# Example 144: CMS - Content Management System

## Overview

This example demonstrates a **Content Management System** built with SQLite. It implements versioned pages with draft/published states, a publishing workflow with review steps, revision history with diff tracking, slug-based URL routing, content hierarchy, categories, metadata, and full-text search.

## Features Demonstrated

### 1. Pages Overview
- 9 pages across 3 content types (page, article, docs)
- Status tracking: draft, review, published, archived
- Author attribution and revision counting

### 2. Draft vs Published States
- Published pages visible to public (5 pages)
- Draft pages invisible (2 pages in progress)
- Review state for pending approval (1 page)

### 3. Publishing Workflow
- State machine: draft -> review -> published
- Direct publish shortcut (for admins)
- Archive transition for outdated content
- Actor and comment tracking per transition

### 4. Revision History
- Full revision trail per page (up to 4 revisions)
- Title/body evolution across revisions
- Author tracking per revision
- Change notes documenting each edit

### 5. Slug-Based Routing
- URL routing via unique slug lookup
- Published-only visibility enforcement
- Slug uniqueness validation
- Content resolution by slug

### 6. Content Hierarchy
- Parent-child page relationships
- Recursive CTE page tree construction
- Full path building (parent/child slugs)
- Breadcrumb navigation generation

### 7. Categories and Tagging
- Hierarchical category tree (Technology > Databases, Programming)
- Page-category many-to-many mapping
- Subcategory inclusion for queries
- Category page count

### 8. Page Metadata
- Key-value metadata per page (SEO, reading time)
- meta_description, meta_keywords, noindex flags
- SEO audit: pages missing descriptions

### 9. Content Search
- LIKE-based text search with context extraction
- Author-filtered content listing
- Content type breakdown statistics

### 10. Publishing Statistics
- Author contribution metrics (pages, revisions)
- Average revisions per page
- Workflow transition frequency
- Time-to-publish calculation

## Architecture

```
+------------------+     +------------------+     +------------------+
| Authors          |     | Pages            |     | Categories       |
| (admin, alice,   |---->| (slug, title,    |<--->| (hierarchical    |
|  bob, charlie)   |     |  body, status)   |     |  taxonomy)       |
+------------------+     +------------------+     +------------------+
                                  |
                    +-------------+-------------+
                    |             |             |
              +----------+  +----------+  +----------+
              | revisions|  | workflow  |  | page_meta|
              | (history)|  | (log)    |  | (SEO)    |
              +----------+  +----------+  +----------+
```

## Publishing Workflow

```
    +-------+     +---------+     +-----------+
    | draft |---->| review  |---->| published |
    +-------+     +---------+     +-----------+
        |              |                |
        +--------------+                v
        (direct publish)          +-----------+
                                  | archived  |
                                  +-----------+
```

## Database Schema

```
+------------------+     +------------------+     +------------------+
| pages            |     | revisions        |     | workflow_log     |
+------------------+     +------------------+     +------------------+
| id (PK)          |     | id (PK)          |     | id (PK)          |
| slug (UNIQUE)    |     | page_id (FK)     |     | page_id (FK)     |
| title            |     | revision_num     |     | from_status      |
| body             |     | title            |     | to_status        |
| content_type     |     | body             |     | actor            |
| parent_id        |     | slug             |     | comment          |
| status           |     | author           |     | timestamp        |
| author           |     | status           |     +------------------+
| current_revision |     | change_note      |
| created_at       |     | created_at       |     +------------------+
| updated_at       |     +------------------+     | page_meta        |
| published_at     |                               +------------------+
+------------------+     +------------------+     | page_id (PK)     |
                          | categories       |     | meta_key (PK)    |
+------------------+     +------------------+     | meta_value       |
| page_categories  |     | id (PK)          |     +------------------+
+------------------+     | name             |
| page_id (PK)     |     | slug (UNIQUE)    |
| category_id (PK) |     | parent_id        |
+------------------+     +------------------+
```

## Compilation

```bash
cd 144_CMS
lazbuild CMS.lpi
./CMS
```

## Sample Output

```
1. Pages: 9 pages (5 published, 2 draft, 1 review, 1 archived)
2. States: published visible, draft hidden, review pending
3. Workflow: 8 transitions (draft->review, review->published, etc.)
4. Revisions: 4 revisions for getting-started, title evolved
5. Routing: /getting-started resolves to published article
6. Hierarchy: faq is child of about, breadcrumb generated
7. Categories: Technology has 2 subcategories, 2 articles
8. Metadata: 7 meta entries, 3 pages missing SEO description
9. Search: "SQLite" found in 1 published page
10. Stats: admin=5 pages, avg 2.8 revisions/page
```

## Related Examples

- **145_FileMetadataStore** - File system index with checksums
- **146_TemplateEngine** - Template storage and rendering

## Best Practices

1. **Slug uniqueness**: Enforced at database level for URL routing
2. **Revision immutability**: Never modify old revisions, always create new ones
3. **Workflow audit**: Log every state transition with actor and timestamp
4. **Hierarchical queries**: Use recursive CTEs for trees (pages, categories)
5. **Metadata flexibility**: Key-value pattern allows arbitrary page properties
6. **SEO validation**: Query for missing meta fields proactively
7. **Content types**: Distinguish pages, articles, docs for different treatment
8. **Soft delete**: Archive instead of delete to preserve history
