# Example 145: File Metadata Store

## Overview

This example demonstrates a **file system index** built with SQLite. It tracks file paths in a hierarchical structure, computes checksums for integrity verification, detects duplicate files through checksum comparison, supports bulk indexing operations, and allows searching files by custom metadata attributes.

## Features Demonstrated

### 1. File Index Overview
- 19 indexed entries (7 directories, 12 files)
- Path, extension, size, and modification tracking
- Full file system snapshot in database

### 2. Path Hierarchy
- Recursive CTE directory tree visualization
- Parent-child path relationships
- Depth-level file distribution analysis

### 3. Checksums and Integrity
- SHA256-style checksums per file
- Integrity validation (checksum + size consistency)
- Truncated display for readability

### 4. Deduplication Detection
- Duplicate detection by matching checksums
- Multi-location tracking (same content, different paths)
- Wasted space calculation (bytes recoverable)

### 5. Bulk Operations
- Operation logging (full_scan, checksum_verify, reindex)
- File count and byte tracking per operation
- Duration measurement
- Simulated re-index of modified files

### 6. Search by Extension
- Extension-based file type grouping
- Count and total size per extension
- Filtered listing (e.g., all *.pas files)

### 7. Search by Metadata
- Custom key-value metadata per file (author, language, LOC)
- Author-based file discovery
- Aggregation queries (total lines of code by author)

### 8. Size Analysis
- Top N largest files
- Recursive directory size calculation
- File count per directory

### 9. Recent Changes
- Time-based modification queries
- Change hotspot detection (most active directories)
- Chronological change listing

### 10. Index Statistics
- Overall metrics (entries, size, extensions)
- MIME type distribution
- Unique checksum count (deduplication ratio)

## Architecture

```
+------------------+     +------------------+     +------------------+
| File System      |     | files            |     | file_metadata    |
| (paths, sizes,   |---->| (path, checksum, |<--->| (key-value       |
|  timestamps)     |     |  mime, dates)    |     |  attributes)     |
+------------------+     +------------------+     +------------------+
                                  |
                          +------------------+
                          | operations       |
                          | (bulk op log)    |
                          +------------------+
```

## Path Hierarchy Example

```
/
├── backups/
│   ├── core.pas.bak
│   ├── database.pas.bak
│   ├── main.pas.bak
│   └── readme.md.bak
└── projects/
    ├── library/
    │   └── src/
    │       ├── core.pas
    │       └── helpers.pas
    └── webapp/
        ├── docs/
        │   ├── api.md
        │   └── readme.md
        └── src/
            ├── config.json
            ├── database.pas
            ├── main.pas
            └── utils.pas
```

## Deduplication Logic

```
File A: /projects/webapp/src/main.pas     checksum=a1b2c3...  size=4520
File B: /backups/main.pas.bak             checksum=a1b2c3...  size=4520
                                                    ↓
                                          Same checksum = DUPLICATE
                                          Wasted: 4520 bytes
```

## Database Schema

```
+------------------+     +------------------+     +------------------+
| files            |     | file_metadata    |     | operations       |
+------------------+     +------------------+     +------------------+
| id (PK)          |     | file_id (PK,FK)  |     | id (PK)          |
| path (UNIQUE)    |     | meta_key (PK)    |     | operation        |
| filename         |     | meta_value       |     | file_count       |
| extension        |     +------------------+     | total_bytes      |
| parent_path      |                              | status           |
| is_directory     |                              | started_at       |
| size_bytes       |                              | completed_at     |
| checksum         |                              +------------------+
| mime_type        |
| created_at       |
| modified_at      |
| indexed_at       |
+------------------+
```

## Compilation

```bash
cd 145_FileMetadataStore
lazbuild FileMetadataStore.lpi
./FileMetadataStore
```

## Sample Output

```
1. Index: 19 entries (7 dirs, 12 files, 56KB)
2. Hierarchy: tree with depth 2-4
3. Checksums: integrity PASSED
4. Duplicates: 5 groups, 25KB wasted
5. Bulk ops: 4 operations logged
6. Extensions: pas=5, bak=4, md=2, json=1
7. Metadata: alice=652 LOC, bob=136 LOC
8. Sizes: core.pas largest (8920 bytes)
9. Changes: 7 files modified since Jan 16
10. Stats: 7 unique checksums out of 14 files
```

## Related Examples

- **144_CMS** - Content management with versioned pages
- **146_TemplateEngine** - Template storage and rendering

## Best Practices

1. **Unique paths**: Use path as natural key for file identity
2. **Checksum-based dedup**: Content-addressable storage detection
3. **Hierarchical queries**: Recursive CTEs for directory traversal
4. **Metadata flexibility**: Key-value pairs for arbitrary file attributes
5. **Bulk operation logging**: Track indexing operations for auditability
6. **MIME types**: Standard content type classification
7. **Parent path indexing**: Enables efficient directory size queries
8. **Integrity checks**: Cross-validate checksum consistency with file sizes
