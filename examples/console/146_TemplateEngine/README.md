# Example 146: Template Engine

## Overview

This example demonstrates a **template engine** built with SQLite. It stores parameterized templates with named variables, supports template inheritance through block replacement, tracks version history, validates required variables before rendering, and logs all render operations.

## Features Demonstrated

### 1. Template Registry
- 8 templates across 9 versions (email_welcome has v1 and v2)
- Parent-child relationships for inheritance (page extends base_layout)
- Author and creation date tracking
- Content size display

### 2. Parameterized Templates
- Named `{{variable}}` placeholders in template content
- Variable metadata: type, required flag, default value, description
- Per-template variable count and required count summary

### 3. Variable Substitution
- SQL REPLACE chain for multi-variable rendering
- Timestamp injection for dynamic values
- Multi-template rendering (notification, invoice)

### 4. Template Inheritance
- Parent-child template relationships
- Block-based content injection (`{{block:name}}` placeholders)
- Resolved template = parent content with child blocks replaced
- Two inheritance chains: page/base_layout, report_sales/report_base

### 5. Template Versions
- Version history per template name
- Content comparison between versions (size delta)
- Latest version lookup per template

### 6. Variable Defaults
- Optional variables with default values
- Rendering with only required variables provided
- Defaults automatically substituted when variable not supplied

### 7. Render Pipeline
- Step-by-step rendering: inheritance resolution, variable substitution, final output
- Partial render detection (unresolved placeholders)
- Full pipeline demonstration with report_sales template

### 8. Render History
- Render log with template, version, caller, timestamp, output length
- Historical output retrieval
- Per-template render tracking

### 9. Template Validation
- Required variables without defaults enumeration
- Pre-render validation (blocks missing required vars)
- Unresolved placeholder detection after partial render
- Render blocking when required variables missing

### 10. Template Statistics
- Per-template render count and unique callers
- Version adoption tracking (renders per version)
- Overall metrics: templates, versions, variables, renders, avg output length

## Architecture

```
+------------------+     +------------------+     +------------------+
| templates        |     | template_vars    |     | template_blocks  |
| (name, version,  |<--->| (variable defs,  |     | (block name,     |
|  content, parent)|     |  types, defaults)|     |  content)        |
+------------------+     +------------------+     +------------------+
         |
+------------------+
| render_log       |
| (renders, output)|
+------------------+
```

## Template Inheritance Model

```
base_layout (parent):
  <!DOCTYPE html>...<main>{{block:content}}</main>...

page (child, extends base_layout):
  Block "content": <h1>{{heading}}</h1><div>{{body}}</div>

Resolved output:
  <!DOCTYPE html>...<main><h1>{{heading}}</h1><div>{{body}}</div></main>...
```

## Variable Substitution Flow

```
Template:  [{{severity}}] {{title}}: {{message}} (at {{timestamp}})
                ↓               ↓          ↓              ↓
Values:     "WARN"      "Disk Space"  "Volume full"  "2025-01-20..."
                ↓
Output:    [WARN] Disk Space: Volume full (at 2025-01-20 15:30:00)
```

## Database Schema

```
+------------------+     +------------------+     +------------------+
| templates        |     | template_vars    |     | template_blocks  |
+------------------+     +------------------+     +------------------+
| id (PK)          |     | id (PK)          |     | id (PK)          |
| name             |     | template_id (FK) |     | template_id (FK) |
| version          |     | var_name         |     | block_name       |
| content          |     | var_type         |     | block_content    |
| parent_template  |     | required         |     +------------------+
| description      |     | default_value    |
| author           |     | description      |
| created_at       |     +------------------+
+------------------+
         |
+------------------+
| render_log       |
+------------------+
| id (PK)          |
| template_id (FK) |
| rendered_by      |
| rendered_at      |
| output           |
+------------------+
```

## Compilation

```bash
cd 146_TemplateEngine
lazbuild TemplateEngine.lpi
./TemplateEngine
```

## Sample Output

```
1. Registry: 8 templates, 9 versions
2. Parameterized: invoice=8 vars, email_welcome v2=5 vars
3. Substitution: notification + invoice rendered
4. Inheritance: page extends base_layout, report_sales extends report_base
5. Versions: email_welcome v1 vs v2 (+22 chars)
6. Defaults: 3 optional vars auto-filled
7. Pipeline: 3-step render (inherit, substitute, finalize)
8. History: 7 renders logged
9. Validation: 17 required vars, missing var blocks render
10. Stats: 8 templates, 27 vars, 7 renders, 116 avg chars
```

## Related Examples

- **144_CMS** - Content management with versioned pages
- **145_FileMetadataStore** - File system index with metadata

## Best Practices

1. **Named variables**: Use descriptive `{{var_name}}` placeholders for clarity
2. **Required vs optional**: Mark critical variables as required, provide defaults for optional ones
3. **Template inheritance**: Compose complex templates from reusable parent layouts
4. **Version tracking**: Keep history of template changes for rollback capability
5. **Pre-render validation**: Check all required variables before attempting render
6. **Render logging**: Track who rendered what and when for audit trails
7. **Block-based inheritance**: Clean separation between layout structure and content
8. **Unresolved detection**: Catch missing variables in final output
