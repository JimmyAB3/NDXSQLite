# Example 101: Workflow Engine - Steps, Approvals, Parallel Branches

## Overview

This example demonstrates a **workflow engine** pattern with typed steps, approval flows, parallel branches (fork/join), deadline tracking, and escalation. It implements a Purchase Order approval workflow as a concrete use case.

## Features Demonstrated

### 1. Workflow Definition
- Named workflow templates with step sequences
- Step types: task, approval, parallel_fork, parallel_join
- Deadline configuration per step

### 2. Workflow Instances
- Create running instances from definitions
- Track current step and status
- Multiple concurrent instances

### 3. Step Execution & Approvals
- Complete tasks, approve or reject approval steps
- Rejection can cancel the workflow
- Assignment tracking per step

### 4. Parallel Branches (Fork/Join)
- Fork step splits into multiple concurrent branches
- Each branch executes independently
- Join step waits for all branches to complete

### 5. Deadlines & Escalation
- Deadline calculation per step (SQLite datetime arithmetic)
- Overdue detection via timestamp comparison
- Escalation to higher authority with new deadline

### 6. Workflow Completion
- Final step completes the instance
- Full lifecycle: active -> completed/cancelled
- Duration tracking (started_at to completed_at)

### 7. Audit Trail
- Complete history of all step executions
- Who did what, when, with what notes
- Branch tracking for parallel steps

### 8. Statistics
- Instance status summary
- Step execution counts by status
- Pending work per assignee

## Database Schema

```
+---------------------+     +---------------------+     +---------------------+
| workflow_definitions |     | workflow_steps       |     | workflow_instances   |
+---------------------+     +---------------------+     +---------------------+
| id (PK)             |<--->| id (PK)             |     | id (PK)             |
| name                |     | workflow_id (FK)     |<--->| workflow_id (FK)     |
| description         |     | step_name           |     | title               |
+---------------------+     | step_type           |     | status              |
                             | step_order          |     | current_step_id     |
+---------------------+     | deadline_hours      |     | started_at          |
| parallel_branches   |     +---------------------+     | completed_at        |
+---------------------+                                  | started_by          |
| id (PK, AUTO)       |     +---------------------+     +---------------------+
| fork_step_id (FK)   |     | step_executions      |
| branch_name         |     +---------------------+
| branch_step_id (FK) |     | id (PK, AUTO)        |
+---------------------+     | instance_id (FK)     |
                             | step_id (FK)         |
                             | status               |
                             | assigned_to          |
                             | started_at           |
                             | completed_at         |
                             | deadline_at          |
                             | notes                |
                             | branch_name          |
                             +---------------------+
```

## Compilation

```bash
cd 101_WorkflowEngine
lazbuild WorkflowEngine.lpi
./WorkflowEngine
```

## Sample Output

```
3. Step Execution & Approvals
   inst-001: Manager approved (within budget)
   inst-002: Manager rejected -> workflow cancelled

4. Parallel Branches (Fork/Join)
   [finance] Finance Check   approved at 2024-03-02T10:00:00
   [legal] Legal Review      approved at 2024-03-03T11:00:00
   -> All branches complete, join step done

5. Deadlines & Escalation
   OVERDUE: Director Approval assigned to dir.johnson, was due 2024-03-04 11:00:00
   -> Escalated to vp.martinez with 8h deadline
   -> VP approved

7. Full Audit Trail
   # 9 Director Approval escalated by dir.johnson "Deadline exceeded"
   #10 Director Approval approved  by vp.martinez "Approved after escalation"
```

## Related Examples

- **94_CQRS** - Event sourcing patterns
- **99_ChangeLog** - Audit trail with changelog
- **102_InventoryManagement** - Business process patterns

## Best Practices

1. **Typed steps**: Use step_type to drive execution logic
2. **Immutable history**: Never modify step_executions, only append
3. **Deadline via SQL**: Use SQLite datetime() for reliable calculation
4. **Fork/Join**: Track branch_name to verify all branches complete
5. **Escalation**: Keep original execution (escalated), create new one
6. **Status machine**: Validate transitions (pending->in_progress->completed)
