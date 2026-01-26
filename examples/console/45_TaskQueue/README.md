# Example 45: Task Queue Database

This example demonstrates how to use SQLite as a robust job/task queue system with NDXSQLite.

## Features Demonstrated

- **Job Queue System**: SQLite as a persistent task queue
- **Enqueue/Dequeue Operations**: Adding and claiming tasks atomically
- **Priority Processing**: Higher priority tasks processed first
- **Delayed Tasks**: Schedule tasks for future execution
- **Retry Logic**: Exponential backoff for failed tasks
- **Dead-Letter Queue**: Permanent storage for failed tasks
- **Worker Management**: Task claiming and heartbeat tracking
- **Multiple Queues**: Named queues for different task types
- **Task History**: Complete audit trail of task execution

## Database Schema

### Tables

1. **tasks**: Main task queue
   - Priority levels (LOW=1, NORMAL=5, HIGH=10, CRITICAL=100)
   - Status tracking (pending, processing, completed, failed, dead)
   - Scheduled execution time support
   - Worker assignment and heartbeat

2. **dead_letter_queue**: Failed tasks after max retries
   - Preserves original task information
   - Records failure reason and timestamp

3. **task_history**: Execution audit trail
   - Logs all task state transitions
   - Records worker assignments

## Key Operations

### Enqueue Task
```pascal
TaskId := Enqueue('queue_name', 'task_type', '{"payload": "data"}',
  PRIORITY_HIGH, 60);  // 60 second delay
```

### Dequeue Task (Worker Claims Task)
```pascal
TaskId := Dequeue('queue_name', 'worker-1');
if TaskId > 0 then
begin
  // Process task
  CompleteTask(TaskId, '{"result": "success"}');
end;
```

### Handle Failure with Retry
```pascal
FailTask(TaskId, 'Connection timeout');
// Automatically retries with exponential backoff
// Moves to dead-letter queue after max retries
```

## Usage Patterns

### Priority Queue
Tasks are processed by priority (highest first), then by scheduled time:
- PRIORITY_CRITICAL (100): Immediate attention
- PRIORITY_HIGH (10): Important tasks
- PRIORITY_NORMAL (5): Standard tasks
- PRIORITY_LOW (1): Background tasks

### Retry with Exponential Backoff
Failed tasks are rescheduled with increasing delays:
- Retry 1: 10 seconds
- Retry 2: 20 seconds
- Retry 3: 40 seconds
- After max retries: moved to dead-letter queue

### Multiple Named Queues
```pascal
Enqueue('emails', 'send_welcome', '{}');
Enqueue('notifications', 'push_alert', '{}');
Enqueue('reports', 'generate_daily', '{}');
```

## Build and Run

```bash
cd 45_TaskQueue
fpc TaskQueue.lpr
./TaskQueue
```

## Cross-Platform

This example works on Windows, Linux, and macOS.

## Use Cases

- Background job processing
- Email sending queues
- Notification delivery
- Report generation
- Data processing pipelines
- Webhook delivery with retries
