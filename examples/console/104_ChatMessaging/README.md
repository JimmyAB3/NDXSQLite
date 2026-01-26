# Example 104: Chat Messaging - Conversations, Messages, Read Receipts

## Overview

This example demonstrates a complete **chat messaging system** with direct and group conversations, message sending/editing, read receipts, typing indicators, online presence, and unread message counts.

## Features Demonstrated

### 1. Conversations List
- Direct (1-on-1) and group conversations
- Member count per conversation
- Last message preview with timestamp
- Sorted by last activity

### 2. Send Messages
- Text messages with sender and timestamp
- Message editing with edited_at tracking
- Auto-incrementing message IDs

### 3. Message History
- Chronological message display per conversation
- Sender name, content, timestamp
- Edited message indicator

### 4. Read Receipts
- Per-message read tracking (who read what, when)
- Direct chat: shows reader usernames
- Group chat: read count vs total recipients
- INSERT OR IGNORE for idempotent read marking

### 5. Unread Count
- Count unread messages per conversation per user
- Excludes messages sent by the user themselves
- Uses LEFT JOIN to find messages without read receipts

### 6. Typing Indicators
- Time-limited typing events (started_at, expires_at)
- Multiple users typing simultaneously
- Automatic expiry check via timestamp comparison
- Cleanup of expired indicators

### 7. User Status & Presence
- Online/offline/away status
- Last seen timestamp
- Conversation member presence list
- Status-sorted member display

### 8. Chat Statistics
- Total users, conversations, messages
- Messages per user
- Messages per conversation
- Read receipt coverage percentage

## Database Schema

```
+------------------+     +------------------+     +---------------------+
| users            |     | conversations    |     | conversation_members|
+------------------+     +------------------+     +---------------------+
| id (PK)          |<--->| id (PK)          |<--->| conversation_id (FK)|
| username (UNIQUE)|     | conv_type        |     | user_id (FK)        |
| display_name     |     | title            |     | joined_at           |
| status           |     | created_at       |     | member_role         |
| last_seen_at     |     | created_by (FK)  |     | PK(conv_id, user_id)|
+------------------+     +------------------+     +---------------------+

+------------------+     +------------------+     +------------------+
| messages         |     | read_receipts    |     | typing_indicators|
+------------------+     +------------------+     +------------------+
| id (PK, AUTO)    |     | message_id (FK)  |     | conversation_id  |
| conversation_id  |     | user_id (FK)     |     | user_id          |
| sender_id (FK)   |     | read_at          |     | started_at       |
| content          |     | PK(msg, user)    |     | expires_at       |
| message_type     |     +------------------+     | PK(conv, user)   |
| sent_at          |                               +------------------+
| edited_at        |
+------------------+
```

## Compilation

```bash
cd 104_ChatMessaging
lazbuild ChatMessaging.lpi
./ChatMessaging
```

## Sample Output

```
1. Conversations List (for Alice)
   [group] Project Alpha (4 members)
      Last: "Let us meet at 2pm to discuss." at 2024-03-01 09:20:00
   [direct] Bob Smith (2 members)
      Last: "Perfect, let me know when it is ready." at 2024-03-01 09:05:00

4. Read Receipts
   #5 "Team, the sprint starts today." - 2/3 read
   #6 "Ready to go!" - 2/3 read

5. Unread Count
   Dave: Project Alpha: 7 unread
   Bob: Project Alpha: 4 unread

6. Typing Indicators
   Who is typing in Project Alpha (at 10:00:03)?
   - Carol White is typing...
   - Bob Smith is typing...
```

## Related Examples

- **101_WorkflowEngine** - Business process workflows
- **102_InventoryManagement** - Stock tracking and alerts
- **103_InvoiceSystem** - Financial transactions

## Best Practices

1. **Read receipts**: Use composite PK (message_id, user_id) with INSERT OR IGNORE for idempotency
2. **Typing indicators**: Use short expiry times (3-5 seconds) with periodic renewal
3. **Unread count**: LEFT JOIN messages against read_receipts WHERE NULL for efficiency
4. **Message editing**: Keep original, update content and set edited_at timestamp
5. **Presence**: Update last_seen_at on every user action
6. **Group vs Direct**: Use conv_type to differentiate display logic
7. **Indexes**: Index messages by (conversation_id, sent_at) for efficient history queries
