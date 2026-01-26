{===============================================================================
  NDXSQLite Example 122 - Resource Booking
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Resource pool and availability management
  - Time slot booking with overlap detection
  - Waitlist management and promotion
  - Booking cancellation and rescheduling
  - Multi-resource pool allocation

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program ResourceBooking;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB, Variants, NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;

{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  // Resource pools - groups of equivalent resources
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS resource_pools (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  pool_id TEXT NOT NULL,' +
    '  pool_name TEXT NOT NULL,' +
    '  pool_type TEXT NOT NULL,' +  // room, equipment, vehicle, person
    '  description TEXT DEFAULT '''',' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  UNIQUE(pool_id)' +
    ')'
  );

  // Resources - individual bookable items
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS resources (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  resource_id TEXT NOT NULL,' +
    '  resource_name TEXT NOT NULL,' +
    '  pool_id TEXT NOT NULL,' +
    '  capacity INTEGER DEFAULT 1,' +
    '  status TEXT DEFAULT ''available'',' +  // available, maintenance, retired
    '  attributes TEXT DEFAULT '''',' +  // JSON attributes
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  UNIQUE(resource_id)' +
    ')'
  );

  // Bookings
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS bookings (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  booking_id TEXT NOT NULL,' +
    '  resource_id TEXT NOT NULL,' +
    '  booked_by TEXT NOT NULL,' +
    '  booking_date TEXT NOT NULL,' +
    '  start_time TEXT NOT NULL,' +
    '  end_time TEXT NOT NULL,' +
    '  purpose TEXT DEFAULT '''',' +
    '  status TEXT DEFAULT ''confirmed'',' +  // confirmed, cancelled, completed
    '  cancelled_at TEXT,' +
    '  cancel_reason TEXT,' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  UNIQUE(booking_id)' +
    ')'
  );

  // Waitlist
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS waitlist (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  waitlist_id TEXT NOT NULL,' +
    '  pool_id TEXT NOT NULL,' +
    '  requested_by TEXT NOT NULL,' +
    '  requested_date TEXT NOT NULL,' +
    '  start_time TEXT NOT NULL,' +
    '  end_time TEXT NOT NULL,' +
    '  purpose TEXT DEFAULT '''',' +
    '  priority INTEGER DEFAULT 0,' +  // higher = more priority
    '  status TEXT DEFAULT ''waiting'',' +  // waiting, promoted, expired, cancelled
    '  promoted_to_booking TEXT,' +  // booking_id if promoted
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  UNIQUE(waitlist_id)' +
    ')'
  );

  // Availability schedule (operating hours per resource)
  Conn.ExecuteNonQuery(
    'CREATE TABLE IF NOT EXISTS availability_schedule (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  resource_id TEXT NOT NULL,' +
    '  day_of_week INTEGER NOT NULL,' +  // 1=Mon..7=Sun
    '  open_time TEXT NOT NULL,' +
    '  close_time TEXT NOT NULL,' +
    '  UNIQUE(resource_id, day_of_week)' +
    ')'
  );
end;

{ Inserts a new resource pool record with the given ID, name, type, and description. }
procedure CreatePool(const PoolId, PoolName, PoolType, Description: string);
begin
  Conn.ExecuteNonQuery(Format(
    'INSERT INTO resource_pools (pool_id, pool_name, pool_type, description) ' +
    'VALUES (''%s'', ''%s'', ''%s'', ''%s'')',
    [PoolId, PoolName, PoolType, Description]));
end;

{ Inserts a new bookable resource into the specified pool with given capacity and attributes. }
procedure CreateResource(const ResourceId, ResourceName, PoolId: string;
  Capacity: Integer; const Attributes: string);
begin
  Conn.ExecuteNonQuery(Format(
    'INSERT INTO resources (resource_id, resource_name, pool_id, capacity, attributes) ' +
    'VALUES (''%s'', ''%s'', ''%s'', %d, ''%s'')',
    [ResourceId, ResourceName, PoolId, Capacity, Attributes]));
end;

{ Checks whether the time slot conflicts with existing bookings for the resource. }
function HasOverlap(const ResourceId, BookingDate, StartTime, EndTime: string): Boolean;
var
  DS: TDataSet;
begin
  DS := Conn.ExecuteQuery(Format(
    'SELECT COUNT(*) as cnt FROM bookings ' +
    'WHERE resource_id = ''%s'' AND booking_date = ''%s'' ' +
    'AND status = ''confirmed'' ' +
    'AND start_time < ''%s'' AND end_time > ''%s''',
    [ResourceId, BookingDate, EndTime, StartTime]));
  try
    Result := DS.FieldByName('cnt').AsInteger > 0;
  finally
    DS.Free;
  end;
end;

{ Attempts to create a confirmed booking for a specific resource after verifying no time overlap exists; returns True on success. }
function BookResource(const BookingId, ResourceId, BookedBy, BookingDate,
  StartTime, EndTime, Purpose: string): Boolean;
begin
  // Check for overlap
  if HasOverlap(ResourceId, BookingDate, StartTime, EndTime) then
  begin
    Result := False;
    Exit;
  end;

  Conn.ExecuteNonQuery(Format(
    'INSERT INTO bookings (booking_id, resource_id, booked_by, booking_date, ' +
    'start_time, end_time, purpose) ' +
    'VALUES (''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'')',
    [BookingId, ResourceId, BookedBy, BookingDate, StartTime, EndTime, Purpose]));

  Result := True;
end;

{ Searches the specified pool for an available resource with no conflicting booking in the given time slot and returns its ID, or empty string if none found. }
function FindAvailableResource(const PoolId, BookingDate, StartTime, EndTime: string): string;
var
  DS: TDataSet;
begin
  Result := '';

  // Find a resource in the pool with no overlapping booking
  DS := Conn.ExecuteQuery(Format(
    'SELECT r.resource_id FROM resources r ' +
    'WHERE r.pool_id = ''%s'' AND r.status = ''available'' ' +
    'AND r.resource_id NOT IN (' +
    '  SELECT b.resource_id FROM bookings b ' +
    '  WHERE b.booking_date = ''%s'' AND b.status = ''confirmed'' ' +
    '  AND b.start_time < ''%s'' AND b.end_time > ''%s''' +
    ') ORDER BY r.resource_name LIMIT 1',
    [PoolId, BookingDate, EndTime, StartTime]));
  try
    if not DS.IsEmpty then
      Result := DS.FieldByName('resource_id').AsString;
  finally
    DS.Free;
  end;
end;

{ Finds the first available resource in a pool for the requested time slot and books it; returns the assigned resource ID or empty string on failure. }
function BookFromPool(const BookingId, PoolId, BookedBy, BookingDate,
  StartTime, EndTime, Purpose: string): string;
var
  ResourceId: string;
begin
  Result := '';
  ResourceId := FindAvailableResource(PoolId, BookingDate, StartTime, EndTime);
  if ResourceId = '' then
    Exit;

  if BookResource(BookingId, ResourceId, BookedBy, BookingDate,
    StartTime, EndTime, Purpose) then
    Result := ResourceId;
end;

{ Inserts a new waitlist entry for the specified pool, date, time slot, and priority level. }
procedure AddToWaitlist(const WaitlistId, PoolId, RequestedBy, RequestedDate,
  StartTime, EndTime, Purpose: string; Priority: Integer);
begin
  Conn.ExecuteNonQuery(Format(
    'INSERT INTO waitlist (waitlist_id, pool_id, requested_by, requested_date, ' +
    'start_time, end_time, purpose, priority) ' +
    'VALUES (''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', %d)',
    [WaitlistId, PoolId, RequestedBy, RequestedDate, StartTime, EndTime, Purpose, Priority]));
end;

{ Marks a confirmed booking as cancelled with the current timestamp and the provided reason. }
procedure CancelBooking(const BookingId, Reason: string);
begin
  Conn.ExecuteNonQuery(Format(
    'UPDATE bookings SET status = ''cancelled'', cancelled_at = datetime(''now''), ' +
    'cancel_reason = ''%s'' WHERE booking_id = ''%s'' AND status = ''confirmed''',
    [Reason, BookingId]));
end;

{ Finds the highest-priority waiting entry matching a freed time slot, books it from the pool, and updates the waitlist entry to promoted status. }
function PromoteFromWaitlist(const PoolId, BookingDate, StartTime, EndTime: string): string;
var
  DS: TDataSet;
  WaitlistId, RequestedBy, Purpose: string;
  ResourceId: string;
  NewBookingId: string;
begin
  Result := '';

  // Find highest-priority waitlist entry that matches the freed slot
  DS := Conn.ExecuteQuery(Format(
    'SELECT waitlist_id, requested_by, purpose FROM waitlist ' +
    'WHERE pool_id = ''%s'' AND requested_date = ''%s'' ' +
    'AND start_time = ''%s'' AND end_time = ''%s'' ' +
    'AND status = ''waiting'' ' +
    'ORDER BY priority DESC, created_at ASC LIMIT 1',
    [PoolId, BookingDate, StartTime, EndTime]));
  try
    if DS.IsEmpty then Exit;
    WaitlistId := DS.FieldByName('waitlist_id').AsString;
    RequestedBy := DS.FieldByName('requested_by').AsString;
    Purpose := DS.FieldByName('purpose').AsString;
  finally
    DS.Free;
  end;

  // Try to book
  NewBookingId := 'bk-promoted-' + WaitlistId;
  ResourceId := BookFromPool(NewBookingId, PoolId, RequestedBy, BookingDate,
    StartTime, EndTime, Purpose);

  if ResourceId <> '' then
  begin
    // Update waitlist entry
    Conn.ExecuteNonQuery(Format(
      'UPDATE waitlist SET status = ''promoted'', promoted_to_booking = ''%s'' ' +
      'WHERE waitlist_id = ''%s''',
      [NewBookingId, WaitlistId]));
    Result := WaitlistId;
  end;
end;

// ============================================================
// Demo Sections
// ============================================================

{ Creates three resource pools (rooms, vehicles, equipment) and populates each with individual resources, then lists all pools with their resource counts. }
procedure Demo1_SetupResourcePools;
var
  DS: TDataSet;
begin
  WriteLn('=== 1. Setup Resource Pools ===');
  WriteLn;

  // Create pools
  CreatePool('pool-rooms', 'Meeting Rooms', 'room', 'Conference and huddle rooms');
  CreatePool('pool-vehicles', 'Company Vehicles', 'vehicle', 'Fleet vehicles');
  CreatePool('pool-equipment', 'AV Equipment', 'equipment', 'Projectors and cameras');

  // Add resources to rooms pool
  CreateResource('room-a', 'Conference Room A', 'pool-rooms', 20, '{"projector": true, "whiteboard": true}');
  CreateResource('room-b', 'Conference Room B', 'pool-rooms', 10, '{"projector": true, "video_conf": true}');
  CreateResource('room-c', 'Huddle Room C', 'pool-rooms', 4, '{"whiteboard": true}');
  CreateResource('room-d', 'Board Room D', 'pool-rooms', 30, '{"projector": true, "video_conf": true, "catering": true}');

  // Add vehicles
  CreateResource('car-001', 'Sedan (White)', 'pool-vehicles', 4, '{"type": "sedan", "fuel": "electric"}');
  CreateResource('car-002', 'SUV (Black)', 'pool-vehicles', 7, '{"type": "suv", "fuel": "hybrid"}');
  CreateResource('van-001', 'Cargo Van', 'pool-vehicles', 2, '{"type": "van", "fuel": "diesel"}');

  // Add equipment
  CreateResource('proj-001', 'Portable Projector', 'pool-equipment', 1, '{"lumens": 3000}');
  CreateResource('proj-002', 'HD Projector', 'pool-equipment', 1, '{"lumens": 5000}');
  CreateResource('cam-001', 'Video Camera', 'pool-equipment', 1, '{"resolution": "4K"}');

  WriteLn('   Pools created:');
  DS := Conn.ExecuteQuery(
    'SELECT p.pool_id, p.pool_name, p.pool_type, COUNT(r.id) as resource_count ' +
    'FROM resource_pools p LEFT JOIN resources r ON r.pool_id = p.pool_id ' +
    'GROUP BY p.pool_id ORDER BY p.pool_id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-16s %-20s %-10s %d resources',
        [DS.FieldByName('pool_id').AsString,
         DS.FieldByName('pool_name').AsString,
         DS.FieldByName('pool_type').AsString,
         DS.FieldByName('resource_count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Books several resources (rooms, a vehicle, and equipment) with direct resource ID specification and reports success or failure for each. }
procedure Demo2_BookResources;
begin
  WriteLn('=== 2. Book Resources ===');
  WriteLn;

  // Direct resource booking
  if BookResource('bk-001', 'room-a', 'alice', '2026-02-05', '09:00', '10:00', 'Sprint Planning') then
    WriteLn('   [OK] bk-001: Room A booked by Alice (09:00-10:00)')
  else
    WriteLn('   [FAIL] bk-001: Could not book Room A');

  if BookResource('bk-002', 'room-a', 'bob', '2026-02-05', '10:00', '11:30', 'Design Review') then
    WriteLn('   [OK] bk-002: Room A booked by Bob (10:00-11:30)')
  else
    WriteLn('   [FAIL] bk-002: Could not book Room A');

  if BookResource('bk-003', 'room-b', 'carol', '2026-02-05', '09:00', '10:30', 'Client Call') then
    WriteLn('   [OK] bk-003: Room B booked by Carol (09:00-10:30)')
  else
    WriteLn('   [FAIL] bk-003: Could not book Room B');

  // Vehicle booking
  if BookResource('bk-004', 'car-001', 'dave', '2026-02-05', '08:00', '12:00', 'Client visit') then
    WriteLn('   [OK] bk-004: Sedan booked by Dave (08:00-12:00)')
  else
    WriteLn('   [FAIL] bk-004: Could not book Sedan');

  // Equipment booking
  if BookResource('bk-005', 'proj-001', 'eve', '2026-02-05', '14:00', '16:00', 'Presentation') then
    WriteLn('   [OK] bk-005: Projector booked by Eve (14:00-16:00)')
  else
    WriteLn('   [FAIL] bk-005: Could not book Projector');

  WriteLn;
end;

{ Tests booking attempts that overlap, partially overlap, are adjacent to, or are on different dates than existing bookings, showing which are blocked and which succeed. }
procedure Demo3_OverlapDetection;
begin
  WriteLn('=== 3. Overlap Detection ===');
  WriteLn;

  // Try to book Room A during Alice's booking
  WriteLn('   Attempting to book Room A 09:30-10:30 (overlaps Alice''s 09:00-10:00):');
  if BookResource('bk-006', 'room-a', 'frank', '2026-02-05', '09:30', '10:30', 'Meeting') then
    WriteLn('   [OK] Booked')
  else
    WriteLn('   [BLOCKED] Overlap detected - booking rejected');

  // Try exact same slot
  WriteLn('   Attempting to book Room A 09:00-10:00 (exact same slot):');
  if BookResource('bk-007', 'room-a', 'grace', '2026-02-05', '09:00', '10:00', 'Meeting') then
    WriteLn('   [OK] Booked')
  else
    WriteLn('   [BLOCKED] Overlap detected - booking rejected');

  // Adjacent slot (no overlap - ends when next begins)
  WriteLn('   Attempting to book Room A 11:30-12:30 (after Bob''s 10:00-11:30):');
  if BookResource('bk-008', 'room-a', 'henry', '2026-02-05', '11:30', '12:30', 'Lunch meeting') then
    WriteLn('   [OK] No overlap - booking confirmed')
  else
    WriteLn('   [BLOCKED] Overlap detected');

  // Different date (no overlap)
  WriteLn('   Attempting to book Room A 09:00-10:00 on different date:');
  if BookResource('bk-009', 'room-a', 'iris', '2026-02-06', '09:00', '10:00', 'Follow-up') then
    WriteLn('   [OK] Different date - booking confirmed')
  else
    WriteLn('   [BLOCKED] Overlap detected');

  WriteLn;
end;

{ Requests bookings from the room pool without specifying a resource, showing automatic assignment until the pool is fully booked for the time slot. }
procedure Demo4_PoolBasedBooking;
var
  ResourceId: string;
begin
  WriteLn('=== 4. Pool-Based Booking (Any Available Resource) ===');
  WriteLn;

  WriteLn('   Booking from room pool (09:00-10:00 on Feb 5):');
  WriteLn('   Room A: already booked (Alice)');
  WriteLn('   Room B: already booked (Carol)');

  ResourceId := BookFromPool('bk-010', 'pool-rooms', 'jack', '2026-02-05',
    '09:00', '10:00', 'Team sync');
  if ResourceId <> '' then
    WriteLn(Format('   [OK] Auto-assigned: %s', [ResourceId]))
  else
    WriteLn('   [FAIL] No available room in pool');

  // Book another from pool at same time
  ResourceId := BookFromPool('bk-011', 'pool-rooms', 'kate', '2026-02-05',
    '09:00', '10:00', 'HR meeting');
  if ResourceId <> '' then
    WriteLn(Format('   [OK] Auto-assigned: %s', [ResourceId]))
  else
    WriteLn('   [FAIL] No available room in pool');

  // All rooms now booked for 09:00-10:00, next should fail
  ResourceId := BookFromPool('bk-012', 'pool-rooms', 'liam', '2026-02-05',
    '09:00', '10:00', 'Quick chat');
  if ResourceId <> '' then
    WriteLn(Format('   [OK] Auto-assigned: %s', [ResourceId]))
  else
    WriteLn('   [FULL] All rooms in pool booked for this slot');

  WriteLn;
end;

{ Demonstrates waitlist management and automatic promotion on cancellation. }
procedure Demo5_Waitlist;
var
  DS: TDataSet;
begin
  WriteLn('=== 5. Waitlist ===');
  WriteLn;

  // Add to waitlist since all rooms are booked
  AddToWaitlist('wl-001', 'pool-rooms', 'liam', '2026-02-05',
    '09:00', '10:00', 'Quick chat', 1);
  WriteLn('   Added to waitlist: liam (priority 1)');

  AddToWaitlist('wl-002', 'pool-rooms', 'mia', '2026-02-05',
    '09:00', '10:00', 'Important meeting', 5);
  WriteLn('   Added to waitlist: mia (priority 5)');

  AddToWaitlist('wl-003', 'pool-rooms', 'noah', '2026-02-05',
    '09:00', '10:00', 'Optional sync', 0);
  WriteLn('   Added to waitlist: noah (priority 0)');

  WriteLn;
  WriteLn('   Waitlist queue:');
  DS := Conn.ExecuteQuery(
    'SELECT waitlist_id, requested_by, purpose, priority, status ' +
    'FROM waitlist ORDER BY priority DESC, created_at ASC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-8s %-8s priority:%d  "%s"  [%s]',
        [DS.FieldByName('waitlist_id').AsString,
         DS.FieldByName('requested_by').AsString,
         DS.FieldByName('priority').AsInteger,
         DS.FieldByName('purpose').AsString,
         DS.FieldByName('status').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Cancels an existing booking and triggers waitlist promotion, assigning the freed resource to the highest-priority waiting entry. }
procedure Demo6_CancellationAndPromotion;
var
  Promoted: string;
  DS: TDataSet;
begin
  WriteLn('=== 6. Cancellation with Waitlist Promotion ===');
  WriteLn;

  // Get the resource for Alice's booking
  DS := Conn.ExecuteQuery(
    'SELECT resource_id FROM bookings WHERE booking_id = ''bk-001''');
  try
    WriteLn(Format('   Alice''s booking (bk-001): Room A, 09:00-10:00', []));
  finally
    DS.Free;
  end;

  // Cancel Alice's booking
  CancelBooking('bk-001', 'Meeting moved to next week');
  WriteLn('   [CANCELLED] bk-001 cancelled: "Meeting moved to next week"');

  // Room A is now free at 09:00-10:00, promote from waitlist
  WriteLn;
  WriteLn('   Attempting waitlist promotion...');
  Promoted := PromoteFromWaitlist('pool-rooms', '2026-02-05', '09:00', '10:00');
  if Promoted <> '' then
    WriteLn(Format('   [PROMOTED] Waitlist entry %s promoted to booking!', [Promoted]))
  else
    WriteLn('   No waitlist entry matched the freed slot');

  WriteLn;
  WriteLn('   Updated waitlist:');
  DS := Conn.ExecuteQuery(
    'SELECT waitlist_id, requested_by, status, promoted_to_booking, priority ' +
    'FROM waitlist ORDER BY priority DESC, created_at ASC');
  try
    while not DS.EOF do
    begin
      if DS.FieldByName('status').AsString = 'promoted' then
        WriteLn(Format('   %-8s %-8s [%s] -> booking: %s',
          [DS.FieldByName('waitlist_id').AsString,
           DS.FieldByName('requested_by').AsString,
           DS.FieldByName('status').AsString,
           DS.FieldByName('promoted_to_booking').AsString]))
      else
        WriteLn(Format('   %-8s %-8s [%s]  priority:%d',
          [DS.FieldByName('waitlist_id').AsString,
           DS.FieldByName('requested_by').AsString,
           DS.FieldByName('status').AsString,
           DS.FieldByName('priority').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Displays all bookings (confirmed and cancelled) for Room A on a specific date, plus all confirmed bookings across all resources for that date. }
procedure Demo7_TimeSlotAvailability;
var
  DS: TDataSet;
begin
  WriteLn('=== 7. Time Slot Availability ===');
  WriteLn;

  WriteLn('   Room A availability on 2026-02-05:');
  WriteLn('   (Showing booked slots)');
  DS := Conn.ExecuteQuery(
    'SELECT start_time, end_time, booked_by, purpose, status ' +
    'FROM bookings WHERE resource_id = ''room-a'' AND booking_date = ''2026-02-05'' ' +
    'ORDER BY start_time');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %s-%s  %-8s %-20s [%s]',
        [DS.FieldByName('start_time').AsString,
         DS.FieldByName('end_time').AsString,
         DS.FieldByName('booked_by').AsString,
         DS.FieldByName('purpose').AsString,
         DS.FieldByName('status').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   All confirmed bookings for Feb 5:');
  DS := Conn.ExecuteQuery(
    'SELECT b.resource_id, r.resource_name, b.start_time, b.end_time, b.booked_by ' +
    'FROM bookings b JOIN resources r ON r.resource_id = b.resource_id ' +
    'WHERE b.booking_date = ''2026-02-05'' AND b.status = ''confirmed'' ' +
    'ORDER BY b.resource_id, b.start_time');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-8s %-20s %s-%s  %s',
        [DS.FieldByName('resource_id').AsString,
         DS.FieldByName('resource_name').AsString,
         DS.FieldByName('start_time').AsString,
         DS.FieldByName('end_time').AsString,
         DS.FieldByName('booked_by').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
end;

{ Sets a resource to maintenance status, verifies it is skipped during pool-based booking, displays all resource statuses, then restores the resource to available. }
procedure Demo8_ResourceMaintenance;
var
  DS: TDataSet;
  ResourceId: string;
begin
  WriteLn('=== 8. Resource Maintenance ===');
  WriteLn;

  // Put room-c under maintenance
  Conn.ExecuteNonQuery(
    'UPDATE resources SET status = ''maintenance'' WHERE resource_id = ''room-c''');
  WriteLn('   Huddle Room C set to maintenance');

  // Try to book from pool - room-c should be skipped
  WriteLn('   Booking from pool (room-c under maintenance):');
  ResourceId := BookFromPool('bk-013', 'pool-rooms', 'olivia', '2026-02-06',
    '09:00', '10:00', 'Planning');
  if ResourceId <> '' then
    WriteLn(Format('   [OK] Assigned: %s (room-c skipped)', [ResourceId]))
  else
    WriteLn('   [FAIL] No available rooms');

  WriteLn;
  WriteLn('   Resource statuses:');
  DS := Conn.ExecuteQuery(
    'SELECT resource_id, resource_name, pool_id, status FROM resources ORDER BY pool_id, resource_id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s %-22s %-16s %s',
        [DS.FieldByName('resource_id').AsString,
         DS.FieldByName('resource_name').AsString,
         DS.FieldByName('pool_id').AsString,
         DS.FieldByName('status').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Restore
  Conn.ExecuteNonQuery(
    'UPDATE resources SET status = ''available'' WHERE resource_id = ''room-c''');

  WriteLn;
end;

{ Books a vehicle across three consecutive days and then verifies that a conflicting booking on one of those days is correctly rejected. }
procedure Demo9_MultiDayBooking;
var
  I: Integer;
  DateStr: string;
begin
  WriteLn('=== 9. Multi-Day Booking Pattern ===');
  WriteLn;

  WriteLn('   Booking car-002 for a 3-day trip (Feb 10-12):');
  for I := 10 to 12 do
  begin
    DateStr := Format('2026-02-%0.2d', [I]);
    if BookResource(Format('bk-trip-%d', [I]), 'car-002', 'peter', DateStr,
      '08:00', '18:00', 'Business trip') then
      WriteLn(Format('   [OK] %s 08:00-18:00', [DateStr]))
    else
      WriteLn(Format('   [FAIL] %s overlap detected', [DateStr]));
  end;

  // Try to book same car on Feb 11
  WriteLn;
  WriteLn('   Attempting to book car-002 on Feb 11 (already booked):');
  if BookResource('bk-014', 'car-002', 'quinn', '2026-02-11', '10:00', '14:00', 'Delivery') then
    WriteLn('   [OK] Booked')
  else
    WriteLn('   [BLOCKED] Car-002 already booked for this date');

  WriteLn;
end;

{ Queries and displays booking counts by status, waitlist counts by status, per-resource utilization for a specific date, and overall summary with cancellation rate. }
procedure Demo10_Statistics;
var
  DS: TDataSet;
  TotalBookings, ConfirmedBookings, CancelledBookings: Integer;
  WaitlistCount, PromotedCount: Integer;
begin
  WriteLn('=== 10. Statistics ===');
  WriteLn;

  // Booking stats
  DS := Conn.ExecuteQuery(
    'SELECT status, COUNT(*) as cnt FROM bookings GROUP BY status ORDER BY status');
  try
    WriteLn('   Bookings by status:');
    TotalBookings := 0;
    ConfirmedBookings := 0;
    CancelledBookings := 0;
    while not DS.EOF do
    begin
      WriteLn(Format('   %-12s %d', [DS.FieldByName('status').AsString,
        DS.FieldByName('cnt').AsInteger]));
      if DS.FieldByName('status').AsString = 'confirmed' then
        ConfirmedBookings := DS.FieldByName('cnt').AsInteger
      else if DS.FieldByName('status').AsString = 'cancelled' then
        CancelledBookings := DS.FieldByName('cnt').AsInteger;
      TotalBookings := TotalBookings + DS.FieldByName('cnt').AsInteger;
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Waitlist stats
  DS := Conn.ExecuteQuery(
    'SELECT status, COUNT(*) as cnt FROM waitlist GROUP BY status ORDER BY status');
  try
    WriteLn;
    WriteLn('   Waitlist by status:');
    WaitlistCount := 0;
    PromotedCount := 0;
    while not DS.EOF do
    begin
      WriteLn(Format('   %-12s %d', [DS.FieldByName('status').AsString,
        DS.FieldByName('cnt').AsInteger]));
      if DS.FieldByName('status').AsString = 'waiting' then
        WaitlistCount := DS.FieldByName('cnt').AsInteger
      else if DS.FieldByName('status').AsString = 'promoted' then
        PromotedCount := DS.FieldByName('cnt').AsInteger;
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Resource utilization
  WriteLn;
  WriteLn('   Resource utilization (Feb 5):');
  DS := Conn.ExecuteQuery(
    'SELECT r.resource_id, r.resource_name, COUNT(b.id) as booking_count ' +
    'FROM resources r LEFT JOIN bookings b ON b.resource_id = r.resource_id ' +
    'AND b.booking_date = ''2026-02-05'' AND b.status = ''confirmed'' ' +
    'GROUP BY r.resource_id ORDER BY booking_count DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s %-22s %d booking(s)',
        [DS.FieldByName('resource_id').AsString,
         DS.FieldByName('resource_name').AsString,
         DS.FieldByName('booking_count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Summary:');
  WriteLn(Format('   Total bookings: %d (confirmed: %d, cancelled: %d)',
    [TotalBookings, ConfirmedBookings, CancelledBookings]));
  WriteLn(Format('   Waitlist: %d waiting, %d promoted', [WaitlistCount, PromotedCount]));
  WriteLn(Format('   Pools: %d, Resources: %d',
    [3, 10]));
  if TotalBookings > 0 then
    WriteLn(Format('   Cancellation rate: %.1f%%',
      [CancelledBookings * 100.0 / TotalBookings]));

  WriteLn;
end;

// ============================================================
// Main
// ============================================================
begin
  WriteLn('Example 122: Resource Booking - Pools, Availability, Waitlists, Cancellation');
  WriteLn(StringOfChar('=', 78));
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    CreateSchema;

    Demo1_SetupResourcePools;
    Demo2_BookResources;
    Demo3_OverlapDetection;
    Demo4_PoolBasedBooking;
    Demo5_Waitlist;
    Demo6_CancellationAndPromotion;
    Demo7_TimeSlotAvailability;
    Demo8_ResourceMaintenance;
    Demo9_MultiDayBooking;
    Demo10_Statistics;

    WriteLn('Done.');
  finally
    Conn.Close;
    Conn.Free;
  end;
end.
