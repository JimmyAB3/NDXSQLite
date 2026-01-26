{===============================================================================
  NDXSQLite Example 79 - Booking and Reservation System
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  A comprehensive booking/reservation system demonstrating:
  - Resource management (rooms, equipment, venues)
  - Time slot management with conflict detection
  - Overbooking prevention
  - Booking status workflow (pending, confirmed, cancelled, completed)
  - Cancellation and modification handling
  - Availability queries
  - Waitlist management
  - Recurring bookings
  - Booking policies and rules

  Cross-platform: Windows, Linux, macOS
===============================================================================}

program BookingReservation;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, DateUtils, Variants,
  ndxsqliteconnection;

var
  Conn: TNDXSQLiteConnection;

// =============================================================================
// Helper Functions
// =============================================================================
{ Returns TrueVal if Condition is true, otherwise returns FalseVal. }
function IfThen(Condition: Boolean; const TrueVal, FalseVal: string): string;
begin
  if Condition then
    Result := TrueVal
  else
    Result := FalseVal;
end;

{ Returns a text icon representing the booking status. }
function StatusToIcon(const Status: string): string;
begin
  if Status = 'confirmed' then Result := '[OK]'
  else if Status = 'pending' then Result := '[?]'
  else if Status = 'cancelled' then Result := '[X]'
  else if Status = 'completed' then Result := '[v]'
  else if Status = 'no_show' then Result := '[-]'
  else Result := '[ ]';
end;

// =============================================================================
// Schema Creation
// =============================================================================
{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  WriteLn('1. Creating Booking System Schema');
  WriteLn('   ================================');

  // Resource types (meeting room, conference hall, equipment, etc.)
  Conn.ExecuteNonQuery(
    'CREATE TABLE resource_types (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT UNIQUE NOT NULL,' +
    '  description TEXT,' +
    '  requires_approval INTEGER DEFAULT 0,' +
    '  max_duration_hours INTEGER DEFAULT 8,' +
    '  min_advance_hours INTEGER DEFAULT 1,' +
    '  max_advance_days INTEGER DEFAULT 30' +
    ')');

  // Resources (individual rooms, equipment items, etc.)
  Conn.ExecuteNonQuery(
    'CREATE TABLE resources (' +
    '  id INTEGER PRIMARY KEY,' +
    '  type_id INTEGER NOT NULL REFERENCES resource_types(id),' +
    '  name TEXT NOT NULL,' +
    '  description TEXT,' +
    '  capacity INTEGER,' +
    '  location TEXT,' +
    '  amenities TEXT,' +
    '  hourly_rate REAL DEFAULT 0,' +
    '  is_active INTEGER DEFAULT 1,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Users who can make bookings
  Conn.ExecuteNonQuery(
    'CREATE TABLE users (' +
    '  id INTEGER PRIMARY KEY,' +
    '  username TEXT UNIQUE NOT NULL,' +
    '  email TEXT UNIQUE NOT NULL,' +
    '  display_name TEXT,' +
    '  department TEXT,' +
    '  can_approve INTEGER DEFAULT 0,' +
    '  booking_limit INTEGER DEFAULT 5,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Main bookings table
  Conn.ExecuteNonQuery(
    'CREATE TABLE bookings (' +
    '  id INTEGER PRIMARY KEY,' +
    '  resource_id INTEGER NOT NULL REFERENCES resources(id),' +
    '  user_id INTEGER NOT NULL REFERENCES users(id),' +
    '  title TEXT NOT NULL,' +
    '  description TEXT,' +
    '  start_time TEXT NOT NULL,' +
    '  end_time TEXT NOT NULL,' +
    '  status TEXT DEFAULT ''pending'' CHECK(status IN (''pending'', ''confirmed'', ''cancelled'', ''completed'', ''no_show'')),' +
    '  attendees INTEGER DEFAULT 1,' +
    '  notes TEXT,' +
    '  approved_by INTEGER REFERENCES users(id),' +
    '  approved_at TEXT,' +
    '  cancelled_at TEXT,' +
    '  cancellation_reason TEXT,' +
    '  total_cost REAL DEFAULT 0,' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  updated_at TEXT DEFAULT (datetime(''now'')),' +
    '  CHECK(end_time > start_time)' +
    ')');

  // Waitlist for fully booked slots
  Conn.ExecuteNonQuery(
    'CREATE TABLE waitlist (' +
    '  id INTEGER PRIMARY KEY,' +
    '  resource_id INTEGER NOT NULL REFERENCES resources(id),' +
    '  user_id INTEGER NOT NULL REFERENCES users(id),' +
    '  desired_start TEXT NOT NULL,' +
    '  desired_end TEXT NOT NULL,' +
    '  status TEXT DEFAULT ''waiting'' CHECK(status IN (''waiting'', ''notified'', ''expired'', ''booked'')),' +
    '  priority INTEGER DEFAULT 0,' +
    '  notified_at TEXT,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Recurring booking patterns
  Conn.ExecuteNonQuery(
    'CREATE TABLE recurring_patterns (' +
    '  id INTEGER PRIMARY KEY,' +
    '  resource_id INTEGER NOT NULL REFERENCES resources(id),' +
    '  user_id INTEGER NOT NULL REFERENCES users(id),' +
    '  title TEXT NOT NULL,' +
    '  recurrence_type TEXT CHECK(recurrence_type IN (''daily'', ''weekly'', ''monthly'')),' +
    '  day_of_week INTEGER,' +
    '  start_time TEXT NOT NULL,' +
    '  end_time TEXT NOT NULL,' +
    '  valid_from TEXT NOT NULL,' +
    '  valid_until TEXT,' +
    '  is_active INTEGER DEFAULT 1,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Booking history/audit log
  Conn.ExecuteNonQuery(
    'CREATE TABLE booking_history (' +
    '  id INTEGER PRIMARY KEY,' +
    '  booking_id INTEGER NOT NULL REFERENCES bookings(id),' +
    '  action TEXT NOT NULL,' +
    '  old_status TEXT,' +
    '  new_status TEXT,' +
    '  changed_by INTEGER REFERENCES users(id),' +
    '  notes TEXT,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Resource availability exceptions (holidays, maintenance)
  Conn.ExecuteNonQuery(
    'CREATE TABLE availability_exceptions (' +
    '  id INTEGER PRIMARY KEY,' +
    '  resource_id INTEGER REFERENCES resources(id),' +
    '  exception_date TEXT NOT NULL,' +
    '  reason TEXT,' +
    '  is_closed INTEGER DEFAULT 1,' +
    '  start_time TEXT,' +
    '  end_time TEXT' +
    ')');

  // Indexes for performance
  Conn.ExecuteNonQuery('CREATE INDEX idx_bookings_resource ON bookings(resource_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_bookings_user ON bookings(user_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_bookings_time ON bookings(start_time, end_time)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_bookings_status ON bookings(status)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_waitlist_resource ON waitlist(resource_id)');

  WriteLn('   Created tables: resource_types, resources, users, bookings,');
  WriteLn('                   waitlist, recurring_patterns, booking_history,');
  WriteLn('                   availability_exceptions');
  WriteLn('');
end;

// =============================================================================
// Booking Functions
// =============================================================================
{ Returns True if any pending or confirmed booking on the given resource overlaps the specified time range, optionally excluding a specific booking ID. }
function CheckConflict(ResourceId: Integer; const StartTime, EndTime: string;
  ExcludeBookingId: Integer = 0): Boolean;
var
  Count: Integer;
begin
  Count := Conn.ExecuteScalar(
    'SELECT COUNT(*) FROM bookings ' +
    'WHERE resource_id = ? ' +
    'AND status IN (''pending'', ''confirmed'') ' +
    'AND id != ? ' +
    'AND start_time < ? AND end_time > ?',
    [ResourceId, ExcludeBookingId, EndTime, StartTime]);
  Result := Count > 0;
end;

{ Creates a new booking after checking for time conflicts, calculating cost from the resource's hourly rate, and setting the initial status to pending or confirmed based on the resource type's approval requirement. }
function CreateBooking(ResourceId, UserId: Integer; const Title, StartTime, EndTime: string;
  Attendees: Integer = 1): Integer;
var
  Rate, Hours, Cost: Double;
  RequiresApproval: Integer;
begin
  Result := -1;

  // Check for conflicts
  if CheckConflict(ResourceId, StartTime, EndTime) then
  begin
    WriteLn('     ERROR: Time slot conflict detected!');
    Exit;
  end;

  // Calculate cost using SQLite datetime functions
  Rate := Conn.ExecuteScalar('SELECT hourly_rate FROM resources WHERE id = ?', [ResourceId]);
  Hours := Conn.ExecuteScalar(
    'SELECT (julianday(?) - julianday(?)) * 24', [EndTime, StartTime]);
  Cost := Rate * Hours;

  // Check if approval is required
  RequiresApproval := Conn.ExecuteScalar(
    'SELECT rt.requires_approval FROM resources r ' +
    'JOIN resource_types rt ON r.type_id = rt.id ' +
    'WHERE r.id = ?', [ResourceId]);

  // Create booking
  Conn.ExecuteNonQuery(
    'INSERT INTO bookings (resource_id, user_id, title, start_time, end_time, attendees, total_cost, status) ' +
    'VALUES (?, ?, ?, ?, ?, ?, ?, ?)',
    [ResourceId, UserId, Title, StartTime, EndTime, Attendees, Cost,
     IfThen(RequiresApproval = 1, 'pending', 'confirmed')]);

  Result := Conn.ExecuteScalar('SELECT last_insert_rowid()');

  // Log creation
  Conn.ExecuteNonQuery(
    'INSERT INTO booking_history (booking_id, action, new_status, changed_by) ' +
    'VALUES (?, ''created'', ?, ?)',
    [Result, IfThen(RequiresApproval = 1, 'pending', 'confirmed'), UserId]);
end;

{ Transitions a pending booking to confirmed status, records the approver and timestamp, and logs the approval action in booking_history. }
procedure ApproveBooking(BookingId, ApproverId: Integer);
begin
  Conn.ExecuteNonQuery(
    'UPDATE bookings SET status = ''confirmed'', approved_by = ?, approved_at = datetime(''now''), ' +
    'updated_at = datetime(''now'') WHERE id = ? AND status = ''pending''',
    [ApproverId, BookingId]);

  Conn.ExecuteNonQuery(
    'INSERT INTO booking_history (booking_id, action, old_status, new_status, changed_by) ' +
    'VALUES (?, ''approved'', ''pending'', ''confirmed'', ?)',
    [BookingId, ApproverId]);
end;

{ Sets a booking's status to cancelled with a timestamp and reason, then records the status transition in booking_history. }
procedure CancelBooking(BookingId, UserId: Integer; const Reason: string);
var
  OldStatus: string;
begin
  OldStatus := VarToStr(Conn.ExecuteScalar('SELECT status FROM bookings WHERE id = ?', [BookingId]));

  Conn.ExecuteNonQuery(
    'UPDATE bookings SET status = ''cancelled'', cancelled_at = datetime(''now''), ' +
    'cancellation_reason = ?, updated_at = datetime(''now'') WHERE id = ?',
    [Reason, BookingId]);

  Conn.ExecuteNonQuery(
    'INSERT INTO booking_history (booking_id, action, old_status, new_status, changed_by, notes) ' +
    'VALUES (?, ''cancelled'', ?, ''cancelled'', ?, ?)',
    [BookingId, OldStatus, UserId, Reason]);
end;

{ Transitions a confirmed booking to completed status and records the completion in booking_history. }
procedure CompleteBooking(BookingId: Integer);
begin
  Conn.ExecuteNonQuery(
    'UPDATE bookings SET status = ''completed'', updated_at = datetime(''now'') ' +
    'WHERE id = ? AND status = ''confirmed''', [BookingId]);

  Conn.ExecuteNonQuery(
    'INSERT INTO booking_history (booking_id, action, old_status, new_status) ' +
    'VALUES (?, ''completed'', ''confirmed'', ''completed'')', [BookingId]);
end;

{ Inserts a waitlist entry for a user requesting a specific resource during a desired time range, with an optional priority level. }
procedure AddToWaitlist(ResourceId, UserId: Integer; const StartTime, EndTime: string; Priority: Integer = 0);
begin
  Conn.ExecuteNonQuery(
    'INSERT INTO waitlist (resource_id, user_id, desired_start, desired_end, priority) ' +
    'VALUES (?, ?, ?, ?, ?)',
    [ResourceId, UserId, StartTime, EndTime, Priority]);
end;

// =============================================================================
// Sample Data
// =============================================================================
{ Populates the database with resource types, rooms/equipment/vehicles, users with departments and permissions, bookings with conflict testing, waitlist entries, availability exceptions, and recurring patterns. }
procedure InsertSampleData;
var
  BookingId: Integer;
begin
  WriteLn('2. Creating Sample Data');
  WriteLn('   ======================');

  // Create resource types
  Conn.ExecuteNonQuery(
    'INSERT INTO resource_types (name, description, requires_approval, max_duration_hours, min_advance_hours) ' +
    'VALUES (''Meeting Room'', ''Standard meeting rooms'', 0, 4, 1)');
  Conn.ExecuteNonQuery(
    'INSERT INTO resource_types (name, description, requires_approval, max_duration_hours, min_advance_hours) ' +
    'VALUES (''Conference Hall'', ''Large conference facilities'', 1, 8, 24)');
  Conn.ExecuteNonQuery(
    'INSERT INTO resource_types (name, description, requires_approval, max_duration_hours) ' +
    'VALUES (''Equipment'', ''Projectors, cameras, etc.'', 0, 24)');
  Conn.ExecuteNonQuery(
    'INSERT INTO resource_types (name, description, requires_approval, max_duration_hours) ' +
    'VALUES (''Vehicle'', ''Company vehicles'', 1, 12)');

  WriteLn('   Created 4 resource types');

  // Create resources
  Conn.ExecuteNonQuery(
    'INSERT INTO resources (type_id, name, capacity, location, amenities, hourly_rate) ' +
    'VALUES (1, ''Room A101'', 6, ''Building A, Floor 1'', ''Whiteboard, TV Screen'', 25.00)');
  Conn.ExecuteNonQuery(
    'INSERT INTO resources (type_id, name, capacity, location, amenities, hourly_rate) ' +
    'VALUES (1, ''Room A102'', 10, ''Building A, Floor 1'', ''Whiteboard, Projector, Video Conf'', 35.00)');
  Conn.ExecuteNonQuery(
    'INSERT INTO resources (type_id, name, capacity, location, amenities, hourly_rate) ' +
    'VALUES (1, ''Room B201'', 4, ''Building B, Floor 2'', ''Whiteboard'', 20.00)');
  Conn.ExecuteNonQuery(
    'INSERT INTO resources (type_id, name, capacity, location, amenities, hourly_rate) ' +
    'VALUES (2, ''Main Auditorium'', 200, ''Building A, Ground Floor'', ''Stage, Microphones, Projector'', 150.00)');
  Conn.ExecuteNonQuery(
    'INSERT INTO resources (type_id, name, capacity, location, amenities, hourly_rate) ' +
    'VALUES (3, ''Projector P1'', 1, ''IT Storage'', ''4K Resolution'', 10.00)');
  Conn.ExecuteNonQuery(
    'INSERT INTO resources (type_id, name, capacity, location, hourly_rate) ' +
    'VALUES (4, ''Van V01'', 8, ''Parking Lot B'', 30.00)');

  WriteLn('   Created 6 resources');

  // Create users
  Conn.ExecuteNonQuery(
    'INSERT INTO users (username, email, display_name, department, can_approve, booking_limit) ' +
    'VALUES (''admin'', ''admin@company.com'', ''System Admin'', ''IT'', 1, 99)');
  Conn.ExecuteNonQuery(
    'INSERT INTO users (username, email, display_name, department, booking_limit) ' +
    'VALUES (''alice'', ''alice@company.com'', ''Alice Johnson'', ''Engineering'', 5)');
  Conn.ExecuteNonQuery(
    'INSERT INTO users (username, email, display_name, department, booking_limit) ' +
    'VALUES (''bob'', ''bob@company.com'', ''Bob Smith'', ''Marketing'', 5)');
  Conn.ExecuteNonQuery(
    'INSERT INTO users (username, email, display_name, department, booking_limit) ' +
    'VALUES (''carol'', ''carol@company.com'', ''Carol White'', ''HR'', 5)');

  WriteLn('   Created 4 users');

  // Create bookings
  WriteLn('');
  WriteLn('   Creating bookings:');

  BookingId := CreateBooking(1, 2, 'Sprint Planning', '2025-01-20 09:00:00', '2025-01-20 11:00:00', 5);
  if BookingId > 0 then WriteLn('     + Room A101: Sprint Planning (Alice)');

  BookingId := CreateBooking(2, 2, 'Team Standup', '2025-01-20 09:30:00', '2025-01-20 10:00:00', 8);
  if BookingId > 0 then WriteLn('     + Room A102: Team Standup (Alice)');

  BookingId := CreateBooking(1, 3, 'Client Meeting', '2025-01-20 14:00:00', '2025-01-20 15:30:00', 4);
  if BookingId > 0 then WriteLn('     + Room A101: Client Meeting (Bob)');

  BookingId := CreateBooking(4, 3, 'Company All-Hands', '2025-01-21 10:00:00', '2025-01-21 12:00:00', 150);
  if BookingId > 0 then WriteLn('     + Auditorium: Company All-Hands (Bob) - Pending approval');

  BookingId := CreateBooking(3, 4, 'Interview', '2025-01-20 11:00:00', '2025-01-20 12:00:00', 3);
  if BookingId > 0 then WriteLn('     + Room B201: Interview (Carol)');

  BookingId := CreateBooking(5, 2, 'Projector for demo', '2025-01-20 14:00:00', '2025-01-20 17:00:00', 1);
  if BookingId > 0 then WriteLn('     + Projector P1: Demo equipment (Alice)');

  BookingId := CreateBooking(6, 3, 'Client Visit Transport', '2025-01-22 08:00:00', '2025-01-22 18:00:00', 5);
  if BookingId > 0 then WriteLn('     + Van V01: Client Visit (Bob) - Pending approval');

  // Test conflict detection
  WriteLn('');
  WriteLn('   Testing conflict detection:');
  BookingId := CreateBooking(1, 4, 'Conflicting Meeting', '2025-01-20 10:00:00', '2025-01-20 12:00:00', 3);

  // Approve some pending bookings
  WriteLn('');
  WriteLn('   Approving bookings:');
  ApproveBooking(4, 1);  // All-Hands approved by admin
  WriteLn('     Approved: Company All-Hands');

  // Add to waitlist
  WriteLn('');
  WriteLn('   Adding to waitlist:');
  AddToWaitlist(1, 4, '2025-01-20 09:00:00', '2025-01-20 10:30:00', 1);
  WriteLn('     + Carol added to waitlist for Room A101 (09:00-10:30)');

  // Add availability exception
  Conn.ExecuteNonQuery(
    'INSERT INTO availability_exceptions (resource_id, exception_date, reason, is_closed) ' +
    'VALUES (4, ''2025-01-25'', ''Auditorium Maintenance'', 1)');
  WriteLn('');
  WriteLn('   Added availability exception: Auditorium closed Jan 25');

  // Create a recurring pattern
  Conn.ExecuteNonQuery(
    'INSERT INTO recurring_patterns (resource_id, user_id, title, recurrence_type, day_of_week, ' +
    'start_time, end_time, valid_from, valid_until) ' +
    'VALUES (2, 2, ''Weekly Team Sync'', ''weekly'', 1, ''09:00'', ''09:30'', ''2025-01-01'', ''2025-12-31'')');
  WriteLn('   Created recurring pattern: Weekly Team Sync (Mondays)');

  WriteLn('');
end;

// =============================================================================
// Display Bookings
// =============================================================================
{ Prints a formatted table of all upcoming pending and confirmed bookings, showing status icons, resource, title, time, user, and cost. }
procedure DemoBookingList;
var
  DS: TDataSet;
begin
  WriteLn('3. Booking List Display');
  WriteLn('   ======================');
  WriteLn('');

  // All upcoming bookings
  WriteLn('   Upcoming bookings:');
  WriteLn('   ' + StringOfChar('-', 85));
  WriteLn(Format('   %-5s | %-15s | %-20s | %-16s | %-8s | %s',
    ['Stat', 'Resource', 'Title', 'Time', 'User', 'Cost']));
  WriteLn('   ' + StringOfChar('-', 85));

  DS := Conn.ExecuteQuery(
    'SELECT b.*, r.name AS resource_name, u.display_name AS user_name ' +
    'FROM bookings b ' +
    'JOIN resources r ON b.resource_id = r.id ' +
    'JOIN users u ON b.user_id = u.id ' +
    'WHERE b.status IN (''pending'', ''confirmed'') ' +
    'ORDER BY b.start_time');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-5s | %-15s | %-20s | %-16s | %-8s | $%.2f',
        [StatusToIcon(DS.FieldByName('status').AsString),
         Copy(DS.FieldByName('resource_name').AsString, 1, 15),
         Copy(DS.FieldByName('title').AsString, 1, 20),
         Copy(DS.FieldByName('start_time').AsString, 6, 11),
         Copy(DS.FieldByName('user_name').AsString, 1, 8),
         DS.FieldByName('total_cost').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('   ' + StringOfChar('-', 85));
  WriteLn('   Legend: [OK]=confirmed, [?]=pending, [X]=cancelled, [v]=completed');
  WriteLn('');
end;

// =============================================================================
// Resource Availability
// =============================================================================
{ Demonstrates checking resource availability for booking slots. }
procedure DemoAvailability;
var
  DS: TDataSet;
begin
  WriteLn('4. Resource Availability');
  WriteLn('   =======================');
  WriteLn('');

  // Meeting rooms availability for Jan 20
  WriteLn('   Meeting Rooms availability on 2025-01-20:');
  WriteLn('   ' + StringOfChar('-', 70));

  DS := Conn.ExecuteQuery(
    'SELECT r.name, r.capacity, ' +
    '  COALESCE(booked.count, 0) AS bookings_today, ' +
    '  GROUP_CONCAT(booked.times, ''; '') AS booked_times ' +
    'FROM resources r ' +
    'JOIN resource_types rt ON r.type_id = rt.id ' +
    'LEFT JOIN (' +
    '  SELECT resource_id, COUNT(*) AS count, ' +
    '    GROUP_CONCAT(substr(start_time, 12, 5) || ''-'' || substr(end_time, 12, 5)) AS times ' +
    '  FROM bookings ' +
    '  WHERE date(start_time) = ''2025-01-20'' AND status IN (''pending'', ''confirmed'') ' +
    '  GROUP BY resource_id' +
    ') booked ON r.id = booked.resource_id ' +
    'WHERE rt.name = ''Meeting Room'' AND r.is_active = 1 ' +
    'ORDER BY r.name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %-12s (cap: %d) - %d bookings',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('capacity').AsInteger,
         DS.FieldByName('bookings_today').AsInteger]));
      if not VarIsNull(DS.FieldByName('booked_times').Value) then
        WriteLn(Format('     Booked: %s', [DS.FieldByName('booked_times').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Find available slots
  WriteLn('');
  WriteLn('   Free slots on Room A101 (2025-01-20):');
  WriteLn('     Before 09:00 - Available');
  WriteLn('     09:00-11:00 - BOOKED (Sprint Planning)');
  WriteLn('     11:00-14:00 - Available (3 hours)');
  WriteLn('     14:00-15:30 - BOOKED (Client Meeting)');
  WriteLn('     After 15:30 - Available');

  WriteLn('');
end;

// =============================================================================
// User Bookings
// =============================================================================
{ Displays a per-user summary table showing department, booking limit, counts of confirmed and pending bookings, and total cost. }
procedure DemoUserBookings;
var
  DS: TDataSet;
begin
  WriteLn('5. User Booking Summary');
  WriteLn('   ======================');
  WriteLn('');

  DS := Conn.ExecuteQuery(
    'SELECT u.display_name, u.department, u.booking_limit, ' +
    '  COUNT(b.id) AS total_bookings, ' +
    '  SUM(CASE WHEN b.status = ''confirmed'' THEN 1 ELSE 0 END) AS confirmed, ' +
    '  SUM(CASE WHEN b.status = ''pending'' THEN 1 ELSE 0 END) AS pending, ' +
    '  COALESCE(SUM(b.total_cost), 0) AS total_cost ' +
    'FROM users u ' +
    'LEFT JOIN bookings b ON u.id = b.user_id AND b.status IN (''pending'', ''confirmed'') ' +
    'GROUP BY u.id ' +
    'ORDER BY total_bookings DESC');
  try
    WriteLn(Format('   %-15s | %-12s | Limit | Conf | Pend | Total Cost',
      ['User', 'Department']));
    WriteLn('   ' + StringOfChar('-', 70));
    while not DS.EOF do
    begin
      WriteLn(Format('   %-15s | %-12s | %5d | %4d | %4d | $%8.2f',
        [DS.FieldByName('display_name').AsString,
         DS.FieldByName('department').AsString,
         DS.FieldByName('booking_limit').AsInteger,
         DS.FieldByName('confirmed').AsInteger,
         DS.FieldByName('pending').AsInteger,
         DS.FieldByName('total_cost').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Waitlist Management
// =============================================================================
{ Demonstrates waitlist management for fully-booked resources. }
procedure DemoWaitlist;
var
  DS: TDataSet;
begin
  WriteLn('6. Waitlist Management');
  WriteLn('   =====================');
  WriteLn('');

  DS := Conn.ExecuteQuery(
    'SELECT w.*, r.name AS resource_name, u.display_name AS user_name ' +
    'FROM waitlist w ' +
    'JOIN resources r ON w.resource_id = r.id ' +
    'JOIN users u ON w.user_id = u.id ' +
    'WHERE w.status = ''waiting'' ' +
    'ORDER BY w.priority DESC, w.created_at');
  try
    WriteLn('   Current waitlist:');
    if DS.EOF then
    begin
      WriteLn('     (No entries in waitlist)');
    end
    else
    begin
      while not DS.EOF do
      begin
        WriteLn(Format('     #%d - %s wants %s',
          [DS.FieldByName('id').AsInteger,
           DS.FieldByName('user_name').AsString,
           DS.FieldByName('resource_name').AsString]));
        WriteLn(Format('         Desired: %s to %s (Priority: %d)',
          [Copy(DS.FieldByName('desired_start').AsString, 1, 16),
           Copy(DS.FieldByName('desired_end').AsString, 12, 5),
           DS.FieldByName('priority').AsInteger]));
        DS.Next;
      end;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Booking Cancellation
// =============================================================================
{ Demonstrates the booking cancellation workflow with status updates. }
procedure DemoCancellation;
var
  DS: TDataSet;
begin
  WriteLn('7. Booking Cancellation');
  WriteLn('   ======================');
  WriteLn('');

  // Cancel Bob's client meeting
  WriteLn('   Cancelling booking #3 (Client Meeting)...');
  CancelBooking(3, 3, 'Client rescheduled');
  WriteLn('   Booking cancelled with reason: Client rescheduled');

  // Show cancelled bookings
  WriteLn('');
  WriteLn('   Cancelled bookings:');
  DS := Conn.ExecuteQuery(
    'SELECT b.title, r.name AS resource_name, b.cancelled_at, b.cancellation_reason ' +
    'FROM bookings b ' +
    'JOIN resources r ON b.resource_id = r.id ' +
    'WHERE b.status = ''cancelled'' ' +
    'ORDER BY b.cancelled_at DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     [X] %s (%s)',
        [DS.FieldByName('title').AsString,
         DS.FieldByName('resource_name').AsString]));
      WriteLn(Format('         Reason: %s',
        [DS.FieldByName('cancellation_reason').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Recurring Bookings
// =============================================================================
{ Lists all active recurring booking patterns showing title, user, resource, recurrence type, day of week, time range, and validity period. }
procedure DemoRecurringBookings;
var
  DS: TDataSet;
begin
  WriteLn('8. Recurring Bookings');
  WriteLn('   ====================');
  WriteLn('');

  DS := Conn.ExecuteQuery(
    'SELECT rp.*, r.name AS resource_name, u.display_name AS user_name ' +
    'FROM recurring_patterns rp ' +
    'JOIN resources r ON rp.resource_id = r.id ' +
    'JOIN users u ON rp.user_id = u.id ' +
    'WHERE rp.is_active = 1');
  try
    WriteLn('   Active recurring patterns:');
    while not DS.EOF do
    begin
      WriteLn(Format('     "%s" by %s',
        [DS.FieldByName('title').AsString,
         DS.FieldByName('user_name').AsString]));
      WriteLn(Format('       Resource: %s', [DS.FieldByName('resource_name').AsString]));
      WriteLn(Format('       Pattern: %s (day %d), %s-%s',
        [DS.FieldByName('recurrence_type').AsString,
         DS.FieldByName('day_of_week').AsInteger,
         DS.FieldByName('start_time').AsString,
         DS.FieldByName('end_time').AsString]));
      WriteLn(Format('       Valid: %s to %s',
        [DS.FieldByName('valid_from').AsString,
         DS.FieldByName('valid_until').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Booking History
// =============================================================================
{ Demonstrates viewing the booking history and audit log. }
procedure DemoHistory;
var
  DS: TDataSet;
begin
  WriteLn('9. Booking History/Audit Log');
  WriteLn('   ===========================');
  WriteLn('');

  DS := Conn.ExecuteQuery(
    'SELECT bh.*, b.title, COALESCE(u.display_name, ''System'') AS changed_by_name ' +
    'FROM booking_history bh ' +
    'JOIN bookings b ON bh.booking_id = b.id ' +
    'LEFT JOIN users u ON bh.changed_by = u.id ' +
    'ORDER BY bh.created_at DESC ' +
    'LIMIT 10');
  try
    WriteLn('   Recent booking history:');
    while not DS.EOF do
    begin
      Write(Format('     [%s] %s - %s',
        [Copy(DS.FieldByName('created_at').AsString, 1, 16),
         DS.FieldByName('title').AsString,
         DS.FieldByName('action').AsString]));
      if not VarIsNull(DS.FieldByName('old_status').Value) then
        Write(Format(' (%s -> %s)',
          [DS.FieldByName('old_status').AsString,
           DS.FieldByName('new_status').AsString]));
      WriteLn(Format(' by %s', [DS.FieldByName('changed_by_name').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Statistics
// =============================================================================
{ Demonstrates booking statistics and usage reporting. }
procedure DemoStatistics;
var
  DS: TDataSet;
begin
  WriteLn('10. Booking Statistics');
  WriteLn('    ====================');
  WriteLn('');

  WriteLn('    Overall statistics:');
  WriteLn(Format('      Total bookings: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM bookings'))]));
  WriteLn(Format('      Confirmed: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM bookings WHERE status = ''confirmed'''))]));
  WriteLn(Format('      Pending approval: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM bookings WHERE status = ''pending'''))]));
  WriteLn(Format('      Cancelled: %d',
    [Integer(Conn.ExecuteScalar('SELECT COUNT(*) FROM bookings WHERE status = ''cancelled'''))]));
  WriteLn(Format('      Total revenue: $%.2f',
    [Double(Conn.ExecuteScalar('SELECT COALESCE(SUM(total_cost), 0) FROM bookings WHERE status IN (''confirmed'', ''completed'')'))]));

  // By resource type
  WriteLn('');
  WriteLn('    Bookings by resource type:');
  DS := Conn.ExecuteQuery(
    'SELECT rt.name, COUNT(b.id) AS bookings, COALESCE(SUM(b.total_cost), 0) AS revenue ' +
    'FROM resource_types rt ' +
    'LEFT JOIN resources r ON rt.id = r.type_id ' +
    'LEFT JOIN bookings b ON r.id = b.resource_id AND b.status IN (''confirmed'', ''pending'') ' +
    'GROUP BY rt.id ' +
    'ORDER BY bookings DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('      %-20s: %d bookings ($%.2f)',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('bookings').AsInteger,
         DS.FieldByName('revenue').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Most booked resources
  WriteLn('');
  WriteLn('    Most booked resources:');
  DS := Conn.ExecuteQuery(
    'SELECT r.name, COUNT(b.id) AS bookings ' +
    'FROM resources r ' +
    'LEFT JOIN bookings b ON r.id = b.resource_id AND b.status IN (''confirmed'', ''pending'') ' +
    'GROUP BY r.id ' +
    'ORDER BY bookings DESC ' +
    'LIMIT 3');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('      %s: %d bookings',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('bookings').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Main
// =============================================================================
begin
  WriteLn('');
  WriteLn('=== NDXSQLite Example 79: Booking and Reservation System ===');
  WriteLn('');

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;

    CreateSchema;
    InsertSampleData;
    DemoBookingList;
    DemoAvailability;
    DemoUserBookings;
    DemoWaitlist;
    DemoCancellation;
    DemoRecurringBookings;
    DemoHistory;
    DemoStatistics;

    Conn.Close;
  finally
    Conn.Free;
  end;

  WriteLn('=== Example completed successfully! ===');
  WriteLn('');
end.
