{===============================================================================
  NDXSQLite Example 101 - Workflow Engine
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  This example demonstrates:
  - Multi-step workflow definitions
  - Step approval, rejection, and escalation
  - Workflow instance lifecycle management
  - Purchase order processing workflow
  - Workflow status tracking and reporting

  Cross-platform: Windows, Linux, macOS
===============================================================================}
program WorkflowEngine;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DB,
  NDXSQLiteConnection;

var
  Conn: TNDXSQLiteConnection;
  DS: TDataSet;

{ Initializes the database with tables and sample data. }
procedure SetupDatabase;
begin
  // Workflow definitions (templates)
  Conn.ExecuteNonQuery(
    'CREATE TABLE workflow_definitions (' +
    '  id TEXT PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  description TEXT' +
    ')');

  // Steps within a workflow definition
  Conn.ExecuteNonQuery(
    'CREATE TABLE workflow_steps (' +
    '  id TEXT PRIMARY KEY,' +
    '  workflow_id TEXT NOT NULL,' +
    '  step_name TEXT NOT NULL,' +
    '  step_type TEXT NOT NULL,' +  // task, approval, parallel_fork, parallel_join
    '  step_order INTEGER NOT NULL,' +
    '  deadline_hours INTEGER DEFAULT 0,' +
    '  FOREIGN KEY (workflow_id) REFERENCES workflow_definitions(id)' +
    ')');

  // Running workflow instances
  Conn.ExecuteNonQuery(
    'CREATE TABLE workflow_instances (' +
    '  id TEXT PRIMARY KEY,' +
    '  workflow_id TEXT NOT NULL,' +
    '  title TEXT,' +
    '  status TEXT DEFAULT ''active'',' +  // active, completed, cancelled
    '  current_step_id TEXT,' +
    '  started_at TEXT,' +
    '  completed_at TEXT,' +
    '  started_by TEXT,' +
    '  FOREIGN KEY (workflow_id) REFERENCES workflow_definitions(id)' +
    ')');

  // Step execution records
  Conn.ExecuteNonQuery(
    'CREATE TABLE step_executions (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  instance_id TEXT NOT NULL,' +
    '  step_id TEXT NOT NULL,' +
    '  status TEXT DEFAULT ''pending'',' +  // pending, in_progress, completed, approved, rejected, escalated
    '  assigned_to TEXT,' +
    '  started_at TEXT,' +
    '  completed_at TEXT,' +
    '  deadline_at TEXT,' +
    '  notes TEXT,' +
    '  branch_name TEXT,' +
    '  FOREIGN KEY (instance_id) REFERENCES workflow_instances(id),' +
    '  FOREIGN KEY (step_id) REFERENCES workflow_steps(id)' +
    ')');

  // Parallel branch definitions
  Conn.ExecuteNonQuery(
    'CREATE TABLE parallel_branches (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  fork_step_id TEXT NOT NULL,' +
    '  branch_name TEXT NOT NULL,' +
    '  branch_step_id TEXT NOT NULL,' +
    '  FOREIGN KEY (fork_step_id) REFERENCES workflow_steps(id),' +
    '  FOREIGN KEY (branch_step_id) REFERENCES workflow_steps(id)' +
    ')');
end;

{ Creates a purchase order workflow. }
procedure CreatePurchaseOrderWorkflow;
begin
  // Define the Purchase Order approval workflow
  Conn.ExecuteNonQuery(
    'INSERT INTO workflow_definitions (id, name, description) VALUES (' +
    '''wf-po'', ''Purchase Order Approval'', ' +
    '''Multi-step approval for purchase orders with parallel review'')');

  // Steps
  Conn.ExecuteNonQuery('INSERT INTO workflow_steps (id, workflow_id, step_name, step_type, step_order, deadline_hours) VALUES ' +
    '(''step-submit'', ''wf-po'', ''Submit Request'', ''task'', 1, 0)');
  Conn.ExecuteNonQuery('INSERT INTO workflow_steps (id, workflow_id, step_name, step_type, step_order, deadline_hours) VALUES ' +
    '(''step-mgr-review'', ''wf-po'', ''Manager Review'', ''approval'', 2, 24)');
  Conn.ExecuteNonQuery('INSERT INTO workflow_steps (id, workflow_id, step_name, step_type, step_order, deadline_hours) VALUES ' +
    '(''step-fork'', ''wf-po'', ''Parallel Review'', ''parallel_fork'', 3, 0)');
  Conn.ExecuteNonQuery('INSERT INTO workflow_steps (id, workflow_id, step_name, step_type, step_order, deadline_hours) VALUES ' +
    '(''step-finance'', ''wf-po'', ''Finance Check'', ''approval'', 4, 48)');
  Conn.ExecuteNonQuery('INSERT INTO workflow_steps (id, workflow_id, step_name, step_type, step_order, deadline_hours) VALUES ' +
    '(''step-legal'', ''wf-po'', ''Legal Review'', ''approval'', 4, 72)');
  Conn.ExecuteNonQuery('INSERT INTO workflow_steps (id, workflow_id, step_name, step_type, step_order, deadline_hours) VALUES ' +
    '(''step-join'', ''wf-po'', ''Reviews Complete'', ''parallel_join'', 5, 0)');
  Conn.ExecuteNonQuery('INSERT INTO workflow_steps (id, workflow_id, step_name, step_type, step_order, deadline_hours) VALUES ' +
    '(''step-director'', ''wf-po'', ''Director Approval'', ''approval'', 6, 24)');
  Conn.ExecuteNonQuery('INSERT INTO workflow_steps (id, workflow_id, step_name, step_type, step_order, deadline_hours) VALUES ' +
    '(''step-complete'', ''wf-po'', ''Order Placed'', ''task'', 7, 0)');

  // Parallel branches
  Conn.ExecuteNonQuery('INSERT INTO parallel_branches (fork_step_id, branch_name, branch_step_id) VALUES ' +
    '(''step-fork'', ''finance'', ''step-finance'')');
  Conn.ExecuteNonQuery('INSERT INTO parallel_branches (fork_step_id, branch_name, branch_step_id) VALUES ' +
    '(''step-fork'', ''legal'', ''step-legal'')');
end;

{ Starts a new workflow instance and creates the initial step execution. }
function StartWorkflow(const WorkflowId, InstanceId, Title, StartedBy, Timestamp: string): string;
begin
  Conn.ExecuteNonQuery(
    'INSERT INTO workflow_instances (id, workflow_id, title, status, current_step_id, started_at, started_by) VALUES (' +
    '''' + InstanceId + ''', ' +
    '''' + WorkflowId + ''', ' +
    '''' + Title + ''', ' +
    '''active'', ''step-submit'', ' +
    '''' + Timestamp + ''', ' +
    '''' + StartedBy + ''')');

  // Create the first step execution
  Conn.ExecuteNonQuery(
    'INSERT INTO step_executions (instance_id, step_id, status, assigned_to, started_at) VALUES (' +
    '''' + InstanceId + ''', ''step-submit'', ''in_progress'', ' +
    '''' + StartedBy + ''', ''' + Timestamp + ''')');

  Result := InstanceId;
end;

{ Marks a step execution as completed with a timestamp and notes for the given instance and step ID. }
procedure CompleteStep(const InstanceId, StepId, CompletedBy, Timestamp, Notes: string);
begin
  Conn.ExecuteNonQuery(
    'UPDATE step_executions SET status = ''completed'', completed_at = ''' + Timestamp + ''', ' +
    'notes = ''' + Notes + ''' ' +
    'WHERE instance_id = ''' + InstanceId + ''' AND step_id = ''' + StepId + ''' AND status IN (''pending'', ''in_progress'')');
end;

{ Marks a pending or in-progress step execution as approved with a timestamp and approver notes. }
procedure ApproveStep(const InstanceId, StepId, ApprovedBy, Timestamp, Notes: string);
begin
  Conn.ExecuteNonQuery(
    'UPDATE step_executions SET status = ''approved'', completed_at = ''' + Timestamp + ''', ' +
    'notes = ''' + Notes + ''' ' +
    'WHERE instance_id = ''' + InstanceId + ''' AND step_id = ''' + StepId + ''' AND status IN (''pending'', ''in_progress'')');
end;

{ Marks a pending or in-progress step execution as rejected with a timestamp and rejection reason. }
procedure RejectStep(const InstanceId, StepId, RejectedBy, Timestamp, Notes: string);
begin
  Conn.ExecuteNonQuery(
    'UPDATE step_executions SET status = ''rejected'', completed_at = ''' + Timestamp + ''', ' +
    'notes = ''' + Notes + ''' ' +
    'WHERE instance_id = ''' + InstanceId + ''' AND step_id = ''' + StepId + ''' AND status IN (''pending'', ''in_progress'')');
end;

{ Creates a new step execution record assigned to a user with optional deadline and branch name, and updates the workflow instance's current step. }
procedure AdvanceToStep(const InstanceId, NextStepId, AssignedTo, Timestamp: string;
  DeadlineHours: Integer; const BranchName: string);
var
  DeadlineAt: string;
begin
  if DeadlineHours > 0 then
  begin
    DS := Conn.ExecuteQuery('SELECT datetime(''' + Timestamp + ''', ''+' + IntToStr(DeadlineHours) + ' hours'') as dl');
    try
      DeadlineAt := DS.FieldByName('dl').AsString;
    finally
      DS.Free;
    end;
  end
  else
    DeadlineAt := '';

  Conn.ExecuteNonQuery(
    'INSERT INTO step_executions (instance_id, step_id, status, assigned_to, started_at, deadline_at, branch_name) VALUES (' +
    '''' + InstanceId + ''', ''' + NextStepId + ''', ''in_progress'', ' +
    '''' + AssignedTo + ''', ''' + Timestamp + ''', ' +
    '''' + DeadlineAt + ''', ' +
    '''' + BranchName + ''')');

  Conn.ExecuteNonQuery(
    'UPDATE workflow_instances SET current_step_id = ''' + NextStepId + ''' WHERE id = ''' + InstanceId + '''');
end;

{ Sets the workflow instance status to completed and records the completion timestamp. }
procedure CompleteWorkflow(const InstanceId, Timestamp: string);
begin
  Conn.ExecuteNonQuery(
    'UPDATE workflow_instances SET status = ''completed'', completed_at = ''' + Timestamp + ''' ' +
    'WHERE id = ''' + InstanceId + '''');
end;

{ Marks an in-progress step execution as escalated and records the escalation reason in the notes field. }
procedure EscalateStep(const InstanceId, StepId, Timestamp, Reason: string);
begin
  Conn.ExecuteNonQuery(
    'UPDATE step_executions SET status = ''escalated'', notes = ''' + Reason + ''' ' +
    'WHERE instance_id = ''' + InstanceId + ''' AND step_id = ''' + StepId + ''' AND status = ''in_progress''');
end;

{ Demo procedures }

procedure DemoWorkflowDefinition;
begin
  WriteLn('=== 1. Workflow Definition ===');
  WriteLn;

  WriteLn('   Purchase Order Approval Workflow:');
  DS := Conn.ExecuteQuery(
    'SELECT s.step_order, s.step_name, s.step_type, s.deadline_hours ' +
    'FROM workflow_steps s WHERE s.workflow_id = ''wf-po'' ORDER BY s.step_order, s.id');
  try
    while not DS.EOF do
    begin
      Write(Format('    Step %d: %-20s [%s]',
        [DS.FieldByName('step_order').AsInteger,
         DS.FieldByName('step_name').AsString,
         DS.FieldByName('step_type').AsString]));
      if DS.FieldByName('deadline_hours').AsInteger > 0 then
        Write(Format('  deadline=%dh', [DS.FieldByName('deadline_hours').AsInteger]));
      WriteLn;
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn;
  WriteLn('   Parallel branches at fork:');
  DS := Conn.ExecuteQuery(
    'SELECT branch_name, branch_step_id FROM parallel_branches WHERE fork_step_id = ''step-fork''');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('    Branch "%s" -> %s',
        [DS.FieldByName('branch_name').AsString,
         DS.FieldByName('branch_step_id').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Starts two workflow instances (a server hardware purchase and an office supplies order), then lists all active instances with their current step. }
procedure DemoStartWorkflow;
begin
  WriteLn('=== 2. Start Workflow Instance ===');
  WriteLn;

  StartWorkflow('wf-po', 'inst-001', 'Server Hardware Purchase $15000', 'john.doe', '2024-03-01T09:00:00');
  WriteLn('   Started: "Server Hardware Purchase $15000" by john.doe');

  StartWorkflow('wf-po', 'inst-002', 'Office Supplies $500', 'jane.smith', '2024-03-01T10:00:00');
  WriteLn('   Started: "Office Supplies $500" by jane.smith');

  WriteLn;
  WriteLn('   Active instances:');
  DS := Conn.ExecuteQuery(
    'SELECT i.id, i.title, i.started_by, s.step_name ' +
    'FROM workflow_instances i JOIN workflow_steps s ON i.current_step_id = s.id ' +
    'WHERE i.status = ''active''');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('    %s: "%s" by %s (at: %s)',
        [DS.FieldByName('id').AsString,
         DS.FieldByName('title').AsString,
         DS.FieldByName('started_by').AsString,
         DS.FieldByName('step_name').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Moves both instances through submit to manager review, approves inst-001 and rejects inst-002 (cancelling it), then shows inst-001's execution history. }
procedure DemoStepExecution;
begin
  WriteLn('=== 3. Step Execution & Approvals ===');
  WriteLn;

  // Instance 1: Submit -> Manager Review
  CompleteStep('inst-001', 'step-submit', 'john.doe', '2024-03-01T09:15:00', 'PO form submitted');
  AdvanceToStep('inst-001', 'step-mgr-review', 'mgr.wilson', '2024-03-01T09:15:00', 24, '');
  WriteLn('   inst-001: Submit completed, moved to Manager Review');
  WriteLn('             Assigned to mgr.wilson, deadline: 24h');

  // Instance 2: Submit -> Manager Review
  CompleteStep('inst-002', 'step-submit', 'jane.smith', '2024-03-01T10:30:00', 'Supply order');
  AdvanceToStep('inst-002', 'step-mgr-review', 'mgr.wilson', '2024-03-01T10:30:00', 24, '');
  WriteLn('   inst-002: Submit completed, moved to Manager Review');

  // Manager approves instance 1
  ApproveStep('inst-001', 'step-mgr-review', 'mgr.wilson', '2024-03-01T14:00:00', 'Approved - within budget');
  WriteLn('   inst-001: Manager approved (within budget)');

  // Manager rejects instance 2
  RejectStep('inst-002', 'step-mgr-review', 'mgr.wilson', '2024-03-01T14:30:00', 'Use existing supplies first');
  Conn.ExecuteNonQuery('UPDATE workflow_instances SET status = ''cancelled'' WHERE id = ''inst-002''');
  WriteLn('   inst-002: Manager rejected -> workflow cancelled');

  WriteLn;
  WriteLn('   Step execution history (inst-001):');
  DS := Conn.ExecuteQuery(
    'SELECT se.step_id, s.step_name, se.status, se.assigned_to, se.notes ' +
    'FROM step_executions se JOIN workflow_steps s ON se.step_id = s.id ' +
    'WHERE se.instance_id = ''inst-001'' ORDER BY se.id');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('    %-20s %-10s by %-12s %s',
        [DS.FieldByName('step_name').AsString,
         DS.FieldByName('status').AsString,
         DS.FieldByName('assigned_to').AsString,
         DS.FieldByName('notes').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Forks inst-001 into parallel finance and legal review branches, approves both, verifies all branches completed, then performs the join step. }
procedure DemoParallelBranches;
begin
  WriteLn('=== 4. Parallel Branches (Fork/Join) ===');
  WriteLn;

  // Move inst-001 to parallel fork
  AdvanceToStep('inst-001', 'step-fork', 'system', '2024-03-01T14:00:00', 0, '');
  CompleteStep('inst-001', 'step-fork', 'system', '2024-03-01T14:00:00', 'Forking to parallel branches');

  // Start both branches
  AdvanceToStep('inst-001', 'step-finance', 'finance.team', '2024-03-01T14:00:00', 48, 'finance');
  AdvanceToStep('inst-001', 'step-legal', 'legal.team', '2024-03-01T14:00:00', 72, 'legal');
  WriteLn('   Forked into parallel branches:');
  WriteLn('    Branch "finance": Finance Check (assigned: finance.team, deadline: 48h)');
  WriteLn('    Branch "legal":   Legal Review  (assigned: legal.team, deadline: 72h)');

  // Finance approves first
  ApproveStep('inst-001', 'step-finance', 'finance.team', '2024-03-02T10:00:00', 'Budget verified');
  WriteLn;
  WriteLn('   Finance branch: APPROVED (Budget verified)');

  // Legal approves later
  ApproveStep('inst-001', 'step-legal', 'legal.team', '2024-03-03T11:00:00', 'No compliance issues');
  WriteLn('   Legal branch:   APPROVED (No compliance issues)');

  // Check if all branches complete
  WriteLn;
  WriteLn('   Checking parallel branch completion:');
  DS := Conn.ExecuteQuery(
    'SELECT se.step_id, s.step_name, se.status, se.branch_name, se.completed_at ' +
    'FROM step_executions se JOIN workflow_steps s ON se.step_id = s.id ' +
    'WHERE se.instance_id = ''inst-001'' AND se.branch_name <> '''' ORDER BY se.branch_name');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('    [%s] %-15s %s at %s',
        [DS.FieldByName('branch_name').AsString,
         DS.FieldByName('step_name').AsString,
         DS.FieldByName('status').AsString,
         DS.FieldByName('completed_at').AsString]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // All branches done -> join
  AdvanceToStep('inst-001', 'step-join', 'system', '2024-03-03T11:00:00', 0, '');
  CompleteStep('inst-001', 'step-join', 'system', '2024-03-03T11:00:00', 'All branches completed');
  WriteLn('   -> All branches complete, join step done');
  WriteLn;
end;

{ Assigns director approval with a 24h deadline, detects the overdue step, escalates to VP with a shorter deadline, and completes the approval. }
procedure DemoDeadlinesAndEscalation;
var
  DeadlineAt: string;
begin
  WriteLn('=== 5. Deadlines & Escalation ===');
  WriteLn;

  // Move to Director approval with 24h deadline
  AdvanceToStep('inst-001', 'step-director', 'dir.johnson', '2024-03-03T11:00:00', 24, '');
  WriteLn('   inst-001: Moved to Director Approval');
  WriteLn('             Assigned to dir.johnson, deadline: 24h');

  // Check deadline
  DS := Conn.ExecuteQuery(
    'SELECT step_id, assigned_to, started_at, deadline_at FROM step_executions ' +
    'WHERE instance_id = ''inst-001'' AND step_id = ''step-director''');
  try
    DeadlineAt := DS.FieldByName('deadline_at').AsString;
    WriteLn(Format('             Started: %s', [DS.FieldByName('started_at').AsString]));
    WriteLn(Format('             Deadline: %s', [DeadlineAt]));
  finally
    DS.Free;
  end;

  // Simulate deadline passed - escalate
  WriteLn;
  WriteLn('   Simulating deadline check at 2024-03-04T12:00:00...');
  DS := Conn.ExecuteQuery(
    'SELECT se.id, se.instance_id, se.step_id, s.step_name, se.assigned_to, se.deadline_at ' +
    'FROM step_executions se JOIN workflow_steps s ON se.step_id = s.id ' +
    'WHERE se.status = ''in_progress'' AND se.deadline_at <> '''' AND se.deadline_at < ''2024-03-04T12:00:00''');
  try
    if not DS.EOF then
    begin
      WriteLn(Format('   OVERDUE: %s (%s) assigned to %s, was due %s',
        [DS.FieldByName('step_name').AsString,
         DS.FieldByName('instance_id').AsString,
         DS.FieldByName('assigned_to').AsString,
         DS.FieldByName('deadline_at').AsString]));
    end;
  finally
    DS.Free;
  end;

  // Escalate
  EscalateStep('inst-001', 'step-director', '2024-03-04T12:00:00', 'Deadline exceeded - escalated to VP');
  AdvanceToStep('inst-001', 'step-director', 'vp.martinez', '2024-03-04T12:00:00', 8, '');
  WriteLn('   -> Escalated to vp.martinez with 8h deadline');

  // VP approves
  ApproveStep('inst-001', 'step-director', 'vp.martinez', '2024-03-04T14:00:00', 'Approved after escalation');
  WriteLn('   -> VP approved');
  WriteLn;
end;

{ Executes the final order-placement step for inst-001, marks the workflow as completed, and prints the instance's lifecycle details. }
procedure DemoCompleteWorkflow;
begin
  WriteLn('=== 6. Complete Workflow ===');
  WriteLn;

  // Final step
  AdvanceToStep('inst-001', 'step-complete', 'procurement', '2024-03-04T14:00:00', 0, '');
  CompleteStep('inst-001', 'step-complete', 'procurement', '2024-03-04T15:00:00', 'PO-2024-001 placed with vendor');
  CompleteWorkflow('inst-001', '2024-03-04T15:00:00');

  WriteLn('   inst-001: Order placed, workflow completed');
  WriteLn;

  DS := Conn.ExecuteQuery(
    'SELECT id, title, status, started_at, completed_at FROM workflow_instances WHERE id = ''inst-001''');
  try
    WriteLn(Format('   Instance: %s', [DS.FieldByName('id').AsString]));
    WriteLn(Format('   Title:    %s', [DS.FieldByName('title').AsString]));
    WriteLn(Format('   Status:   %s', [DS.FieldByName('status').AsString]));
    WriteLn(Format('   Started:  %s', [DS.FieldByName('started_at').AsString]));
    WriteLn(Format('   Completed: %s', [DS.FieldByName('completed_at').AsString]));
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Prints the complete step execution history for inst-001 including step name, status, assignee, branch, and notes for each execution record. }
procedure DemoAuditTrail;
begin
  WriteLn('=== 7. Full Audit Trail ===');
  WriteLn;

  WriteLn('   Complete execution history for inst-001:');
  DS := Conn.ExecuteQuery(
    'SELECT se.id, s.step_name, se.status, se.assigned_to, ' +
    'se.started_at, se.completed_at, se.branch_name, se.notes ' +
    'FROM step_executions se JOIN workflow_steps s ON se.step_id = s.id ' +
    'WHERE se.instance_id = ''inst-001'' ORDER BY se.id');
  try
    while not DS.EOF do
    begin
      Write(Format('    #%2d %-20s %-10s by %-14s',
        [DS.FieldByName('id').AsInteger,
         DS.FieldByName('step_name').AsString,
         DS.FieldByName('status').AsString,
         DS.FieldByName('assigned_to').AsString]));
      if DS.FieldByName('branch_name').AsString <> '' then
        Write(Format(' [%s]', [DS.FieldByName('branch_name').AsString]));
      if DS.FieldByName('notes').AsString <> '' then
        Write(Format(' "%s"', [DS.FieldByName('notes').AsString]));
      WriteLn;
      DS.Next;
    end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Prints workflow instance counts by status, step execution counts by status, total approvals and escalations, and any remaining pending work. }
procedure DemoWorkflowStats;
var
  TotalSteps, ApprovedCount, EscalatedCount: Integer;
begin
  WriteLn('=== 8. Workflow Statistics ===');
  WriteLn;

  // Instance stats
  WriteLn('   Instance summary:');
  DS := Conn.ExecuteQuery(
    'SELECT status, COUNT(*) as cnt FROM workflow_instances GROUP BY status');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('    %s: %d', [DS.FieldByName('status').AsString, DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Step execution stats
  WriteLn;
  WriteLn('   Step execution stats (inst-001):');
  DS := Conn.ExecuteQuery(
    'SELECT status, COUNT(*) as cnt FROM step_executions WHERE instance_id = ''inst-001'' GROUP BY status');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('    %s: %d', [DS.FieldByName('status').AsString, DS.FieldByName('cnt').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Avg time per step
  WriteLn;
  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) as total FROM step_executions WHERE instance_id = ''inst-001''');
  try
    TotalSteps := DS.FieldByName('total').AsInteger;
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) as cnt FROM step_executions WHERE instance_id = ''inst-001'' AND status = ''approved''');
  try
    ApprovedCount := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  DS := Conn.ExecuteQuery(
    'SELECT COUNT(*) as cnt FROM step_executions WHERE instance_id = ''inst-001'' AND status = ''escalated''');
  try
    EscalatedCount := DS.FieldByName('cnt').AsInteger;
  finally
    DS.Free;
  end;

  WriteLn(Format('   Total step executions: %d', [TotalSteps]));
  WriteLn(Format('   Approvals: %d, Escalations: %d', [ApprovedCount, EscalatedCount]));

  // Pending work
  WriteLn;
  WriteLn('   Pending/In-progress work:');
  DS := Conn.ExecuteQuery(
    'SELECT se.assigned_to, COUNT(*) as cnt ' +
    'FROM step_executions se ' +
    'WHERE se.status IN (''pending'', ''in_progress'') ' +
    'GROUP BY se.assigned_to');
  try
    if DS.EOF then
      WriteLn('    (none - all work completed)')
    else
      while not DS.EOF do
      begin
        WriteLn(Format('    %s: %d pending', [DS.FieldByName('assigned_to').AsString, DS.FieldByName('cnt').AsInteger]));
        DS.Next;
      end;
  finally
    DS.Free;
  end;
  WriteLn;
end;

{ Displays a summary of workflow engine patterns demonstrated. }
procedure DemoSummary;
begin
  WriteLn('=== Workflow Engine Summary ===');
  WriteLn;
  WriteLn('   Patterns demonstrated:');
  WriteLn('   - Workflow definition with typed steps');
  WriteLn('   - Workflow instance lifecycle (active -> completed/cancelled)');
  WriteLn('   - Step execution with approvals and rejections');
  WriteLn('   - Parallel branches (fork/join pattern)');
  WriteLn('   - Deadline tracking and escalation');
  WriteLn('   - Full audit trail of all actions');
  WriteLn('   - Workflow statistics and reporting');
end;

begin
  WriteLn('Example 101: Workflow Engine - Steps, Approvals, Parallel Branches');
  WriteLn('===================================================================');
  WriteLn;

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;
    SetupDatabase;
    CreatePurchaseOrderWorkflow;

    WriteLn('Database setup complete.');
    WriteLn;

    DemoWorkflowDefinition;
    DemoStartWorkflow;
    DemoStepExecution;
    DemoParallelBranches;
    DemoDeadlinesAndEscalation;
    DemoCompleteWorkflow;
    DemoAuditTrail;
    DemoWorkflowStats;
    DemoSummary;

    WriteLn;
    WriteLn('Done.');
  finally
    Conn.Close;
    Conn.Free;
  end;
end.
