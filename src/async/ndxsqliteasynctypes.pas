{===============================================================================
  NDXSQLite - Asynchronous Types
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.
  Level: 1 (no internal dependencies)
===============================================================================}
unit ndxsqliteasynctypes;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils, DB;

type
  { Asynchronous operation status - inspired by .NET Task }
  TNDXAsyncStatus = (
    asPending,      // Pending execution
    asRunning,      // Running
    asCompleted,    // Completed successfully
    asFaulted,      // Completed with error
    asCanceled      // Canceled
  );

  { Generic asynchronous result - like Task<T> in .NET }
  generic TNDXAsyncResult<T> = record
    Success: Boolean;           // True if execution successful
    Data: T;                    // Result data
    Status: TNDXAsyncStatus;    // Operation status
    ErrorMessage: string;       // Error message if failed
    ErrorCode: Integer;         // SQLite error code
    ExecutionTimeMs: Int64;     // Execution time in ms

    { Creates a successful result containing the given data and execution time. }
    class function CreateSuccess(const AData: T; ATimeMs: Int64): specialize TNDXAsyncResult<T>; static;
    { Creates a faulted result with the specified error message and optional error code. }
    class function CreateError(const AMessage: string; ACode: Integer = 0): specialize TNDXAsyncResult<T>; static;
    { Creates a canceled result indicating the operation was aborted. }
    class function CreateCanceled: specialize TNDXAsyncResult<T>; static;
  end;

  { Specializations for common types }
  TNDXAsyncResultInt = specialize TNDXAsyncResult<Integer>;
  TNDXAsyncResultInt64 = specialize TNDXAsyncResult<Int64>;
  TNDXAsyncResultVariant = specialize TNDXAsyncResult<Variant>;
  TNDXAsyncResultString = specialize TNDXAsyncResult<string>;
  TNDXAsyncResultBoolean = specialize TNDXAsyncResult<Boolean>;
  TNDXAsyncResultDataSet = specialize TNDXAsyncResult<TDataSet>;

  { Typed callbacks - rich signature }
  TNDXAsyncCallbackInt = procedure(const AResult: TNDXAsyncResultInt) of object;
  TNDXAsyncCallbackInt64 = procedure(const AResult: TNDXAsyncResultInt64) of object;
  TNDXAsyncCallbackVariant = procedure(const AResult: TNDXAsyncResultVariant) of object;
  TNDXAsyncCallbackString = procedure(const AResult: TNDXAsyncResultString) of object;
  TNDXAsyncCallbackBoolean = procedure(const AResult: TNDXAsyncResultBoolean) of object;
  TNDXAsyncCallbackDataSet = procedure(const AResult: TNDXAsyncResultDataSet) of object;

  { Generic callback for operations without return value }
  TNDXAsyncCallback = procedure(const ASuccess: Boolean; const AErrorMessage: string) of object;

  { Progress callback }
  TNDXProgressCallback = procedure(ACurrent, ATotal: Integer; const AMessage: string) of object;

  { Helper for TNDXAsyncStatus }
  TNDXAsyncStatusHelper = type helper for TNDXAsyncStatus
    { Returns the status as a human-readable string (Pending, Running, Completed, Faulted, Canceled). }
    function ToString: string;
    { Returns True if the status represents a final state (Completed, Faulted, or Canceled). }
    function IsTerminal: Boolean;
  end;

implementation

{ TNDXAsyncResult<T> }

class function TNDXAsyncResult.CreateSuccess(const AData: T; ATimeMs: Int64): specialize TNDXAsyncResult<T>;
begin
  Result.Success := True;
  Result.Data := AData;
  Result.Status := asCompleted;
  Result.ErrorMessage := '';
  Result.ErrorCode := 0;
  Result.ExecutionTimeMs := ATimeMs;
end;

class function TNDXAsyncResult.CreateError(const AMessage: string; ACode: Integer): specialize TNDXAsyncResult<T>;
begin
  Result.Success := False;
  Result.Status := asFaulted;
  Result.ErrorMessage := AMessage;
  Result.ErrorCode := ACode;
  Result.ExecutionTimeMs := 0;
end;

class function TNDXAsyncResult.CreateCanceled: specialize TNDXAsyncResult<T>;
begin
  Result.Success := False;
  Result.Status := asCanceled;
  Result.ErrorMessage := 'Operation canceled';
  Result.ErrorCode := 0;
  Result.ExecutionTimeMs := 0;
end;

{ TNDXAsyncStatusHelper }

function TNDXAsyncStatusHelper.ToString: string;
begin
  case Self of
    asPending:   Result := 'Pending';
    asRunning:   Result := 'Running';
    asCompleted: Result := 'Completed';
    asFaulted:   Result := 'Faulted';
    asCanceled:  Result := 'Canceled';
  else
    Result := 'Unknown';
  end;
end;

function TNDXAsyncStatusHelper.IsTerminal: Boolean;
begin
  Result := Self in [asCompleted, asFaulted, asCanceled];
end;

end.
