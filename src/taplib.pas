unit TapLib;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, TestReporter;

type
  TTestProc = procedure;
  TReporterType = (rtTAP, rtJUnit, rtGitHubActions);

procedure set_reporter(AReporter: TTestReporter);
procedure set_reporter(AReporterType: TReporterType);
procedure add_reporter(AReporter: TTestReporter);

procedure start_tests;
procedure end_tests;

procedure start_unit(const AUnitName: string);
procedure end_unit;

procedure register_test(const ATestName: string; AProc: TTestProc = nil);
procedure register_subtest(const ATestName: string; AProc: TTestProc);
procedure run_tests;

procedure assert_equal(const ATestName: string; AExpected, AGot: longint);
procedure assert_equal(const ATestName: string; const AExpected, AGot: string);
procedure assert_true(const ATestName: string; ACondition: boolean);
procedure assert_false(const ATestName: string; ACondition: boolean);
procedure assert_equal_files(const ATestName, AExpectedFile, AGotFile: string);

implementation

uses
  Classes;

type
  pSubTestCase = ^TSubTestCase;
  TSubTestCase = record
    name: string;
    proc: TTestProc;
  end;

  pTestCase = ^TTestCase;
  TTestCase = record
    name: string;
    proc: TTestProc;
    idx: longint;
    subtests: array of TSubTestCase;
  end;

  pUnit = ^TUnit;
  TUnit = record
    name: string;
    idx: longint;
    tests: array of TTestCase;
  end;

var
  units: array of TUnit;
  unit_count: longint = 0;

  current_unit: pUnit;
  current_test: pTestCase;
  current_subtest: pSubTestCase;

  total_tests: longint = 0;
  main_tests: longint = 0;
  is_subtest: boolean = false;

  passed: longint = 0;
  failed: longint = 0;
  test_count: longint = 0;
  subtest_count: longint = 0;
  last_result: boolean;

  // Accumulator for multiple assertions per test
  test_all_passed: boolean = true;
  failures: TFailureArray;
  failure_count: longint = 0;

  // Reporter
  reporter: TTestReporter = nil;
  owns_reporter: boolean = false;

procedure reset_test_state; forward;
procedure finalize_test_output; forward;

{----------------------------- Reporter management -----------------------------}

procedure free_reporter;
begin
  if owns_reporter and (reporter <> nil) then
    reporter.Free;
  reporter := nil;
  owns_reporter := false;
end;

procedure set_reporter(AReporter: TTestReporter);
begin
  free_reporter;
  reporter := AReporter;
  owns_reporter := false; // Caller owns it
end;

procedure set_reporter(AReporterType: TReporterType);
begin
  free_reporter;
  case AReporterType of
    rtTAP: reporter := TTAPReporter.Create;
    rtJUnit: reporter := TJUnitReporter.Create;
    rtGitHubActions: reporter := TGitHubActionsReporter.Create;
  end;
  owns_reporter := true;
end;

procedure add_reporter(AReporter: TTestReporter);
var
  multi: TMultiReporter;
begin
  if reporter = nil then
  begin
    reporter := AReporter;
    owns_reporter := false;
  end
  else if reporter is TMultiReporter then
  begin
    TMultiReporter(reporter).AddReporter(AReporter);
  end
  else
  begin
    multi := TMultiReporter.Create(owns_reporter);
    multi.AddReporter(reporter);
    multi.AddReporter(AReporter);
    reporter := multi;
    owns_reporter := true;
  end;
end;

procedure ensure_reporter;
begin
  if reporter = nil then
  begin
    reporter := TTAPReporter.Create;
    owns_reporter := true;
  end;
end;

{----------------------------- Unit management -----------------------------}

procedure start_tests;
begin
  ensure_reporter;
  SetLength(units, 0);
  unit_count := 0;
  total_tests := 0;
  main_tests := 0;
  passed := 0;
  failed := 0;
  test_count := 0;
  subtest_count := 0;
  current_unit := nil;
  current_test := nil;
  current_subtest := nil;
end;

procedure end_tests;
begin
  reporter.EndTests(total_tests, passed, failed);
  free_reporter;
  if failed > 0 then
    halt(1)
  else
    halt(0);
end;

procedure start_unit(const AUnitName: string);
begin
  inc(unit_count);
  SetLength(units, unit_count);
  current_unit := @units[unit_count - 1];
  current_unit^.name := AUnitName;
  current_unit^.idx := 0;
  current_test := nil;
end;

procedure end_unit;
begin
  // Called explicitly by user if needed
end;

{----------------------------- Test registration -----------------------------}

procedure register_test(const ATestName: string; AProc: TTestProc = nil);
begin
  if current_unit = nil then
  begin
    writeln('error: no unit declared for test: ', ATestName);
    halt(-1);
  end;

  inc(main_tests);
  inc(total_tests);

  inc(current_unit^.idx);
  SetLength(current_unit^.tests, current_unit^.idx);

  current_test := @current_unit^.tests[current_unit^.idx - 1];
  current_test^.name := current_unit^.name + '::' + ATestName;
  current_test^.proc := AProc;
  current_test^.idx := 0;
end;

procedure register_subtest(const ATestName: string; AProc: TTestProc);
begin
  if current_test = nil then
  begin
    writeln('error: no test declared for subtest: ', ATestName);
    halt(-1);
  end;

  if current_test^.proc <> nil then
  begin
    writeln('error: test procedure declared for subtest');
    halt(-1);
  end;

  inc(total_tests);

  inc(current_test^.idx);
  SetLength(current_test^.subtests, current_test^.idx);

  current_test^.subtests[current_test^.idx - 1].name := current_test^.name + '::' + ATestName;
  current_test^.subtests[current_test^.idx - 1].proc := AProc;
end;

{----------------------------- Test execution -----------------------------}

procedure reset_test_state;
begin
  test_all_passed := true;
  failure_count := 0;
  SetLength(failures, 0);
end;

procedure finalize_test_output;
var
  count: longint;
  name: string;
begin
  if is_subtest then
  begin
    count := subtest_count;
    name := current_subtest^.name;
  end
  else
  begin
    count := test_count;
    name := current_test^.name;
  end;

  if test_all_passed then
  begin
    inc(passed);
    last_result := true;
    reporter.TestPassed(name, count, is_subtest);
  end
  else
  begin
    inc(failed);
    last_result := false;
    reporter.TestFailed(name, count, is_subtest, failures);
  end;
end;

procedure run_tests;
var
  i, j, k: longint;
  success: boolean;
begin
  reporter.StartTests(main_tests);

  // Run units
  for i := 0 to High(units) do
  begin
    reporter.StartUnit(units[i].name);

    // Run tests
    for j := 0 to units[i].idx - 1 do
    begin
      is_subtest := false;
      inc(test_count);

      current_test := @units[i].tests[j];

      // Run subtests if any
      if current_test^.idx > 0 then
      begin
        reporter.StartSubtests(current_test^.idx);

        success := true;
        subtest_count := 0;
        for k := 0 to current_test^.idx - 1 do
        begin
          is_subtest := true;
          inc(subtest_count);

          current_subtest := @current_test^.subtests[k];

          reset_test_state;
          current_subtest^.proc;
          finalize_test_output;

          success := success and last_result;
        end;

        // Output parent test result
        is_subtest := false;
        if success then
        begin
          inc(passed);
          reporter.TestPassed(current_test^.name, test_count, false);
          last_result := true;
        end
        else
        begin
          inc(failed);
          SetLength(failures, 0); // Empty failures for parent
          reporter.TestFailed(current_test^.name, test_count, false, failures);
          last_result := false;
        end;
      end
      else
      begin
        // Run the test
        reset_test_state;
        current_test^.proc;
        finalize_test_output;
      end;
    end;

    reporter.EndUnit;
  end;
end;

{----------------------------- Assertions -----------------------------}

procedure record_failure(const ATestName, AOperator, AExpected, AGot: string);
begin
  test_all_passed := false;
  inc(failure_count);
  SetLength(failures, failure_count);
  failures[failure_count - 1].testname := ATestName;
  failures[failure_count - 1].operator_ := AOperator;
  failures[failure_count - 1].expected := AExpected;
  failures[failure_count - 1].got := AGot;
end;

procedure assert_equal(const ATestName: string; AExpected, AGot: longint);
begin
  if AExpected <> AGot then
    record_failure(ATestName, 'equal', IntToStr(AExpected), IntToStr(AGot));
end;

procedure assert_equal(const ATestName: string; const AExpected, AGot: string);
begin
  if AExpected <> AGot then
    record_failure(ATestName, 'equal', '"' + AExpected + '"', '"' + AGot + '"');
end;

procedure assert_true(const ATestName: string; ACondition: boolean);
begin
  if not ACondition then
    record_failure(ATestName, 'true', 'TRUE', 'FALSE');
end;

procedure assert_false(const ATestName: string; ACondition: boolean);
begin
  if ACondition then
    record_failure(ATestName, 'false', 'FALSE', 'TRUE');
end;

procedure assert_equal_files(const ATestName, AExpectedFile, AGotFile: string);
var
  expected, got: TStringList;
  i, maxLines: Integer;
  diffLines: string;
begin
  expected := TStringList.Create;
  got := TStringList.Create;
  try
    if FileExists(AExpectedFile) then
      expected.LoadFromFile(AExpectedFile)
    else
      expected.Text := '';

    if FileExists(AGotFile) then
      got.LoadFromFile(AGotFile)
    else
      got.Text := '';

    if expected.Text <> got.Text then
    begin
      diffLines := '';
      maxLines := expected.Count;
      if got.Count > maxLines then
        maxLines := got.Count;

      for i := 0 to maxLines - 1 do
      begin
        if (i >= expected.Count) then
          diffLines := diffLines + '+' + got[i] + ' '
        else if (i >= got.Count) then
          diffLines := diffLines + '-' + expected[i] + ' '
        else if expected[i] <> got[i] then
          diffLines := diffLines + '-' + expected[i] + ' +' + got[i] + ' ';
      end;

      record_failure(ATestName, 'equal_files',
        AExpectedFile + ' (' + IntToStr(expected.Count) + ' lines)',
        AGotFile + ' (' + IntToStr(got.Count) + ' lines) diff: ' + diffLines);
    end;
  finally
    expected.Free;
    got.Free;
  end;
end;

end.
