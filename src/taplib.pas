unit TapLib;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  TTestProc = procedure;

procedure start_tests;
procedure end_tests;

procedure start_unit(const unitname: string);
procedure end_unit;

procedure register_test(const testname: string; proc: TTestProc = nil);
procedure register_subtest(const testname: string; proc: TTestProc);
procedure run_tests;

procedure assert_equal(const testname: string; expected, got: longint);
procedure assert_equal(const testname: string; expected, got: string);
procedure assert_true(const testname: string; condition: boolean);
procedure assert_false(const testname: string; condition: boolean);
procedure assert_equal_files(const testname, expectedFile, gotFile: string);

implementation

uses
  Process, Classes;

type
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

  total_tests: longint = 0;
  main_tests: longint = 0;
  subtest: boolean = false;

  passed: longint = 0;
  failed: longint = 0;
  test_count: longint = 0;
  subtest_count: longint = 0;
  last_result: boolean;

procedure print_pass; forward;
procedure print_fail_header;  forward;
{----------------------------- Unit management -----------------------------}

procedure start_tests;
begin
  writeln('TAP version 14');
  setlength(units, 0);
end;

procedure end_tests;
begin
  writeln('# tests ', total_tests);
  writeln('# pass  ', passed);
  writeln('# fail  ', failed);
  if failed > 0 then
    halt(1)
  else
    halt(0);
end;

procedure start_unit(const unitname: string);
begin
  //add new unit
  inc(unit_count);
  setlength(units, unit_count);
  current_unit := @units[unit_count-1];
  current_unit^.name:=unitname;
end;

procedure end_unit;
begin
end;

{----------------------------- Test registration -----------------------------}

procedure register_test(const testname: string; proc: TTestProc = nil);
begin
  if current_unit = nil then
  begin
  writeln('error: no unit declared for test: ', testname);
    halt(-1);
  end;

  inc(main_tests);
  inc(total_tests);

  inc(current_unit^.idx);
  setlength(current_unit^.tests, current_unit^.idx);

  current_test := @current_unit^.tests[current_unit^.idx-1];
  current_test^.name := current_unit^.name + '::' + testname;
  current_test^.proc := proc;
end;

procedure register_subtest(const testname: string; proc: TTestProc);
begin
  if current_test = nil then
  begin
    writeln('error: no test declared for subtest: ', testname);
    halt(-1);
  end;

  if current_test^.proc <> nil then
  begin
    writeln('error: test procedure declared for subtest');
    halt(-1);
  end;

  inc(total_tests);

  inc(current_test^.idx);
  setlength(current_test^.subtests, current_test^.idx);

  current_test^.subtests[current_test^.idx-1].name := current_test^.name + '::' + testname;
  current_test^.subtests[current_test^.idx-1].proc := proc;
end;

{----------------------------- Test execution -----------------------------}

procedure run_tests;
var
  i, j, k: longint;
  succes: boolean = true;
begin
  writeln('1..', main_tests);

  //run units
  for i := 0 to high(units) do
  begin
    writeln('# ', units[i].name);

    //run tests
    for j := 0 to units[i].idx-1 do
    begin
      subtest := false;
      inc(test_count);

      current_test := @units[i].tests[j];

      // run sub tests before tests
      if current_test^.idx > 0 then
      begin
         writeln('    1..', current_test^.idx);

        //run subtests
        subtest_count := 0;
        for k:=0 to units[i].tests[j].idx-1 do
        begin
          subtest := true;
          inc(subtest_count);

          current_test := @units[i].tests[j].subtests[k];
          current_test^.proc;

          succes := succes and last_result;
        end;

        subtest := false;
        current_test := @units[i].tests[j];
        if succes then
          print_pass
        else
          print_fail_header

      end
      else
      begin
        //run the test
        current_test^.proc;
      end;
    end;
  end;
end;

{----------------------------- Assertions -----------------------------}

function indent: string;
begin
  if subtest then
    exit('    ')
  else
    exit('');
end;

function get_count: longint;
begin
  if subtest then
    exit(subtest_count)
  else
    exit(test_count);
end;

procedure print_pass;
begin
  inc(passed);
  writeln(indent + 'ok ', get_count, ' - ', current_test^.name);
end;

procedure print_fail_header;
begin
  inc(failed);
  writeln(indent + 'not ok ', get_count, ' - ', current_test^.name);
end;

procedure print_fail(operator_: string; expected, got: longint);
begin
  print_fail_header;
  writeln(indent + '  ---');
  writeln(indent + '    operator: ', operator_);
  writeln(indent + '    expected: ', expected);
  writeln(indent + '    actual:   ', got);
  writeln(indent + '  ...');

  last_result := true;
end;

procedure print_fail(operator_: string; expected, got: boolean);
begin
  print_fail_header;
  writeln(indent + '  ---');
  writeln(indent + '    operator: ', operator_);
  writeln(indent + '    expected: ', expected);
  writeln(indent + '    actual:   ', got);
  writeln(indent + '  ...');

  last_result := false;
end;

procedure assert_equal(const testname: string; expected, got: longint);
begin
  if expected = got then
    print_pass
  else
    print_fail('equal', expected, got);
end;

procedure assert_equal(const testname: string; expected, got: string);
var
  pos: integer;
begin
  if expected = got then
    print_pass
  else
  begin
    print_fail_header;
    writeln(indent + '  ---');
    writeln(indent + '    operator: equal');
    writeln(indent + '    expected: "', expected, '"');
    writeln(indent + '    actual:   "', got, '"');
    writeln(indent + '  ...');

    last_result := false;
  end;
end;

procedure assert_true(const testname: string; condition: boolean);
begin
  if condition then
    print_pass
  else
    print_fail('true', true, false);
end;

procedure assert_false(const testname: string; condition: boolean);
begin
  if not condition then
    print_pass
  else
    print_fail('false', false, true);
end;

procedure assert_equal_files(const testname, expectedFile, gotFile: string);
var
  expected, got: TStringList;
  i, maxLines: Integer;
  ok: Boolean;
begin
  expected := TStringList.Create;
  got := TStringList.Create;
  try
    if FileExists(expectedFile) then
      expected.LoadFromFile(expectedFile)
    else
      expected.Text := '';

    if FileExists(gotFile) then
      got.LoadFromFile(gotFile)
    else
      got.Text := '';

    ok := expected.Text = got.Text;
    if ok then
      print_pass
    else
    begin
      print_fail_header;
      writeLn(indent + '# --- Diff between ', expectedFile, ' and ', gotFile, ' ---');
      maxLines := expected.Count;
      if got.Count > maxLines then
        maxLines := got.Count;

      for i := 0 to maxLines - 1 do
      begin
        if (i >= expected.Count) then
          WriteLn(indent + '# + ', got[i])                // extra line in got
        else if (i >= got.Count) then
          WriteLn(indent + '# - ', expected[i])           // missing line in got
        else if expected[i] <> got[i] then
        begin
          WriteLn(indent + '# - ', expected[i]);
          WriteLn(indent + '# + ', got[i]);
        end;
      end;
      WriteLn(indent + '# --- End diff ---');
      last_result := false;
    end;
  finally
    expected.Free;
    got.Free;
  end;
end;


end.
