program example;

uses
  SysUtils, TapLib, TestReporter;

procedure TestAddition;
begin
  assert_equal('2 + 2 = 4', 4, 2 + 2);
  assert_equal('2 + 3 = 5', 5, 2 + 3);
end;

procedure TestStrings;
begin
  assert_equal('Hello = Hello', 'Hello', 'Hello');
  assert_equal('World = World', 'World', 'World');
  assert_equal('Mismatch example', 'foo', 'bar'); // should fail
end;

procedure TestBoolean;
begin
  assert_true('True is true', True);
  assert_false('False is false', False);
  assert_true('False is not true', False); // should fail
end;

procedure SubtestExample;
begin
  assert_equal('Subtest 1', 1, 1);
  assert_equal('Subtest 2', 2, 3); // should fail
end;

var
  format: string;

begin
  // Select reporter based on command line: --format=tap|junit|github
  // Default is TAP format
  if ParamCount > 0 then
  begin
    format := LowerCase(ParamStr(1));
    if (format = '--format=junit') or (format = '-junit') then
      set_reporter(rtJUnit)
    else if (format = '--format=github') or (format = '-github') then
      set_reporter(rtGitHubActions)
    else if (format = '--format=tap') or (format = '-tap') then
      set_reporter(rtTAP);
    // else: default TAP
  end;

  // start testing
  start_tests;

  // define a unit
  start_unit('MathTests');
  register_test('AdditionTests', @TestAddition);
  register_test('BooleanTests', @TestBoolean);

  start_unit('StringTests');
  register_test('StringEquality', nil); // no main proc, use subtests
  register_subtest('SubtestExample', @SubtestExample);
  register_test('StringTestsMain', @TestStrings);

  // run everything
  run_tests;

  // finalize
  end_tests;
end.
