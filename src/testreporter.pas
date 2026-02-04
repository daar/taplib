unit TestReporter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TFailureInfo = record
    testname: string;
    operator_: string;
    expected: string;
    got: string;
  end;
  TFailureArray = array of TFailureInfo;

  { TTestReporter - Abstract base class for test reporters }

  TTestReporter = class
  public
    procedure StartTests(AMainTestCount: longint); virtual; abstract;
    procedure EndTests(ATotal, APassed, AFailed: longint); virtual; abstract;

    procedure StartUnit(const AUnitName: string); virtual; abstract;
    procedure EndUnit; virtual; abstract;

    procedure StartSubtests(ACount: longint); virtual; abstract;
    procedure TestPassed(const ATestName: string; ACount: longint; AIsSubtest: boolean); virtual; abstract;
    procedure TestFailed(const ATestName: string; ACount: longint; AIsSubtest: boolean;
      const AFailures: TFailureArray); virtual; abstract;
  end;

  { TTAPReporter - TAP v14 format output }

  TTAPReporter = class(TTestReporter)
  public
    procedure StartTests(AMainTestCount: longint); override;
    procedure EndTests(ATotal, APassed, AFailed: longint); override;
    procedure StartUnit(const AUnitName: string); override;
    procedure EndUnit; override;
    procedure StartSubtests(ACount: longint); override;
    procedure TestPassed(const ATestName: string; ACount: longint; AIsSubtest: boolean); override;
    procedure TestFailed(const ATestName: string; ACount: longint; AIsSubtest: boolean;
      const AFailures: TFailureArray); override;
  private
    function Indent(AIsSubtest: boolean): string;
  end;

  { TJUnitReporter - JUnit XML format for CI systems }

  TJUnitReporter = class(TTestReporter)
  private
    FDoc: TStringList;
    FCurrentSuite: string;
    FOutputFile: string;
  public
    constructor Create(const AOutputFile: string = 'junit.xml');
    destructor Destroy; override;

    procedure StartTests(AMainTestCount: longint); override;
    procedure EndTests(ATotal, APassed, AFailed: longint); override;
    procedure StartUnit(const AUnitName: string); override;
    procedure EndUnit; override;
    procedure StartSubtests(ACount: longint); override;
    procedure TestPassed(const ATestName: string; ACount: longint; AIsSubtest: boolean); override;
    procedure TestFailed(const ATestName: string; ACount: longint; AIsSubtest: boolean;
      const AFailures: TFailureArray); override;
  end;

  { TGitHubActionsReporter - GitHub Actions workflow commands }

  TGitHubActionsReporter = class(TTestReporter)
  private
    FCurrentUnit: string;
  public
    procedure StartTests(AMainTestCount: longint); override;
    procedure EndTests(ATotal, APassed, AFailed: longint); override;
    procedure StartUnit(const AUnitName: string); override;
    procedure EndUnit; override;
    procedure StartSubtests(ACount: longint); override;
    procedure TestPassed(const ATestName: string; ACount: longint; AIsSubtest: boolean); override;
    procedure TestFailed(const ATestName: string; ACount: longint; AIsSubtest: boolean;
      const AFailures: TFailureArray); override;
  end;

  { TMultiReporter - Sends output to multiple reporters }

  TMultiReporter = class(TTestReporter)
  private
    FReporters: array of TTestReporter;
    FOwnsReporters: boolean;
  public
    constructor Create(AOwnsReporters: boolean = true);
    destructor Destroy; override;
    procedure AddReporter(AReporter: TTestReporter);

    procedure StartTests(AMainTestCount: longint); override;
    procedure EndTests(ATotal, APassed, AFailed: longint); override;
    procedure StartUnit(const AUnitName: string); override;
    procedure EndUnit; override;
    procedure StartSubtests(ACount: longint); override;
    procedure TestPassed(const ATestName: string; ACount: longint; AIsSubtest: boolean); override;
    procedure TestFailed(const ATestName: string; ACount: longint; AIsSubtest: boolean;
      const AFailures: TFailureArray); override;
  end;

implementation

function EscapeXML(const S: string): string;
var
  I: integer;
begin
  Result := '';
  for I := 1 to Length(S) do
    case S[I] of
      '<': Result := Result + '&lt;';
      '>': Result := Result + '&gt;';
      '&': Result := Result + '&amp;';
      '"': Result := Result + '&quot;';
      '''': Result := Result + '&apos;';
    else
      Result := Result + S[I];
    end;
end;

{ TTAPReporter }

procedure TTAPReporter.StartTests(AMainTestCount: longint);
begin
  writeln('TAP version 14');
  writeln('1..', AMainTestCount);
end;

procedure TTAPReporter.EndTests(ATotal, APassed, AFailed: longint);
begin
  writeln('# tests ', ATotal);
  writeln('# pass  ', APassed);
  writeln('# fail  ', AFailed);
end;

procedure TTAPReporter.StartUnit(const AUnitName: string);
begin
  writeln('# ', AUnitName);
end;

procedure TTAPReporter.EndUnit;
begin
end;

procedure TTAPReporter.StartSubtests(ACount: longint);
begin
  writeln('    1..', ACount);
end;

procedure TTAPReporter.TestPassed(const ATestName: string; ACount: longint; AIsSubtest: boolean);
begin
  writeln(Indent(AIsSubtest), 'ok ', ACount, ' - ', ATestName);
end;

procedure TTAPReporter.TestFailed(const ATestName: string; ACount: longint; AIsSubtest: boolean;
  const AFailures: TFailureArray);
var
  Prefix: string;
  I: longint;
begin
  Prefix := Indent(AIsSubtest);
  writeln(Prefix, 'not ok ', ACount, ' - ', ATestName);

  if Length(AFailures) > 0 then
  begin
    writeln(Prefix, '  ---');

    if Length(AFailures) = 1 then
    begin
      writeln(Prefix, '    operator: ', AFailures[0].operator_);
      writeln(Prefix, '    expected: ', AFailures[0].expected);
      writeln(Prefix, '    actual:   ', AFailures[0].got);
    end
    else
    begin
      writeln(Prefix, '    failures:');
      for I := 0 to High(AFailures) do
      begin
        writeln(Prefix, '      - assertion: ', AFailures[I].testname);
        writeln(Prefix, '        operator: ', AFailures[I].operator_);
        writeln(Prefix, '        expected: ', AFailures[I].expected);
        writeln(Prefix, '        actual:   ', AFailures[I].got);
      end;
    end;

    writeln(Prefix, '  ...');
  end;
end;

function TTAPReporter.Indent(AIsSubtest: boolean): string;
begin
  if AIsSubtest then
    Result := '    '
  else
    Result := '';
end;

{ TJUnitReporter }

constructor TJUnitReporter.Create(const AOutputFile: string);
begin
  inherited Create;
  FDoc := TStringList.Create;
  FOutputFile := AOutputFile;
end;

destructor TJUnitReporter.Destroy;
begin
  FDoc.Free;
  inherited Destroy;
end;

procedure TJUnitReporter.StartTests(AMainTestCount: longint);
begin
  FDoc.Clear;
  FDoc.Add('<?xml version="1.0" encoding="UTF-8"?>');
  FDoc.Add('<testsuites>');
end;

procedure TJUnitReporter.EndTests(ATotal, APassed, AFailed: longint);
begin
  FDoc.Add('</testsuites>');
  FDoc.SaveToFile(FOutputFile);
end;

procedure TJUnitReporter.StartUnit(const AUnitName: string);
begin
  FCurrentSuite := AUnitName;
  FDoc.Add(Format('  <testsuite name="%s">', [EscapeXML(AUnitName)]));
end;

procedure TJUnitReporter.EndUnit;
begin
  FDoc.Add('  </testsuite>');
end;

procedure TJUnitReporter.StartSubtests(ACount: longint);
begin
  // JUnit doesn't have subtest concept - tests are flat
end;

procedure TJUnitReporter.TestPassed(const ATestName: string; ACount: longint; AIsSubtest: boolean);
begin
  FDoc.Add(Format('    <testcase name="%s" classname="%s" />',
    [EscapeXML(ATestName), EscapeXML(FCurrentSuite)]));
end;

procedure TJUnitReporter.TestFailed(const ATestName: string; ACount: longint; AIsSubtest: boolean;
  const AFailures: TFailureArray);
var
  Msg: string;
  I: longint;
begin
  FDoc.Add(Format('    <testcase name="%s" classname="%s">',
    [EscapeXML(ATestName), EscapeXML(FCurrentSuite)]));

  if Length(AFailures) = 0 then
    Msg := 'One or more subtests failed'
  else if Length(AFailures) = 1 then
    Msg := Format('%s: expected %s, got %s',
      [AFailures[0].operator_, AFailures[0].expected, AFailures[0].got])
  else
  begin
    Msg := '';
    for I := 0 to High(AFailures) do
    begin
      if I > 0 then Msg := Msg + '; ';
      Msg := Msg + Format('%s: %s expected %s got %s',
        [AFailures[I].testname, AFailures[I].operator_,
         AFailures[I].expected, AFailures[I].got]);
    end;
  end;

  FDoc.Add(Format('      <failure message="%s"/>', [EscapeXML(Msg)]));
  FDoc.Add('    </testcase>');
end;

{ TGitHubActionsReporter }

procedure TGitHubActionsReporter.StartTests(AMainTestCount: longint);
begin
  writeln('Running ', AMainTestCount, ' tests...');
  writeln;
end;

procedure TGitHubActionsReporter.EndTests(ATotal, APassed, AFailed: longint);
begin
  writeln;
  if AFailed = 0 then
    writeln('::notice::All ', ATotal, ' tests passed')
  else
    writeln('::error::Tests failed: ', AFailed, ' of ', ATotal, ' tests failed');
end;

procedure TGitHubActionsReporter.StartUnit(const AUnitName: string);
begin
  FCurrentUnit := AUnitName;
  writeln('::group::', AUnitName);
end;

procedure TGitHubActionsReporter.EndUnit;
begin
  writeln('::endgroup::');
end;

procedure TGitHubActionsReporter.StartSubtests(ACount: longint);
begin
  // GitHub Actions doesn't need special subtest handling
end;

procedure TGitHubActionsReporter.TestPassed(const ATestName: string; ACount: longint; AIsSubtest: boolean);
begin
  writeln('[PASS] ', ATestName);
end;

procedure TGitHubActionsReporter.TestFailed(const ATestName: string; ACount: longint; AIsSubtest: boolean;
  const AFailures: TFailureArray);
var
  Msg: string;
  I: longint;
begin
  writeln('[FAIL] ', ATestName);

  if Length(AFailures) = 0 then
    Msg := 'One or more subtests failed'
  else if Length(AFailures) = 1 then
    Msg := Format('%s: expected %s, got %s',
      [AFailures[0].operator_, AFailures[0].expected, AFailures[0].got])
  else
  begin
    Msg := '';
    for I := 0 to High(AFailures) do
    begin
      if I > 0 then Msg := Msg + ' | ';
      Msg := Msg + Format('%s (%s: expected %s got %s)',
        [AFailures[I].testname, AFailures[I].operator_,
         AFailures[I].expected, AFailures[I].got]);
    end;
  end;

  // GitHub Actions annotation
  writeln('::error title=', ATestName, '::', Msg);
end;

{ TMultiReporter }

constructor TMultiReporter.Create(AOwnsReporters: boolean);
begin
  inherited Create;
  FOwnsReporters := AOwnsReporters;
  SetLength(FReporters, 0);
end;

destructor TMultiReporter.Destroy;
var
  I: longint;
begin
  if FOwnsReporters then
    for I := 0 to High(FReporters) do
      FReporters[I].Free;
  inherited Destroy;
end;

procedure TMultiReporter.AddReporter(AReporter: TTestReporter);
begin
  SetLength(FReporters, Length(FReporters) + 1);
  FReporters[High(FReporters)] := AReporter;
end;

procedure TMultiReporter.StartTests(AMainTestCount: longint);
var
  I: longint;
begin
  for I := 0 to High(FReporters) do
    FReporters[I].StartTests(AMainTestCount);
end;

procedure TMultiReporter.EndTests(ATotal, APassed, AFailed: longint);
var
  I: longint;
begin
  for I := 0 to High(FReporters) do
    FReporters[I].EndTests(ATotal, APassed, AFailed);
end;

procedure TMultiReporter.StartUnit(const AUnitName: string);
var
  I: longint;
begin
  for I := 0 to High(FReporters) do
    FReporters[I].StartUnit(AUnitName);
end;

procedure TMultiReporter.EndUnit;
var
  I: longint;
begin
  for I := 0 to High(FReporters) do
    FReporters[I].EndUnit;
end;

procedure TMultiReporter.StartSubtests(ACount: longint);
var
  I: longint;
begin
  for I := 0 to High(FReporters) do
    FReporters[I].StartSubtests(ACount);
end;

procedure TMultiReporter.TestPassed(const ATestName: string; ACount: longint; AIsSubtest: boolean);
var
  I: longint;
begin
  for I := 0 to High(FReporters) do
    FReporters[I].TestPassed(ATestName, ACount, AIsSubtest);
end;

procedure TMultiReporter.TestFailed(const ATestName: string; ACount: longint; AIsSubtest: boolean;
  const AFailures: TFailureArray);
var
  I: longint;
begin
  for I := 0 to High(FReporters) do
    FReporters[I].TestFailed(ATestName, ACount, AIsSubtest, AFailures);
end;

end.
