unit uExecAndCapture;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Forms,
  Generics.Collections, Generics.Defaults;

type
  TArg< T > = reference to procedure( const Arg: T );

procedure ExecAndCaptureReal( const ACommand, AParameters: String; CallBack: TArg< PAnsiChar > );

{
  procedure TForm1.CaptureToMemo( const Line: PAnsiChar );
  begin
  Memo1.Lines.Add( String( Line ) );
  end;

  procedure TForm1.Button1Click( Sender: TObject );
  begin
  ExecAndCaptureReal( ACommand.Text, AParameters.Text, CaptureToMemo );
  end;

  procedure TForm1.Button1Click( Sender: TObject );
  begin
  ExecAndCaptureReal
  (
  ComboBox1.Text, Edit1.Text,
  procedure( const Line: PAnsiChar )
  begin
  Memo1.Lines.Add( String( Line ) );
  end
  );
  end;
}

function ExecAndCapture( const ACommand, AParameters: String; var AOutput: AnsiString ): Integer;

implementation

procedure ExecAndCaptureReal( const ACommand, AParameters: String; CallBack: TArg< PAnsiChar > );
const
  CReadBuffer = 2400;
var
  saSecurity: TSecurityAttributes;
  hRead: THandle;
  hWrite: THandle;
  suiStartup: TStartupInfo;
  piProcess: TProcessInformation;
  pBuffer: array [ 0 .. CReadBuffer ] of AnsiChar;
  dBuffer: array [ 0 .. CReadBuffer ] of AnsiChar;
  dRead: DWORD;
  dRunning: DWORD;
  dAvailable: DWORD;
  CommandAndParameters: string;
begin
  saSecurity.nLength := SizeOf( TSecurityAttributes );
  saSecurity.bInheritHandle := true;
  saSecurity.lpSecurityDescriptor := nil;
  CommandAndParameters := ACommand + ' ' + AParameters;
  if CreatePipe( hRead, hWrite, @saSecurity, 0 ) then
    try
      FillChar( suiStartup, SizeOf( TStartupInfo ), #0 );
      suiStartup.cb := SizeOf( TStartupInfo );
      suiStartup.hStdInput := hRead;
      suiStartup.hStdOutput := hWrite;
      suiStartup.hStdError := hWrite;
      suiStartup.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
      suiStartup.wShowWindow := SW_HIDE;
      if CreateProcess( nil, PChar( CommandAndParameters ), @saSecurity, @saSecurity, true, NORMAL_PRIORITY_CLASS, nil,
        nil, suiStartup, piProcess ) then
        try
          repeat
            dRunning := WaitForSingleObject( piProcess.hProcess, 100 );
            PeekNamedPipe( hRead, nil, 0, nil, @dAvailable, nil );
            if ( dAvailable > 0 ) then
              repeat
                dRead := 0;
                ReadFile( hRead, pBuffer[ 0 ], CReadBuffer, dRead, nil );
                pBuffer[ dRead ] := #0;
                OemToCharA( pBuffer, dBuffer );
                CallBack( dBuffer );
              until ( dRead < CReadBuffer );
            Application.ProcessMessages;
          until ( dRunning <> WAIT_TIMEOUT );
        finally
          CloseHandle( piProcess.hProcess );
          CloseHandle( piProcess.hThread );
        end;
    finally
      CloseHandle( hRead );
      CloseHandle( hWrite );
    end;
end;

function ExecAndCapture( const ACommand, AParameters: String; var AOutput: AnsiString ): Integer;
type
  TAnoPipe = record
    Input: THandle;
    Output: THandle;
  end;

const
  cBufferSize = 2048;

var
  ACmdLine: string;

  vStartupInfo: TStartupInfo;
  vSecurityAttributes: TSecurityAttributes;
  vReadBytes: DWORD;
  vProcessInfo: TProcessInformation;
  vStdInPipe: TAnoPipe;
  vStdOutPipe: TAnoPipe;

  pBuffer: array [ 0 .. cBufferSize ] of AnsiChar;
  dBuffer: array [ 0 .. cBufferSize ] of AnsiChar;
begin
  Result := 0;

  ACmdLine := ACommand + ' ' + AParameters;

  with vSecurityAttributes do
  begin
    nLength := SizeOf( TSecurityAttributes );
    bInheritHandle := true;
    lpSecurityDescriptor := nil;
  end;

  // Create anonymous pipe for standard input
  if not CreatePipe( vStdInPipe.Output, vStdInPipe.Input, @vSecurityAttributes, 0 ) then
    raise Exception.Create( 'Failed to create pipe for standard input. System error message: ' +
      SysErrorMessage( GetLastError ) );

  try
    // Create anonymous pipe for standard output (and also for standard error)
    if not CreatePipe( vStdOutPipe.Output, vStdOutPipe.Input, @vSecurityAttributes, 0 ) then
      raise Exception.Create( 'Failed to create pipe for standard output. System error message: ' +
        SysErrorMessage( GetLastError ) );

    try
      // initialize the startup info to match our purpose
      FillChar( vStartupInfo, SizeOf( TStartupInfo ), #0 );
      vStartupInfo.cb := SizeOf( TStartupInfo );
      vStartupInfo.wShowWindow := SW_HIDE; // we don't want to show the process
      // assign our pipe for the process' standard input
      vStartupInfo.hStdInput := vStdInPipe.Output;
      // assign our pipe for the process' standard output
      vStartupInfo.hStdOutput := vStdOutPipe.Input;
      vStartupInfo.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;

      if not CreateProcess( nil, PChar( ACmdLine ), @vSecurityAttributes, @vSecurityAttributes, true,
        NORMAL_PRIORITY_CLASS, nil, nil, vStartupInfo, vProcessInfo ) then
        raise Exception.Create( 'Failed creating the console process. System error msg: ' +
          SysErrorMessage( GetLastError ) );

      try
        // wait until the console program terminated
        while WaitForSingleObject( vProcessInfo.hProcess, 50 ) = WAIT_TIMEOUT do
          Sleep( 0 );

        // clear the output storage
        AOutput := '';
        // Read text returned by the console program in its StdOut channel
        // The problem is that the console application emits UTF-16.
        repeat
          ReadFile( vStdOutPipe.Output, pBuffer[ 0 ], cBufferSize, vReadBytes, nil );
          if vReadBytes > 0 then
          begin
            pBuffer[ vReadBytes ] := #0;
            OemToCharA( pBuffer, dBuffer );
            AOutput := AOutput + AnsiString( dBuffer );
            Inc( Result, vReadBytes );
          end;
        until ( vReadBytes < cBufferSize );

      finally
        CloseHandle( vProcessInfo.hProcess );
        CloseHandle( vProcessInfo.hThread );
      end;

    finally
      CloseHandle( vStdOutPipe.Input );
      CloseHandle( vStdOutPipe.Output );
    end;

  finally
    CloseHandle( vStdInPipe.Input );
    CloseHandle( vStdInPipe.Output );
  end;
end;

end.
