unit uMisc;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes, System.Character, System.Variants,
  Generics.Collections, Generics.Defaults,
  VCL.Forms;

type
  TArg< T > = reference to procedure( const Arg: T );

// Generate Temp FileName uses Windows API GetTempFileName()
function GenerateTempFileName( const Extension: String ): String;

// Generate Temp FileName uses GUID -- CreateGUID()
function GenerateTempFileNameEx( const Extension: String ): String;

// Remove Char < 0x20 or Char > 0x7F, and Merge Multi 0x20 Spaces
function RemoveWhiteSpace( const s: String ): String;

// CopyFile uses TMemoryStream
procedure CopyFile( SourceFileName, DestFileName: string );

// Capture all to Aoutput
function ExecAndCapture( const ACommand, AParameters: String; var AOutput: AnsiString ): Integer;

// Capture Every Line
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

implementation

{
  function GetTempFileName(lpPathName, lpPrefixString: LPCWSTR; uUnique: UINT; lpTempFileName: LPWSTR): UINT;

  Why does GetTempFileName function create new empty file ?
  It's in the GetTempFileName function description (emphasized by me):
  Creates a name for a temporary file.
  If a unique file name is generated, an empty file is created and the handle to it is released;
  otherwise, only a file name is generated.

  How to avoid creating the new file when using GetTempFileName function ?
  Either you can't let the function generate a unique file name (which creates an empty file)
  or just delete that created empty file after the GetTempFileName function returns.

  From the reference (emphasized by me):
  Temporary files whose names have been created by this function are not automatically deleted.
  To delete these files call DeleteFile.
}

function GenerateTempFileName( const Extension: String ): String;
var
  // This buffer should be MAX_PATH characters to accommodate the path plus the terminating null character.
  lpTempFileName: Array [ 0 .. MAX_PATH - 1 ] of Char;
  lpTempPathName: Array [ 0 .. MAX_PATH - 1 ] of Char;
begin
  Winapi.Windows.GetTempPath( MAX_PATH, lpTempPathName );

  // If uUnique is zero, GetTempFileName creates an empty file and closes it.
  //
  // If uUnique is not zero, you must create the file yourself.
  // Only a file name is created, because GetTempFileName is not able to guarantee that the file name is unique.
  repeat
    Winapi.Windows.GetTempFileName( lpTempPathName, nil, 1, lpTempFileName );
    Result := ChangeFileExt( lpTempFileName, Extension );
  until not FileExists( Result );

  // The GetTempFile function just returns a unique filename but doesn't create the file.
  // If uUnique is not zero, you must create the file yourself.
  // DeleteFile( lpTempFileName );
end;

{
  #ifdef UNICODE
  typedef wchar_t TCHAR;
  #else
  typedef unsigned char TCHAR;
  #endif
}

function GenerateTempFileNameEx( const Extension: String ): String;
var
  Guid: TGUID;
  GuidString: String;
  lpTempFileName: String;
  lpTempPathName: Array [ 0 .. MAX_PATH - 1 ] of Char;
begin
  // Windows.GetTempPath( Sizeof( lpTempPathName ), lpTempPathName );
  // nBufferLength = 0x0208 = 520 = MAX_PATH * Sizeof(Char) : 520 * 2 = 1040 Bytes
  // Sizeof : Returns the number of bytes occupied by a variable or type.
  // nBufferLength : The size of the string buffer identified by lpBuffer, in TCHARs

  Winapi.Windows.GetTempPath( MAX_PATH, lpTempPathName );
  // nBufferLength = 260 = MAX_PATH : 260 * 2 = 520 Bytes
  // nBufferLength : The size of the string buffer identified by lpBuffer, in TCHARs

  repeat
    CreateGUID( Guid );
    GuidString := GUIDToString( Guid );
    GuidString := Copy( GuidString, 2, Length( GuidString ) - 2 );
    lpTempFileName := lpTempPathName + GuidString + Extension;
  until not FileExists( lpTempFileName );

  Result := lpTempFileName;
end;

procedure CopyFile( SourceFileName, DestFileName: string );
var
  MemoryStream: TMemoryStream;
begin
  MemoryStream := TMemoryStream.Create;
  try
    MemoryStream.LoadFromFile( SourceFileName );
    MemoryStream.SaveToFile( DestFileName );
  finally
    MemoryStream.Free;
  end;
end;

function RemoveWhiteSpace( const s: String ): String;
var
  i, j: Integer;
  HasSpace: Boolean;
begin
  SetLength( Result, Length( s ) );
  HasSpace := False;
  j := 0;
  for i := 1 to Length( s ) do
  begin
    if Ord( s[ i ] ) = $20 then
    begin
      if HasSpace then
        Continue;
      HasSpace := True;
      Inc( j );
      Result[ j ] := s[ i ];
    end else if ( Ord( s[ i ] ) > $20 ) and ( Ord( s[ i ] ) < $7F ) then
    begin
      HasSpace := False;
      Inc( j );
      Result[ j ] := s[ i ];
    end;
  end;

  SetLength( Result, j );
end;

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
  saSecurity.bInheritHandle := True;
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
      if CreateProcess( nil, PChar( CommandAndParameters ), @saSecurity, @saSecurity, True, NORMAL_PRIORITY_CLASS, nil,
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
    bInheritHandle := True;
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

      if not CreateProcess( nil, PChar( ACmdLine ), @vSecurityAttributes, @vSecurityAttributes, True,
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
