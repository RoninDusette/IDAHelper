unit uMisc;

interface

uses
  Windows, SysUtils, System.Classes;

function GenerateTempFileName( const Extension: String ): String;
function GenerateTempFileNameEx( const Extension: String ): String;
procedure CopyFile( SourceFileName, DestFileName: string );

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
  Windows.GetTempPath( MAX_PATH, lpTempPathName );

  // If uUnique is zero, GetTempFileName creates an empty file and closes it.
  //
  // If uUnique is not zero, you must create the file yourself.
  // Only a file name is created, because GetTempFileName is not able to guarantee that the file name is unique.
  repeat
    Windows.GetTempFileName( lpTempPathName, nil, 1, lpTempFileName );
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

  Windows.GetTempPath( MAX_PATH, lpTempPathName );
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

end.
