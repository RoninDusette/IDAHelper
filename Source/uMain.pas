unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, VirtualTrees, Vcl.ComCtrls, Vcl.ToolWin, Vcl.ExtCtrls, System.ImageList, Vcl.ImgList, Vcl.Menus;

type
  TMainForm = class( TForm )
    DlgOpen: TOpenDialog;
    StatusBar1: TStatusBar;
    Pages: TPageControl;
    TabSheet1: TTabSheet;
    MemoSVDLog: TMemo;
    Splitter1: TSplitter;
    MemoSVDFile: TMemo;
    TabSheet2: TTabSheet;
    ImageList1: TImageList;
    Panel3: TPanel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    Exit1: TMenuItem;
    ToolBarSVD: TToolBar;
    btnLoadSVD: TToolButton;
    btnCheckSVD: TToolButton;
    ToolButton3: TToolButton;
    btnGotoPrev: TToolButton;
    btnGotoNext: TToolButton;
    btnGotoLast: TToolButton;
    btnSaveSVD: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    Panel1: TPanel;
    Panel2: TPanel;
    ToolBar1: TToolBar;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton20: TToolButton;
    Panel4: TPanel;
    VirtualStringTree3: TVirtualStringTree;
    Splitter3: TSplitter;
    VirtualStringTree2: TVirtualStringTree;
    Splitter2: TSplitter;
    VirtualStringTree1: TVirtualStringTree;
    Splitter4: TSplitter;
    Memo1: TMemo;
    ToolButton21: TToolButton;
    btnGotoFirst: TToolButton;
    procedure FormCreate( Sender: TObject );
    procedure btnLoadSVDClick( Sender: TObject );
    procedure Exit1Click( Sender: TObject );
    procedure About1Click( Sender: TObject );
    procedure btnCheckSVDClick( Sender: TObject );
    procedure StatusBar1DrawPanel( StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect );
    procedure btnGotoFirstClick( Sender: TObject );
    procedure btnGotoPrevClick( Sender: TObject );
    procedure btnGotoNextClick( Sender: TObject );
    procedure btnGotoLastClick( Sender: TObject );
  private
    procedure CreateObjects;
    procedure ApplicationIdle( Sender: TObject; var Done: Boolean );
    procedure UpdataSVDFileCheckResult;
    procedure GotoLineNumberOfSVDFile;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses uAbout, uExecAndCapture, uMisc;

type
  TSVDErrorWarningLine = record
    SVDFileLine: Integer;
    SVDLogLine: Integer;
  end;

const
  MainFormCaption = 'IDA Helper -- ';
  SVDConvExeName = 'SVDConv.exe';
  SVDStatusBarPanel0Caption = 'ERROR : ';
  SVDStatusBarPanel1Caption = 'WARNING : ';
  SVDStatusBarPanel2Caption = 'INFO : ';

  SVDNoFileName = 'Please load a *.svd file.';

  SVDFileErrorStr = '*** ERROR';
  SVDFileWarningStr = '*** WARNING';
  SVDFileInfoStr = '*** INFO';

var
  SVDTempFileNameOnly: String; // No Path, No Ext : 6C79A68C-1945-48BB-A20B-4FACA9A46231

  SVDTempFileName: String;
  SVDFileName: String;
  SVDInfoCount: Integer;
  SVDWarningCount: Integer;
  SVDErrorCount: Integer;

  SVDErrorWarningLineArrayIndex: Integer;
  SVDErrorWarningLineArrayCount: Integer;
  SVDErrorWarningLineArray: array of TSVDErrorWarningLine;
  // First, Last, Prev, Next : LineNumber := SVDErrorWarningLineArray[SVDErrorWarningLineArrayIndex]

  { TForm1 }
procedure TMainForm.Exit1Click( Sender: TObject );
begin
  Close;
end;

procedure TMainForm.FormCreate( Sender: TObject );
begin
  ReportMemoryLeaksOnShutdown := True;
  SVDFileName := '';
  SVDInfoCount := 0;
  SVDWarningCount := 0;
  SVDErrorCount := 0;
  Application.OnIdle := ApplicationIdle;
  Application.Title := 'IDA Helper';
end;

procedure TMainForm.GotoLineNumberOfSVDFile;
var
  FirstVisibleLine: Integer;
  LineNumber: Integer;
  Point: TPoint;
begin
  LineNumber := SVDErrorWarningLineArray[ SVDErrorWarningLineArrayIndex ].SVDLogLine;

  // we define number of the first visible line
  FirstVisibleLine := SendMessage( MemoSVDLog.Handle, EM_GETFIRSTVISIBLELINE, 0, 0 );

  // we install a line with the cursor as the first visible
  SendMessage( MemoSVDLog.Handle, EM_LINESCROLL, 0, LineNumber - FirstVisibleLine );

  LineNumber := SVDErrorWarningLineArray[ SVDErrorWarningLineArrayIndex ].SVDFileLine;
  MemoSVDFile.SetFocus;
  MemoSVDFile.SelStart := MemoSVDFile.Perform( EM_LINEINDEX, LineNumber, 0 );
  MemoSVDFile.SelLength := 0;
  MemoSVDFile.Perform( EM_SCROLLCARET, 0, 0 );
end;

procedure TMainForm.StatusBar1DrawPanel( StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect );
var
  RectForText: TRect;
begin
  RectForText := Rect;
  with StatusBar.Canvas do
  begin
    Font.Name := 'FixedSys';
    Brush.Color := clBtnFace;

    if Panel.Index = 0 then // Error
    begin
      Font.Color := clRed;
    end
    else if Panel.Index = 1 then // Warning
    begin
      Font.Color := clBlue;
    end
    else if Panel.Index = 2 then // Info
    begin
      Font.Color := clBlack;
    end
    else
    begin
      Font.Color := clBlack;
    end;

    FillRect( RectForText );
    TextOut( RectForText.Left + 5, RectForText.Top + 2, Panel.Text );
  end;

  // DrawText( StatusBar.Canvas.Handle, PChar( Panel.Text ), -1, RectForText, DT_SINGLELINE or DT_VCENTER or DT_LEFT );
end;

// *** INFO M211: A:\CY8C5888LP.svd (Line 9)
// Ignoring Peripheral : 'FLASH_DATA' (see previous message)
//
// *** WARNING M352: A:\CY8C5888LP.svd (Line 57471)
// AddressBlock of Peripheral 'USB_ARB_RW7' (@0x400060e4) [0x400060e8 ... 0x400060e4] overlaps ...
//
// *** ERROR M343: A:\CY8C5888LP.svd (Line 71525)
// Peripheral 'UWRK_UWRK16_DEF_B0' (@0x40006800) has same address as 'UWRK_UWRK16_CAT_B0' (Line 65333)
//
// *** WARNING M223: A:\Temp\6C79A68C-1945-48BB-A20B-4FACA9A46231.svd (Line 36)
// Input File Name '6C79A68C-1945-48BB-A20B-4FACA9A46231' does not match the tag <name> in the <device> section:
//
procedure TMainForm.UpdataSVDFileCheckResult;
var
  I: Integer;
  LineNumber: Integer;
  LineArryIndex: Integer;
  LineArryCapacity: Integer;

  // (Line 71525)
  function GetLineNumber( LineIndex: Cardinal ): Integer;
  var
    StrStartPos, StrEndPos: Integer;
    LineNumberStr: string;
  begin
    Result := 0;
    StrStartPos := Pos( '(Line ', MemoSVDLog.Lines[ LineIndex ] );
    if StrStartPos > 0 then
    begin
      StrEndPos := Pos( ')', MemoSVDLog.Lines[ LineIndex ], StrStartPos );
      if StrEndPos > 0 then
      begin
        LineNumberStr := Copy( MemoSVDLog.Lines[ LineIndex ], StrStartPos + 6, StrEndPos - StrStartPos - 6 );
        Result := StrToInt( LineNumberStr );
      end;
    end;
  end;

begin
  SVDErrorCount := 0;
  SVDWarningCount := 0;
  SVDInfoCount := 0;

  LineArryIndex := 0;
  LineArryCapacity := 256;

  SVDErrorWarningLineArrayCount := 0;
  SVDErrorWarningLineArrayIndex := 0;
  SetLength( SVDErrorWarningLineArray, LineArryCapacity );

  for I := 0 to MemoSVDLog.Lines.Count - 1 do
  begin
    if Pos( SVDFileErrorStr, MemoSVDLog.Lines[ I ] ) > 0 then
    begin
      SVDErrorCount := SVDErrorCount + 1;
      SVDErrorWarningLineArray[ LineArryIndex ].SVDLogLine := I;
      SVDErrorWarningLineArray[ LineArryIndex ].SVDFileLine := GetLineNumber( I );
      Inc( SVDErrorWarningLineArrayCount );
      Inc( LineArryIndex );
      if LineArryIndex = LineArryCapacity then
      begin
        LineArryCapacity := LineArryCapacity + LineArryCapacity div 2;
        SetLength( SVDErrorWarningLineArray, LineArryCapacity );
      end;
    end;

    if Pos( SVDFileWarningStr, MemoSVDLog.Lines[ I ] ) > 0 then
    begin
      // *** WARNING M223: A:\Temp\6C79A68C-1945-48BB-A20B-4FACA9A46231.svd (Line 36)
      // Input File Name '6C79A68C-1945-48BB-A20B-4FACA9A46231'
      if Pos( SVDTempFileNameOnly, MemoSVDLog.Lines[ I + 1 ] ) > 0 then
        Continue;

      SVDWarningCount := SVDWarningCount + 1;
      SVDErrorWarningLineArray[ LineArryIndex ].SVDLogLine := I;
      SVDErrorWarningLineArray[ LineArryIndex ].SVDFileLine := GetLineNumber( I );
      Inc( SVDErrorWarningLineArrayCount );
      Inc( LineArryIndex );
      if LineArryIndex = LineArryCapacity then
      begin
        LineArryCapacity := LineArryCapacity + LineArryCapacity div 2;
        SetLength( SVDErrorWarningLineArray, LineArryCapacity );
      end;
    end;

    if Pos( SVDFileInfoStr, MemoSVDLog.Lines[ I ] ) > 0 then
      SVDInfoCount := SVDInfoCount + 1;
  end;

end;

procedure TMainForm.btnLoadSVDClick( Sender: TObject );
var
  MemoryStream: TMemoryStream;
begin
  if not DlgOpen.Execute then
    Exit;

  SVDFileName := DlgOpen.FileName;
  SVDTempFileName := GenerateTempFileNameEx( '.svd' );
  // 6C79A68C-1945-48BB-A20B-4FACA9A46231.svd
  SVDTempFileNameOnly := ExtractFileName( SVDTempFileName );

  // 6C79A68C-1945-48BB-A20B-4FACA9A46231
  SVDTempFileNameOnly := Copy( SVDTempFileNameOnly, 1, Length( SVDTempFileNameOnly ) - 4 );

  CopyFile( SVDFileName, SVDTempFileName );

  MemoSVDFile.Lines.LoadFromFile( SVDTempFileName );

  Self.Caption := MainFormCaption + SVDFileName;
  StatusBar1.Panels[ 3 ].Text := SVDNoFileName;
  StatusBar1.Invalidate;
end;

{
  CMSIS-SVD SVD Consistency Checker / Header File Generator V3.2.40
  Copyright (C) 2010 - 2016 ARM Ltd and ARM Germany GmbH. All rights reserved.

  Usage:
  SVDConv.exe <SVD file> [Options]

  Options:
  -o <output path>           Output Path, default current directory
  -b <log file>              Log file for Messages
  --generate=header          Generate CMSIS header file
  --fields=struct            Generate struct/union for bitfields
  --fields=macro             Generate macros for bitfields
  --fields=struct-ansic      Generate structs for bitfields (ANSI C)
  --fields=enum              Generate enumerated values for bitfields
  --debug-headerfile         Add Addresses to Registers comments
  --generate=partition       Generate CMSIS partition file
  -x [Mxxx, INFO, WARNING]   Suppress Message, Warnings or Infos
  -h, -?                     Show this help

  Examples:
  Check only>        SVDConv.exe myDevice.svd
  Header Generation> SVDConv.exe myDevice.svd --generate=header
}

procedure TMainForm.ApplicationIdle( Sender: TObject; var Done: Boolean );
begin
  if Pages.ActivePageIndex = 0 then
  begin
    StatusBar1.Panels[ 0 ].Text := SVDStatusBarPanel0Caption + IntToStr( SVDErrorCount );
    StatusBar1.Panels[ 1 ].Text := SVDStatusBarPanel1Caption + IntToStr( SVDWarningCount );
    StatusBar1.Panels[ 2 ].Text := SVDStatusBarPanel2Caption + IntToStr( SVDInfoCount );
    if ( SVDFileName <> '' ) then
      StatusBar1.Panels[ 3 ].Text := SVDFileName
    else
      StatusBar1.Panels[ 3 ].Text := SVDNoFileName;
  end
  else
  begin
    StatusBar1.Panels[ 0 ].Text := '';
    StatusBar1.Panels[ 1 ].Text := '';
    StatusBar1.Panels[ 2 ].Text := '';
    StatusBar1.Panels[ 3 ].Text := '';
  end;
end;

procedure TMainForm.btnCheckSVDClick( Sender: TObject );
var
  Output: AnsiString;
begin
  if SVDTempFileName = '' then
    Exit;

  MemoSVDLog.Lines.Clear;

  if True then
  begin
    ExecAndCaptureReal( SVDConvExeName, SVDTempFileName,
      procedure( const Line: PAnsiChar )
      begin
        MemoSVDLog.Lines.Add( String( Line ) );
      end );
  end
  else
  begin
    ExecAndCapture( SVDConvExeName, SVDTempFileName, Output );
    MemoSVDLog.Lines.Text := String( Output );
  end;

  SendMessage( MemoSVDLog.Handle, WM_VSCROLL, SB_LINEDOWN, 0 );

  UpdataSVDFileCheckResult( );
end;

procedure TMainForm.btnGotoFirstClick( Sender: TObject );
begin
  SVDErrorWarningLineArrayIndex := 0;
  GotoLineNumberOfSVDFile;
end;

procedure TMainForm.btnGotoLastClick( Sender: TObject );
begin
  SVDErrorWarningLineArrayIndex := SVDErrorWarningLineArrayCount - 1;
  GotoLineNumberOfSVDFile;
end;

procedure TMainForm.btnGotoNextClick( Sender: TObject );
begin
  Inc( SVDErrorWarningLineArrayIndex );
  if SVDErrorWarningLineArrayIndex = SVDErrorWarningLineArrayCount then
    SVDErrorWarningLineArrayIndex := SVDErrorWarningLineArrayCount - 1;
  GotoLineNumberOfSVDFile;
end;

procedure TMainForm.btnGotoPrevClick( Sender: TObject );
begin
  Dec( SVDErrorWarningLineArrayIndex );
  if SVDErrorWarningLineArrayIndex < 0 then
    SVDErrorWarningLineArrayIndex := 0;
  GotoLineNumberOfSVDFile;
end;

procedure TMainForm.About1Click( Sender: TObject );
begin
  AboutBox.ShowModal;
end;

procedure TMainForm.CreateObjects;
var
  Strings: TStringList;
  MemoryStream: TMemoryStream;
begin
  Strings := TStringList.Create;
  try

  finally
    Strings.Free;
  end;

  MemoryStream := TMemoryStream.Create;
  try

  finally
    MemoryStream.Free;
  end;

end;

end.
