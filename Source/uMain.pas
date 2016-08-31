unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.ShellAPI, // Windows
  System.Classes, System.Variants, System.SysUtils, System.ImageList, // System
  Vcl.Forms, Vcl.Menus, Vcl.ImgList, Vcl.Graphics, Vcl.Dialogs, Vcl.ToolWin, // VCL
  Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, // VCL Controls
  Generics.Collections, Generics.Defaults,
  NativeXml, sdDebug, VirtualTrees, uSVD_Type, uIDC_Script;

type
  TMainForm = class( TForm )
    DlgOpen: TOpenDialog;
    StatusBar: TStatusBar;
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
    ToolButton3: TToolButton;
    btnGotoPrev: TToolButton;
    btnGotoNext: TToolButton;
    btnGotoLast: TToolButton;
    btnReload: TToolButton;
    ToolButton9: TToolButton;
    Panel1: TPanel;
    Panel2: TPanel;
    ToolBar1: TToolBar;
    btnDecodeSVD: TToolButton;
    btnMakeScript: TToolButton;
    btnSaveScript: TToolButton;
    Panel4: TPanel;
    VST_Main: TVirtualStringTree;
    Splitter4: TSplitter;
    MemoIDC: TMemo;
    ToolButton21: TToolButton;
    btnGotoFirst: TToolButton;
    btnExpandTree: TToolButton;
    btnCollapseTree: TToolButton;
    ToolButton1: TToolButton;
    btnMakeHeader: TToolButton;
    Panel5: TPanel;
    MemoSVDHeader: TMemo;
    Splitter5: TSplitter;
    btnSaveHeader: TToolButton;
    Panel6: TPanel;
    Panel7: TPanel;
    RadioGroup1: TRadioGroup;
    DlgSave: TSaveDialog;
    procedure FormCreate( Sender: TObject );
    procedure btnLoadSVDClick( Sender: TObject );
    procedure Exit1Click( Sender: TObject );
    procedure About1Click( Sender: TObject );
    procedure StatusBarDrawPanel( StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect );
    procedure btnGotoFirstClick( Sender: TObject );
    procedure btnGotoPrevClick( Sender: TObject );
    procedure btnGotoNextClick( Sender: TObject );
    procedure btnGotoLastClick( Sender: TObject );
    procedure btnDecodeSVDClick( Sender: TObject );
    procedure VST_MainGetText( Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: String );
    procedure VST_MainGetNodeDataSize( Sender: TBaseVirtualTree; var NodeDataSize: Integer );
    procedure FormDestroy( Sender: TObject );
    procedure VST_MainFreeNode( Sender: TBaseVirtualTree; Node: PVirtualNode );
    procedure btnMakeScriptClick( Sender: TObject );
    procedure btnSaveScriptClick( Sender: TObject );
    procedure btnExpandTreeClick( Sender: TObject );
    procedure btnCollapseTreeClick( Sender: TObject );
    procedure btnReloadClick( Sender: TObject );
    procedure btnMakeHeaderClick( Sender: TObject );
    procedure btnSaveHeaderClick( Sender: TObject );
    procedure RadioGroup1Click( Sender: TObject );
  private
    ProgressBarPosition: Integer;
    ProgressBar: TProgressBar;
    procedure ProgressBarVisible( Visible: Boolean );
    //
    // Callbacks
    //
    procedure ApplicationIdle( Sender: TObject; var Done: Boolean );
    procedure XmlNodeNew( Sender: TObject; ANode: TXmlNode );
    procedure XmlNodeLoaded( Sender: TObject; ANode: TXmlNode );
    procedure XmlProgress( Sender: TObject; Position: int64 );
    procedure XmlDebugOut( Sender: TObject; WarnStyle: TsdWarnStyle; const AMessage: Utf8String );

    //
    // Check SVDFile Functions
    //
    procedure UpdataSVDFileCheckResult;
    procedure GotoLineNumberOfSVDFile;
    //
    // Decode Peripherals and Interrupts from VST_Main
    //
    procedure CheckSVDFile;

    //
    // Decode SVD
    //
    procedure DecodeCPU( Node: PVirtualNode; cpu: PSVD_CPU );
    procedure DecodeAddressBlock( Node: PVirtualNode; addressBlock: PSVD_AddressBlock );
    procedure DecodeInterrupt( Node: PVirtualNode; interrupt: PSVD_Interrupt );
    //
    // From Top to Bottom
    //
    procedure ClearDevice;

    procedure DecodeDevice;

    function DecodePeripheral( Node: PVirtualNode; AObject: PSVD_Object ): Boolean;
    //

    // 1.3: nesting of cluster is supported
    function DecodeCluster( Node: PVirtualNode; AObject: PSVD_Object ): Boolean;
    function DecodeRegister( Node: PVirtualNode; AObject: PSVD_Object ): Boolean;
    //
    function DecodeField( Node: PVirtualNode; AObject: PSVD_Object ): Boolean;

    function DecodeEnumeratedValues( Node: PVirtualNode; AObject: PSVD_Object ): Boolean;
    procedure DecodeEnumeratedValue( Node: PVirtualNode; eumeratedValue: PSVD_EnumeratedValue );

    function FindDerivedObject( FullName: String; ObjectType: TSVD_ObjectType ): PSVD_Object;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}
{ $DEFINE XML_DEBUG }

uses uAbout, uMisc, uDynArray;

type
  PSVDErrorWarningLine = ^TSVDErrorWarningLine;

  TSVDErrorWarningLine = record
    SVDFileLine: Integer;
    SVDLogLine: Integer;
  end;

type
  TXmlNodeInfo = record
    NodeType: TsdElementType;
    TypeName: String;
    Name: Utf8String;
    value: Utf8String;
    Depth: Integer;
  end;

type
  // -----------------------------------------------------------------------------------------------
  // Virtual String Tree Struct
  // -----------------------------------------------------------------------------------------------
  PVST_MainData = ^TVST_MainData;

  // Free name onFreeNode()
  TVST_MainData = record
    Name: String;
    value: String;
  end;

const
  ST3ExeName = 'sublime_text.exe';

  MainFormCaption = 'IDA Helper -- ';
  SVDConvExeName = 'SVDConv.exe';
  SVDStatusBarPanel0Caption = 'ERROR : ';
  SVDStatusBarPanel1Caption = 'WARNING : ';
  SVDStatusBarPanel2Caption = 'INFO : ';

  SVDNoFileName = 'Please load a *.svd file.';

  SVDFileErrorStr = '*** ERROR';
  SVDFileWarningStr = '*** WARNING';
  SVDFileInfoStr = '*** INFO';

  XmlNodeTypeName: array [ xeElement .. xeError ] of String = //
    ( 'xeElement', 'xeAttribute', 'xeComment', 'xeCData', 'xeCondSection', 'xeDeclaration', 'xeStylesheet', 'xeDocType',
    'xeDtdItem', 'xeDtdAttList', 'xeDtdEntity', 'xeDtdNotation', 'xeInstruction', 'xeCharData', 'xeWhiteSpace',
    'xeQuotedText', 'xeUnknown', 'xeEndTag', 'xeError' );

var
  SVDFileName: String;
  SVDWarningCount: Integer;
  SVDErrorCount: Integer;
  SVDInfoCount: Integer;

  SVDErrorWarningLineArrayIndex: Integer;
  SVDErrorWarningLineArray: TDynArray< TSVDErrorWarningLine >;
  // First, Last, Prev, Next :
  // LineNumber := SVDErrorWarningLineArray[SVDErrorWarningLineArrayIndex]

  XmlDoc: TNativeXml;

  ActiveNode: PVirtualNode;
  SavedNode: PVirtualNode;

  SVD_Decoded: Boolean;

  SVD_Device: TSVD_Device;

procedure Foo;
begin

end;

{ TForm1 }
procedure TMainForm.Exit1Click( Sender: TObject );
begin
  Close;
end;

procedure TMainForm.FormCreate( Sender: TObject );
var
  ProgressBarStyle: Integer;
begin
  ReportMemoryLeaksOnShutdown := True;

  ClearDevice( );
  // FillChar( SVD_Device, 0, Sizeof( SVD_Device ) );

  SVDFileName := '';
  Application.OnIdle := ApplicationIdle;
  Application.Title := 'IDA Helper';
  ActiveNode := nil;

  { create a run progress bar in the status bar }
  ProgressBar := TProgressBar.Create( StatusBar ); // Owner
  ProgressBar.Parent := StatusBar; // Parent
  ProgressBar.Visible := False;

  { remove progress bar border }
  ProgressBarStyle := GetWindowLong( ProgressBar.Handle, GWL_EXSTYLE );
  ProgressBarStyle := ProgressBarStyle - WS_EX_STATICEDGE;
  SetWindowLong( ProgressBar.Handle, GWL_EXSTYLE, ProgressBarStyle );
end;

procedure TMainForm.FormDestroy( Sender: TObject );
begin
  ClearDevice( );

  if Assigned( ProgressBar ) then
    ProgressBar.free;

  if Assigned( XmlDoc ) then
    XmlDoc.free;
end;

procedure TMainForm.GotoLineNumberOfSVDFile;
var
  FirstVisibleLine: Integer;
  LineNumber: Integer;
begin

  if SVDErrorWarningLineArray.Count = 0 then
    Exit;

  LineNumber := SVDErrorWarningLineArray.Items[ SVDErrorWarningLineArrayIndex ].SVDLogLine;

  // we define number of the first visible line
  FirstVisibleLine := SendMessage( MemoSVDLog.Handle, EM_GETFIRSTVISIBLELINE, 0, 0 );

  // we install a line with the cursor as the first visible
  SendMessage( MemoSVDLog.Handle, EM_LINESCROLL, 0, LineNumber - FirstVisibleLine );

  LineNumber := SVDErrorWarningLineArray.Items[ SVDErrorWarningLineArrayIndex ].SVDFileLine;
  MemoSVDFile.SetFocus;
  MemoSVDFile.SelStart := MemoSVDFile.Perform( EM_LINEINDEX, LineNumber, 0 );
  MemoSVDFile.SelLength := 0;
  MemoSVDFile.Perform( EM_SCROLLCARET, 0, 0 );

  ShellExecute( Handle, nil, ST3ExeName, pChar( SVDFileName + ':' + IntToStr( LineNumber ) ), nil, SW_SHOWNORMAL );
end;

procedure TMainForm.ProgressBarVisible( Visible: Boolean );
begin
  ProgressBarPosition := 0;
  ProgressBar.Visible := Visible;
  ProgressBar.Position := 0;

  // Update ProgressBar
  if Visible then
    StatusBar.Refresh;
end;

procedure TMainForm.RadioGroup1Click( Sender: TObject );
begin
  btnMakeHeaderClick( Self );
end;

procedure TMainForm.StatusBarDrawPanel( StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect );
var
  RectForText: TRect;
begin
  RectForText := Rect;
  if Pages.ActivePageIndex = 0 then
  begin
    with StatusBar.Canvas do
    begin
      Font.Name := 'FixedSys';
      Brush.Color := clBtnFace;

      if Panel.Index = 0 then // Error
      begin
        Font.Color := clRed;
      end else if Panel.Index = 1 then // Warning
      begin
        Font.Color := clBlue;
      end else if Panel.Index = 2 then // Info
      begin
        Font.Color := clBlack;
      end
      else // Panel.Index = 3
      begin
        Font.Color := clBlack;
      end;

      FillRect( RectForText );
      TextOut( RectForText.Left + 5, RectForText.Top + 2, Panel.Text );
      // DrawText( StatusBar.Canvas.Decode, PChar( Panel.Text ), -1,
      // RectForText, DT_SINGLELINE or DT_VCENTER or DT_LEFT );
    end;
  end else if Pages.ActivePageIndex = 1 then
  begin
    with StatusBar.Canvas do
    begin
      Font.Name := 'FixedSys';
      Font.Color := clBlack;
      Brush.Color := clBtnFace;

      if ProgressBar.Visible and ( Panel.Index = StatusBar.Panels.Count - 1 ) then
      begin
        with ProgressBar do
        begin
          Top := Rect.Top + 2;
          Left := Rect.Left;
          Width := Rect.Right - Rect.Left - 10;
          Height := Rect.Bottom - Rect.Top - 4;
        end;
      end else begin
        FillRect( RectForText );
        TextOut( RectForText.Left + 5, RectForText.Top + 2, Panel.Text );
        // DrawText( StatusBar.Canvas.Decode, PChar( Panel.Text ), -1,
        // RectForText, DT_SINGLELINE or DT_VCENTER or DT_LEFT );
        Exit;
      end;
    end;
  end;
end;

procedure TMainForm.btnExpandTreeClick( Sender: TObject );
begin
  VST_Main.FullExpand( VST_Main.GetFirstChild( nil ) );
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
// Input File name '6C79A68C-1945-48BB-A20B-4FACA9A46231' does not match the tag <name> in the <device> section:
//
procedure TMainForm.UpdataSVDFileCheckResult;

// (Line 71525)
  function GetLineNumber( LineIndex: Cardinal ): Integer;
  var
    StrStartPos, StrEndPos: Integer;
    LineNumberStr: String;
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

var
  I: Integer;
  SVDErrorWarningLine: PSVDErrorWarningLine;
begin
  SVDInfoCount := 0;
  SVDErrorCount := 0;
  SVDWarningCount := 0;

  SVDErrorWarningLineArray.Init( 4 );

  for I := 0 to MemoSVDLog.Lines.Count - 1 do
  begin
    if Pos( SVDFileErrorStr, MemoSVDLog.Lines[ I ] ) > 0 then
    begin
      Inc( SVDErrorCount );
      SVDErrorWarningLine := SVDErrorWarningLineArray.NewItem( );
      SVDErrorWarningLine.SVDLogLine := I;
      SVDErrorWarningLine.SVDFileLine := GetLineNumber( I );
    end;

    if Pos( SVDFileWarningStr, MemoSVDLog.Lines[ I ] ) > 0 then
    begin
      Inc( SVDWarningCount );
      SVDErrorWarningLine := SVDErrorWarningLineArray.NewItem( );
      SVDErrorWarningLine.SVDLogLine := I;
      SVDErrorWarningLine.SVDFileLine := GetLineNumber( I );
    end;

    if Pos( SVDFileInfoStr, MemoSVDLog.Lines[ I ] ) > 0 then
      SVDInfoCount := SVDInfoCount + 1;
  end;

  SVDErrorWarningLineArray.Trim( );
end;

procedure TMainForm.VST_MainFreeNode( Sender: TBaseVirtualTree; Node: PVirtualNode );
var
  MainData: PVST_MainData;
begin
  MainData := VST_Main.GetNodeData( Node );
  MainData.Name := '';
  MainData.value := '';
end;

procedure TMainForm.VST_MainGetNodeDataSize( Sender: TBaseVirtualTree; var NodeDataSize: Integer );
begin
  NodeDataSize := Sizeof( TVST_MainData );
end;

procedure TMainForm.VST_MainGetText( Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: String );
var
  DataAll: PVST_MainData;
begin
  DataAll := VST_Main.GetNodeData( Node );
  if Column = 0 then
    CellText := DataAll.Name
  else
    CellText := DataAll.value;
end;

function Indent( ACount: Integer ): String;
begin
  while ACount > 0 do
  begin
    Result := Result + '  ';
    dec( ACount );
  end;
end;

procedure TMainForm.XmlProgress( Sender: TObject; Position: int64 );
begin
  ProgressBarPosition := Position;
  ProgressBar.Position := ProgressBarPosition;
end;

procedure TMainForm.XmlNodeNew( Sender: TObject; ANode: TXmlNode );
var
  MainData: PVST_MainData;
  iNode: TXmlNodeInfo;
begin
  SavedNode := ActiveNode;

  iNode.NodeType := ANode.ElementType;
  iNode.TypeName := XmlNodeTypeName[ iNode.NodeType ];
  iNode.Name := ANode.Name;
  iNode.value := ANode.value;
  iNode.Depth := ANode.TreeDepth;

{$IFDEF XML_DEBUG}
  MemoIDC.Lines.Add( Format( '%sOnNewNode Depth=%d, Type=%s, name=%s, value=%s', //
    [ Indent( iNode.Depth ), iNode.Depth, iNode.TypeName, iNode.Name, iNode.value ] ) );
{$ENDIF}
  //
  if ( ActiveNode = nil ) and ( ANode.ElementType = xeElement ) then
  begin
    ActiveNode := VST_Main.AddChild( nil );
    MainData := VST_Main.GetNodeData( ActiveNode );
    MainData.Name := iNode.Name;
    MainData.value := '';
    Exit;
  end;

  if ActiveNode = nil then
    Exit;

  if ANode.ElementType = xeElement then
  begin
    ActiveNode := VST_Main.AddChild( ActiveNode );
    MainData := VST_Main.GetNodeData( ActiveNode );
    MainData.Name := iNode.Name;
    MainData.value := 'Expand to see more information';
  end else if ANode.ElementType = xeAttribute then
  begin
    ActiveNode := VST_Main.AddChild( ActiveNode );
    MainData := VST_Main.GetNodeData( ActiveNode );
    MainData.Name := 'Attribute name to be set';
    MainData.value := 'Attribute value To be set';
  end else if ANode.ElementType = xeCharData then
  begin
    if False then
    begin
      ActiveNode := VST_Main.AddChild( ActiveNode );
      MainData := VST_Main.GetNodeData( ActiveNode );
      MainData.Name := iNode.Name;
      MainData.value := iNode.value;
    end;
  end;

{$IFDEF XML_DEBUG}
  MemoIDC.Lines.Add( Format( '<<<<OnNewNode OldNode 0x%.8x, NewNode 0x%.8x', //
    [ DWORD( SavedMainNode ), DWORD( MainNode ) ] ) );
  MemoIDC.Lines.Add( '' );
{$ENDIF}
end;

procedure TMainForm.XmlDebugOut( Sender: TObject; WarnStyle: TsdWarnStyle; const AMessage: Utf8String );
var
  WarnStlyeStr: Utf8String;
begin
  case WarnStyle of
    wsInfo:
      WarnStlyeStr := 'INFO: ';
    wsHint:
      WarnStlyeStr := 'HINT: ';
    wsWarn:
      WarnStlyeStr := 'WARN: ';
    wsFail:
      WarnStlyeStr := 'FAIL: ';
  end;

  MemoIDC.Lines.Add( WarnStlyeStr + AMessage );
end;

procedure TMainForm.XmlNodeLoaded( Sender: TObject; ANode: TXmlNode );
var
  MainData: PVST_MainData;
  iNode: TXmlNodeInfo;
begin
  SavedNode := ActiveNode;

  iNode.NodeType := ANode.ElementType;
  iNode.TypeName := XmlNodeTypeName[ iNode.NodeType ];
  iNode.Name := ANode.Name;
  iNode.value := ANode.value;
  iNode.Depth := ANode.TreeDepth;

{$IFDEF XML_DEBUG}
  MemoIDC.Lines.Add( Format( '%sOnLoaded Depth=%d, Type=%s, name=%s, value=%s', //
    [ Indent( iNode.Depth ), iNode.Depth, iNode.TypeName, iNode.Name, iNode.value ] ) );
{$ENDIF}
  //
  if ActiveNode = nil then
    Exit;

  if ANode.ElementType = xeElement then
  begin
    if iNode.Depth <> 0 then
    begin
      MainData := VST_Main.GetNodeData( ActiveNode );
      MainData.Name := iNode.Name;

      if iNode.Name = 'description' then
        MainData.value := RemoveWhiteSpace( iNode.value )
      else
        MainData.value := iNode.value;

      ActiveNode := ActiveNode.Parent;
    end;
  end else if ( ANode.ElementType = xeAttribute ) then
  begin
    MainData := VST_Main.GetNodeData( ActiveNode );
    MainData.Name := iNode.Name;
    MainData.value := iNode.value;
    ActiveNode := ActiveNode.Parent;
  end else if ANode.ElementType = xeCharData then
  begin
    if False then
    begin
      MainData := VST_Main.GetNodeData( ActiveNode );
      MainData.Name := iNode.Name;
      MainData.value := iNode.value;
      ActiveNode := ActiveNode.Parent;
    end;
  end;

{$IFDEF XML_DEBUG}
  MemoIDC.Lines.Add( Format( '<<<<OnNewNode OldNode 0x%.8x, NewNode 0x%.8x', //
    [ DWORD( SavedMainNode ), DWORD( MainNode ) ] ) );
  MemoIDC.Lines.Add( '' );
{$ENDIF}
end;

procedure TMainForm.btnLoadSVDClick( Sender: TObject );
begin
  if not DlgOpen.Execute then
    Exit;

  SVDFileName := DlgOpen.FileName;

  Self.Caption := MainFormCaption + SVDFileName;
  StatusBar.Panels[ 3 ].Text := SVDFileName;
  StatusBar.Invalidate;

  CheckSVDFile( );
end;

procedure TMainForm.btnReloadClick( Sender: TObject );
begin
  if SVDFileName = '' then
    btnLoadSVDClick( Self )
  else
    CheckSVDFile( );
end;

procedure TMainForm.CheckSVDFile;
var
  Output: AnsiString;
begin
  MemoSVDFile.Lines.LoadFromFile( SVDFileName );
  MemoSVDLog.Lines.Clear;

  Output := '';
  if True then
  begin
    ExecAndCaptureReal( SVDConvExeName, SVDFileName,
      procedure( const Line: PAnsiChar )
      begin
        MemoSVDLog.Lines.Add( String( Line ) );
      end );
  end else begin
    ExecAndCapture( SVDConvExeName, SVDFileName, Output );
    MemoSVDLog.Lines.Text := String( Output );
  end;

  SendMessage( MemoSVDLog.Handle, WM_VSCROLL, SB_LINEDOWN, 0 );

  UpdataSVDFileCheckResult( );
end;

{
  CMSIS-SVD SVD Consistency Checker / Header File Generator V3.2.40
  Check only>        SVDConv.exe myDevice.svd
}

procedure TMainForm.ApplicationIdle( Sender: TObject; var Done: Boolean );
begin
  if Pages.ActivePageIndex = 0 then
  begin
    StatusBar.Panels[ 0 ].Text := SVDStatusBarPanel0Caption + IntToStr( SVDErrorCount );
    StatusBar.Panels[ 1 ].Text := SVDStatusBarPanel1Caption + IntToStr( SVDWarningCount );
    StatusBar.Panels[ 2 ].Text := SVDStatusBarPanel2Caption + IntToStr( SVDInfoCount );
    if ( SVDFileName <> '' ) then
      StatusBar.Panels[ 3 ].Text := SVDFileName
    else
      StatusBar.Panels[ 3 ].Text := SVDNoFileName;
  end else begin
    StatusBar.Panels[ 0 ].Text := '';
    StatusBar.Panels[ 1 ].Text := '';
    StatusBar.Panels[ 2 ].Text := '';
    StatusBar.Panels[ 3 ].Text := '';
  end;
end;

procedure TMainForm.btnCollapseTreeClick( Sender: TObject );
begin
  VST_Main.FullCollapse( );
end;

procedure TMainForm.btnDecodeSVDClick( Sender: TObject );
var
  F: TFileStream;
begin
  if SVDFileName = '' then
    Exit;

  MemoIDC.Clear;
  MemoIDC.Invalidate;

  VST_Main.Clear;
  VST_Main.Invalidate;

  if not Assigned( XmlDoc ) then
  begin
    XmlDoc := TNativeXml.Create( Self );
    XmlDoc.OnProgress := XmlProgress;
    XmlDoc.OnNodeNew := XmlNodeNew;
    XmlDoc.OnNodeLoaded := XmlNodeLoaded;
    XmlDoc.OnDebugOut := XmlDebugOut;
  end;

  ActiveNode := nil;

  SVD_Decoded := False;

  VST_Main.BeginUpdate;
  ProgressBarVisible( True );

  F := TFileStream.Create( SVDFileName, fmOpenRead or fmShareDenyWrite );
  ProgressBar.Max := F.Size;

  try
    XmlDoc.LoadFromStream( F );
  finally
    F.free;
  end;

  ProgressBarVisible( False );
  btnExpandTreeClick( Self );
  VST_Main.EndUpdate;
  DecodeDevice( );
  SVD_Decoded := True;
end;

procedure TMainForm.btnGotoFirstClick( Sender: TObject );
begin
  SVDErrorWarningLineArrayIndex := 0;
  GotoLineNumberOfSVDFile;
end;

procedure TMainForm.btnGotoLastClick( Sender: TObject );
begin
  SVDErrorWarningLineArrayIndex := SVDErrorWarningLineArray.Count - 1;
  GotoLineNumberOfSVDFile;
end;

procedure TMainForm.btnGotoNextClick( Sender: TObject );
begin
  Inc( SVDErrorWarningLineArrayIndex );
  if SVDErrorWarningLineArrayIndex = SVDErrorWarningLineArray.Count then
    SVDErrorWarningLineArrayIndex := SVDErrorWarningLineArray.Count - 1;
  GotoLineNumberOfSVDFile;
end;

procedure TMainForm.btnGotoPrevClick( Sender: TObject );
begin
  dec( SVDErrorWarningLineArrayIndex );
  if SVDErrorWarningLineArrayIndex < 0 then
    SVDErrorWarningLineArrayIndex := 0;
  GotoLineNumberOfSVDFile;
end;

procedure TMainForm.About1Click( Sender: TObject );
begin
  AboutBox.ShowModal;
end;

{
  Usage: SVDConv.exe <SVD file> [Options]

  Options:
  --debug-headerfile         Add Addresses to Registers comments
  --generate=header          Generate CMSIS header file

  --fields=struct            Generate struct/union for bitfields
  --fields=struct-ansic      Generate structs for bitfields (ANSI C)
  --fields=macro             Generate macros for bitfields
  --fields=enum              Generate enumerated values for bitfields

  Examples:
  Check only>        SVDConv.exe myDevice.svd
  Header Generation> SVDConv.exe myDevice.svd --generate=header
}
procedure TMainForm.btnMakeHeaderClick( Sender: TObject );
var
  AOutputString: AnsiString;
  FieldOption: String;
begin
  if SVDFileName = '' then
    Exit;

  AOutputString := '';

  MemoSVDHeader.Lines.Clear;
  case RadioGroup1.ItemIndex of
    0:
      FieldOption := 'enum';
    1:
      FieldOption := 'macro';
    2:
      FieldOption := 'struct';
    3:
      FieldOption := 'ansic';
  end;

  if False then
  begin
    ExecAndCapture( SVDConvExeName, '"' + SVDFileName + '" --generate=header --debug-headerfile --fields=' +
      FieldOption, AOutputString );
    MemoSVDLog.Text := String( AOutputString );
  end else begin
    ExecAndCaptureReal( SVDConvExeName, '"' + SVDFileName + '" --generate=header --debug-headerfile --fields=' +
      FieldOption,
      procedure( const Line: PAnsiChar )
      begin
        MemoSVDLog.Lines.Add( String( Line ) );
      end );
  end;

  SendMessage( MemoSVDLog.Handle, WM_VSCROLL, SB_LINEDOWN, 0 );

  UpdataSVDFileCheckResult( );

  MemoSVDHeader.Lines.LoadFromFile( ChangeFileExt( ExtractFileName( SVDFileName ), '.h' ) );
end;

procedure TMainForm.btnSaveHeaderClick( Sender: TObject );
var
  FieldOption: String;
begin
  if MemoSVDHeader.Text = '' then
    Exit;

  case RadioGroup1.ItemIndex of
    0:
      FieldOption := 'enum';
    1:
      FieldOption := 'macro';
    2:
      FieldOption := 'struct';
    3:
      FieldOption := 'ansic';
  end;

  DlgSave.FileName := ChangeFileExt( SVDFileName, '_' + FieldOption + '.h' );
  DlgSave.Filter := 'C Header Files(*.h)|*.h';
  if not DlgSave.Execute then
    Exit;

  MemoSVDHeader.Lines.SaveToFile( DlgSave.FileName );
end;

procedure TMainForm.btnSaveScriptClick( Sender: TObject );
begin
  if MemoIDC.Text = '' then
    Exit;

  DlgSave.FileName := ChangeFileExt( SVDFileName, '.idc' );
  DlgSave.Filter := 'IDC Script Files(*.idc)|*.idc';
  if not DlgSave.Execute then
    Exit;

  MemoIDC.Lines.SaveToFile( DlgSave.FileName );
end;

// <xxxx derivedFrom="derivedFromName">
// . <name>Name</name>
// </xxxx>
//
// <peripheral derivedFrom="TIMER0">
//
// <register derivedFrom="CR">
// <register derivedFrom="TIMER0.CR">
//
// <cluster derivedFrom="TX[%s]">
// <cluster derivedFrom="TIMER0.TX[%s]">
//
// <field derivedFrom="IDR0">
// <field derivedFrom="CR.IDR0">
// <field derivedFrom="TIMER0.CR.IDR0">
//
// <enumeratedValues derivedFrom="VAL">
// <enumeratedValues derivedFrom="IDR0.VAL">
// <enumeratedValues derivedFrom="CR.IDR0.VAL">
// <enumeratedValues derivedFrom="TIMER0.CR.IDR0.VAL">
//
function TMainForm.FindDerivedObject( FullName: String; ObjectType: TSVD_ObjectType ): PSVD_Object;
var
  ObjectNames: TDynArray< String >;
  ObjectName: PString;
  AObject: PSVD_Object;
  Name: String;
  DelimiterPos: Integer;
  Offset: Integer;
  Index: Integer;
  I: Integer;
begin
  ObjectNames.Init( 2 );

  Offset := 1;
  while True do
  begin
    ObjectName := ObjectNames.NewItem;

    DelimiterPos := Pos( '.', FullName, Offset );
    if DelimiterPos > 0 then
    begin
      // "TIMER0.CR.IDR0.VAL"
      // -12345678 : Offset=1, DelimiterPos=7, Length=6
      ObjectName^ := Copy( FullName, Offset, DelimiterPos - Offset );
      // "TIMER0.CR.IDR0.VAL"
      // -12345678, Offset=8
      Offset := DelimiterPos + 1;
    end else begin
      // "TIMER0.CR.IDR0.VAL"
      // -123456789ABCDEF0123 : Offset=0x11, FullNameLength=0x13
      ObjectName^ := Copy( FullName, Offset, Length( FullName ) - Offset + 1 );
      break;
    end;
  end;

  // TIMER0 CR IDR0 VAL
  // CR IDR0 VAL
  // IDR0 VAL
  // VAL
  ObjectNames.Trim;

  for I := 0 to SVD_Device.ObjectArray.Count - 1 do
  begin
    Result := SVD_Device.ObjectArray.GetItem( I );
    if Result._type <> ObjectType then
      Continue;

    Index := ObjectNames.Count - 1;
    AObject := Result;
    while AObject <> nil do
    begin
      Name := AObject.Name;
      if Name <> ObjectNames.Items[ Index ] then
        break;

      dec( Index );
      if Index < 0 then
        Exit;

      AObject := AObject.Parent;
    end;
  end;

  Result := nil;
end;

procedure TMainForm.DecodeEnumeratedValue( Node: PVirtualNode; eumeratedValue: PSVD_EnumeratedValue );
var
  Data: PVST_MainData;
begin
  Node := VST_Main.GetFirstChild( Node );
  while Assigned( Node ) do
  begin
    Data := VST_Main.GetNodeData( Node );

    if Data.Name = 'name' then
      eumeratedValue.Name := Data.value
    else if Data.Name = 'description' then
      eumeratedValue.Description := Data.value
    else if Data.Name = 'value' then
      eumeratedValue.value := Str2Int( Data.value )
    else if Data.Name = 'isDefault' then
      eumeratedValue.isDefault := Data.value;

    Node := VST_Main.GetNextSibling( Node );
  end;
end;

procedure TMainForm.ClearDevice;
begin
  SVD_Device := Default ( TSVD_Device );
end;

function TMainForm.DecodeEnumeratedValues( Node: PVirtualNode; AObject: PSVD_Object ): Boolean;
var
  DerivedFromObject: PSVD_Object;
  eumeratedValues: PSVD_EnumeratedValues;

  Data: PVST_MainData;
  eumeratedValue: PSVD_EnumeratedValue;
begin
  Result := False;
  eumeratedValues := AObject.enumeratedValues;

  eumeratedValues.enumeratedValueArray.Init( 4 );

  Node := VST_Main.GetFirstChild( Node );
  while Assigned( Node ) do
  begin
    Data := VST_Main.GetNodeData( Node );
    if Data.Name = 'derivedFrom' then
    begin
      DerivedFromObject := FindDerivedObject( Data.value, otEnumeratedValues );
      if DerivedFromObject = nil then
      begin
        eumeratedValues.enumeratedValueArray.Purge( );
        Exit;
      end;

      eumeratedValues^ := DerivedFromObject.enumeratedValues^;
      eumeratedValues.DerivedFrom := Data.value;

    end else if Data.Name = 'name' then
    begin
      eumeratedValues.Name := Data.value;
      AObject.Name := Data.value;

    end else if Data.Name = 'usage' then
    begin
      eumeratedValues.usage := Data.value;
    end else if Data.Name = 'enumeratedValue' then
    begin
      eumeratedValue := eumeratedValues.enumeratedValueArray.NewItem( );
      eumeratedValue.Parent := eumeratedValues;
      DecodeEnumeratedValue( Node, eumeratedValue );
    end;

    Node := VST_Main.GetNextSibling( Node );
  end;

  eumeratedValues.enumeratedValueArray.Trim( );

  Result := True;
end;

function TMainForm.DecodeField( Node: PVirtualNode; AObject: PSVD_Object ): Boolean;
var
  DerivedFromObject: PSVD_Object;
  Data: PVST_MainData;
  field: PSVD_Field;

  LObject: PSVD_Object;
begin
  Result := False;

  field := AObject.field;

  Node := VST_Main.GetFirstChild( Node );
  while Assigned( Node ) do
  begin
    Data := VST_Main.GetNodeData( Node );

    if Data.Name = 'derivedFrom' then
    begin
      DerivedFromObject := FindDerivedObject( Data.value, otField );
      if DerivedFromObject = nil then
        Exit;

      field^ := DerivedFromObject.field^;
      field.DerivedFrom := Data.value;

    end else if Data.Name = 'name' then
    begin
      AObject.Name := Data.value;
      field.Name := Data.value;

    end else if Data.Name = 'description' then
      field.Description := Data.value

    else if Data.Name = 'dim' then
      field.dimElement.dim := Str2Int( Data.value )
    else if Data.Name = 'dimIncrement' then
      field.dimElement.dimIncrement := Str2Int( Data.value )
    else if Data.Name = 'dimIndex' then
      field.dimElement.dimIndex := Data.value
    else if Data.Name = 'dimArrayIndex' then
      field.dimElement.dimArrayIndex := Data.value

    else if Data.Name = 'bitOffset' then
    begin
      field.bitRange.bitRangeType := brOffsetWidth;
      field.bitRange.bitRangeOffsetWidth.bitOffset := Str2Int( Data.value )
    end else if Data.Name = 'bitWidth' then
    begin
      field.bitRange.bitRangeType := brOffsetWidth;
      field.bitRange.bitRangeOffsetWidth.bitWidth := Str2Int( Data.value );
    end else if Data.Name = 'lsb' then
    begin
      field.bitRange.bitRangeType := brLsbMsb;
      field.bitRange.bitRangeLsbMsb.lsb := Str2Int( Data.value )
    end else if Data.Name = 'msb' then
    begin
      field.bitRange.bitRangeType := brLsbMsb;
      field.bitRange.bitRangeLsbMsb.msb := Str2Int( Data.value )
    end else if Data.Name = 'bitRange' then
    begin
      field.bitRange.bitRangeType := brString;
      field.bitRange.bitRangeString := Data.value;
    end else if Data.Name = 'enumeratedValues' then
    begin
      LObject := SVD_Device.ObjectArray.NewItem( );
      LObject._type := otEnumeratedValues;
      LObject.Parent := AObject;

      LObject.enumeratedValues := Addr( field.enumeratedValues );
      LObject.enumeratedValues.Parent := field;
      if not DecodeEnumeratedValues( Node, LObject ) then
      begin
        SVD_Device.ObjectArray.Pop( );
      end;
    end;

    Node := VST_Main.GetNextSibling( Node );
  end;

  Result := True;
end;

function TMainForm.DecodeRegister( Node: PVirtualNode; AObject: PSVD_Object ): Boolean;
var
  DerivedFromObject: PSVD_Object;
  _register: PSVD_Register;

  Data: PVST_MainData;
  LObject: PSVD_Object;
  LNode: PVirtualNode;
begin
  Result := False;

  _register := AObject._register;
  _register.fieldArray.Init( 8 );

  Node := VST_Main.GetFirstChild( Node );
  while Assigned( Node ) do
  begin
    Data := VST_Main.GetNodeData( Node );
    if Data.Name = 'derivedFrom' then
    begin
      DerivedFromObject := FindDerivedObject( Data.value, otRegister );
      if DerivedFromObject = nil then
      begin
        _register.fieldArray.Purge( );
        Exit;
      end;

      _register^ := DerivedFromObject._register^;
      _register.DerivedFrom := Data.value;

    end else if Data.Name = 'name' then
    begin
      AObject.Name := Data.value;
      _register.Name := Data.value;

    end else if Data.Name = 'displayName' then
      _register.displayName := Data.value
    else if Data.Name = 'description' then
      _register.Description := Data.value
    else if Data.Name = 'addressOffset' then
      _register.addressOffset := Str2Int( Data.value )
    else if Data.Name = 'alternateGroup' then
      _register.alternateGroup := Data.value
    else if Data.Name = 'alternateRegister' then
      _register.alternateRegister := Data.value
    else if Data.Name = 'dataType' then
      _register.dataType := Data.value

    else if Data.Name = 'size' then
      _register.registerProperties.Size := Str2Int( Data.value )
    else if Data.Name = 'resetMask' then
      _register.registerProperties.resetMask := Str2Int( Data.value )
    else if Data.Name = 'resetValue' then
      _register.registerProperties.resetValue := Str2Int( Data.value )
    else if Data.Name = 'access' then
      _register.registerProperties.access := Data.value
    else if Data.Name = 'protection' then
      _register.registerProperties.protection := Data.value

    else if Data.Name = 'dim' then
      _register.dimElement.dim := Str2Int( Data.value )
    else if Data.Name = 'dimIncrement' then
      _register.dimElement.dimIncrement := Str2Int( Data.value )
    else if Data.Name = 'dimIndex' then
      _register.dimElement.dimIndex := Data.value
    else if Data.Name = 'dimArrayIndex' then
      _register.dimElement.dimArrayIndex := Data.value

    else if Data.Name = 'fields' then
    begin
      LNode := VST_Main.GetFirstChild( Node );
      while Assigned( LNode ) do
      begin
        Data := VST_Main.GetNodeData( LNode );

        if Data.Name = 'field' then
        begin
          LObject := SVD_Device.ObjectArray.NewItem( );
          LObject._type := otField;
          LObject.Parent := AObject;

          LObject.field := _register.fieldArray.NewItem( );
          LObject.field.Parent := _register;
          if not DecodeField( LNode, LObject ) then
          begin
            SVD_Device.ObjectArray.Pop( );
            _register.fieldArray.Pop( );
          end;
        end;

        LNode := VST_Main.GetNextSibling( LNode );
      end;

      _register.fieldArray.Trim( );
    end;

    Node := VST_Main.GetNextSibling( Node );
  end;

  Result := True;
end;

function TMainForm.DecodeCluster( Node: PVirtualNode; AObject: PSVD_Object ): Boolean;
var
  DerivedFromObject: PSVD_Object;
  cluster: PSVD_Cluster;

  Data: PVST_MainData;
  LObject: PSVD_Object;
begin
  Result := False;

  cluster := AObject.cluster;

  cluster.registerArray.Init( 4 );
  cluster.clusterArray.Init( 2 );

  Node := VST_Main.GetFirstChild( Node );
  while Assigned( Node ) do
  begin
    Data := VST_Main.GetNodeData( Node );
    if Data.Name = 'derivedFrom' then
    begin
      DerivedFromObject := FindDerivedObject( Data.value, otCluster );
      if DerivedFromObject = nil then
      begin
        cluster.registerArray.Trim( );
        cluster.clusterArray.Trim( );
        Exit;
      end;

      AObject.cluster^ := DerivedFromObject.cluster^;
      cluster.DerivedFrom := Data.Name;

    end else if Data.Name = 'name' then
    begin
      AObject.Name := Data.value;
      cluster.Name := Data.value;

    end else if Data.Name = 'displayName' then
      cluster.displayName := Data.value
    else if Data.Name = 'description' then
      cluster.Description := Data.value
    else if Data.Name = 'alternateCluster' then
      cluster.alternateCluster := Data.value
    else if Data.Name = 'addressOffset' then
      cluster.addressOffset := Str2Int( Data.value )

    else if Data.Name = 'size' then
      cluster.registerProperties.Size := Str2Int( Data.value )
    else if Data.Name = 'resetMask' then
      cluster.registerProperties.resetMask := Str2Int( Data.value )
    else if Data.Name = 'resetValue' then
      cluster.registerProperties.resetValue := Str2Int( Data.value )
    else if Data.Name = 'access' then
      cluster.registerProperties.access := Data.value
    else if Data.Name = 'protection' then
      cluster.registerProperties.protection := Data.value

    else if Data.Name = 'dim' then
      cluster.dimElement.dim := Str2Int( Data.value )
    else if Data.Name = 'dimIncrement' then
      cluster.dimElement.dimIncrement := Str2Int( Data.value )
    else if Data.Name = 'dimIndex' then
      cluster.dimElement.dimIndex := Data.value
    else if Data.Name = 'dimArrayIndex' then
      cluster.dimElement.dimArrayIndex := Data.value

    else if Data.Name = 'register' then
    begin
      LObject := SVD_Device.ObjectArray.NewItem;
      LObject.Parent := AObject;
      LObject._type := otRegister;

      LObject._register := cluster.registerArray.NewItem( );
      LObject._register.parentPeripheral := nil;
      LObject._register.parentCluster := cluster;
      if not DecodeRegister( Node, LObject ) then
      begin
        cluster.registerArray.Pop( );
        SVD_Device.ObjectArray.Pop( );
      end;
    end else if Data.Name = 'cluster' then
    begin
      LObject := SVD_Device.ObjectArray.NewItem;
      LObject.Parent := AObject;
      LObject._type := otCluster;

      LObject.cluster := cluster.clusterArray.NewItem( );
      LObject.cluster.parentPeripheral := nil;
      LObject.cluster.parentCluster := cluster;
      if not DecodeCluster( Node, LObject ) then
      begin
        cluster.clusterArray.Pop( );
        SVD_Device.ObjectArray.Pop( );
      end;
    end;

    Node := VST_Main.GetNextSibling( Node );
  end;

  cluster.registerArray.Trim( );
  cluster.clusterArray.Trim( );

  Result := True;
end;

procedure TMainForm.DecodeAddressBlock( Node: PVirtualNode; addressBlock: PSVD_AddressBlock );
var
  Data: PVST_MainData;
begin
  Node := VST_Main.GetFirstChild( Node );
  while Assigned( Node ) do
  begin
    Data := VST_Main.GetNodeData( Node );
    if Data.Name = 'offset' then
      addressBlock.Offset := Str2Int( Data.value )
    else if Data.Name = 'size' then
      addressBlock.Size := Str2Int( Data.value );

    Node := VST_Main.GetNextSibling( Node );
  end;
end;

procedure TMainForm.DecodeInterrupt( Node: PVirtualNode; interrupt: PSVD_Interrupt );
var
  Data: PVST_MainData;
begin
  Node := VST_Main.GetFirstChild( Node );
  while Assigned( Node ) do
  begin
    Data := VST_Main.GetNodeData( Node );
    if Data.Name = 'name' then
      interrupt.Name := Data.value
    else if Data.Name = 'description' then
      interrupt.Description := Data.value
    else if Data.Name = 'value' then
      interrupt.value := Str2Int( Data.value );

    Node := VST_Main.GetNextSibling( Node );
  end;

end;

function TMainForm.DecodePeripheral( Node: PVirtualNode; AObject: PSVD_Object ): Boolean;
var
  DerivedFromObject: PSVD_Object;
  peripheral: PSVD_Peripheral;
  addressBlock: PSVD_AddressBlock;

  Data: PVST_MainData;
  LNode: PVirtualNode;
  LObject: PSVD_Object;
begin
  Result := False;

  peripheral := AObject.peripheral;

  peripheral.addressBlockArray.Init( 2 );
  peripheral.clusterArray.Init( 2 );
  peripheral.registerArray.Init( 4 );

  Node := VST_Main.GetFirstChild( Node );
  while Assigned( Node ) do
  begin
    Data := VST_Main.GetNodeData( Node );
    if Data.Name = 'derivedFrom' then
    begin
      DerivedFromObject := FindDerivedObject( Data.value, otPeripheral );
      if DerivedFromObject = nil then
      begin
        peripheral.addressBlockArray.Purge( );
        peripheral.registerArray.Purge( );
        peripheral.clusterArray.Purge( );
        Exit;
      end;

      peripheral^ := DerivedFromObject.peripheral^;
      peripheral.DerivedFrom := Data.value;

    end else if Data.Name = 'name' then
    begin
      AObject.Name := Data.value;
      peripheral.Name := Data.value;
    end else if Data.Name = 'version' then
      peripheral.version := Data.value
    else if Data.Name = 'description' then
      peripheral.Description := Data.value
    else if Data.Name = 'alternatePeripheral' then
      peripheral.alternatePeripheral := Data.value
    else if Data.Name = 'prependToName' then
      peripheral.prependToName := Data.value
    else if Data.Name = 'appendToName' then
      peripheral.appendToName := Data.value
    else if Data.Name = 'baseAddress' then
      peripheral.baseAddress := Str2Int( Data.value )

    else if Data.Name = 'size' then
      peripheral.registerProperties.Size := Str2Int( Data.value )
    else if Data.Name = 'resetMask' then
      peripheral.registerProperties.resetMask := Str2Int( Data.value )
    else if Data.Name = 'resetValue' then
      peripheral.registerProperties.resetValue := Str2Int( Data.value )
    else if Data.Name = 'access' then
      peripheral.registerProperties.access := Data.value
    else if Data.Name = 'protection' then
      peripheral.registerProperties.protection := Data.value

    else if Data.Name = 'dim' then
      peripheral.dimElement.dim := Str2Int( Data.value )
    else if Data.Name = 'dimIncrement' then
      peripheral.dimElement.dimIncrement := Str2Int( Data.value )
    else if Data.Name = 'dimIndex' then
      peripheral.dimElement.dimIndex := Data.value
    else if Data.Name = 'dimArrayIndex' then
      peripheral.dimElement.dimArrayIndex := Data.value

    else if Data.Name = 'interrupt' then
      DecodeInterrupt( Node, Addr( peripheral.interrupt ) )
    else if Data.Name = 'addressBlock' then
    begin
      addressBlock := peripheral.addressBlockArray.NewItem( );
      DecodeAddressBlock( Node, addressBlock );
    end else if Data.Name = 'registers' then
    begin

      LNode := VST_Main.GetFirstChild( Node );
      while Assigned( LNode ) do
      begin
        Data := VST_Main.GetNodeData( LNode );

        if Data.Name = 'register' then
        begin
          LObject := SVD_Device.ObjectArray.NewItem;
          LObject.Parent := AObject;
          LObject._type := otRegister;

          LObject._register := peripheral.registerArray.NewItem( );
          LObject._register.parentPeripheral := peripheral;
          LObject._register.parentCluster := nil;
          if not DecodeRegister( LNode, LObject ) then
          begin
            peripheral.registerArray.Pop( );
            SVD_Device.ObjectArray.Pop( );
          end;
        end else if Data.Name = 'cluster' then
        begin
          LObject := SVD_Device.ObjectArray.NewItem;
          LObject.Parent := AObject;
          LObject._type := otCluster;

          LObject.cluster := peripheral.clusterArray.NewItem( );
          LObject.cluster.parentPeripheral := peripheral;
          LObject.cluster.parentCluster := nil;
          if not DecodeCluster( LNode, LObject ) then
          begin
            peripheral.clusterArray.Pop( );
            SVD_Device.ObjectArray.Pop( );
          end;
        end;

        LNode := VST_Main.GetNextSibling( LNode );
      end;

    end;

    Node := VST_Main.GetNextSibling( Node );
  end;

  peripheral.registerArray.Trim( );
  peripheral.clusterArray.Trim( );
  peripheral.addressBlockArray.Trim( );

  Result := True;
end;

procedure TMainForm.DecodeCPU( Node: PVirtualNode; cpu: PSVD_CPU );
var
  Data: PVST_MainData;
begin
  Node := VST_Main.GetFirstChild( Node );
  while Assigned( Node ) do
  begin
    Data := VST_Main.GetNodeData( Node );
    if Data.Name = 'name' then
      cpu.Name := Data.value
    else if Data.Name = 'revision' then
      cpu.revision := Data.value
    else if Data.Name = 'endian' then
      cpu.endian := Data.value
    else if Data.Name = 'itcmPresent' then
      cpu.itcmPresent := Data.value
    else if Data.Name = 'dtcmPresent' then
      cpu.dtcmPresent := Data.value
    else if Data.Name = 'icachePresent' then
      cpu.icachePresent := Data.value
    else if Data.Name = 'dcachePresent' then
      cpu.dcachePresent := Data.value
    else if Data.Name = 'mpuPresent' then
      cpu.mpuPresent := Data.value
    else if Data.Name = 'fpuPresent' then
      cpu.fpuPresent := Data.value
    else if Data.Name = 'fpuDP' then
      cpu.fpuDP := Data.value
    else if Data.Name = 'vtorPresent' then
      cpu.vtorPresent := Data.value
    else if Data.Name = 'vendorSystickConfig' then
      cpu.vendorSystickConfig := Data.value

    else if Data.Name = 'sauNumRegions' then
      cpu.sauNumRegions := Str2Int( Data.value )
    else if Data.Name = 'nvicPrioBits' then
      cpu.nvicPrioBits := Str2Int( Data.value )
    else if Data.Name = 'deviceNumInterrupts' then
      cpu.deviceNumInterrupts := Str2Int( Data.value );

    Node := VST_Main.GetNextSibling( Node );
  end;
end;

procedure TMainForm.DecodeDevice( );
var
  Data: PVST_MainData;
  Node: PVirtualNode;
  LNode: PVirtualNode;
  LObject: PSVD_Object;
begin
  ClearDevice( );

  SVD_Device.ObjectArray.Init( 2 );
  SVD_Device.peripheralArray.Init( 2 );

  Node := VST_Main.GetFirstChild( nil );
  while Assigned( Node ) do
  begin
    Data := VST_Main.GetNodeData( Node );
    if Data.Name = 'device' then
    begin
      Node := VST_Main.GetFirstChild( Node );
      while Assigned( Node ) do
      begin
        Data := VST_Main.GetNodeData( Node );
        if Data.Name = 'name' then
          SVD_Device.Name := Data.value
        else if Data.Name = 'vendor' then
          SVD_Device.vendor := Data.value
        else if Data.Name = 'vendorID' then
          SVD_Device.vendorID := Data.value
        else if Data.Name = 'series' then
          SVD_Device.series := Data.value
        else if Data.Name = 'version' then
          SVD_Device.version := Data.value
        else if Data.Name = 'description' then
          SVD_Device.Description := Data.value
        else if Data.Name = 'addressUnitBits' then
          SVD_Device.addressUnitBits := Str2Int( Data.value )
        else if Data.Name = 'width' then
          SVD_Device.Width := Str2Int( Data.value )

        else if Data.Name = 'size' then
          SVD_Device.registerProperties.Size := Str2Int( Data.value )
        else if Data.Name = 'resetMask' then
          SVD_Device.registerProperties.resetMask := Str2Int( Data.value )
        else if Data.Name = 'resetValue' then
          SVD_Device.registerProperties.resetValue := Str2Int( Data.value )
        else if Data.Name = 'access' then
          SVD_Device.registerProperties.access := Data.value
        else if Data.Name = 'protection' then
          SVD_Device.registerProperties.protection := Data.value

        else if Data.Name = 'cpu' then
          DecodeCPU( Node, Addr( SVD_Device.cpu ) )

        else if Data.Name = 'peripherals' then
        begin

          LNode := VST_Main.GetFirstChild( Node );
          while Assigned( LNode ) do
          begin
            Data := VST_Main.GetNodeData( LNode );
            if Data.Name = 'peripheral' then
            begin
              LObject := SVD_Device.ObjectArray.NewItem( );
              LObject._type := otPeripheral;
              LObject.Parent := nil;

              LObject.peripheral := SVD_Device.peripheralArray.NewItem( );
              if not DecodePeripheral( LNode, LObject ) then
              begin
                SVD_Device.peripheralArray.Pop( );
                SVD_Device.ObjectArray.Pop( );
              end;
            end;

            LNode := VST_Main.GetNextSibling( LNode );
          end;

          SVD_Device.peripheralArray.Trim( );
        end;

        Node := VST_Main.GetNextSibling( Node );
      end;

      Node := VST_Main.GetNextSibling( Node );
    end;

  end;

  SVD_Device.ObjectArray.Trim( );
end;

procedure TMainForm.btnMakeScriptClick( Sender: TObject );
var
  StringList: TStringList;
begin
  if not SVD_Decoded then
    Exit;

  MemoIDC.Lines.Clear;
  StringList := TStringList.Create;
  try
    IDC_Decode( SVD_Device, StringList );
    MemoIDC.Lines.AddStrings( StringList );

  finally
    StringList.free;
  end;
end;

end.
