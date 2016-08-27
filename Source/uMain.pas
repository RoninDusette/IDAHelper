unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.ShellAPI, // Windows
  System.Classes, System.Variants, System.SysUtils, System.ImageList, // System
  Vcl.Forms, Vcl.Menus, Vcl.ImgList, Vcl.Graphics, Vcl.Dialogs, Vcl.ToolWin, // VCL
  Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, // VCL Controls
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
    Splitter3: TSplitter;
    VST_Peripheral: TVirtualStringTree;
    Splitter2: TSplitter;
    VST_Interrupt: TVirtualStringTree;
    Splitter4: TSplitter;
    MemoIDC: TMemo;
    ToolButton21: TToolButton;
    btnGotoFirst: TToolButton;
    DlgSave: TSaveDialog;
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
    procedure VST_PeripheralGetNodeDataSize( Sender: TBaseVirtualTree; var NodeDataSize: Integer );
    procedure VST_InterruptGetNodeDataSize( Sender: TBaseVirtualTree; var NodeDataSize: Integer );
    procedure VST_PeripheralGetText( Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: String );
    procedure VST_InterruptGetText( Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: String );
    procedure FormDestroy( Sender: TObject );
    procedure VST_MainFreeNode( Sender: TBaseVirtualTree; Node: PVirtualNode );
    procedure VST_PeripheralFreeNode( Sender: TBaseVirtualTree; Node: PVirtualNode );
    procedure VST_InterruptFreeNode( Sender: TBaseVirtualTree; Node: PVirtualNode );
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

    function Decode_VST_Main: Boolean;

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
    procedure DecodeDevice0;
    procedure DecodePeripherals( Node: PVirtualNode; peripherals: PSVD_Peripherals );
    procedure DecodePeripheral( Node: PVirtualNode; peripheral: PSVD_Peripheral );
    //
    procedure DecodeRegisters( Node: PVirtualNode; registers: PSVD_Registers );

    // 1.3: nesting of cluster is supported
    procedure DecodeCluster( Node: PVirtualNode; cluster: PSVD_Cluster );
    procedure DecodeRegister( Node: PVirtualNode; _register: PSVD_Register );
    //
    procedure DecodeFields( Node: PVirtualNode; fields: PSVD_Fields );
    procedure DecodeField( Node: PVirtualNode; field: PSVD_Field );
    procedure DecodeEnumeratedValues( Node: PVirtualNode; eumeratedValues: PSVD_EnumeratedValues );
    procedure DecodeEnumeratedValue( Node: PVirtualNode; eumeratedValue: PSVD_EnumeratedValue );

  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}
{ $DEFINE XML_DEBUG }

uses uAbout, uMisc;

type
  TSVDErrorWarningLine = record
    SVDFileLine: Integer;
    SVDLogLine: Integer;
  end;

type
  TXmlNodeInfo = record
    NodeType: TsdElementType;
    TypeName: String;
    name: String;
    value: String;
    Depth: Integer;
  end;

type
  // -----------------------------------------------------------------------------------------------
  // Virtual String Tree Struct
  // -----------------------------------------------------------------------------------------------
  PVST_MainData = ^TVST_MainData;

  // Free name onFreeNode()
  TVST_MainData = record
    name: String;
    value: String;
  end;

  PVST_PeriphData = ^TVST_PeriphData;

  // Free name onFreeNode()
  TVST_PeriphData = record
    name: String;
    Description: String;
    BaseAddr: String;
    Offset: String;
    Size: String;
    DerivedFrom: String;
  end;

  PVST_IrqData = ^TVST_IrqData;

  // Free name onFreeNode()
  TVST_IrqData = record
    name: String;
    Description: String;
    Number: String;
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
    'xeDtdElement', 'xeDtdAttList', 'xeDtdEntity', 'xeDtdNotation', 'xeInstruction', 'xeCharData', 'xeWhiteSpace',
    'xeQuotedText', 'xeUnknown', 'xeEndTag', 'xeError' );

var
  SVDFileName: String;
  SVDInfoCount: Integer;
  SVDWarningCount: Integer;
  SVDErrorCount: Integer;

  SVDErrorWarningLineArrayIndex: Cardinal;
  SVDErrorWarningLineArrayCount: Cardinal;
  SVDErrorWarningLineArray: TArray< TSVDErrorWarningLine >;
  // First, Last, Prev, Next : LineNumber := SVDErrorWarningLineArray[SVDErrorWarningLineArrayIndex]

  XmlDoc: TNativeXml;

  ActiveNode: PVirtualNode;
  SavedNode: PVirtualNode;

  SVD_FileDecoded: Boolean;
  SVD_DeviceDecoded: Boolean;

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
  SVDInfoCount := 0;
  SVDWarningCount := 0;
  SVDErrorCount := 0;
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
  Point: TPoint;
begin
  if SVDErrorWarningLineArrayCount = 0 then
    Exit;

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
      Font.name := 'FixedSys';
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
      Font.name := 'FixedSys';
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
  VST_Peripheral.FullExpand( );
  VST_Interrupt.FullExpand( );
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
var
  I: Integer;
  LineNumber: Integer;
  LineArryIndex: Cardinal;
  LineArryCapacity: Cardinal;

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
      Inc( SVDErrorCount );
      Inc( SVDErrorWarningLineArrayCount );

      SVDErrorCount := SVDErrorCount + 1;
      SVDErrorWarningLineArray[ LineArryIndex ].SVDLogLine := I;
      SVDErrorWarningLineArray[ LineArryIndex ].SVDFileLine := GetLineNumber( I );

      if True then
        TArrayHelper< TSVDErrorWarningLine >.Increment( SVDErrorWarningLineArray, LineArryIndex, LineArryCapacity )
      else
      begin
        Inc( LineArryIndex );
        if LineArryIndex = LineArryCapacity then
        begin
          LineArryCapacity := LineArryCapacity + LineArryCapacity div 2;
          SetLength( SVDErrorWarningLineArray, LineArryCapacity );
        end;
      end;
    end;

    if Pos( SVDFileWarningStr, MemoSVDLog.Lines[ I ] ) > 0 then
    begin
      Inc( SVDWarningCount );
      Inc( SVDErrorWarningLineArrayCount );

      SVDErrorWarningLineArray[ LineArryIndex ].SVDLogLine := I;
      SVDErrorWarningLineArray[ LineArryIndex ].SVDFileLine := GetLineNumber( I );

      TArrayHelper< TSVDErrorWarningLine >.Increment( SVDErrorWarningLineArray, LineArryIndex, LineArryCapacity );
    end;

    if Pos( SVDFileInfoStr, MemoSVDLog.Lines[ I ] ) > 0 then
      SVDInfoCount := SVDInfoCount + 1;
  end;

  SetLength( SVDErrorWarningLineArray, LineArryIndex );
end;

procedure TMainForm.VST_MainFreeNode( Sender: TBaseVirtualTree; Node: PVirtualNode );
var
  MainData: PVST_MainData;
begin
  MainData := VST_Main.GetNodeData( Node );
  MainData.name := '';
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
    CellText := DataAll.name
  else
    CellText := DataAll.value;
end;

procedure TMainForm.VST_InterruptFreeNode( Sender: TBaseVirtualTree; Node: PVirtualNode );
var
  IrqData: PVST_IrqData;
begin
  IrqData := VST_Main.GetNodeData( Node );
  IrqData.name := '';
  IrqData.Number := '';
  IrqData.Description := '';
end;

procedure TMainForm.VST_InterruptGetNodeDataSize( Sender: TBaseVirtualTree; var NodeDataSize: Integer );
begin
  NodeDataSize := Sizeof( TVST_IrqData );
end;

procedure TMainForm.VST_InterruptGetText( Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: String );
var
  DataInterrupt: PVST_IrqData;
begin
  DataInterrupt := VST_Main.GetNodeData( Node );
  if Column = 0 then
    CellText := DataInterrupt.name
  else if Column = 1 then
    CellText := DataInterrupt.Number
  else
    CellText := DataInterrupt.Description;
end;

procedure TMainForm.VST_PeripheralFreeNode( Sender: TBaseVirtualTree; Node: PVirtualNode );
var
  PeriphData: PVST_PeriphData;
begin
  PeriphData := VST_Main.GetNodeData( Node );
  PeriphData.name := '';
  PeriphData.Size := '';
  PeriphData.Offset := '';
  PeriphData.BaseAddr := '';
  PeriphData.Description := '';
  PeriphData.DerivedFrom := '';
end;

procedure TMainForm.VST_PeripheralGetNodeDataSize( Sender: TBaseVirtualTree; var NodeDataSize: Integer );
begin
  NodeDataSize := Sizeof( TVST_PeriphData );
end;

procedure TMainForm.VST_PeripheralGetText( Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: String );
var
  DataPeripheral: PVST_PeriphData;
begin
  DataPeripheral := VST_Main.GetNodeData( Node );
  if Column = 0 then
    CellText := DataPeripheral.name
  else if Column = 1 then
    CellText := DataPeripheral.BaseAddr
  else if Column = 2 then
    CellText := DataPeripheral.Offset
  else if Column = 3 then
    CellText := DataPeripheral.Size
  else
    CellText := DataPeripheral.Description;
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
  iNode.name := ANode.name;
  iNode.value := ANode.value;
  iNode.Depth := ANode.TreeDepth;

{$IFDEF XML_DEBUG}
  MemoIDC.Lines.Add( Format( '%sOnNewNode Depth=%d, Type=%s, name=%s, value=%s', //
    [ Indent( iNode.Depth ), iNode.Depth, iNode.TypeName, iNode.name, iNode.value ] ) );
{$ENDIF}
  //
  if ( ActiveNode = nil ) and ( ANode.ElementType = xeElement ) then
  begin
    ActiveNode := VST_Main.AddChild( nil );
    MainData := VST_Main.GetNodeData( ActiveNode );
    MainData.name := iNode.name;
    MainData.value := '';
    Exit;
  end;

  if ActiveNode = nil then
    Exit;

  if ANode.ElementType = xeElement then
  begin
    ActiveNode := VST_Main.AddChild( ActiveNode );
    MainData := VST_Main.GetNodeData( ActiveNode );
    MainData.name := iNode.name;
    MainData.value := 'Expand to see more information';
  end else if ANode.ElementType = xeAttribute then
  begin
    ActiveNode := VST_Main.AddChild( ActiveNode );
    MainData := VST_Main.GetNodeData( ActiveNode );
    MainData.name := 'Attribute name to be set';
    MainData.value := 'Attribute value To be set';
  end else if ANode.ElementType = xeCharData then
  begin
    if False then
    begin
      ActiveNode := VST_Main.AddChild( ActiveNode );
      MainData := VST_Main.GetNodeData( ActiveNode );
      MainData.name := iNode.name;
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
  iNode.name := ANode.name;
  iNode.value := ANode.value;
  iNode.Depth := ANode.TreeDepth;

{$IFDEF XML_DEBUG}
  MemoIDC.Lines.Add( Format( '%sOnLoaded Depth=%d, Type=%s, name=%s, value=%s', //
    [ Indent( iNode.Depth ), iNode.Depth, iNode.TypeName, iNode.name, iNode.value ] ) );
{$ENDIF}
  //
  if ActiveNode = nil then
    Exit;

  if ANode.ElementType = xeElement then
  begin
    if iNode.Depth <> 0 then
    begin
      MainData := VST_Main.GetNodeData( ActiveNode );
      MainData.name := iNode.name;

      if iNode.name = 'description' then
        MainData.value := RemoveWhiteSpace( iNode.value )
      else
        MainData.value := iNode.value;

      ActiveNode := ActiveNode.Parent;
    end;
  end else if ( ANode.ElementType = xeAttribute ) then
  begin
    MainData := VST_Main.GetNodeData( ActiveNode );
    MainData.name := iNode.name;
    MainData.value := iNode.value;
    ActiveNode := ActiveNode.Parent;
  end else if ANode.ElementType = xeCharData then
  begin
    if False then
    begin
      MainData := VST_Main.GetNodeData( ActiveNode );
      MainData.name := iNode.name;
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
  VST_Peripheral.FullCollapse( );
  VST_Interrupt.FullCollapse( );
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

  VST_Peripheral.Clear;
  VST_Peripheral.Invalidate;

  VST_Interrupt.Clear;
  VST_Interrupt.Invalidate;

  if not Assigned( XmlDoc ) then
  begin
    XmlDoc := TNativeXml.Create( Self );
    XmlDoc.OnProgress := XmlProgress;
    XmlDoc.OnNodeNew := XmlNodeNew;
    XmlDoc.OnNodeLoaded := XmlNodeLoaded;
    XmlDoc.OnDebugOut := XmlDebugOut;
  end;

  ActiveNode := nil;

  SVD_DeviceDecoded := False;
  // -----------------------------------------------------------------------------------------------
  SVD_FileDecoded := False;
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
  VST_Main.EndUpdate;

  Decode_VST_Main( );
  btnExpandTreeClick( Self );

  SVD_FileDecoded := True;
  // -----------------------------------------------------------------------------------------------

  DecodeDevice( );
  // -----------------------------------------------------------------------------------------------
  SVD_DeviceDecoded := True;
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
  dec( SVDErrorWarningLineArrayIndex );
  if SVDErrorWarningLineArrayIndex < 0 then
    SVDErrorWarningLineArrayIndex := 0;
  GotoLineNumberOfSVDFile;
end;

procedure TMainForm.About1Click( Sender: TObject );
begin
  AboutBox.ShowModal;
end;

function TMainForm.Decode_VST_Main: Boolean;
type
  TVST_PeriphInfo = record
    Node: PVirtualNode;
    name: String;
  end;

var
  I: Integer;
  Found: Boolean;

  NodeData: PVST_MainData;
  PeriphData: TVST_PeriphData;
  IrqData: TVST_IrqData;

  ANode: PVirtualNode;
  BNode: PVirtualNode;
  CNode: PVirtualNode;

  SVD_Peripherals: TArray< TVST_PeriphInfo >;
  SVD_PerphCount: Cardinal;
  SVD_PerphCapacity: Cardinal;
begin
  Found := False;
  ANode := VST_Main.GetFirstChild( nil );
  if not Assigned( ANode ) then
    Exit;
  NodeData := VST_Main.GetNodeData( ANode );
  if NodeData.name <> 'device' then
    Exit;

  ANode := VST_Main.GetFirstChild( ANode );
  if not Assigned( ANode ) then
    Exit;

  while Assigned( ANode ) do
  begin
    NodeData := VST_Main.GetNodeData( ANode );
    if NodeData.name = 'peripherals' then
    begin
      Found := True;
      break;
    end;
    ANode := VST_Main.GetNextSibling( ANode );
  end;

  if not Found then
    Exit;

  SVD_PerphCount := 0;
  SVD_PerphCapacity := 128;
  SetLength( SVD_Peripherals, SVD_PerphCapacity );

  ANode := VST_Main.GetFirstChild( ANode ); // peripherals.peripheral
  while Assigned( ANode ) do
  begin
    NodeData := VST_Main.GetNodeData( ANode );
    if NodeData.name = 'peripheral' then
    begin
      PeriphData.name := '';
      PeriphData.Size := '';
      PeriphData.Offset := '';
      PeriphData.BaseAddr := '';
      PeriphData.Description := '';
      PeriphData.DerivedFrom := '';

      IrqData.name := '';
      IrqData.Number := '';
      IrqData.Description := '';

      BNode := VST_Main.GetFirstChild( ANode );
      while Assigned( BNode ) do
      begin
        NodeData := VST_Main.GetNodeData( BNode );
        if NodeData.name = 'addressBlock' then
        begin
          CNode := VST_Main.GetFirstChild( BNode );
          while Assigned( CNode ) do
          begin
            NodeData := VST_Main.GetNodeData( CNode );
            if NodeData.name = 'offset' then
              PeriphData.Offset := NodeData.value
            else if NodeData.name = 'size' then
              PeriphData.Size := NodeData.value;
            CNode := VST_Main.GetNextSibling( CNode );
          end;
        end else if NodeData.name = 'interrupt' then
        begin
          CNode := VST_Main.GetFirstChild( BNode );
          while Assigned( CNode ) do
          begin
            NodeData := VST_Main.GetNodeData( CNode );
            if NodeData.name = 'name' then
              IrqData.name := NodeData.value
            else if NodeData.name = 'value' then
              IrqData.Number := NodeData.value
            else if NodeData.name = 'description' then
              // IrqData.Description := RemoveWhiteSpace( NodeData.value );
              IrqData.Description := NodeData.value;

            CNode := VST_Main.GetNextSibling( CNode );
          end;
        end else if NodeData.name = 'name' then
          PeriphData.name := NodeData.value
        else if NodeData.name = 'description' then
          // PeriphData.Description := RemoveWhiteSpace( NodeData.value )
          PeriphData.Description := NodeData.value
        else if NodeData.name = 'baseAddress' then
          PeriphData.BaseAddr := NodeData.value
        else if NodeData.name = 'derivedFrom' then
          PeriphData.DerivedFrom := NodeData.value;

        // Find next node
        BNode := VST_Main.GetNextSibling( BNode );
      end;

      CNode := VST_Peripheral.AddChild( nil );

      // Add Seg to Array to query late
      SVD_Peripherals[ SVD_PerphCount ].name := PeriphData.name;
      SVD_Peripherals[ SVD_PerphCount ].Node := CNode;
      TArrayHelper< TVST_PeriphInfo >.Increment( SVD_Peripherals, SVD_PerphCount, SVD_PerphCapacity );

      if PeriphData.DerivedFrom <> '' then
      begin
        for I := 0 to SVD_PerphCount - 1 do
        begin
          if PeriphData.DerivedFrom = SVD_Peripherals[ I ].name then
          begin
            NodeData := VST_Main.GetNodeData( SVD_Peripherals[ I ].Node );
            if PeriphData.BaseAddr = '' then // Never
              PeriphData.BaseAddr := PVST_PeriphData( NodeData ).BaseAddr;
            if PeriphData.Description = '' then // Maybe
              PeriphData.Description := PVST_PeriphData( NodeData ).Description;
            if PeriphData.Offset = '' then // Always ?
              PeriphData.Offset := PVST_PeriphData( NodeData ).Offset;
            if PeriphData.Size = '' then // Always ?
              PeriphData.Size := PVST_PeriphData( NodeData ).Size;

            break;
          end;
        end;
      end;

      // To here, PeriphData is ready to add to VST_Peripheral
      NodeData := VST_Main.GetNodeData( CNode );

      PVST_PeriphData( NodeData ).name := PeriphData.name;
      PVST_PeriphData( NodeData ).Description := PeriphData.Description;
      PVST_PeriphData( NodeData ).BaseAddr := PeriphData.BaseAddr;
      PVST_PeriphData( NodeData ).Offset := PeriphData.Offset;
      PVST_PeriphData( NodeData ).Size := PeriphData.Size;

      if ( IrqData.name <> '' ) or ( IrqData.Number <> '' ) then
      begin
        CNode := VST_Interrupt.AddChild( nil );
        NodeData := VST_Main.GetNodeData( CNode );

        PVST_IrqData( NodeData ).name := IrqData.name;
        PVST_IrqData( NodeData ).Description := IrqData.Description;
        PVST_IrqData( NodeData ).Number := IrqData.Number;
      end;
    end;

    // Find next node named 'peripheral'
    ANode := VST_Main.GetNextSibling( ANode );
  end;
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
    MemoSVDLog.Text := AOutputString;
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

procedure TMainForm.DecodeEnumeratedValue( Node: PVirtualNode; eumeratedValue: PSVD_EnumeratedValue );
var
  Data: PVST_MainData;
begin
  Node := VST_Main.GetFirstChild( Node );
  while Assigned( Node ) do
  begin
    Data := VST_Main.GetNodeData( Node );

    if Data.name = 'derivedFrom' then
    begin
      eumeratedValue.DerivedFrom := Data.value;

    end else if Data.name = 'name' then
      eumeratedValue.name := Data.value
    else if Data.name = 'description' then
      eumeratedValue.Description := Data.value
    else if Data.name = 'value' then
      eumeratedValue.value := Str2Int( Data.value )
    else if Data.name = 'isDefault' then
      eumeratedValue.isDefault := Data.value;

    Node := VST_Main.GetNextSibling( Node );
  end;

end;

procedure TMainForm.ClearDevice;
begin
  SetLength( SVD_Device.peripherals.peripheralArray, 0 );
  SVD_Device.cpu := Default ( TSVD_CPU );
  SVD_Device := Default ( TSVD_Device );
end;

procedure TMainForm.DecodeEnumeratedValues( Node: PVirtualNode; eumeratedValues: PSVD_EnumeratedValues );
var
  Data: PVST_MainData;
  eumeratedValueCapacity: Cardinal;
begin
  eumeratedValueCapacity := 128;
  SetLength( eumeratedValues.enumeratedValueArray, eumeratedValueCapacity );
  Node := VST_Main.GetFirstChild( Node );
  while Assigned( Node ) do
  begin
    Data := VST_Main.GetNodeData( Node );

    if Data.name = 'enumeratedValue' then
    begin
      eumeratedValues.enumeratedValueArray[ eumeratedValues.enumeratedValueCount ].enumeratedValues := eumeratedValues;
      DecodeEnumeratedValue( Node,
        Addr( eumeratedValues.enumeratedValueArray[ eumeratedValues.enumeratedValueCount ] ) );
      if eumeratedValues.enumeratedValueArray[ eumeratedValues.enumeratedValueCount ].enumeratedValues <> nil then
        TArrayHelper< TSVD_EnumeratedValue >.Increment( eumeratedValues.enumeratedValueArray,
          eumeratedValues.enumeratedValueCount, eumeratedValueCapacity );
    end;

    Node := VST_Main.GetNextSibling( Node );
  end;

  SetLength( eumeratedValues.enumeratedValueArray, eumeratedValues.enumeratedValueCount );
end;

procedure TMainForm.DecodeField( Node: PVirtualNode; field: PSVD_Field );
var
  Data: PVST_MainData;
begin

  Node := VST_Main.GetFirstChild( Node );
  while Assigned( Node ) do
  begin
    Data := VST_Main.GetNodeData( Node );

    if Data.name = 'derivedFrom' then
    begin
      field.DerivedFrom := Data.value;
    end else if Data.name = 'name' then
      field.name := Data.value
    else if Data.name = 'description' then
      field.Description := Data.value

    else if Data.name = 'dim' then
      field.dimElement.dim := Str2Int( Data.value )
    else if Data.name = 'dimIncrement' then
      field.dimElement.dimIncrement := Str2Int( Data.value )
    else if Data.name = 'dimIndex' then
      field.dimElement.dimIndex := Data.value
    else if Data.name = 'dimArrayIndex' then
      field.dimElement.dimArrayIndex := Data.value

    else if Data.name = 'bitOffset' then
    begin
      field.bitRange.bitRangeType := brOffsetWidth;
      field.bitRange.bitRangeOffsetWidth.bitOffset := Str2Int( Data.value )
    end else if Data.name = 'bitWidth' then
    begin
      field.bitRange.bitRangeType := brOffsetWidth;
      field.bitRange.bitRangeOffsetWidth.bitWidth := Str2Int( Data.value );
    end else if Data.name = 'lsb' then
    begin
      field.bitRange.bitRangeType := brLsbMsb;
      field.bitRange.bitRangeLsbMsb.lsb := Str2Int( Data.value )
    end else if Data.name = 'msb' then
    begin
      field.bitRange.bitRangeType := brLsbMsb;
      field.bitRange.bitRangeLsbMsb.msb := Str2Int( Data.value )
    end else if Data.name = 'bitRange' then
    begin
      field.bitRange.bitRangeType := brString;
      field.bitRange.bitRangeString := Data.value;
    end else if Data.name = 'enumeratedValues' then
    begin
      field.enumeratedValues.field := field;
      DecodeEnumeratedValues( Node, Addr( field.enumeratedValues ) );
    end;

    Node := VST_Main.GetNextSibling( Node );
  end;

end;

procedure TMainForm.DecodeFields( Node: PVirtualNode; fields: PSVD_Fields );
var
  Data: PVST_MainData;
  fieldCapacity: Cardinal;
begin
  fieldCapacity := 128;
  SetLength( fields.fieldArray, fieldCapacity );

  Node := VST_Main.GetFirstChild( Node );
  while Assigned( Node ) do
  begin
    Data := VST_Main.GetNodeData( Node );

    if Data.name = 'field' then
    begin
      fields.fieldArray[ fields.fieldCount ].fields := fields;
      DecodeField( Node, Addr( fields.fieldArray[ fields.fieldCount ] ) );
      if fields.fieldArray[ fields.fieldCount ].fields <> nil then
        TArrayHelper< TSVD_Field >.Increment( fields.fieldArray, fields.fieldCount, fieldCapacity );
    end;

    Node := VST_Main.GetNextSibling( Node );
  end;

  SetLength( fields.fieldArray, fields.fieldCount );
end;

procedure TMainForm.DecodeRegister( Node: PVirtualNode; _register: PSVD_Register );
var
  Data: PVST_MainData;
begin

  Node := VST_Main.GetFirstChild( Node );
  while Assigned( Node ) do
  begin
    Data := VST_Main.GetNodeData( Node );
    if Data.name = 'derivedFrom' then
    begin
      _register.DerivedFrom := Data.value;
    end else if Data.name = 'name' then
      _register.name := Data.value
    else if Data.name = 'displayName' then
      _register.displayName := Data.value
    else if Data.name = 'description' then
      _register.Description := Data.value
    else if Data.name = 'addressOffset' then
      _register.addressOffset := Str2Int( Data.value )
    else if Data.name = 'alternateGroup' then
      _register.alternateGroup := Data.value
    else if Data.name = 'alternateRegister' then
      _register.alternateRegister := Data.value
    else if Data.name = 'dataType' then
      _register.dataType := Data.value

    else if Data.name = 'size' then
      _register.registerProperties.Size := Str2Int( Data.value )
    else if Data.name = 'resetMask' then
      _register.registerProperties.resetMask := Str2Int( Data.value )
    else if Data.name = 'resetValue' then
      _register.registerProperties.resetValue := Str2Int( Data.value )
    else if Data.name = 'access' then
      _register.registerProperties.access := Data.value
    else if Data.name = 'protection' then
      _register.registerProperties.protection := Data.value

    else if Data.name = 'dim' then
      _register.dimElement.dim := Str2Int( Data.value )
    else if Data.name = 'dimIncrement' then
      _register.dimElement.dimIncrement := Str2Int( Data.value )
    else if Data.name = 'dimIndex' then
      _register.dimElement.dimIndex := Data.value
    else if Data.name = 'dimArrayIndex' then
      _register.dimElement.dimArrayIndex := Data.value

    else if Data.name = 'fields' then
    begin
      _register.fields._register := _register;
      DecodeFields( Node, Addr( _register.fields ) );
    end;

    Node := VST_Main.GetNextSibling( Node );
  end;

end;

procedure TMainForm.DecodeCluster( Node: PVirtualNode; cluster: PSVD_Cluster );
var
  I: Integer;
  Data: PVST_MainData;
  registerCapacity: Cardinal;
  clusterCapacity: Cardinal;
  derivedFromFound: Boolean;
begin
  registerCapacity := 128;
  SetLength( cluster.registerArray, registerCapacity );

  clusterCapacity := 128;
  SetLength( cluster.clusterArray, clusterCapacity );

  Node := VST_Main.GetFirstChild( Node );
  while Assigned( Node ) do
  begin
    Data := VST_Main.GetNodeData( Node );
    if Data.name = 'derivedFrom' then
    begin
      derivedFromFound := False;
      if cluster.registers <> nil then // cluster in regiters
      begin
        for I := 0 to cluster.registers.clusterCount - 1 do // Exclude Self
        begin

        end;

      end else if cluster.cluster <> nil then
      begin // cluster in cluster

      end;

      for I := 0 to cluster.registers.clusterCount - 1 do // Exclude Self
      begin
        if cluster.cluster.clusterArray[ I ].registers = nil then // Invalid cluster
          Continue;

        if cluster.cluster.clusterArray[ I ].cluster = nil then // Invalid cluster
          Continue;

        if Data.value = cluster.cluster.clusterArray[ I ].name then // Parent Found
        begin
          derivedFromFound := True;
          // Values are inherit from derivedFrom Peripheral
          // MemCpy( peripheral, peripheral.peripherals.peripheralArray[ I ], sizeof( TSVD_Peripheral ) )
          cluster^ := cluster.cluster.clusterArray[ I ];
          cluster.DerivedFrom := Data.value;
          break;
        end;
      end;

      if not derivedFromFound then
      begin
        cluster.cluster := nil; // Mark this peripheral as Invalid
        Exit;
      end
      // Elements specified underneath will override inherited values.
    end else if Data.name = 'name' then
      cluster.name := Data.value
    else if Data.name = 'displayName' then
      cluster.displayName := Data.value
    else if Data.name = 'description' then
      cluster.Description := Data.value
    else if Data.name = 'alternateCluster' then
      cluster.alternateCluster := Data.value
    else if Data.name = 'addressOffset' then
      cluster.addressOffset := Str2Int( Data.value )

    else if Data.name = 'size' then
      cluster.registerProperties.Size := Str2Int( Data.value )
    else if Data.name = 'resetMask' then
      cluster.registerProperties.resetMask := Str2Int( Data.value )
    else if Data.name = 'resetValue' then
      cluster.registerProperties.resetValue := Str2Int( Data.value )
    else if Data.name = 'access' then
      cluster.registerProperties.access := Data.value
    else if Data.name = 'protection' then
      cluster.registerProperties.protection := Data.value

    else if Data.name = 'dim' then
      cluster.dimElement.dim := Str2Int( Data.value )
    else if Data.name = 'dimIncrement' then
      cluster.dimElement.dimIncrement := Str2Int( Data.value )
    else if Data.name = 'dimIndex' then
      cluster.dimElement.dimIndex := Data.value
    else if Data.name = 'dimArrayIndex' then
      cluster.dimElement.dimArrayIndex := Data.value

    else if Data.name = 'register' then
    begin
      cluster.registerArray[ cluster.registerCount ].registers := nil;
      cluster.registerArray[ cluster.registerCount ].cluster := cluster;
      DecodeRegister( Node, Addr( cluster.registerArray[ cluster.registerCount ] ) );
      TArrayHelper< TSVD_Register >.Increment( cluster.registerArray, cluster.registerCount, registerCapacity );
    end else if Data.name = 'cluster' then
    begin
      cluster.clusterArray[ cluster.clusterCount ].registers := nil;
      cluster.clusterArray[ cluster.clusterCount ].cluster := cluster;
      DecodeCluster( Node, Addr( cluster.clusterArray[ cluster.clusterCount ] ) );
      TArrayHelper< TSVD_Cluster >.Increment( cluster.clusterArray, cluster.clusterCount, clusterCapacity );
    end;

    Node := VST_Main.GetNextSibling( Node );
  end;

  SetLength( cluster.registerArray, cluster.registerCount );
  SetLength( cluster.clusterArray, cluster.clusterCount );
end;

procedure TMainForm.DecodeRegisters( Node: PVirtualNode; registers: PSVD_Registers );
var
  Data: PVST_MainData;
  registerCapacity: Cardinal;
  clusterCapacity: Cardinal;
begin
  registerCapacity := 128;
  SetLength( registers.registerArray, registerCapacity );

  clusterCapacity := 128;
  SetLength( registers.clusterArray, clusterCapacity );

  Node := VST_Main.GetFirstChild( Node );
  while Assigned( Node ) do
  begin
    Data := VST_Main.GetNodeData( Node );
    if Data.name = 'register' then
    begin
      registers.registerArray[ registers.registerCount ].registers := registers;
      registers.registerArray[ registers.registerCount ].cluster := nil;
      DecodeRegister( Node, Addr( registers.registerArray[ registers.registerCount ] ) );
      TArrayHelper< TSVD_Register >.Increment( registers.registerArray, registers.registerCount, registerCapacity );
    end else if Data.name = 'cluster' then
    begin
      registers.clusterArray[ registers.clusterCount ].registers := registers;
      registers.clusterArray[ registers.clusterCount ].cluster := nil;

      DecodeCluster( Node, Addr( registers.clusterArray[ registers.clusterCount ] ) );
      TArrayHelper< TSVD_Cluster >.Increment( registers.clusterArray, registers.clusterCount, clusterCapacity );
    end;

    Node := VST_Main.GetNextSibling( Node );
  end;

  SetLength( registers.registerArray, registers.registerCount );
  SetLength( registers.clusterArray, registers.clusterCount );
end;

procedure TMainForm.DecodeAddressBlock( Node: PVirtualNode; addressBlock: PSVD_AddressBlock );
var
  Data: PVST_MainData;
begin
  Node := VST_Main.GetFirstChild( Node );
  while Assigned( Node ) do
  begin
    Data := VST_Main.GetNodeData( Node );
    if Data.name = 'offset' then
      addressBlock.Offset := Str2Int( Data.value )
    else if Data.name = 'size' then
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
    if Data.name = 'name' then
      interrupt.name := Data.value
    else if Data.name = 'description' then
      interrupt.Description := Data.value
    else if Data.name = 'value' then
      interrupt.value := Str2Int( Data.value );

    Node := VST_Main.GetNextSibling( Node );
  end;

end;

procedure TMainForm.DecodePeripheral( Node: PVirtualNode; peripheral: PSVD_Peripheral );
var
  I: Integer;
  Data: PVST_MainData;
  derivedFromFound: Boolean;
  addressBlockCapacity: Cardinal;
begin
  addressBlockCapacity := 8;
  SetLength( peripheral.addressBlockArray, addressBlockCapacity );

  Node := VST_Main.GetFirstChild( Node );
  while Assigned( Node ) do
  begin
    Data := VST_Main.GetNodeData( Node );
    if Data.name = 'derivedFrom' then
    begin
      derivedFromFound := False;
      for I := 0 to peripheral.peripherals.peripheralCount - 1 do // Exclude Self
      begin
        if peripheral.peripherals.peripheralArray[ I ].peripherals = nil then // Invalid peripheral
          Continue;

        if Data.value = peripheral.peripherals.peripheralArray[ I ].name then // Parent Found
        begin
          derivedFromFound := True;
          // Values are inherit from derivedFrom Peripheral
          // MemCpy( peripheral, peripheral.peripherals.peripheralArray[ I ], sizeof( TSVD_Peripheral ) )
          peripheral^ := peripheral.peripherals.peripheralArray[ I ];
          peripheral.DerivedFrom := Data.value;
          break;
        end;
      end;

      if not derivedFromFound then
      begin
        peripheral.peripherals := nil; // Mark this peripheral as Invalid
        Exit;
      end;
      // Elements specified underneath will override inherited values.
    end else if Data.name = 'name' then
      peripheral.name := Data.value
    else if Data.name = 'version' then
      peripheral.version := Data.value
    else if Data.name = 'description' then
      peripheral.Description := Data.value
    else if Data.name = 'alternatePeripheral' then
      peripheral.alternatePeripheral := Data.value
    else if Data.name = 'prependToName' then
      peripheral.prependToName := Data.value
    else if Data.name = 'appendToName' then
      peripheral.appendToName := Data.value
    else if Data.name = 'baseAddress' then
      peripheral.baseAddress := Str2Int( Data.value )

    else if Data.name = 'size' then
      peripheral.registerProperties.Size := Str2Int( Data.value )
    else if Data.name = 'resetMask' then
      peripheral.registerProperties.resetMask := Str2Int( Data.value )
    else if Data.name = 'resetValue' then
      peripheral.registerProperties.resetValue := Str2Int( Data.value )
    else if Data.name = 'access' then
      peripheral.registerProperties.access := Data.value
    else if Data.name = 'protection' then
      peripheral.registerProperties.protection := Data.value

    else if Data.name = 'dim' then
      peripheral.dimElement.dim := Str2Int( Data.value )
    else if Data.name = 'dimIncrement' then
      peripheral.dimElement.dimIncrement := Str2Int( Data.value )
    else if Data.name = 'dimIndex' then
      peripheral.dimElement.dimIndex := Data.value
    else if Data.name = 'dimArrayIndex' then
      peripheral.dimElement.dimArrayIndex := Data.value

    else if Data.name = 'interrupt' then
      DecodeInterrupt( Node, Addr( peripheral.interrupt ) )
    else if Data.name = 'addressBlock' then
    begin
      DecodeAddressBlock( Node, Addr( peripheral.addressBlockArray[ peripheral.addressBlockCount ] ) );
      TArrayHelper< TSVD_AddressBlock >.Increment( peripheral.addressBlockArray, peripheral.addressBlockCount,
        addressBlockCapacity );
    end else if Data.name = 'registers' then
    begin
      peripheral.registers.peripheral := peripheral;
      DecodeRegisters( Node, Addr( peripheral.registers ) );
    end;

    Node := VST_Main.GetNextSibling( Node );
  end;

  SetLength( peripheral.addressBlockArray, peripheral.addressBlockCount );
end;

procedure TMainForm.DecodePeripherals( Node: PVirtualNode; peripherals: PSVD_Peripherals );
var
  Data: PVST_MainData;
  peripheralCapacity: Cardinal;
begin
  peripheralCapacity := 128;
  SetLength( peripherals.peripheralArray, peripheralCapacity );

  Node := VST_Main.GetFirstChild( Node );
  while Assigned( Node ) do
  begin
    Data := VST_Main.GetNodeData( Node );
    if Data.name = 'peripheral' then
    begin
      peripherals.peripheralArray[ peripherals.peripheralCount ].peripherals := peripherals;
      DecodePeripheral( Node, Addr( peripherals.peripheralArray[ peripherals.peripheralCount ] ) );

      if peripherals.peripheralArray[ peripherals.peripheralCount ].peripherals <> nil then
      begin
        // peripherals.peripheralArray[ peripherals.peripheralCount ].derivedFrom <> nil
        // and derivedFrom is Found, Add current peripheral to peripheralArray, Update peripheralCount
        TArrayHelper< TSVD_Peripheral >.Increment( peripherals.peripheralArray, peripherals.peripheralCount,
          peripheralCapacity );
      end else begin
        // Ignore this peripheral, peripheralCount is not updated
      end;
    end;

    Node := VST_Main.GetNextSibling( Node );
  end;

  SetLength( peripherals.peripheralArray, peripherals.peripheralCount );
end;

procedure TMainForm.DecodeCPU( Node: PVirtualNode; cpu: PSVD_CPU );
var
  Data: PVST_MainData;
begin
  Node := VST_Main.GetFirstChild( Node );
  while Assigned( Node ) do
  begin
    Data := VST_Main.GetNodeData( Node );
    if Data.name = 'name' then
      cpu.name := Data.value
    else if Data.name = 'revision' then
      cpu.revision := Data.value
    else if Data.name = 'endian' then
      cpu.endian := Data.value
    else if Data.name = 'itcmPresent' then
      cpu.itcmPresent := Data.value
    else if Data.name = 'dtcmPresent' then
      cpu.dtcmPresent := Data.value
    else if Data.name = 'icachePresent' then
      cpu.icachePresent := Data.value
    else if Data.name = 'dcachePresent' then
      cpu.dcachePresent := Data.value
    else if Data.name = 'mpuPresent' then
      cpu.mpuPresent := Data.value
    else if Data.name = 'fpuPresent' then
      cpu.fpuPresent := Data.value
    else if Data.name = 'fpuDP' then
      cpu.fpuDP := Data.value
    else if Data.name = 'vtorPresent' then
      cpu.vtorPresent := Data.value
    else if Data.name = 'vendorSystickConfig' then
      cpu.vendorSystickConfig := Data.value

    else if Data.name = 'sauNumRegions' then
      cpu.sauNumRegions := Str2Int( Data.value )
    else if Data.name = 'nvicPrioBits' then
      cpu.nvicPrioBits := Str2Int( Data.value )
    else if Data.name = 'deviceNumInterrupts' then
      cpu.deviceNumInterrupts := Str2Int( Data.value );

    Node := VST_Main.GetNextSibling( Node );
  end;
end;

procedure TMainForm.DecodeDevice0( );
var
  Data: PVST_MainData;
  peripheralCapacity: Cardinal;
  I: Integer;
  J: Integer;
  K: Integer;
  L: Integer;
begin
  ClearDevice( );

  peripheralCapacity := 4;
  SetLength( SVD_Device.peripherals.peripheralArray, peripheralCapacity );
  for I := 0 to 3 do
  begin
    SVD_Device.peripherals.peripheralArray[ I ].name := 'Peripheral';
    SetLength( SVD_Device.peripherals.peripheralArray[ I ].registers.registerArray, 4 );
    for J := 0 to 3 do
    begin
      SVD_Device.peripherals.peripheralArray[ I ].registers.registerArray[ J ].name := 'Register';
      SetLength( SVD_Device.peripherals.peripheralArray[ I ].registers.registerArray[ J ].fields.fieldArray, 4 );
      for K := 0 to 3 do
      begin
        SVD_Device.peripherals.peripheralArray[ I ].registers.registerArray[ J ].fields.fieldArray[ K ].name := 'Field';
        SetLength( SVD_Device.peripherals.peripheralArray[ I ].registers.registerArray[ J ].fields.fieldArray[ K ]
          .enumeratedValues.enumeratedValueArray, 4 );
        for L := 0 to 3 do
        begin
          SVD_Device.peripherals.peripheralArray[ I ].registers.registerArray[ J ].fields.fieldArray[ K ]
            .enumeratedValues.enumeratedValueArray[ L ].name := 'enumeratedValue';
        end;
      end;
    end;
  end;
end;

procedure TMainForm.DecodeDevice( );
var
  Data: PVST_MainData;
  Node: PVirtualNode;
begin
  ClearDevice( );

  Node := VST_Main.GetFirstChild( nil );
  while Assigned( Node ) do
  begin
    Data := VST_Main.GetNodeData( Node );
    if Data.name = 'device' then
    begin
      Node := VST_Main.GetFirstChild( Node );
      while Assigned( Node ) do
      begin
        Data := VST_Main.GetNodeData( Node );
        if Data.name = 'name' then
          SVD_Device.name := Data.value
        else if Data.name = 'vendor' then
          SVD_Device.vendor := Data.value
        else if Data.name = 'vendorID' then
          SVD_Device.vendorID := Data.value
        else if Data.name = 'series' then
          SVD_Device.series := Data.value
        else if Data.name = 'version' then
          SVD_Device.version := Data.value
        else if Data.name = 'description' then
          SVD_Device.Description := Data.value
        else if Data.name = 'addressUnitBits' then
          SVD_Device.addressUnitBits := Str2Int( Data.value )
        else if Data.name = 'width' then
          SVD_Device.Width := Str2Int( Data.value )

        else if Data.name = 'size' then
          SVD_Device.registerProperties.Size := Str2Int( Data.value )
        else if Data.name = 'resetMask' then
          SVD_Device.registerProperties.resetMask := Str2Int( Data.value )
        else if Data.name = 'resetValue' then
          SVD_Device.registerProperties.resetValue := Str2Int( Data.value )
        else if Data.name = 'access' then
          SVD_Device.registerProperties.access := Data.value
        else if Data.name = 'protection' then
          SVD_Device.registerProperties.protection := Data.value

        else if Data.name = 'cpu' then
          DecodeCPU( Node, Addr( SVD_Device.cpu ) )

        else if Data.name = 'peripherals' then
        begin
          SVD_Device.peripherals.Device := Addr( SVD_Device );
          DecodePeripherals( Node, Addr( SVD_Device.peripherals ) );
        end;

        Node := VST_Main.GetNextSibling( Node );
      end;

      Node := VST_Main.GetNextSibling( Node );
    end;

    SVD_DeviceDecoded := True;
  end;
end;

procedure TMainForm.btnMakeScriptClick( Sender: TObject );
var
  I: Integer;
  StringList: TStringList;
begin
  if not SVD_DeviceDecoded then
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
