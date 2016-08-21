unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, ShellAPI, NativeXml,
  Dialogs, StdCtrls, VirtualTrees, Vcl.ComCtrls, Vcl.ToolWin, Vcl.ExtCtrls, System.ImageList, Vcl.ImgList, Vcl.Menus;

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
    btnDecodeSVD: TToolButton;
    btnMakeScript: TToolButton;
    btnSaveScript: TToolButton;
    Panel4: TPanel;
    VST_Main: TVirtualStringTree;
    Splitter3: TSplitter;
    VST_Segment: TVirtualStringTree;
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
    procedure FormCreate( Sender: TObject );
    procedure btnLoadSVDClick( Sender: TObject );
    procedure Exit1Click( Sender: TObject );
    procedure About1Click( Sender: TObject );
    procedure btnCheckSVDClick( Sender: TObject );
    procedure StatusBarDrawPanel( StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect );
    procedure btnGotoFirstClick( Sender: TObject );
    procedure btnGotoPrevClick( Sender: TObject );
    procedure btnGotoNextClick( Sender: TObject );
    procedure btnGotoLastClick( Sender: TObject );
    procedure btnSaveSVDClick( Sender: TObject );
    procedure btnDecodeSVDClick( Sender: TObject );
    procedure VST_MainGetText( Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string );
    procedure VST_MainGetNodeDataSize( Sender: TBaseVirtualTree; var NodeDataSize: Integer );
    procedure VST_SegmentGetNodeDataSize( Sender: TBaseVirtualTree; var NodeDataSize: Integer );
    procedure VST_InterruptGetNodeDataSize( Sender: TBaseVirtualTree; var NodeDataSize: Integer );
    procedure VST_SegmentGetText( Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string );
    procedure VST_InterruptGetText( Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string );
    procedure FormDestroy( Sender: TObject );
    procedure VST_MainFreeNode( Sender: TBaseVirtualTree; Node: PVirtualNode );
    procedure VST_SegmentFreeNode( Sender: TBaseVirtualTree; Node: PVirtualNode );
    procedure VST_InterruptFreeNode( Sender: TBaseVirtualTree; Node: PVirtualNode );
    procedure btnMakeScriptClick( Sender: TObject );
    procedure btnSaveScriptClick( Sender: TObject );
    procedure btnExpandTreeClick( Sender: TObject );
    procedure btnCollapseTreeClick( Sender: TObject );
    procedure PagesChange( Sender: TObject );
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
    //
    // Check SVDFile Functions
    //
    procedure UpdataSVDFileCheckResult;
    procedure GotoLineNumberOfSVDFile;
    //
    // Decode Segments and Interrupts from VST_Main
    //
    function DecodeInterrupts: Boolean;
    function DecodeSegments: Boolean;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}
{ $DEFINE XML_DEBUG }

uses uAbout, uExecAndCapture, uMisc;

type
  TSVDErrorWarningLine = record
    SVDFileLine: Integer;
    SVDLogLine: Integer;
  end;

type
  TXmlNodeInfo = record
    NodeType: TsdElementType;
    TypeName: String;
    Name: String;
    Value: String;
    Depth: Integer;
  end;

type
  PVST_MainData = ^TVST_MainData;

  // Free Name onFreeNode()
  TVST_MainData = record
    Name: String;
    Value: String;
  end;

  PVST_IrqData = ^TVST_IrqData;

  // Free Name onFreeNode()
  TVST_IrqData = record
    Name: String;
    Description: String;
    Number: String;
  end;

  PVST_SegData = ^TVST_SegData;

  // Free Name onFreeNode()
  TVST_SegData = record
    Name: String;
    Description: String;
    BaseAddr: String;
    Offset: String;
    Size: String;
    DerivedFrom: String;
  end;

type
  TSVD_SegInfo = record
    Node: PVirtualNode;
    Name: String;
  end;

  TSVD_SegData = record
    Name: String;
    Description: String;
    BaseAddr: Cardinal;
    Offset: Cardinal;
    Size: Cardinal;
  end;

  TSVD_IrqData = record
    Name: String;
    Description: String;
    Number: Cardinal;
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

  XmlNodeTypeName: array [ xeElement .. xeError ] of string = ( 'xeElement', 'xeAttribute', 'xeComment', 'xeCData',
    'xeCondSection', 'xeDeclaration', 'xeStylesheet', 'xeDocType', 'xeDtdElement', 'xeDtdAttList', 'xeDtdEntity',
    'xeDtdNotation', 'xeInstruction', 'xeCharData', 'xeWhiteSpace', 'xeQuotedText', 'xeUnknown', 'xeEndTag',
    'xeError' );

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

  XmlDoc: TNativeXml;

  ActiveNode: PVirtualNode;
  SavedNode: PVirtualNode;

  SVD_SegInfoArray: array of TSVD_SegInfo;
  SVD_SegInfoCount: Cardinal;
  SVD_DecodeDone: Boolean;

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

  ShellExecute( Handle, nil, ST3ExeName, pChar( SVDTempFileName + ':' + IntToStr( LineNumber ) ), nil, SW_SHOWNORMAL );
end;

procedure TMainForm.PagesChange( Sender: TObject );
begin
  // ProgressBar.Visible := Pages.ActivePageIndex = 1;
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
      end
      else if Panel.Index = 1 then // Warning
      begin
        Font.Color := clBlue;
      end
      else if Panel.Index = 2 then // Info
      begin
        Font.Color := clBlack;
      end
      else // Panel.Index = 3
      begin
        Font.Color := clBlack;
      end;

      FillRect( RectForText );
      TextOut( RectForText.Left + 5, RectForText.Top + 2, Panel.Text );
      // DrawText( StatusBar.Canvas.Handle, PChar( Panel.Text ), -1,
      // RectForText, DT_SINGLELINE or DT_VCENTER or DT_LEFT );
    end;
  end
  else if Pages.ActivePageIndex = 1 then
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
      end
      else
      begin
        FillRect( RectForText );
        TextOut( RectForText.Left + 5, RectForText.Top + 2, Panel.Text );
        // DrawText( StatusBar.Canvas.Handle, PChar( Panel.Text ), -1,
        // RectForText, DT_SINGLELINE or DT_VCENTER or DT_LEFT );
        Exit;
      end;
    end;
  end;
end;

procedure TMainForm.btnExpandTreeClick( Sender: TObject );
begin
  VST_Main.FullExpand( );
  VST_Segment.FullExpand( );
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
// Input File Name '6C79A68C-1945-48BB-A20B-4FACA9A46231' does not match the tag <name> in the <device> section:
//
procedure TMainForm.UpdataSVDFileCheckResult;
var
  i: Integer;
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

  for i := 0 to MemoSVDLog.Lines.Count - 1 do
  begin
    if Pos( SVDFileErrorStr, MemoSVDLog.Lines[ i ] ) > 0 then
    begin
      SVDErrorCount := SVDErrorCount + 1;
      SVDErrorWarningLineArray[ LineArryIndex ].SVDLogLine := i;
      SVDErrorWarningLineArray[ LineArryIndex ].SVDFileLine := GetLineNumber( i );
      Inc( SVDErrorWarningLineArrayCount );
      Inc( LineArryIndex );
      if LineArryIndex = LineArryCapacity then
      begin
        LineArryCapacity := LineArryCapacity + LineArryCapacity div 2;
        SetLength( SVDErrorWarningLineArray, LineArryCapacity );
      end;
    end;

    if Pos( SVDFileWarningStr, MemoSVDLog.Lines[ i ] ) > 0 then
    begin
      // *** WARNING M223: A:\Temp\6C79A68C-1945-48BB-A20B-4FACA9A46231.svd (Line 36)
      // Input File Name '6C79A68C-1945-48BB-A20B-4FACA9A46231'
      if Pos( SVDTempFileNameOnly, MemoSVDLog.Lines[ i + 1 ] ) > 0 then
        Continue;

      SVDWarningCount := SVDWarningCount + 1;
      SVDErrorWarningLineArray[ LineArryIndex ].SVDLogLine := i;
      SVDErrorWarningLineArray[ LineArryIndex ].SVDFileLine := GetLineNumber( i );
      Inc( SVDErrorWarningLineArrayCount );
      Inc( LineArryIndex );
      if LineArryIndex = LineArryCapacity then
      begin
        LineArryCapacity := LineArryCapacity + LineArryCapacity div 2;
        SetLength( SVDErrorWarningLineArray, LineArryCapacity );
      end;
    end;

    if Pos( SVDFileInfoStr, MemoSVDLog.Lines[ i ] ) > 0 then
      SVDInfoCount := SVDInfoCount + 1;
  end;

end;

procedure TMainForm.VST_MainFreeNode( Sender: TBaseVirtualTree; Node: PVirtualNode );
var
  MainData: PVST_MainData;
begin
  MainData := VST_Main.GetNodeData( Node );
  MainData.Name := '';
  MainData.Value := '';
end;

procedure TMainForm.VST_MainGetNodeDataSize( Sender: TBaseVirtualTree; var NodeDataSize: Integer );
begin
  NodeDataSize := Sizeof( TVST_MainData );
end;

procedure TMainForm.VST_MainGetText( Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string );
var
  DataAll: PVST_MainData;
begin
  DataAll := VST_Main.GetNodeData( Node );
  if Column = 0 then
    CellText := DataAll.Name
  else
    CellText := DataAll.Value;
end;

procedure TMainForm.VST_InterruptFreeNode( Sender: TBaseVirtualTree; Node: PVirtualNode );
var
  IrqData: PVST_IrqData;
begin
  IrqData := VST_Main.GetNodeData( Node );
  IrqData.Name := '';
  IrqData.Number := '';
  IrqData.Description := '';
end;

procedure TMainForm.VST_InterruptGetNodeDataSize( Sender: TBaseVirtualTree; var NodeDataSize: Integer );
begin
  NodeDataSize := Sizeof( TVST_IrqData );
end;

procedure TMainForm.VST_InterruptGetText( Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string );
var
  DataInterrupt: PVST_IrqData;
begin
  DataInterrupt := VST_Main.GetNodeData( Node );
  if Column = 0 then
    CellText := DataInterrupt.Name
  else if Column = 1 then
    CellText := DataInterrupt.Number
  else
    CellText := DataInterrupt.Description;
end;

procedure TMainForm.VST_SegmentFreeNode( Sender: TBaseVirtualTree; Node: PVirtualNode );
var
  SegData: PVST_SegData;
begin
  SegData := VST_Main.GetNodeData( Node );
  SegData.Name := '';
  SegData.Size := '';
  SegData.Offset := '';
  SegData.BaseAddr := '';
  SegData.Description := '';
  SegData.DerivedFrom := '';
end;

procedure TMainForm.VST_SegmentGetNodeDataSize( Sender: TBaseVirtualTree; var NodeDataSize: Integer );
begin
  NodeDataSize := Sizeof( TVST_SegData );
end;

procedure TMainForm.VST_SegmentGetText( Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string );
var
  DataSegment: PVST_SegData;
begin
  DataSegment := VST_Main.GetNodeData( Node );
  if Column = 0 then
    CellText := DataSegment.Name
  else if Column = 1 then
    CellText := DataSegment.BaseAddr
  else if Column = 2 then
    CellText := DataSegment.Offset
  else if Column = 3 then
    CellText := DataSegment.Size
  else
    CellText := DataSegment.Description;
end;

function Indent( ACount: Integer ): string;
begin
  while ACount > 0 do
  begin
    Result := Result + '  ';
    dec( ACount );
  end;
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
  iNode.Value := ANode.Value;
  iNode.Depth := ANode.TreeDepth;

{$IFDEF XML_DEBUG}
  MemoIDC.Lines.Add( Format( '%sOnNewNode Depth=%d, Type=%s, Name=%s, Value=%s', //
    [ Indent( iNode.Depth ), iNode.Depth, iNode.TypeName, iNode.Name, iNode.Value ] ) );
{$ENDIF}
  //
  if ( ActiveNode = nil ) and ( ANode.ElementType = xeElement ) then
  begin
    ActiveNode := VST_Main.AddChild( nil );
    MainData := VST_Main.GetNodeData( ActiveNode );
    MainData.Name := iNode.Name;
    MainData.Value := '';
    Exit;
  end;

  if ActiveNode = nil then
    Exit;

  if ANode.ElementType = xeElement then
  begin
    ActiveNode := VST_Main.AddChild( ActiveNode );
    MainData := VST_Main.GetNodeData( ActiveNode );
    MainData.Name := iNode.Name;
    MainData.Value := 'Expand to see more information';
  end
  else if ANode.ElementType = xeAttribute then
  begin
    ActiveNode := VST_Main.AddChild( ActiveNode );
    MainData := VST_Main.GetNodeData( ActiveNode );
    MainData.Name := 'Attribute name to be set';
    MainData.Value := 'Attribute value To be set';
  end
  else if ANode.ElementType = xeCharData then
  begin
    if False then
    begin
      ActiveNode := VST_Main.AddChild( ActiveNode );
      MainData := VST_Main.GetNodeData( ActiveNode );
      MainData.Name := iNode.Name;
      MainData.Value := iNode.Value;
    end;
  end;

{$IFDEF XML_DEBUG}
  MemoIDC.Lines.Add( Format( '<<<<OnNewNode OldNode 0x%.8x, NewNode 0x%.8x', //
    [ DWORD( SavedMainNode ), DWORD( MainNode ) ] ) );
  MemoIDC.Lines.Add( '' );
{$ENDIF}
end;

procedure TMainForm.XmlProgress( Sender: TObject; Position: int64 );
begin
  ProgressBarPosition := Position;
  ProgressBar.Position := ProgressBarPosition;
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
  iNode.Value := ANode.Value;
  iNode.Depth := ANode.TreeDepth;

{$IFDEF XML_DEBUG}
  MemoIDC.Lines.Add( Format( '%sOnLoaded Depth=%d, Type=%s, Name=%s, Value=%s', //
    [ Indent( iNode.Depth ), iNode.Depth, iNode.TypeName, iNode.Name, iNode.Value ] ) );
{$ENDIF}
  //
  if ActiveNode = nil then
    Exit;

  if ANode.ElementType = xeElement then
  begin
    if iNode.Depth = 0 then
    begin
      SVD_DecodeDone := True;
      ProgressBarVisible( False );
      VST_Main.EndUpdate;
    end
    else
    begin
      MainData := VST_Main.GetNodeData( ActiveNode );
      MainData.Name := iNode.Name;

      if iNode.Name = 'description' then
        MainData.Value := RemoveWhiteSpace( iNode.Value )
      else
        MainData.Value := iNode.Value;

      ActiveNode := ActiveNode.Parent;
    end;
  end
  else if ( ANode.ElementType = xeAttribute ) then
  begin
    MainData := VST_Main.GetNodeData( ActiveNode );
    MainData.Name := iNode.Name;
    MainData.Value := iNode.Value;
    ActiveNode := ActiveNode.Parent;
  end
  else if ANode.ElementType = xeCharData then
  begin
    if False then
    begin
      MainData := VST_Main.GetNodeData( ActiveNode );
      MainData.Name := iNode.Name;
      MainData.Value := iNode.Value;
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
  StatusBar.Panels[ 3 ].Text := SVDNoFileName;
  StatusBar.Invalidate;
end;

procedure TMainForm.btnMakeScriptClick( Sender: TObject );
begin
  if not SVD_DecodeDone then
    Exit;

end;

procedure TMainForm.btnSaveScriptClick( Sender: TObject );
begin
  if MemoIDC.Text = '' then
    Exit;

  DlgSave.Filter := 'IDC Script Files(*.idc)|*.idc';
  if not DlgSave.Execute then
    Exit;

  MemoIDC.Lines.SaveToFile( DlgSave.FileName );
end;

procedure TMainForm.btnSaveSVDClick( Sender: TObject );
begin
  DlgSave.Filter := 'SVD Files(*.svd)|*.svd';
  DlgSave.FileName := SVDFileName;
  if not DlgSave.Execute then
    Exit;

  CopyFile( SVDTempFileName, DlgSave.FileName );
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
  end
  else
  begin
    StatusBar.Panels[ 0 ].Text := '';
    StatusBar.Panels[ 1 ].Text := '';
    StatusBar.Panels[ 2 ].Text := '';
    StatusBar.Panels[ 3 ].Text := '';
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

procedure TMainForm.btnCollapseTreeClick( Sender: TObject );
begin
  VST_Segment.FullCollapse( );
  VST_Interrupt.FullCollapse( );
  VST_Main.FullCollapse( );
end;

procedure TMainForm.btnDecodeSVDClick( Sender: TObject );
var
  F: TFileStream;
begin
  if SVDTempFileName = '' then
    Exit;

  MemoIDC.Clear;
  MemoIDC.Invalidate;

  VST_Main.Clear;
  VST_Main.Invalidate;

  VST_Segment.Clear;
  VST_Segment.Invalidate;

  VST_Interrupt.Clear;
  VST_Interrupt.Invalidate;

  if not Assigned( XmlDoc ) then
  begin
    XmlDoc := TNativeXml.Create( Self );
    XmlDoc.OnProgress := XmlProgress;
    XmlDoc.OnNodeNew := XmlNodeNew;
    XmlDoc.OnNodeLoaded := XmlNodeLoaded;
  end;

  ActiveNode := nil;
  SVD_DecodeDone := False;
  VST_Main.BeginUpdate;
  ProgressBarVisible( True );

  // XmlDoc.LoadFromFile( SVDTempFileName );
  F := TFileStream.Create( SVDTempFileName, fmOpenRead or fmShareDenyWrite );
  ProgressBar.Max := F.Size;

  try
    XmlDoc.LoadFromStream( F );
  finally
    F.free;
  end;

  DecodeSegments( );

  btnExpandTreeClick( Self );
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

function TMainForm.DecodeInterrupts: Boolean;
var
  MainNode: PVirtualNode;
  IrqNode: PVirtualNode;

  MainData: PVST_MainData;
  IrqData: PVST_IrqData;
begin

end;

function TMainForm.DecodeSegments: Boolean;
var
  i: Integer;
  Found: Boolean;
  SVD_SegInfoCapacity: Cardinal;

  NodeData: PVST_MainData;
  SegData: TVST_SegData;
  IrqData: TVST_IrqData;

  ANode: PVirtualNode;
  BNode: PVirtualNode;
  CNode: PVirtualNode;
begin
  Found := False;
  ANode := VST_Main.GetFirstChild( nil );
  if not Assigned( ANode ) then
    Exit;
  NodeData := VST_Main.GetNodeData( ANode );
  if NodeData.Name <> 'device' then
    Exit;

  ANode := VST_Main.GetFirstChild( ANode );
  if not Assigned( ANode ) then
    Exit;

  while Assigned( ANode ) do
  begin
    NodeData := VST_Main.GetNodeData( ANode );
    if NodeData.Name = 'peripherals' then
    begin
      Found := True;
      break;
    end;
    ANode := VST_Main.GetNextSibling( ANode );
  end;

  if not Found then
    Exit;

  SVD_SegInfoCount := 0;
  SVD_SegInfoCapacity := 128;
  SetLength( SVD_SegInfoArray, SVD_SegInfoCapacity );

  ANode := VST_Main.GetFirstChild( ANode ); // peripherals.peripheral
  while Assigned( ANode ) do
  begin
    NodeData := VST_Main.GetNodeData( ANode );
    if NodeData.Name = 'peripheral' then
    begin
      SegData.Name := '';
      SegData.Size := '';
      SegData.Offset := '';
      SegData.BaseAddr := '';
      SegData.Description := '';
      SegData.DerivedFrom := '';

      IrqData.Name := '';
      IrqData.Number := '';
      IrqData.Description := '';

      BNode := VST_Main.GetFirstChild( ANode );
      while Assigned( BNode ) do
      begin
        NodeData := VST_Main.GetNodeData( BNode );
        if NodeData.Name = 'addressBlock' then
        begin
          CNode := VST_Main.GetFirstChild( BNode );
          while Assigned( CNode ) do
          begin
            NodeData := VST_Main.GetNodeData( CNode );
            if NodeData.Name = 'offset' then
              SegData.Offset := NodeData.Value
            else if NodeData.Name = 'size' then
              SegData.Size := NodeData.Value;
            CNode := VST_Main.GetNextSibling( CNode );
          end;
        end
        else if NodeData.Name = 'interrupt' then
        begin
          CNode := VST_Main.GetFirstChild( BNode );
          while Assigned( CNode ) do
          begin
            NodeData := VST_Main.GetNodeData( CNode );
            if NodeData.Name = 'name' then
              IrqData.Name := NodeData.Value
            else if NodeData.Name = 'value' then
              IrqData.Number := NodeData.Value
            else if NodeData.Name = 'description' then
              // IrqData.Description := RemoveWhiteSpace( NodeData.Value );
              IrqData.Description := NodeData.Value;

            CNode := VST_Main.GetNextSibling( CNode );
          end;
        end
        else if NodeData.Name = 'name' then
          SegData.Name := NodeData.Value
        else if NodeData.Name = 'description' then
          // SegData.Description := RemoveWhiteSpace( NodeData.Value )
          SegData.Description := NodeData.Value
        else if NodeData.Name = 'baseAddress' then
          SegData.BaseAddr := NodeData.Value
        else if NodeData.Name = 'derivedFrom' then
          SegData.DerivedFrom := NodeData.Value;

        // Find next node
        BNode := VST_Main.GetNextSibling( BNode );
      end;

      CNode := VST_Segment.AddChild( nil );

      if SegData.DerivedFrom = '' then
      begin
        SVD_SegInfoArray[ SVD_SegInfoCount ].Name := SegData.Name;
        SVD_SegInfoArray[ SVD_SegInfoCount ].Node := CNode;
        Inc( SVD_SegInfoCount );
        if SVD_SegInfoCount = SVD_SegInfoCapacity then
        begin
          // SVD_SegInfoCapacity *= 1.5
          SVD_SegInfoCapacity := SVD_SegInfoCapacity + SVD_SegInfoCapacity shr 1;
          SetLength( SVD_SegInfoArray, SVD_SegInfoCapacity );
        end;
      end
      else
      begin
        for i := 0 to SVD_SegInfoCount - 1 do
        begin
          if SegData.DerivedFrom = SVD_SegInfoArray[ i ].Name then
          begin
            NodeData := VST_Main.GetNodeData( SVD_SegInfoArray[ i ].Node );
            if SegData.BaseAddr = '' then // Never
              SegData.BaseAddr := PVST_SegData( NodeData ).BaseAddr;
            if SegData.Description = '' then // Maybe
              SegData.Description := PVST_SegData( NodeData ).Description;
            if SegData.Offset = '' then // Always ?
              SegData.Offset := PVST_SegData( NodeData ).Offset;
            if SegData.Size = '' then // Always ?
              SegData.Size := PVST_SegData( NodeData ).Size;

            break;
          end;
        end;
      end;

      // To here, SegData is ready to add to VST_Segment
      NodeData := VST_Main.GetNodeData( CNode );

      PVST_SegData( NodeData ).Name := SegData.Name;
      PVST_SegData( NodeData ).Description := SegData.Description;
      PVST_SegData( NodeData ).BaseAddr := SegData.BaseAddr;
      PVST_SegData( NodeData ).Offset := SegData.Offset;
      PVST_SegData( NodeData ).Size := SegData.Size;

      if ( IrqData.Name <> '' ) or ( IrqData.Number <> '' ) then
      begin
        CNode := VST_Interrupt.AddChild( nil );
        NodeData := VST_Main.GetNodeData( CNode );

        PVST_IrqData( NodeData ).Name := IrqData.Name;
        PVST_IrqData( NodeData ).Description := IrqData.Description;
        PVST_IrqData( NodeData ).Number := IrqData.Number;
      end;
    end;

    // Find next node named 'peripheral'
    ANode := VST_Main.GetNextSibling( ANode );
  end;
end;

end.
