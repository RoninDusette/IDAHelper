program IDAHelper;

uses
  Vcl.Forms,
  uMain in 'Source\uMain.pas' {MainForm},
  uAbout in 'Source\uAbout.pas' {AboutBox},
  uMisc in 'Common\Misc\uMisc.pas',
  NativeXml in 'Common\NativeXML\NativeXml.pas',
  NativeXmlC14n in 'Common\NativeXML\NativeXmlC14n.pas',
  NativeXmlCodepages in 'Common\NativeXML\NativeXmlCodepages.pas',
  NativeXmlNodes in 'Common\NativeXML\NativeXmlNodes.pas',
  NativeXmlObjectStorage in 'Common\NativeXML\NativeXmlObjectStorage.pas',
  sdDebug in 'Common\NativeXML\sdDebug.pas',
  sdSortedLists in 'Common\NativeXML\sdSortedLists.pas',
  sdStreams in 'Common\NativeXML\sdStreams.pas',
  sdStringTable in 'Common\NativeXML\sdStringTable.pas',
  uSVD_Type in 'Source\uSVD_Type.pas',
  uIDC_Script in 'Source\uIDC_Script.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.Run;
end.
