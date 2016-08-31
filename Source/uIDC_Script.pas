unit uIDC_Script;

interface

uses
  Winapi.Windows, Winapi.Messages, // Windows
  System.Classes, System.Variants, System.SysUtils, // System
  uSVD_Type;

// Comment

// Enum and BitField

// Struct and Union

// Segment

procedure IDC_Decode( SVD_Device: TSVD_Device; var StringList: TStringList );

implementation

var
  MainStringList: TStringList;

  {
    name           value    mask     maskname             comment
    ----------------------------------------------------------------------------------------------------------------------
    OOFS_IFSIGN    0x0000   0x0003   OOF_SIGNMASK
    OOFS_NOSIGN    0x0001   0x0003   OOF_SIGNMASK
    OOFS_NEEDSIGN  0x0002   0x0003   OOF_SIGNMASK
    ----------------------------------------------------------------------------------------------------------------------
    OOF_SIGNED     0x0004   0x0004
    ----------------------------------------------------------------------------------------------------------------------
    OOF_NUMBER     0x0008   0x0008
    ----------------------------------------------------------------------------------------------------------------------
    OOFW_IMM       0x0000   0x0030   OOF_WIDTHMASK
    OOFW_16        0x0010   0x0030   OOF_WIDTHMASK
    OOFW_32        0x0020   0x0030   OOF_WIDTHMASK
    OOFW_8         0x0030   0x0030   OOF_WIDTHMASK
    ----------------------------------------------------------------------------------------------------------------------
    OOF_ADDR       0x0040   0x0040
    OOF_OUTER      0x0080   0x0080
    OOF_ZSTROFF    0x0100   0x0100
    ----------------------------------------------------------------------------------------------------------------------
  }
procedure GetFieldPosMask( const BitRange: TSVD_BitRange; var fieldPos: Cardinal; var fieldMask: Cardinal );
var
  lsb: Cardinal;
  msb: Cardinal;
  width: Cardinal;
  Start, Middle, Stop: Integer;
begin
  lsb := 0;
  width := 0;

  case BitRange.bitRangeType of
    brOffsetWidth:
      begin
        lsb := BitRange.bitRangeOffsetWidth.bitOffset;
        width := BitRange.bitRangeOffsetWidth.bitWidth;
      end;
    brLsbMsb:
      begin
        lsb := BitRange.bitRangeLsbMsb.lsb;
        msb := BitRange.bitRangeLsbMsb.msb;
        width := msb - lsb + 1;
      end;
    brString: // [31:28]
      begin
        Start := Pos( '[', BitRange.bitRangeString );
        Middle := Pos( ':', BitRange.bitRangeString );
        Stop := Pos( ']', BitRange.bitRangeString );
        msb := StrToInt( Copy( BitRange.bitRangeString, Start + 1, Middle - Start - 1 ) );
        lsb := StrToInt( Copy( BitRange.bitRangeString, Middle + 1, Stop - Middle - 1 ) );
        width := msb - lsb + 1;
      end;
  end;

  // [31:28], lsb=28, width=4 : (1<<4)-1 = 0x0F, Mask=(0x0F<<28)=0xF0000000
  fieldPos := lsb;
  fieldMask := ( ( 1 shl width ) - 1 ) shl lsb;
end;

function GetClusterByName( Name: String; Cluster: TSVD_Cluster; var RetValue: TSVD_Cluster ): Boolean;
begin

end;

function GetRegisterByName( Name: String; _Register: TSVD_Register; var RetValue: TSVD_Register ): Boolean;
begin

end;

function GetFieldByName( Name: String; Filed: TSVD_Field; var RetValue: TSVD_Field ): Boolean;
begin

end;

function GetEnumeratedValuesByName( Name: String; EnumeratedValues: TSVD_EnumeratedValues;
  var RetValue: TSVD_EnumeratedValues ): Boolean;
begin

end;

procedure IDC_AddEnum( StringList: TStringList; _Register: TSVD_Register );
var
  I: Integer;
  J: Integer;
  K: Integer;
  fieldPos: Cardinal;
  fieldMask: Cardinal;

  peripheralName: String;
  registerName: String;
  fieldName: String;
  fieldDescription: String;
  enumName: String;
  enumValue: Cardinal;
  enumDescription: String;
  derivedFrom: String;
  Name: String;
begin
end;

procedure IDC_DecodeDevice( SVD_Device: TSVD_Device; var StringList: TStringList );
var
  LineNumber: Cardinal;

  procedure IDC_DecodeDeviceExtLine( AString: String );
  begin
    StringList.Add( '  ExtLinA(EA, 0x' + IntToHex( LineNumber, 2 ) + ', "' + AString + '");' );
    Inc( LineNumber );
  end;

begin
  LineNumber := 0;

  StringList.Add( '#include <idc.idc>' );
  StringList.Add( '' );
  StringList.Add( 'static IDC_JumpStartAddress(void)' );
  StringList.Add( '{' );
  StringList.Add( '  Jump( MinEA() );' );
  StringList.Add( '}' );
  StringList.Add( '' );

  StringList.Add( 'static IDC_DecodeDevice(void)' );
  StringList.Add( '{' );
  StringList.Add( '  auto EA = MinEA();' );

  IDC_DecodeDeviceExtLine( '; Device Information' );
  IDC_DecodeDeviceExtLine( '; ===========================================================================' );
  IDC_DecodeDeviceExtLine( '' );
  IDC_DecodeDeviceExtLine( '; name            : ' + SVD_Device.Name );
  IDC_DecodeDeviceExtLine( '; series          : ' + SVD_Device.series );
  IDC_DecodeDeviceExtLine( '; version         : ' + SVD_Device.version );
  IDC_DecodeDeviceExtLine( '; description     : ' + SVD_Device.description );
  IDC_DecodeDeviceExtLine( '; addressUnitBits : 0x' + IntToStr( SVD_Device.addressUnitBits ) );
  IDC_DecodeDeviceExtLine( '' );

  IDC_DecodeDeviceExtLine( '; CPU Information' );
  IDC_DecodeDeviceExtLine( '; ===========================================================================' );
  IDC_DecodeDeviceExtLine( '' );
  IDC_DecodeDeviceExtLine( '; name            : ' + SVD_Device.cpu.Name );
  IDC_DecodeDeviceExtLine( '; revision        : ' + SVD_Device.cpu.revision );
  IDC_DecodeDeviceExtLine( '; endian          : ' + SVD_Device.cpu.endian );
  IDC_DecodeDeviceExtLine( '; itcmPresent     : ' + SVD_Device.cpu.itcmPresent );
  IDC_DecodeDeviceExtLine( '; dtcmPresent     : ' + SVD_Device.cpu.dtcmPresent );
  IDC_DecodeDeviceExtLine( '; icachePresent   : ' + SVD_Device.cpu.icachePresent );
  IDC_DecodeDeviceExtLine( '; dcachePresent   : ' + SVD_Device.cpu.dcachePresent );
  IDC_DecodeDeviceExtLine( '; mpuPresent      : ' + SVD_Device.cpu.mpuPresent );
  IDC_DecodeDeviceExtLine( '; fpuPresent      : ' + SVD_Device.cpu.fpuPresent );
  IDC_DecodeDeviceExtLine( '; fpuDP           : ' + SVD_Device.cpu.fpuDP );
  IDC_DecodeDeviceExtLine( '; vtorPresent     : ' + SVD_Device.cpu.vtorPresent );
  IDC_DecodeDeviceExtLine( '; vendorSystick   : ' + SVD_Device.cpu.vendorSystickConfig );
  IDC_DecodeDeviceExtLine( '; sauNumRegions   : ' + IntToStr( SVD_Device.cpu.sauNumRegions ) );
  IDC_DecodeDeviceExtLine( '; nvicPrioBits    : ' + IntToStr( SVD_Device.cpu.nvicPrioBits ) );
  IDC_DecodeDeviceExtLine( '; numInterrupts   : ' + IntToStr( SVD_Device.cpu.deviceNumInterrupts ) );
  IDC_DecodeDeviceExtLine( '' );

  IDC_DecodeDeviceExtLine( '; Register Information' );
  IDC_DecodeDeviceExtLine( '; ===========================================================================' );
  IDC_DecodeDeviceExtLine( '' );
  IDC_DecodeDeviceExtLine( '; access          : ' + SVD_Device.registerProperties.access );
  IDC_DecodeDeviceExtLine( '; resetValue      : 0x' + IntToHex( SVD_Device.registerProperties.resetValue, 8 ) );
  IDC_DecodeDeviceExtLine( '; resetMask       : 0x' + IntToHex( SVD_Device.registerProperties.resetMask, 8 ) );
  IDC_DecodeDeviceExtLine( '; protection      : ' + SVD_Device.registerProperties.protection );
  IDC_DecodeDeviceExtLine( '' );
  IDC_DecodeDeviceExtLine( '; ===========================================================================' );
  StringList.Add( '}' );
  StringList.Add( '' );

  MainStringList.Add( '' );
  MainStringList.Add( '  IDC_JumpStartAddress();' );

  MainStringList.Add( '' );
  MainStringList.Add( '  IDC_DecodeDevice();' );

end;

{
  success AddSegEx(long startea, long endea, long base, long use32, long align, long comb, long flags);
  // returns: 0-failed, 1-ok
  //
  // Create a new segment
  //
  // startea  - linear address of the start of the segment
  // endea    - linear address of the end of the segment this address will not belong to the segment
  // base     - base paragraph or selector of the segment.
  // use32    - 0: 16bit segment, 1: 32bit segment, 2: 64bit segment
  //
  // align    - segment alignment. see below for alignment values
  //
  // #define saAbs      0    // Absolute segment.
  // #define saRelByte  1    // Relocatable, byte aligned.
  // #define saRelWord  2    // Relocatable, word (2-byte, 16-bit) aligned.
  // #define saRelPara  3    // Relocatable, paragraph (16-byte) aligned.
  // #define saRelPage  4    // Relocatable, aligned on 256-byte boundary
  //                         // (a "page" in the original Intel specification).
  // #define saRelDble  5    // Relocatable, aligned on a double word (4-byte) boundary.
  //                         // This value is used by the PharLap OMF for the same alignment.
  // #define saRel4K    6    // This value is used by the PharLap OMF for page (4K) alignment.
  //                         // It is not supported by LINK.
  // #define saGroup    7    // Segment group
  // #define saRel32Bytes 8  // 32 bytes
  // #define saRel64Bytes 9  // 64 bytes
  // #define saRelQword 10   // 8 bytes
  //
  // comb     - segment combination. see below for combination values.
  //
  // #define scPriv     0    // Private. Do not combine with any other program segment.
  // #define scPub      2    // Public. Combine by appending at an offset that meets the alignment requirement.
  // #define scPub2     4    // As defined by Microsoft, same as C=2 (public).
  // #define scPub3     7    // As defined by Microsoft, same as C=2 (public).
  // #define scStack    5    // Stack. Combine as for C=2. This combine type forces byte alignment.
  // #define scCommon   6    // Common. Combine by overlay using maximum size.
  //
  // flags    - combination of ADDSEG_... bits
  //
  // #define ADDSEG_NOSREG   0x0001  // set all default segment register values to BADSELs (undefine all default segment registers)
  // #define ADDSEG_OR_DIE   0x0002  // qexit() if can't add a segment
  // #define ADDSEG_NOTRUNC  0x0004  // don't truncate the new segment at the beginning of the next segment if they overlap.
  //                                 // destroy/truncate old segments instead.
  // #define ADDSEG_QUIET    0x0008  // silent mode, no "Adding segment..." in the messages window
  // #define ADDSEG_FILLGAP  0x0010  // If there is a gap between the new segment and the previous one, and this gap is less
  //                                 // than 64K, then fill the gap by extending the previous segment and adding .align directive to it.
  //                                 // This way we avoid gaps between segments. Too many gaps lead to a virtual array failure.
  //                                 // It can not hold more than ~1000 gaps.
  // #define ADDSEG_SPARSE   0x0020  // Use sparse storage method for the new segment
}
procedure IDC_DecodePeripheral( const Peripheral: TSVD_Peripheral; var StringList: TStringList );
begin
  MainStringList.Add( '' );
  MainStringList.Add( '  IDC_DecodeSegment();' );

  StringList.Add( 'static IDC_DecodeSegment(void)' );
  StringList.Add( '{' );

  StringList.Add( '}' );
  StringList.Add( '' );
end;

procedure IDC_Decode( SVD_Device: TSVD_Device; var StringList: TStringList );
var
  I: Integer;
  Peripheral: TSVD_Peripheral;
begin
  MainStringList := TStringList.Create;
  try
    MainStringList.Add( '' );
    MainStringList.Add( 'static main(void)' );
    MainStringList.Add( '{' );

    IDC_DecodeDevice( SVD_Device, StringList );

    for I := 0 to SVD_Device.peripheralArray.Count - 1 do
      IDC_DecodePeripheral( Peripheral, StringList );

    MainStringList.Add( '' );
    MainStringList.Add( '}' );

    StringList.AddStrings( MainStringList );
  finally
    MainStringList.Free;
  end;

end;

end.
