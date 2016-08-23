unit uSVD_Type;

interface

uses
  Winapi.Windows, Winapi.Messages, // Windows
  System.Classes, System.Variants, System.SysUtils, // System
  uMisc;

type
  // -----------------------------------------------------------------------------------------------
  // System View description : CMSIS-SVD.xsd, CMSIS-SVD Schema File V1.3.2, 22. January 2016
  // -----------------------------------------------------------------------------------------------

  TSVD_BitRangeType = ( brOffsetWidth, brLsbMsb, brString );

  TSVD_RegisterParent = ( rpCluster, rpRegisters );

  TSVD_BitRangeOffsetWidth = record
    { Value defining the position of the least significant bit of the field within the register it belongs to. }
    bitOffset: Cardinal; // ---- 28
    { Value defining the bit-width of the bitfield within the register it belongs to. }
    bitWidth: Cardinal; // ----- 4
  end;

  TSVD_BitRangeLsbMsb = record
    { Value defining the bit position of the least significant bit within the register it belongs to. }
    lsb: Cardinal; // ---------- 28
    { Value defining the bit position of the most significant bit within the register it belongs to. }
    msb: Cardinal; // ---------- 31
  end;

  TSVD_BitRange = record
    bitRangeType: TSVD_BitRangeType;
    bitRangeLsbMsb: TSVD_BitRangeLsbMsb;
    bitRangeOffsetWidth: TSVD_BitRangeOffsetWidth;

    { A String in the format: "[<msb>:<lsb>]" }
    bitRangeString: String;
  end;

  { dimElementGroup specifies a series of elements (dim), the address offset between to consecutive elements
    and an a comma seperated list of strings  being used for identifying each element.
    A specialized case is an array of elements, where the name is contructed using [%s]
    and the dimIndex being integers from 0 to n }
  TSVD_dimElement = record
    { xs:sequence }
    { the value defines the number of elements in an array of registers. }
    dim: Cardinal;

    { If dim is specified, this element becomes mandatory.
      The element specifies the address increment in between two neighboring
      registers of the register array in the address map. }
    dimIncrement: Cardinal;

    { Specifies the substrings that replaces the %s placeholder within the register name.
      By default, the index is a decimal value starting with 0 for the first register.
      e.g. dim : 3, dimIndex : A,B,C :: name : GPIO_%s  :: GPIO_A_CTRL, GPIO_B_CTRL, GPIO_C_CTRL
      e.g. dim : 3, dimIndex : 3-6   :: name : IRQ%s    :: IRQ3, IRQ4, IRQ5, IRQ6
      e.g. dim : 4, dimIndex : ???   :: name : DATA[%s] :: DATA[4] }
    dimIndex: String;

    { V1.3.2 adding dimIndexArray to peripheral-, cluster- and register-array to describe enumeration of array indices. }
    dimArrayIndex: String;
    { xs:sequence }

    { name of peripheral, cluster and register. }
    { name : String; }
  end;

  { register properties specifies register size, access permission and reset value this is used in multiple locations.
    Settings are inherited downstream }
  TSVD_RegisterProperties = record
    { xs:sequence }
    size: Cardinal;

    { Predefined strings can be used to define the allowed access types for this field:
      read-only, write-only, read-write, writeOnce, and read-writeOnce.
      Can be omitted if it matches the access permission set for the parent register.
      ---- overwrite element values defined on the parent register. }
    access: String;
    protection: String;
    resetValue: Cardinal;
    resetMask: Cardinal;
    { xs:sequence }
  end;

  PSVD_AddressBlock = ^TSVD_AddressBlock;
  PSVD_Interrupt = ^TSVD_Interrupt;
  PSVD_CPU = ^TSVD_CPU;

  PSVD_Device = ^TSVD_Device;
  PSVD_Peripherals = ^TSVD_Peripherals;
  PSVD_Peripheral = ^TSVD_Peripheral;
  PSVD_Registers = ^TSVD_Registers;
  PSVD_Cluster = ^TSVD_Cluster;
  PSVD_Register = ^TSVD_Register;
  PSVD_Fields = ^TSVD_Fields;
  PSVD_Field = ^TSVD_Field;
  PSVD_EnumeratedValues = ^TSVD_EnumeratedValues;
  PSVD_EnumeratedValue = ^TSVD_EnumeratedValue;

  { Specifies an address range uniquely mapped to this peripheral.
    A peripheral must have at least one address block, but may allocate multiple distinct address ranges.
    If a peripheral is derived form another peripheral, the addressBlock is not mandatory.
    addressBlockType specifies the elements to describe an address block }
  TSVD_AddressBlock = record

    { Specifies the start address of an address block relative to the peripheral baseAddress. }
    Offset: Cardinal;

    { Specifies the number of addressUnitBits being covered by this address block.
      The end address of an address block results from the sum of baseAddress, offset, and (size - 1). }
    size: Cardinal;
  end;

  { A peripheral can have multiple associated interrupts.
    This entry allows the debugger to show interrupt names instead of interrupt numbers. }
  TSVD_Interrupt = record
    { The String represents the interrupt name. }
    name: String;

    description: String;

    { Is the enumeration index value associated to the interrupt. 0 : vector_address : 0x00000040 }
    value: Cardinal;
  end;

  TSVD_CPU = record
    name: string;
    revision: string;
    endian: string;

    itcmPresent: string;
    dtcmPresent: string;
    icachePresent: string;
    dcachePresent: string;
    mpuPresent: string;
    fpuPresent: string;
    fpuDP: string;
    vtorPresent: string;
    vendorSystickConfig: string;

    sauNumRegions: Cardinal;
    nvicPrioBits: Cardinal;
    deviceNumInterrupts: Cardinal;
  end;

  { An enumeratedValue defines a map between an unsigned integer and a human readable String.
    value   name          description
    0 <-----> disabled -> "the clock source clk0 is turned off"
    1 <-----> enabled --> "the clock source clk1 is running" }
  TSVD_EnumeratedValue = record
    enumeratedValues: PSVD_EnumeratedValues;

    { enumeratedValue derivedFrom=<identifierType> }
    derivedFrom: String;

    { name is a ANSI C indentifier representing the value (C Enumeration) }
    name: String;

    { description contains the details about the semantics/behavior specified by this value }
    description: String;

    { Defines the constant of the bit-field that the name corresponds to. }
    value: Cardinal;

    { isDefault specifies the name and description for all values that are not specifically described individually }
    isDefault: String;

  end;

  TSVD_EnumeratedValues = record
    field: PSVD_Field;

    { name specfies a reference to this enumeratedValues section for reuse purposes
      this name does not appear in the System Viewer nor the Header File. }
    name: String;

    { usage specifies whether this enumeration is to be used for read or write or (read and write) accesses }
    usage: String;

    enumeratedValueCount: Cardinal;
    enumeratedValueArray: TDynArrayType< TSVD_EnumeratedValue >;
  end;

  { A bit-field has a name that is unique within the register.
    The position and size within the register is either described
    by the combination of the least significant bit's position (lsb)
    and the most significant bit's position (msb),
    or the lsb and the bit-width of the field.
    A field may define an enumeratedValue in order
    to make the display more intuitive to read. }
  TSVD_Field = record
    fields: PSVD_Fields;

    { The field is cloned from a previously defined field with a unique name. }
    derivedFrom: String;

    dimElement: TSVD_dimElement;

    { name specifies a field's name. The System Viewer and the device header file will use the name of the field as identifier }
    name: String;

    { description contains reference manual level information about the function and options of a field. }
    description: String;

    bitRange: TSVD_BitRange;

    enumeratedValues: TSVD_EnumeratedValues;
  end;

  TSVD_Fields = record
    _register: PSVD_Register;

    name: String;
    fieldCount: Cardinal;
    fieldArray: TDynArrayType< TSVD_Field >;
  end;

  TSVD_Register = record
    registers: PSVD_Registers;
    cluster: PSVD_Cluster;

    derivedFrom: String;

    { describes a series of consecutive registers }
    dimElement: TSVD_dimElement;
    { name specifies the name of the register. The register name is used by System Viewer and
      device header file generator to represent a register }
    name: String;

    { display name specifies a register name without the restritions of an ANSIS C identifier. The use of this tag
      is discouraged because it does not allow consistency between the System View and the device header file. }
    displayName: String;

    { description contains a reference manual level description about the register and it's purpose }
    description: String;

    { alternateGroup specifies the identifier of the subgroup a register belongs to.
      This is useful if a register has a different description per mode but a single name }
    alternateGroup: String;

    { V1.1: alternateRegister specifies an alternate register description for an address
      that is already fully described. In this case the register name must be unique within the peripheral }
    alternateRegister: String;

    { addressOffset describes the address of the register relative to the baseOffset of the peripheral }
    addressOffset: Cardinal;

    { registerPropertiesGroup elements specify the default values for register size, access permission and
      reset value. These default values are inherited to all registers contained in this peripheral }
    registerProperties: TSVD_RegisterProperties;

    { V1.1: dataType specifies a CMSIS compliant native dataType for a register (i.e. signed, unsigned, pointer) }
    dataType: String;

    fields: TSVD_Fields;
  end;

  TSVD_Cluster = record
    registers: PSVD_Registers;

    { 1.3: nesting of cluster is supported }
    cluster: PSVD_Cluster;

    derivedFrom: String;

    { describes a series of consecutive clusters }
    dimElement: TSVD_dimElement;

    name: String;
    displayName: String;
    description: String;

    { V1.1: alternateCluster specifies an alternative description for a cluster address range that is
      already fully described. In this case the cluster name must be unique within the peripheral }
    alternateCluster: String;

    addressOffset: Cardinal;

    { registerPropertiesGroup elements specify the default values for register size, access permission and
      reset value. These default values are inherited to all registers contained in this peripheral }
    registerProperties: TSVD_RegisterProperties;

    { 1.3: nesting of cluster is supported }
    clusterCount: Cardinal;
    clusterArray: TDynArrayType< TSVD_Cluster >;

    registerCount: Cardinal;
    registerArray: TDynArrayType< TSVD_Register >;
  end;

  { the registers section can have an arbitrary list of cluster and register sections }
  TSVD_Registers = record
    peripheral: PSVD_Peripheral;

    clusterCount: Cardinal;
    clusterArray: TDynArrayType< TSVD_Cluster >;

    registerCount: Cardinal;
    registerArray: TDynArrayType< TSVD_Register >;
  end;

  TSVD_Peripheral = record
    peripherals: PSVD_Peripherals;

    derivedFrom: String;

    { 1.3: specify uni-dimensional array of peripheral - requires name="<name>[%s]" }
    dimElement: TSVD_dimElement;

    { name specifies the name of a peripheral. This name is used for the System View and device header file }
    name: String;

    version: String;

    { description provides a high level functional description of the peripheral }
    description: String;
    { V1.1: alternatePeripheral specifies an alternative description for an address range that is
      already fully by a peripheral described. In this case the peripheral name must be unique within the device description }
    alternatePeripheral: String;

    { prependToName specifies a prefix that is placed in front of each register name of this peripheral.
      The device header file will show the registers in a C-Struct of the peripheral without the prefix. }
    prependToName: String;

    { appendToName is a postfix that is appended to each register name of this peripheral. The device header
      file will sho the registers in a C-Struct of the peripheral without the postfix }
    appendToName: String;
    { baseAddress specifies the absolute base address of a peripheral. For derived peripherals it is mandatory
      to specify a baseAddress. }
    baseAddress: Cardinal;

    { registerPropertiesGroup elements specify the default values for register size, access permission and
      reset value. These default values are inherited to all registers contained in this peripheral }
    registerProperties: TSVD_RegisterProperties;

    { addressBlock specifies *one or more* address ranges that are assigned exclusively to this peripheral.
      derived peripherals may have no addressBlock, however none-derived peripherals are required to specify
      at least one address block }
    addressBlockCount: Cardinal;
    addressBlockArray: TDynArrayType< TSVD_AddressBlock >;

    { interrupt specifies can specify one or more interrtupts by name, description and value }
    interrupt: TSVD_Interrupt;

    { registers section contains all registers owned by the peripheral.
      In case a peripheral gets derived it does not have its own registers section, hence this section is optional.
      A unique peripheral without a registers section is not allowed }
    registers: TSVD_Registers;
  end;

  TSVD_Peripherals = record
    device: PSVD_Device;
    peripheralCount: Cardinal;
    peripheralArray: TDynArrayType< TSVD_Peripheral >;
  end;

  TSVD_Device = record
    vendor: String;
    vendorID: String;

    { The name string is used to identify the device or device series. Device names are required to be unique. }
    name: string;
    series: String;

    { The string defines the version of the file.
      Silicon vendors maintain the description throughout the life-cycle
      of the device and ensure that all updated and released copies have
      a unique version string.
      Higher numbers indicate a more recent version. }
    version: string;

    { String for describing main features of a device
      (for example CPU, clock frequency, peripheral overview). }
    description: string;

    { Defines the number of data bits uniquely selected by each address.
      The value for Cortex-M based devices is 8 (byte-addressable). }
    addressUnitBits: Cardinal;

    { Defines the number of data bit-width of the maximum single data transfer
      supported by the bus infrastructure.
      This information is relevant for debuggers when accessing registers,
      because it might be required to issue multiple accesses
      for accessing a resource of a bigger size.
      The expected value for Cortex-M based devices is 32. }
    width: Cardinal;

    { registerPropertiesGroup elements specify the default values for register size, access permission and
      reset value. These default values are inherited to all registers contained in this peripheral }
    registerProperties: TSVD_RegisterProperties;

    cpu: TSVD_CPU;

    peripherals: TSVD_Peripherals;
  end;

function Str2Int( const S: String ): Cardinal;

implementation

(*
  Constants

  Number constants shall be entered in hexadecimal, decimal, or binary format.
  The Hexadecimal format is indicated by a leading "0x".
  The Binary format is indicated by a leading "#".
  All other formats are interpreted as decimal numbers.

  The value tag in enumeratedValue accepts 'do not care' bits represented by "x".

  <xs:simpleType name="scaledNonNegativeInteger">
  <xs:restriction base="xs:string">
  <xs:pattern value="[+]?(0x|0X|#)?[0-9a-fA-F]+[kmgtKMGT]?"/>
  </xs:restriction>
  </xs:simpleType>

  A scaled integer.
  It supports any string recognized by java.lang.Long.decode().
  It also supports a magnitude scale suffix of upper or lower case
  K (kilo=2^10), M (mega=2^20), G (giga=2^30) or T (tera=2^40)

  <enumeratedValue>
  <name>KEEP</name>
  <value>#00</value>
  </enumeratedValue>

  <!-- enumeratedValue 1 -->
  <enumeratedValue>
  <name>INCREMENT</name>
  <value>#01</value>
  </enumeratedValue>

  <!-- enumeratedValue 2, 3 -->
  <enumeratedValue>
  <name>DECREMENT</name>
  <value>#1X</value> or <value>#1x</value>
  </enumeratedValue>

  typedef enum {
  TIMER2_CR_IDR0_VAL_KEEP              = 0,
  TIMER2_CR_IDR0_VAL_INCREMENT         = 1,
  TIMER2_CR_IDR0_VAL_DECREMENT_2       = 2,
  TIMER2_CR_IDR0_VAL_DECREMENT_3       = 3,
  } TIMER2_CR_IDR0_VAL_Enum;

*)

function Str2Int( const S: String ): Cardinal;

  function BinToInt( value: string ): Cardinal;
  var
    i, iValueSize: Cardinal;
  begin
    Result := 0;
    iValueSize := Length( value );
    for i := iValueSize downto 1 do
      if value[ i ] = '1' then
        Result := Result + ( 1 shl ( iValueSize - i ) );
  end;

var
  leadPos: Integer;

begin
  // The Binary format is indicated by a leading "#10101010"
  leadPos := Pos( '#', S );
  if leadPos > 0 then
  begin
    // The value tag in enumeratedValue accepts 'do not care' bits represented by "x" or "X".
    StringReplace( S, 'x', '0', [ rfReplaceAll, rfIgnoreCase ] );

    Result := BinToInt( Copy( S, 2, Length( S ) - 1 ) );
    Exit;
  end;

  // The Hexadecimal format is indicated by a leading "0x".
  // All other formats are interpreted as decimal numbers.
  Result := StrToInt( S );
  Exit;

  // The Hexadecimal format is indicated by a leading "0x".
  leadPos := Pos( '0x', S );
  if leadPos > 0 then
  begin
    Result := StrToInt( S );
    Exit;
  end;

end;

end.
