unit uDynArray;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes, System.Character, System.Variants,
  Generics.Collections, Generics.Defaults,
  VCL.Forms;

type
  TDynArrayObjectDestroyEvent = procedure( Sender: TObject );

  TDynArrayObject< T > = class
  private
    FItems: TArray< T >;
    FCapacity: Cardinal;
    FCount: Cardinal;
    FIndex: Integer;
    FOnDestroy: TDynArrayObjectDestroyEvent;
    function GetItem( Index: Integer ): T;
    procedure SetItem( Index: Integer; Value: T );
  public
    constructor Create( Capacity: Cardinal; OnDestroy: TDynArrayObjectDestroyEvent );
    destructor Destroy; override;
    procedure Append( Value: T );
    procedure Trim;
    procedure Purge;
    property Count: Cardinal read FCount;
    property Items[ Index: Integer ]: T read GetItem write SetItem; default;
  end;

type
  TDynArray< T > = record
  private
    FCapacity: Integer;
    FCount: Integer;
    procedure Expand;
    procedure SetCapacity( Capacity: Integer );
  public
    Items: TArray< T >;

  type
    P = ^T;
  procedure Init( Capacity: Integer );
  procedure Purge;
  procedure Trim;
  procedure Push( Item: T );
  procedure Pop; overload;
  procedure Pop( var Item: T ); overload;
  function GetItem( Index: Integer ): Pointer;
  function NewItem: Pointer;
  function FirstItem: Pointer;
  function LastItem: Pointer;
  procedure LastItemFrom( Source: P { P = ^T } ); overload;
  procedure LastItemFrom( Source: TArray< T >; SourceIndex: Integer ); overload;
  procedure CopyFrom( Source: TArray< T >; SourceIndex: Integer; DestIndex: Integer; Count: Integer = 1 );

  property Count: Integer read FCount;
  end;

implementation

{ TDynArrayObject<T> }

procedure TDynArrayObject< T >.Append( Value: T );
var
  i: Cardinal;
  Delta: Cardinal;
begin
  if FCount < FCapacity then
    Delta := 0
  else if FCapacity > 64 then
    Delta := FCapacity div 4
  else if FCapacity > 8 then
    Delta := 16
  else
    Delta := 4;

  FCapacity := FCapacity + Delta;

  if Delta > 0 then
    SetLength( FItems, FCapacity );

  FItems[ FCount ] := Value;
  FCount := FCount + 1;
end;

procedure TDynArrayObject< T >.Purge;
begin
  FItems := nil;
end;

constructor TDynArrayObject< T >.Create( Capacity: Cardinal; OnDestroy: TDynArrayObjectDestroyEvent );
begin
  inherited Create;

  FCount := 0;
  FOnDestroy := OnDestroy;
  FCapacity := Capacity;
  SetLength( FItems, Capacity ); // Purge memory after GetMem()
end;

destructor TDynArrayObject< T >.Destroy;
begin
  if Assigned( FOnDestroy ) then
    FOnDestroy( Self );

  FItems := nil; // Free everything
  inherited;
end;

function TDynArrayObject< T >.GetItem( Index: Integer ): T;
begin
  Result := FItems[ Index ];
end;

procedure TDynArrayObject< T >.SetItem( Index: Integer; Value: T );
begin
  FItems[ Index ] := Value;
end;

procedure TDynArrayObject< T >.Trim;
begin
  FCapacity := FCount;
  SetLength( FItems, FCapacity );
end;

{ TDynArray<T> }

procedure TDynArray< T >.Pop( var Item: T );
begin
  Dec( FCount );
  Item := Items[ FCount ];
  Items[ FCount ] := Default ( T );
end;

procedure TDynArray< T >.Pop;
begin
  Dec( FCount );
  Items[ FCount ] := Default ( T );
end;

procedure TDynArray< T >.CopyFrom( Source: TArray< T >; SourceIndex: Integer; DestIndex: Integer; Count: Integer );
begin
  CopyArray( Addr( Items[ DestIndex ] ), Addr( Source[ SourceIndex ] ), TypeInfo( T ), Count );
end;

procedure TDynArray< T >.Expand;
var
  i: Cardinal;
  Delta: Cardinal;
begin
  if FCount < FCapacity then
    Exit;

  if FCapacity > 64 then
    Delta := FCapacity div 4
  else if FCapacity > 8 then
    Delta := 16
  else
    Delta := 4;

  FCapacity := FCapacity + Delta;
  SetLength( Items, FCapacity );
end;

procedure TDynArray< T >.Push( Item: T );
begin
  Expand( );
  Items[ FCount ] := Item;
  FCount := FCount + 1;
end;

function TDynArray< T >.GetItem( Index: Integer ): Pointer;
begin
  Result := Addr( Items[ Index ] );
end;

function TDynArray< T >.NewItem: Pointer;
begin
  Expand( );
  Result := Addr( Items[ FCount ] );
  FCount := FCount + 1;
end;

procedure TDynArray< T >.Purge;
begin
  FCount := 0;
  SetCapacity( 0 ); // Free everything
end;

function TDynArray< T >.FirstItem: Pointer;
begin
  Result := Addr( Items[ 0 ] );
end;

procedure TDynArray< T >.Init( Capacity: Integer );
begin
  Purge;
  SetCapacity( Capacity );
end;

function TDynArray< T >.LastItem: Pointer;
begin
  Result := Addr( Items[ FCount - 1 ] );
end;

procedure TDynArray< T >.LastItemFrom( Source: P );
begin
  CopyArray( Addr( Items[ Self.FCount - 1 ] ), Source, TypeInfo( T ), 1 );
end;

procedure TDynArray< T >.LastItemFrom( Source: TArray< T >; SourceIndex: Integer );
begin
  CopyArray( Addr( Items[ FCount - 1 ] ), Addr( Source[ SourceIndex ] ), TypeInfo( T ), 1 );
end;

procedure TDynArray< T >.SetCapacity( Capacity: Integer );
begin
  FCapacity := Capacity;
  SetLength( Items, Capacity ); // Clear memory after GetMem()
end;

procedure TDynArray< T >.Trim;
begin
  FCapacity := FCount;
  SetLength( Items, FCapacity );
end;

end.
