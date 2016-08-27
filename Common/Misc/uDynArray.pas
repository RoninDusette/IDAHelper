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
    procedure Add( Value: T );
    procedure Trim;
    procedure Clear;
    property Count: Cardinal read FCount;
    property Items[ Index: Integer ]: T read GetItem write SetItem; default;
  end;

type
  TDynArray< T > = record
  private
    FCapacity: Cardinal;
  public
    Items: TArray< T >;
    Count: Cardinal;

    procedure SetCapacity( Capacity: Cardinal );
    procedure Add( Value: T );
    procedure Trim;
    procedure Clear;
  end;

implementation

{ TDynArrayObject<T> }

procedure TDynArrayObject< T >.Add( Value: T );
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

procedure TDynArrayObject< T >.Clear;
begin
  FItems := nil;
end;

constructor TDynArrayObject< T >.Create( Capacity: Cardinal; OnDestroy: TDynArrayObjectDestroyEvent );
begin
  inherited Create;

  FCount := 0;
  FOnDestroy := OnDestroy;
  FCapacity := Capacity;
  SetLength( FItems, Capacity ); // Clear memory after GetMem()
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

procedure TDynArray< T >.Add( Value: T );
var
  i: Cardinal;
  Delta: Cardinal;
begin
  if Count < FCapacity then
    Delta := 0
  else if FCapacity > 64 then
    Delta := FCapacity div 4
  else if FCapacity > 8 then
    Delta := 16
  else
    Delta := 4;

  FCapacity := FCapacity + Delta;

  if Delta > 0 then
    SetLength( Items, FCapacity );

  Items[ Count ] := Value;
  Count := Count + 1;
end;

procedure TDynArray< T >.Clear;
begin
  Items := nil; // Free everything
end;

procedure TDynArray< T >.SetCapacity( Capacity: Cardinal );
begin
  FCapacity := Capacity;
  Count := 0;
  SetLength( Items, Capacity ); // Clear memory after GetMem()
end;

procedure TDynArray< T >.Trim;
begin
  FCapacity := Count;
  SetLength( Items, FCapacity );
end;

end.
