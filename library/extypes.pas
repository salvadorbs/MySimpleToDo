unit extypes;

{$mode delphi}

interface

uses
  Classes, SysUtils, contnrs;

type

  { TStringStack }

  TStringStack = class(TStringList)
  public
    procedure Push(AItem: String);
    Function Pop: String;
    Function Peek: String;
  end;

implementation

{ TStringStack }

procedure TStringStack.Push(AItem: String);
begin
  if AItem <> '' then
    Add(AItem);
end;

function TStringStack.Pop: String;
begin
  If Count > 0 then
  begin
    Result := Self[Count - 1];
    Delete(Count - 1);
  end
  else
    Result := '';
end;

function TStringStack.Peek: String;
begin
  If Count > 0 then
    Result := Self[Count - 1]
  else
    Result := '';
end;

end.

