unit SubUtils;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

// Version 1.0
//
// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
// specific language governing rights and limitations under the License.
//
// Site: http://www.mythcode.org
// Author: Dzianis Koshkin
// E-mail: k5@yandex.ru
//
// (C) 2005 MYTHcode.org

interface

uses Types, SysUtils;

type

  TValueRelationship = -1..1;


  TSysByteSet = set of Byte;
  TStrCompare = function(const A, B: string): TValueRelationship;
  TStrArray = array of string;
  
const
  LessThanValue = Low(TValueRelationship);
  EqualsValue = 0;
  GreaterThanValue = High(TValueRelationship);


//--- Get substring ---//

function Sub(const S: string; After: Integer): string; overload;
function Sub(const S: string; After: Integer; Before: Integer): string; overload;
function Sub(const S: string; After: Integer; Before: string): string; overload;
function Sub(const S: string; After: Integer; Before: TSysCharSet): string; overload;
function Sub(const S: string; After: string): string; overload;
function Sub(const S: string; After: string; Before: Integer): string; overload;
function Sub(const S: string; After: string; Before: string): string; overload;
function Sub(const S: string; After: string; Before: TSysCharSet): string; overload;
function Sub(const S: string; After: TSysCharSet): string; overload;
function Sub(const S: string; After: TSysCharSet; Before: Integer): string; overload;
function Sub(const S: string; After: TSysCharSet; Before: string): string; overload;
function Sub(const S: string; After: TSysCharSet; Before: TSysCharSet): string; overload;
function Sub(const After, Before: PChar): string; overload;

function Get(const From, Before: PChar): string; overload;

//--- Position of value ---//

function PosOf(Value: string; S: string; Offset: Integer = 1): Integer; overload;
function PosOf(Value: TSysCharSet; S: string; Offset: Integer = 1): Integer; overload;

//--- Set of string ---//

function SetOf(const S: string): TSysCharSet; overload;
function SetOf(const S: array of Char): TSysCharSet; overload;
function SetOf(P: PChar): TSysCharSet; overload;

//--- Convert to string ---//

function _(const Value: string): string; overload;
function _(const Value: Boolean): string; overload;
function _(const Value: Integer): string; overload;
function _(const Value: Int64): string; overload;
function _(const Value: Currency): string; overload;
function _(const Value: Extended): string; overload;
function _(const Value: TSysCharSet): string; overload;
function _(const Value: TDateTime): string; overload;
function _(const Value: TPoint): string; overload;
function _(const Value: TRect): string; overload;
function _(const Value: TValueRelationship): string; overload;
function _(const Value: OleVariant): string; overload;
function _(const Value: TClass): string; overload;

function File_(const Value: TFileName): string; overload;

//--- Compare values ---//

function Compare(const A, B: Byte): TValueRelationship; overload;
function Compare(const A, B: ShortInt): TValueRelationship; overload;
function Compare(const A, B: Word): TValueRelationship; overload;
function Compare(const A, B: Smallint): TValueRelationship; overload;
function Compare(const A, B: Cardinal): TValueRelationship; overload;
function Compare(const A, B: Integer): TValueRelationship; overload;
function Compare(const A, B: Int64): TValueRelationship; overload;
function Compare(const A, B: Single): TValueRelationship; overload;
//function Compare(const A, B: Real48): TValueRelationship; overload; //TODO: Lazarus doesn't support this function
function Compare(const A, B: Real): TValueRelationship; overload;
//function Compare(const A, B: Double): TValueRelationship; overload;
function Compare(const A, B: Extended): TValueRelationship; overload;
function Compare(const A, B: Comp): TValueRelationship; overload;
function Compare(const A, B: Currency): TValueRelationship; overload;
function Compare(const A, B: TDateTime): TValueRelationship; overload;
function Compare(const A, B: string): TValueRelationship; overload;
function Compare(const A, B: PChar): TValueRelationship; overload;
function Compare(const A, B: Pointer): TValueRelationship; overload;
function Compare(const A, B: TSysCharSet): TValueRelationship; overload;
function Compare(const A, B: TSysByteSet): TValueRelationship; overload;
function CompareAsText(const A, B: string): TValueRelationship; overload;

function _R(const Value: Integer): TValueRelationship;

//--- Other ---//

function Local(const FileName: TFileName): TFileName;
function UnQuote(const S: string): string;
function Straight(const S: string): string;
function Union(const S1, S2: string; const Separator: string = ' '): string; overload;
function Separate(const Value: string; const Separator: string = ' '): TStrArray; overload;

implementation

type
  TSetAsArray = array[1..SizeOf(TSysCharSet)] of Byte;

function Sub(const S: string; After: Integer): string;
begin
  if After<0 then After:=Succ(Length(S)+After);
  Result:=Copy(S, Succ(After), Length(S)-After);
end;

function Sub(const S: string; After: Integer; Before: Integer): string;
begin
  if After<0 then After:=Succ(Length(S)+After);
  if Before<=0 then Before:=Succ(Length(S)+Before);
  Result:=Copy(S, Succ(After), Pred(Before-After));
end;

function Sub(const S: string; After: Integer; Before: string): string; overload;
var
  B: Integer;
begin
  if After<0 then After:=Succ(Length(S)+After);
  B:=PosOf(Before, S, Succ(After));
  Result:=Copy(S, Succ(After), Pred(B-After));
end;

function Sub(const S: string; After: Integer; Before: TSysCharSet): string; overload;
var
  B: Integer;
begin
  if After<0 then After:=Succ(Length(S)+After);
  B:=PosOf(Before, S, Succ(After));
  Result:=Copy(S, Succ(After), Pred(B-After));
end;

function Sub(const S: string; After: string): string; overload;
var
  A: Integer;
begin
  A:=Pos(After, S);
  if A=0 then Result:='' else
  begin
    A:=A+Pred(Length(After));
    Result:=Copy(S, Succ(A), Length(S)-A);
  end;
end;

function Sub(const S: string; After: string; Before: Integer): string; overload;
var
  A: Integer;
begin
  A:=Pos(After, S);
  if A=0 then Result:='' else
  begin
    A:=A+Pred(Length(After));
    if Before<=0 then Before:=Succ(Length(S)+Before);
    Result:=Copy(S, Succ(A), Pred(Before-A));
  end;
end;

function Sub(const S: string; After: string; Before: string): string; overload;
var
  A: Integer;
  B: Integer;
begin
  A:=Pos(After, S);
  if A=0 then Result:='' else
  begin
    A:=A+Pred(Length(After));
    B:=PosOf(Before,S,Succ(A));
    Result:=Copy(S,Succ(A),Pred(B-A));
  end;
end;

function Sub(const S: string; After: string; Before: TSysCharSet): string; overload;
var
  A,B: Integer;
begin
  A:=Pos(After, S);
  if A=0 then Result:='' else
  begin
    A:=A+Pred(Length(After));
    B:=PosOf(Before, S, Succ(A));
    Result:=Copy(S, Succ(A), Pred(B-A));
  end;
end;

function Sub(const S: string; After: TSysCharSet): string; overload;
var
  A: Integer;
begin
  A:=PosOf(After, S);
  if A=0 then Result:='' else
  begin
    Result:=Copy(S, Succ(A), Length(S)-A);
  end;
end;

function Sub(const S: string; After: TSysCharSet; Before: Integer): string; overload;
var
  A: Integer;
begin
  A:=PosOf(After, S);
  if A=0 then Result:='' else
  begin
    if Before<=0 then Before:=Succ(Length(S)+Before);
    Result:=Copy(S, Succ(A), Pred(Before-A));
  end;
end;

function Sub(const S: string; After: TSysCharSet; Before: string): string; overload;
var
  A,B: Integer;
begin
  A:=PosOf(After,S);
  if A=0 then Result:='' else
  begin
    B:=PosOf(Before,S,Succ(A));
    Result:=Copy(S,Succ(A),Pred(B-A));
  end;
end;

function Sub(const S: string; After: TSysCharSet; Before: TSysCharSet): string; overload;
var
  A,B: Integer;
begin
  A:=PosOf(After,S);
  if A=0 then Result:='' else
  begin
    B:=PosOf(Before,S,Succ(A));
    Result:=Copy(S,Succ(A),Pred(B-A));
  end;
end;

function Sub(const After, Before: PChar): string;
begin
  Assert(Before<>nil);
  if After=nil then Result:='' else
  begin
    SetLength(Result, Pred(Before-After));
    Move(After[1], PChar(Result)^, Length(Result));
  end;
end;

function Get(const From, Before: PChar): string;
begin
  Assert(Before<>nil);
  if From=nil then Result:='' else
  begin
    SetLength(Result, Before-From);
    Move(From^, Pointer(Result)^, Length(Result));
  end;
end;

function _R(const Value: Integer): TValueRelationship;
begin
  if Value>0 then Result:=GreaterThanValue else
  if Value<0 then Result:=LessThanValue else
  Result:=EqualsValue;
end;

function Compare(const A, B: string): TValueRelationship; overload;
begin
  Result:=_R(CompareStr(A,B));
end;

function Compare(const A, B: PChar): TValueRelationship; overload;
begin
  Result:=_R(StrComp(A,B));
end;

function Compare(const A, B: Pointer): TValueRelationship;
begin
  if Cardinal(A)>Cardinal(B) then Result:=GreaterThanValue else
  if Cardinal(A)<Cardinal(B) then Result:=LessThanValue else
  Result:=EqualsValue;
end;

function Compare(const A, B: Byte): TValueRelationship;
begin
  if A>B then Result:=GreaterThanValue else
  if A<B then Result:=LessThanValue else
  Result:=EqualsValue;
end;

function Compare(const A, B: ShortInt): TValueRelationship;
begin
  if A>B then Result:=GreaterThanValue else
  if A<B then Result:=LessThanValue else
  Result:=EqualsValue;
end;

function Compare(const A, B: Word): TValueRelationship;
begin
  if A>B then Result:=GreaterThanValue else
  if A<B then Result:=LessThanValue else
  Result:=EqualsValue;
end;

function Compare(const A, B: Smallint): TValueRelationship;
begin
  if A>B then Result:=GreaterThanValue else
  if A<B then Result:=LessThanValue else
  Result:=EqualsValue;
end;

function Compare(const A, B: Cardinal): TValueRelationship;
begin
  if A>B then Result:=GreaterThanValue else
  if A<B then Result:=LessThanValue else
  Result:=EqualsValue;
end;

function Compare(const A, B: Integer): TValueRelationship;
begin
  if A>B then Result:=GreaterThanValue else
  if A<B then Result:=LessThanValue else
  Result:=EqualsValue;
end;

function Compare(const A, B: Int64): TValueRelationship;
begin
  if A>B then Result:=GreaterThanValue else
  if A<B then Result:=LessThanValue else
  Result:=EqualsValue;
end;

function Compare(const A, B: Single): TValueRelationship;
begin
  if A>B then Result:=GreaterThanValue else
  if A<B then Result:=LessThanValue else
  Result:=EqualsValue;
end;

//function Compare(const A, B: Real48): TValueRelationship;
//begin
//  if A>B then Result:=GreaterThanValue else
//  if A<B then Result:=LessThanValue else
//  Result:=EqualsValue;
//end;

function Compare(const A, B: Real): TValueRelationship;
begin
  if A>B then Result:=GreaterThanValue else
  if A<B then Result:=LessThanValue else
  Result:=EqualsValue;
end;

function Compare(const A, B: Double): TValueRelationship;
begin
  if A>B then Result:=GreaterThanValue else
  if A<B then Result:=LessThanValue else
  Result:=EqualsValue;
end;

function Compare(const A, B: Extended): TValueRelationship;
begin
  if A>B then Result:=GreaterThanValue else
  if A<B then Result:=LessThanValue else
  Result:=EqualsValue;
end;

function Compare(const A, B: Comp): TValueRelationship;
begin
  if A>B then Result:=GreaterThanValue else
  if A<B then Result:=LessThanValue else
  Result:=EqualsValue;
end;

function Compare(const A, B: Currency): TValueRelationship;
begin
  if A>B then Result:=GreaterThanValue else
  if A<B then Result:=LessThanValue else
  Result:=EqualsValue;
end;

function Compare(const A, B: TDateTime): TValueRelationship; overload;
begin
  if A>B then Result:=GreaterThanValue else
  if A<B then Result:=LessThanValue else
  Result:=EqualsValue;
end;

function Compare(const A, B: TSysCharSet): TValueRelationship;
var
  A_: TSetAsArray absolute A;
  B_: TSetAsArray absolute B;
  i: Byte;
begin
  for i:=Low(A_) to High(A_) do
  begin
    if A_[i]>B_[i] then begin Result:=GreaterThanValue; Exit; end else
    if A_[i]<B_[i] then begin Result:=LessThanValue; Exit; end
  end;
  Result:=EqualsValue;
end;

function Compare(const A, B: TSysByteSet): TValueRelationship; overload;
var
  A_: TSetAsArray absolute A;
  B_: TSetAsArray absolute B;
  i: Byte;
begin
  for i:=Low(A_) to High(A_) do
  begin
    if A_[i]>B_[i] then begin Result:=GreaterThanValue; Exit; end else
    if A_[i]<B_[i] then begin Result:=LessThanValue; Exit; end
  end;
  Result:=EqualsValue;
end;

function CompareAsText(const A, B: string): TValueRelationship; overload;
begin
  Result:=_R(CompareText(A,B));
end;

function PosOf(Value: TSysCharSet; S: string; Offset: Integer = 1): Integer; overload;
begin
  Result:=Offset;
  while Result<=Length(S) do
  begin
    if S[Result] in Value then Exit;
    Inc(Result);
  end;
  Result:=0;
end;

function PosOf(Value: string; S: string; Offset: Integer = 1): Integer; overload;
var
  i,d: Integer;
begin
  Result:=Offset;
  if Value<>'' then
  begin
    d:=Length(S)-Length(Value)+2;
    while Offset<d do
    begin
      i:=1;
      while S[Offset]=Value[i] do
      begin
        if i=Length(Value) then Exit;
        Inc(i);
        Inc(Offset);
      end;
      Inc(Result);
      Offset:=Result;
    end;
  end;
  Result:=0;
end;

function SetOf(const S: string): TSysCharSet;
var
  i: Integer;
begin
  Result:=[];
  for i:=1 to Length(S) do Include(Result, S[i]);
end;

function SetOf(const S: array of Char): TSysCharSet; overload;
var
  i: Integer;
begin
  Result:=[];
  for i:=Low(S) to High(S) do Include(Result, S[i]);
end;

function SetOf(P: PChar): TSysCharSet; overload;
begin
  Result:=[];
  if P=nil then Exit;
  while P^<>#0 do
  begin
    Include(Result, P^);
    Inc(P);
  end;
end;

function Straight(const S: string): string;
var
  i,j: Integer;
begin

  Result:=S;
  if Result='' then Exit;
  j:=1;

  for i:=j to Length(S)-1 do
  begin
    if (S[i]<#33) then
    begin
      if (S[i+1]>#32) then
      begin
        Result[j]:=' ';
        Inc(j);
      end;
    end else
    begin
      Result[j]:=S[i];
      Inc(j);
    end;
  end;

  if  (S[Length(S)]>#32) then
  begin
    Result[j]:=S[i];
    SetLength(Result, j);
  end else SetLength(Result, j-1);

end;

function UnQuote(const S: string): string;
begin
  if (Length(S)>1) and (S[1]=S[Length(S)]) and (S[1] in ['''','"'])
  then Result:=Sub(S,1,-1)
  else Result:=S;
end;

function Union(const S1, S2: string; const Separator: string = ' '): string;
begin
  if S1='' then
  begin
    if S2='' then Result:='' else Result:=S2;
  end else
  begin
    if S2='' then Result:=S1 else Result:=S1+Separator+S2;
  end;
end;

function Separate(const Value: string; const Separator: string = ' '): TStrArray; overload;
var
  Offset: Integer;
  Position: Integer;
label
  Loop;
begin
  Offset:=1;
  SetLength(Result,0);
  Loop:
  Position:=PosOf(Separator, Value, Offset);
  if Position<>0 then
  begin
    SetLength(Result,Succ(Length(Result)));
    Result[Length(Result)-1]:=Sub(Value, Offset-1, Position);
    Offset:=Position+Length(Separator);
    goto Loop;
  end;
  SetLength(Result,Succ(Length(Result)));
  Result[Length(Result)-1]:=Sub(Value, Offset-1);
end;

function Local(const FileName: TFileName): TFileName;
begin
  Result:=ExtractFilePath(ParamStr(0))+FileName;
end;

function _(const Value: Boolean): string; overload;
begin
  if Value then Result:='True' else Result:='False';
end;

function _(const Value: Integer): string; overload;
begin
  FmtStr(Result, '%d', [Value]);
end;

function _(const Value: Int64): string; overload;
begin
  FmtStr(Result, '%d', [Value]);
end;

function _(const Value: Extended): string; overload;
var
  Buffer: array[0..63] of Char;
begin
  SetString(Result, Buffer, FloatToText(Buffer, Value, ffGeneral, 15, 0));
end;

function _(const Value: Currency): string; overload;
var
  Buffer: array[0..63] of Char;
begin
  SetString(Result, Buffer, FloatToText(Buffer, Value, ffGeneral, 0, 0));
end;

function _(const Value: TSysCharSet): string; overload;
var
  C: Char;
begin
  Result:='';
  for  C:=Low(Char) to High(Char) do
  begin
    if C in Value then
    begin
      SetLength(Result, Succ(Length(Result)));
      Result[Length(Result)]:=C;
    end;
  end;
end;

function _(const Value: TDateTime): string; overload;
begin
  DateTimeToString(Result, '', Value);
end;

function _(const Value: TPoint): string; overload;
begin
  Result:='Point('+_(Value.X)+','+_(Value.Y)+')';
end;

function _(const Value: TRect): string; overload;
begin
  Result:='Rect('+_(Value.Left)+','+_(Value.Top)+','+_(Value.Right)+','+_(Value.Bottom)+')';
end;

function _(const Value: TValueRelationship): string; overload;
begin
  case Value of
    GreaterThanValue: Result:='>';
    LessThanValue: Result:='<';
    EqualsValue: Result:='=';
  end;
end;

function _(const Value: Variant): string; overload;
begin
  Result:=Value;
end;

function _(const Value: OleVariant): string; overload;
begin
  Result:=Value;
end;

function _(const Value: string): string; overload;
begin
  Result:=Value;
end;

function _(const Value: TClass): string; overload;
begin
  if Value<>nil then Result:=Value.ClassName;
end;

function File_(const Value: TFileName): string; overload;
var
  f: file of Char;
begin
  try
    AssignFile(f, Value);
    FileMode:=0;
    try
      Reset(f);
      SetLength(Result, FileSize(f));
      if Length(Result)>0 then BlockRead(f, Result[1], Length(Result));
    finally
      CloseFile(f);
    end;
  except
    Result:='';
  end;
end;

end.
