unit UniParser;

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

uses SysUtils, Classes;

type

TUniParser = class
protected
  PInit: PChar;
  PFine: PChar;
  PItem: PChar;
  PNext: PChar;
  PStep: PChar;
public
  procedure Restart;
  constructor Create(const S: PChar); overload;
  constructor Create(const S: string); overload;
  function Next: boolean; overload;
  function Next(const S: Char): boolean; overload;
  function Next(const S: TSysCharSet): boolean; overload;
  function Next(const S: string): boolean; overload;
  function Next(const S: array of string; out Index: Integer): boolean; overload;
  function Next(const S: TStrings; out Index: Integer): boolean; overload;
  function Item: string;
  function TrimItem: string;
  function Separator: string;
  function Head: string;
  function Tail: string;
end;

implementation

const
  Nullator: Char = #0;

procedure TUniParser.Restart;
begin
  PItem:=PInit;
  PNext:=PInit;
  PStep:=PInit;
end;

constructor TUniParser.Create(const S: PChar);
begin
  if S=nil
  then PInit:=@Nullator
  else PInit:=S;
  PFine:=nil;
  Restart;
end;

constructor TUniParser.Create(const S: string);
begin
  Create(Pointer(S));
  PFine:=PInit+Length(S);
end;

function TUniParser.Next: boolean;
begin
  PItem:=PStep;
  PNext:=PItem;
  if PNext^<>#0 then
  begin
    Inc(PNext);
    PStep:=PNext;
    Result:=True;
    Exit;
  end;
  PStep:=PNext;
  Result:=False;
end;

function TUniParser.Next(const S: Char): boolean;
begin
  PItem:=PStep;
  PNext:=PItem;
  while PNext^<>#0 do
  begin
    if PNext^=S then
    begin
      PStep:=PNext+1;
      Result:=True;
      Exit;
    end;
    Inc(PNext);
  end;
  PStep:=PNext;
  Result:=False;
end;

function TUniParser.Next(const S: TSysCharSet): boolean;
begin
  PItem:=PStep;
  PNext:=PItem;
  while PNext^<>#0 do
  begin
    if (PNext^ in S) then
    begin
      PStep:=PNext+1;
      Result:=True;
      Exit;
    end;
    Inc(PNext);
  end;
  PStep:=PNext;
  Result:=False;
end;

function TUniParser.Next(const S: string): boolean;
var
  L: Integer;
begin
  PItem:=PStep;
  PNext:=PItem;
  while PNext^<>#0 do
  begin
    L:=Length(S);
    if CompareMem(PNext,Pointer(S),L) then
    begin
      if L=0 then L:=1;
      PStep:=PNext+L;
      Result:=True;
      Exit;
    end;
    Inc(PNext);
  end;
  PStep:=PNext;
  Result:=False;
end;

function TUniParser.Next(const S: array of string; out Index: Integer): boolean;
var
  L: Integer;
procedure GetIndex;
begin
  Index:=Low(S);
  while Index<=High(S) do
  begin
    L:=Length(S[Index]);
    if CompareMem(PNext,Pointer(S[Index]),L) then Exit;
    Inc(Index);
  end;
  Index:=-1;
end;
begin
  PItem:=PStep;
  PNext:=PItem;
  while PNext^<>#0 do
  begin
    GetIndex;
    if Index<>-1 then
    begin
      if L=0 then L:=1;
      PStep:=PNext+L;
      Result:=True;
      Exit;
    end;
    Inc(PNext);
  end;
  PStep:=PNext;
  Result:=False;
end;

function TUniParser.Next(const S: TStrings; out Index: Integer): boolean;
var
  L: Integer;
procedure GetIndex;
begin
  Index:=0;
  if S<>nil then
  while Index<S.Count do
  begin
    L:=Length(S[Index]);
    if CompareMem(PNext,Pointer(S[Index]),L) then Exit;
    Inc(Index);
  end;
  Index:=-1;
end;
begin
  PItem:=PStep;
  PNext:=PItem;
  while PNext^<>#0 do
  begin
    GetIndex;
    if Index<>-1 then
    begin
      if L=0 then L:=1;
      PStep:=PNext+L;
      Result:=True;
      Exit;
    end;
    Inc(PNext);
  end;
  PStep:=PNext;
  Result:=False;
end;

function TUniParser.Item: string;
begin
  SetLength(Result, PNext-PItem);
  Move(PItem^, Pointer(Result)^, Length(Result));
end;

function TUniParser.TrimItem: string;
var
  _P,P_: PChar;
begin
  _P:=PItem;
  P_:=PNext-1;
  while (_P^<#33) do Inc(_P);
  while (P_^<#33) do Dec(P_);
  SetLength(Result, P_-_P+1);
  Move(_P^, Pointer(Result)^, Length(Result));
end;

function TUniParser.Separator: string;
begin
  SetLength(Result, PStep-PNext);
  Move(PNext^, Pointer(Result)^, Length(Result));
end;

function TUniParser.Head: string;
begin
  SetLength(Result, PItem-PInit);
  Move(PInit^, Pointer(Result)^, Length(Result));
end;

function TUniParser.Tail: string;
begin
  if PFine=nil then
  begin
    PFine:=PStep;
    while PFine<>#0 do Inc(PFine);
  end;
  SetLength(Result, PFine-PStep);
  Move(PStep^, Pointer(Result)^, Length(Result));
end;

end.
