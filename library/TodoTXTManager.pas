{
MySimpleToDo - A simple ToDo Manager for you, based of ToDo.TXT
Copyright (C) 2014 Matteo Salvi

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

unit TodoTXTManager;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees, BaseNodeData, Graphics, ColorUtils, Settings;

type
  { TToDoTXTManager }
  TNodeDataFunc = function(ANodeData: PBaseNodeData): string of object;

  TToDoTXTManager = class
  private
    FTree: TBaseVirtualTree;
    //Save
    function SaveCheckedState(ANodeData: PBaseNodeData): string;
    function SavePriority(ANodeData: PBaseNodeData): string;
    function SaveCreateData(ANodeData: PBaseNodeData): string;
    function SaveText(ANodeData: PBaseNodeData): string;
    function SaveProjects(ANodeData: PBaseNodeData): string;
    function SaveContexts(ANodeData: PBaseNodeData): string;
    function SaveDeadLine(ANodeData: PBaseNodeData): string;
    function SaveThresold(ANodeData: PBaseNodeData): string;
    function SaveColor(ANodeData: PBaseNodeData): string;

    //Load
    procedure LoadString(ANodeData: TBaseNodeData; const str, ASeparator: string);
    function LoadCheckedState(ANodeData: TBaseNodeData; const str: string): Boolean;
    function LoadDate(ANodeData: TBaseNodeData; const str: string): Boolean;
    function LoadPriority(ANodeData: TBaseNodeData; const str: string): Boolean;
    function LoadProjects(ANodeData: TBaseNodeData; const str: string): Boolean;
    function LoadContexts(ANodeData: TBaseNodeData; const str: string): Boolean;
    function LoadDeadLine(ANodeData: TBaseNodeData; const str: string): Boolean;
    function LoadThresold(ANodeData: TBaseNodeData; const str: string): Boolean;
    function LoadColor(ANodeData: TBaseNodeData; const str: string): Boolean;

    procedure ExcludeLastSeparator(var FullString: string);
  public
    constructor Create(ATree: TBaseVirtualTree);
    destructor Destroy; override;

    procedure Load(const AFileName: string);
    procedure SaveAs(const AFileName: string);
    procedure ArchiveToDos(const AFileName: string);

    //Single conversion
    function NodeToString(const ANode: PVirtualNode): string;
    function StringToNode(const ALine: string): PVirtualNode;

    //Multi conversion
    function NodesToString(const ANodes: TNodeArray): string;
    function StringsToNode(const AStringList: TStringList): TNodeArray;
  end;

const
  CHAR_SEPARATOR = ' ';
  CHAR_CHECKED   = 'x';
  CHAR_PROJECT   = '+';
  CHAR_CONTEXT   = '@';
  CHAR_SEPARATORKEY = ':';

  KEY_DEADLINE   = 'due';
  KEY_THRESOLD   = 't';
  KEY_COLOR      = 'color';

implementation

uses
  Utility, SubUtils, UniParser;

{ TToDoTXTManager }

function TToDoTXTManager.LoadCheckedState(ANodeData: TBaseNodeData; const str: string): Boolean;
begin
  Result := (str = CHAR_CHECKED);
  if Result then
  begin
    ANodeData.Checked := Result;
    ANodeData.DateCompleted := 0;
  end;
end;

function TToDoTXTManager.NodeToString(const ANode: PVirtualNode): string;
var
  NodeData: PBaseNodeData;

  function PrepareString(AFunc: TNodeDataFunc): string;
  begin
    Result := AFunc(NodeData);
    if Result <> '' then
      Result := Result + CHAR_SEPARATOR;
  end;

begin
  Result   := '';
  NodeData := FTree.GetNodeData(ANode);
  if Assigned(NodeData) then
  begin
    //Get full string of ToDo
    Result := PrepareString(SaveCheckedState) +
              PrepareString(SavePriority) +
              PrepareString(SaveCreateData) +
              PrepareString(SaveText) +
              PrepareString(SaveProjects) +
              PrepareString(SaveContexts) +
              PrepareString(SaveDeadLine) +
              PrepareString(SaveColor) +
              SaveThresold(NodeData);
    ExcludeLastSeparator(Result);
  end;
end;

procedure TToDoTXTManager.ExcludeLastSeparator(var FullString: string);
var
  Idx: Integer;
begin
  Idx := Length(FullString);
  if (FullString[Idx] = CHAR_SEPARATOR) then
    Delete(FullString, Idx, 1);
end;

function TToDoTXTManager.SaveCheckedState(ANodeData: PBaseNodeData): string;
begin
  Result := '';
  if ANodeData.Checked then
    Result := CHAR_CHECKED + CHAR_SEPARATOR + DateToStrEx(ANodeData.DateCompleted);
end;

function TToDoTXTManager.SavePriority(ANodeData: PBaseNodeData): string;
begin
  Result := '';
  if ANodeData.Priority <> '' then
    Result := Format('(%s)', [ANodeData.Priority]);
end;

function TToDoTXTManager.SaveCreateData(ANodeData: PBaseNodeData): string;
begin
  Result := DateToStrEx(ANodeData.DateCreate);
end;

function TToDoTXTManager.SaveText(ANodeData: PBaseNodeData): string;
begin
  Result := ANodeData.Text;
end;

function TToDoTXTManager.SaveProjects(ANodeData: PBaseNodeData): string;
var
  I: Integer;
begin
  Result := '';
  if (ANodeData.Projects.Count > 0) then
  begin
    for I := 0 to ANodeData.Projects.Count - 1 do
    begin
      Result := Result + CHAR_PROJECT + ANodeData.Projects[I];
      if (ANodeData.Projects.Count - 1) <> I then
        Result := Result + CHAR_SEPARATOR;
    end;
  end;
end;

function TToDoTXTManager.SaveContexts(ANodeData: PBaseNodeData): string;
var
  I: Integer;
begin
  Result := '';
  if (ANodeData.Contexts.Count > 0) then
  begin
    for I := 0 to ANodeData.Contexts.Count - 1 do
    begin
      Result := Result + CHAR_CONTEXT + ANodeData.Contexts[I];
      if (ANodeData.Contexts.Count - 1) <> I then
        Result := Result + CHAR_SEPARATOR;
    end;
  end;
end;

function TToDoTXTManager.SaveDeadLine(ANodeData: PBaseNodeData): string;
begin
  Result := '';
  if ANodeData.DateDeadLine <> 0 then
    Result := KEY_DEADLINE + CHAR_SEPARATORKEY + DateToStrEx(ANodeData.DateDeadLine);
end;

function TToDoTXTManager.SaveThresold(ANodeData: PBaseNodeData): string;
begin
  Result := '';
  if ANodeData.DateThresold <> 0 then
    Result := KEY_THRESOLD + CHAR_SEPARATORKEY + DateToStrEx(ANodeData.DateThresold);
end;

function TToDoTXTManager.SaveColor(ANodeData: PBaseNodeData): string;
begin
  if ANodeData.Color <> clNone then
    Result := KEY_COLOR + CHAR_SEPARATORKEY + ColorToHtml(ANodeData.Color);
end;

function TToDoTXTManager.LoadDate(ANodeData: TBaseNodeData; const str: string): Boolean;
var
  dtDate: TDate;
begin
  Result := False;
  dtDate := StrToDateEx(str);
  if dtDate <> 0 then
  begin
    Result := True;
    if (ANodeData.Checked) and (ANodeData.DateCompleted = 0) then
      ANodeData.DateCompleted := dtDate
    else
      ANodeData.DateCreate := dtDate;
  end;
end;

function TToDoTXTManager.LoadPriority(ANodeData: TBaseNodeData; const str: string): Boolean;
var
  strPriority: string;
begin
  Result := False;
  strPriority := Sub(str, '(', ')');
  if (Length(strPriority) = 1) and (strPriority[1] in ['A'..'Z']) then
  begin
    ANodeData.Priority := strPriority;
    Result := True;
  end;
end;

function TToDoTXTManager.LoadProjects(ANodeData: TBaseNodeData; const str: string
  ): Boolean;
begin
  Result := False;
  if Length(str) > 0 then
    if str[1] = CHAR_PROJECT then
    begin
      ANodeData.Projects.Add(Sub(str, 1));
      Result := True;
    end;
end;

function TToDoTXTManager.LoadContexts(ANodeData: TBaseNodeData; const str: string
  ): Boolean;
begin
  Result := False;
  if Length(str) > 0 then
    if str[1] = CHAR_CONTEXT then
    begin
      ANodeData.Contexts.Add(Sub(str, 1));
      Result := True;
    end;
end;

function TToDoTXTManager.LoadDeadLine(ANodeData: TBaseNodeData;
  const str: string): Boolean;
var
  dtDate: TDate;
  StrArray: TStrArray;
begin
  Result := False;
  StrArray := Separate(str, CHAR_SEPARATORKEY);
  if Length(StrArray) = 2 then
  begin
    if StrArray[0] = KEY_DEADLINE then
    begin
      dtDate := StrToDateEx(StrArray[1]);
      if dtDate <> 0 then
      begin
        Result := True;
        ANodeData.DateDeadLine := dtDate;
      end;
    end;
  end;
end;

function TToDoTXTManager.LoadThresold(ANodeData: TBaseNodeData;
  const str: string): Boolean;
var
  dtDate: TDate;
  StrArray: TStrArray;
begin
  Result := False;
  StrArray := Separate(str, CHAR_SEPARATORKEY);
  if Length(StrArray) = 2 then
  begin
    if StrArray[0] = KEY_THRESOLD then
    begin
      dtDate := StrToDateEx(StrArray[1]);
      if dtDate <> 0 then
      begin
        Result := True;
        ANodeData.DateThresold := dtDate;
      end;
    end;
  end;
end;

function TToDoTXTManager.LoadColor(ANodeData: TBaseNodeData; const str: string
  ): Boolean;
var
  StrArray: TStrArray;
begin
  Result := False;
  StrArray := Separate(str, CHAR_SEPARATORKEY);
  if Length(StrArray) = 2 then
  begin
    if StrArray[0] = KEY_COLOR then
    begin
      Result := True;
      ANodeData.Color := HtmlToColor(StrArray[1]);
    end;
  end;
end;

procedure TToDoTXTManager.LoadString(ANodeData: TBaseNodeData; const str, ASeparator: string);
begin
  if Assigned (ANodeData) then
  begin
    if not LoadCheckedState(ANodeData, str) then
    if not LoadDate(ANodeData, str) then
    if not LoadPriority(ANodeData, str) then
    if not LoadProjects(ANodeData, str) then
    if not LoadContexts(ANodeData, str) then
    if not LoadDeadLine(ANodeData, str) then
    if not LoadThresold(ANodeData, str) then
    if not LoadColor(ANodeData, str) then
    begin
      if ANodeData.Text = '' then
        ANodeData.Text := str
      else
        ANodeData.Text := ANodeData.Text + ASeparator + str;
    end;
  end;
end;

function TToDoTXTManager.StringToNode(const ALine: string): PVirtualNode;
var
  Parser: TUniParser;
  NodeData: PTreeNodeData;
begin
  if ALine = '' then
    Exit;
  //Add new node and get its data
  Result   := AddVSTNode(FTree);
  NodeData := FTree.GetNodeData(Result);
  if Assigned(NodeData) then
  begin
    Parser := TUniParser.Create(ALine);
    try
      //Process str in pieces separated by space
      while Parser.Next(CHAR_SEPARATOR) do
        LoadString(NodeData.Data, Parser.Item, CHAR_SEPARATOR);
      //Last piece of str to process
      if Parser.Item <> '' then
        LoadString(NodeData.Data, Parser.Item, CHAR_SEPARATOR);
      //Checked node or not
      if (NodeData.Data.Checked) then
        FTree.CheckState[Result] := csCheckedNormal;
    finally
      Parser.Free;
    end;
  end;
end;

constructor TToDoTXTManager.Create(ATree: TBaseVirtualTree);
begin
  FTree := ATree;
end;

destructor TToDoTXTManager.Destroy;
begin
  inherited Destroy;
end;

procedure TToDoTXTManager.Load(const AFileName: string);
var
  MyFile: TextFile;
  str: string;
begin
  Assert(Assigned(FTree), 'FTree is not assigned!');

  if FileExists(AFileName) then
  begin
    Log('Loading file ' + AFileName + ' in progress', llInfo);
    //Load FFileName
    Assign(MyFile, AFileName);
    try
      Reset(MyFile);
      //Read FFileName and load ToDo
      while not(EOF(MyFile)) do
      begin
        //Read line and create node from it
        ReadLn(MyFile, str);
        if Assigned(StringToNode(str)) then
          Log('Loaded line in todo.txt format = ' + QuotedStr(str), llInfo);
      end;
      Log('Loading file ' + AFileName + ' completed', llInfo);
    finally
      CloseFile(MyFile);
    end;
  end
  else
    Log('File ' + AFileName + 'don''t found', llInfo);
end;

procedure TToDoTXTManager.SaveAs(const AFileName: string);
var
  MyFile: TextFile;
  Node: PVirtualNode;
begin
  Assign(MyFile, AFileName);
  try
    Rewrite(MyFile);
    Node := FTree.GetFirst;
    while Assigned(Node) do
    begin
      //Get node string and write in file
      WriteLn(MyFile, NodeToString(Node));
      //Get next node
      Node := FTree.GetNext(Node);
    end;
  finally
    Log('ToDo list saved in ' + AFileName, llInfo);
    CloseFile(MyFile);
  end;
end;

procedure TToDoTXTManager.ArchiveToDos(const AFileName: string);
var
  MyFile: TextFile;
  Node: PVirtualNode;
begin
  Assign(MyFile, AFileName);
  try
    if FileExists(AFileName) then
      Append(MyFile)
    else
      Rewrite(MyFile);
    Node := FTree.GetFirst;
    while Assigned(Node) do
    begin
      //Get node string and write in file
      if (FTree.CheckState[Node] = csCheckedNormal) then
        WriteLn(MyFile, NodeToString(Node));
      //Get next node
      Node := FTree.GetNext(Node);
    end;
  finally
    Log('Export completed ToDos in ' + AFileName, llInfo);
    CloseFile(MyFile);
  end;
end;

function TToDoTXTManager.NodesToString(const ANodes: TNodeArray): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Length(ANodes) - 1 do
  begin
    if Result = '' then
      Result := NodeToString(ANodes[I])
    else
      Result := Result + LineEnding + NodeToString(ANodes[I]);
  end;
end;

function TToDoTXTManager.StringsToNode(const AStringList: TStringList
  ): TNodeArray;
var
  I: Integer;
begin
  if AStringList.Count > 0 then
  begin
    SetLength(Result, AStringList.Count);
    for I := 0 to AStringList.Count - 1 do
      Result[I] := StringToNode(AStringList[I]);
  end;
end;

end.

