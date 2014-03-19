unit TodoTXTManager;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees, BaseNodeData;

type
  { TToDoTXTManager }
  TNodeDataFunc = function(ANodeData: PBaseNodeData): string of object;

  TToDoTXTManager = class
  private
    FFileName: string;
    FTree: TBaseVirtualTree;

    procedure ExcludeLastSeparator(var FullString: string);
    function NodeToString(const ANode: PVirtualNode): string;
    function SaveCheckedState(ANodeData: PBaseNodeData): string;
    function SavePriority(ANodeData: PBaseNodeData): string;
    function SaveCreateData(ANodeData: PBaseNodeData): string;
    function SaveText(ANodeData: PBaseNodeData): string;
    function SaveProjects(ANodeData: PBaseNodeData): string;
    function SaveContexts(ANodeData: PBaseNodeData): string;
    function SaveDeadLine(ANodeData: PBaseNodeData): string;
    function SaveThresold(ANodeData: PBaseNodeData): string;

    procedure LoadNodeToDo(const ALine: string; ANode: PVirtualNode);
    function LoadCheckedState(ANodeData: TBaseNodeData; const str: string): Boolean;
    function LoadDate(ANodeData: TBaseNodeData; const str: string): Boolean;
    function LoadPriority(ANodeData: TBaseNodeData; const str: string): Boolean;
    function LoadProjects(ANodeData: TBaseNodeData; const str: string): Boolean;
    function LoadContexts(ANodeData: TBaseNodeData; const str: string): Boolean;
    function LoadDeadLine(ANodeData: TBaseNodeData; const str: string): Boolean;
    function LoadThresold(ANodeData: TBaseNodeData; const str: string): Boolean;
    procedure LoadString(ANodeData: TBaseNodeData; const str, ASeparator: string);

  public
    constructor Create(const AFileName: string; ATree: TBaseVirtualTree);
    destructor Destroy; override;

    procedure Load;
    procedure Save;

    property FileName: string read FFileName write FFileName;
  end;

const
  CHAR_SEPARATOR = ' ';
  CHAR_CHECKED   = 'x';
  CHAR_PROJECT   = '+';
  CHAR_CONTEXT   = '@';
  CHAR_SEPARATORKEY = ':';

  KEY_DEADLINE   = 'due';
  KEY_THRESOLD   = 't';

implementation

uses
  Utility, SubUtils, UniParser;

{ TToDoTXTManager }

function TToDoTXTManager.LoadCheckedState(ANodeData: TBaseNodeData; const str: string): Boolean;
begin
  Result := (str = CHAR_CHECKED);
  if Result then
    ANodeData.Checked := Result;
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
    begin
      if ANodeData.Text = '' then
        ANodeData.Text := str
      else
        ANodeData.Text := ANodeData.Text + ASeparator + str;
    end;
  end;
end;

procedure TToDoTXTManager.LoadNodeToDo(const ALine: string; ANode: PVirtualNode);
var
  Parser: TUniParser;
  NodeData: PTreeNodeData;
begin
  NodeData := FTree.GetNodeData(ANode);
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
        FTree.CheckState[ANode] := csCheckedNormal;
    finally
      Parser.Free;
    end;
  end;
end;

constructor TToDoTXTManager.Create(const AFileName: string; ATree: TBaseVirtualTree);
begin
  FFileName := AFileName;
  FTree := ATree;
end;

destructor TToDoTXTManager.Destroy;
begin
  FFileName := '';
  inherited Destroy;
end;

procedure TToDoTXTManager.Load;
var
  MyFile: TextFile;
  str: string;
begin
  Assert(Assigned(FTree), 'FTree is not assigned!');

  if FileExists(FFileName) then
  begin
    //Load FFileName
    Assign(MyFile, FFileName);
    try
      Reset(MyFile);
      //Read FFileName and load ToDo
      while not(EOF(MyFile)) do
      begin
        //Read line and create node from it
        ReadLn(MyFile, str);
        if str <> '' then
          LoadNodeToDo(str, AddVSTNode(FTree, nil));
      end;
    finally
      CloseFile(MyFile);
    end;
  end;
end;

procedure TToDoTXTManager.Save;
var
  MyFile: TextFile;
  Node: PVirtualNode;
begin
  Assign(MyFile, FFileName);
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
    CloseFile(MyFile);
  end;
end;

end.

