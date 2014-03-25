unit main;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  StdCtrls, EditBtn, ExtCtrls, Buttons, VirtualTrees, TodoTXTManager,
  {$IFDEF WINDOWS}ActiveX{$ELSE}FakeActiveX{$ENDIF}, filechannel, multilog,
  sharedloggerlcl, Clipbrd;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    mniPaste: TMenuItem;
    mniCopy: TMenuItem;
    mniCut: TMenuItem;
    mniSep1: TMenuItem;
    mniSep2: TMenuItem;
    mniDelete: TMenuItem;
    mniProperties: TMenuItem;
    mniSeparator1: TMenuItem;
    mniAddItem: TMenuItem;
    pmList: TPopupMenu;
    TrayIcon1: TTrayIcon;
    vstList: TVirtualStringTree;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mniCopyClick(Sender: TObject);
    procedure mniCutClick(Sender: TObject);
    procedure mniDeleteClick(Sender: TObject);
    procedure mniAddItemClick(Sender: TObject);
    procedure mniPasteClick(Sender: TObject);
    procedure mniPropertiesClick(Sender: TObject);
    procedure pmListPopup(Sender: TObject);
    procedure vstListChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstListDblClick(Sender: TObject);
    procedure vstListDragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
      const Pt: TPoint; var Effect: LongWord; Mode: TDropMode);
    procedure vstListDragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; const Pt: TPoint; Mode: TDropMode;
      var Effect: LongWord; var Accept: Boolean);
    procedure vstListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure vstListInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vstListPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
  private
    { private declarations }
    FToDoManager: TToDoTXTManager;
    procedure CopyToClipboard;
    procedure PasteFromClipboard;
    procedure ShowProperty(ATree: TBaseVirtualTree; ANode: PVirtualNode);
  public
    { public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  BaseNodeData, PropertyNode, Utility;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Logger.Channels.Add(TFileChannel.Create(ChangeFileExt(Application.ExeName, '.log')));
  vstList.NodeDataSize := SizeOf(rTreeNodeData);
  FToDoManager := TToDoTXTManager.Create('todo.txt', vstList);
  FToDoManager.Load;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FToDoManager.Save;
  Log('Closing application', llInfo);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FToDoManager.Free;
end;

procedure TfrmMain.mniCopyClick(Sender: TObject);
begin
  CopyToClipboard;
end;

procedure TfrmMain.mniCutClick(Sender: TObject);
begin
  CopyToClipboard;
  vstList.DeleteSelectedNodes;
end;

procedure TfrmMain.mniDeleteClick(Sender: TObject);
var
  I: Integer;
begin
  I := vstList.SelectedCount;
  if I > 0 then
  begin
    Log(Format('Removed %d selected ToDo Items', [I]), llInfo);
    vstList.DeleteSelectedNodes;
  end;
end;

procedure TfrmMain.mniAddItemClick(Sender: TObject);
var
  Node: PVirtualNode;
begin
  Log('Added new ToDo Item', llInfo);
  Node := AddVSTNode(vstList, nil);
  if Assigned(Node) then
    ShowProperty(vstList, Node);
end;

procedure TfrmMain.mniPasteClick(Sender: TObject);
begin
  PasteFromClipboard;
end;

procedure TfrmMain.mniPropertiesClick(Sender: TObject);
begin
  ShowProperty(vstList, vstList.FocusedNode);
end;

procedure TfrmMain.pmListPopup(Sender: TObject);
begin
  //Enable menu items based of Node or clipboard
  mniCut.Enabled   := (vstList.SelectedCount > 0);
  mniCopy.Enabled  := (vstList.SelectedCount > 0);
  mniPaste.Enabled := (Clipboard.AsText <> '');
  mniProperties.Enabled := (vstList.SelectedCount > 0);
end;

procedure TfrmMain.vstListChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  NodeData: PTreeNodeData;
begin
  NodeData := Sender.GetNodeData(Node);
  if Assigned(NodeData.Data) then
    NodeData.Data.Checked := (Sender.CheckState[Node] = csCheckedNormal);
end;

procedure TfrmMain.vstListDblClick(Sender: TObject);
var
  Tree: TVirtualStringTree;
begin
  if Sender is TVirtualStringTree then
  begin
    Tree := TVirtualStringTree(Sender);
    ShowProperty(Tree, Tree.FocusedNode);
  end;
end;

procedure TfrmMain.vstListDragDrop(Sender: TBaseVirtualTree; Source: TObject;
  DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
  const Pt: TPoint; var Effect: LongWord; Mode: TDropMode);
var
  AttachMode: TVTNodeAttachMode;
  Nodes: TNodeArray;
  I: integer;

  procedure DetermineEffect;
  begin
    // In the case the source is a Virtual Treeview we know 'move' is the default if dragging within
    // the same tree and copy if dragging to another tree. Set Effect accordingly.
    if Shift = [] then
    begin
      // No modifier key, so use standard action.
      if Source = Sender then
        Effect := DROPEFFECT_MOVE
      else
        Effect := DROPEFFECT_COPY;
    end
    else
    begin
      // A modifier key is pressed, hence use this to determine action.
      if (Shift = [ssAlt]) or (Shift = [ssCtrl, ssAlt]) then
        Effect := DROPEFFECT_LINK
      else
        if Shift = [ssCtrl] then
          Effect := DROPEFFECT_COPY
        else
          Effect := DROPEFFECT_MOVE;
    end;
  end;

  function CheckTargetNode: boolean;
  var
    J:Integer;
  begin
    //We must check if a selected nodes (and first previous node) is DropTargetNode
    //If it is true, we must abort Drag&Drop move
    Result := True;
    if High(Nodes) > 0 then
    begin
      if Sender.GetPrevious(Nodes[0]) = Sender.DropTargetNode then
        Exit(False);
      for J := 0 to High(Nodes) do
      begin
        if Nodes[J] = Sender.DropTargetNode then
          Exit(False);
      end;
    end;
  end;

begin
  case Mode of
    dmAbove  : AttachMode := amInsertBefore;
    dmOnNode : AttachMode := amInsertAfter;
    dmBelow  : AttachMode := amInsertAfter;
  else
    AttachMode := amNowhere;
  end;
  DetermineEffect;
  Nodes := Sender.GetSortedSelection(True);
  if Effect = DROPEFFECT_COPY then
  begin
    //Copy nodes
    for I := 0 to High(Nodes) do
      CopyVSTNode(Sender, Sender.DropTargetNode, Nodes[I], AttachMode);
    Log(Format('Copied %d ToDo items by Drag&Drop', [High(Nodes) + 1]), llInfo);
  end
  else begin
    //If a selected nodes is DropTargetNode, we must abort drag & drop
    if CheckTargetNode then
    begin
      //Move nodes in new location
      for I := 0 to High(Nodes) do
        Sender.MoveTo(Nodes[I], Sender.DropTargetNode, AttachMode, False);
      Log(Format('Moved %d ToDo items by Drag&Drop', [High(Nodes) + 1]), llInfo);
    end;
  end;
end;

procedure TfrmMain.vstListDragOver(Sender: TBaseVirtualTree; Source: TObject;
  Shift: TShiftState; State: TDragState; const Pt: TPoint; Mode: TDropMode;
  var Effect: LongWord; var Accept: Boolean);
begin
  Accept := (Sender = Source);
end;

procedure TfrmMain.vstListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode
  );
var
  NodeData: PTreeNodeData;
begin
  NodeData := Sender.GetNodeData(Node);
  if Assigned(NodeData.Data) then
    NodeData.Data.Free;
end;

procedure TfrmMain.vstListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  NodeData: PTreeNodeData;
begin
  NodeData := Sender.GetNodeData(Node);
  if Assigned(NodeData.Data) then
    CellText := NodeData.Data.Text;
end;

procedure TfrmMain.vstListInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  Sender.CheckType[Node] := ctCheckBox;
end;

procedure TfrmMain.vstListPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  if Sender.CheckState[Node] = csCheckedNormal then
    TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsStrikeOut];
end;

procedure TfrmMain.ShowProperty(ATree: TBaseVirtualTree; ANode: PVirtualNode);
var
  NodeData: PTreeNodeData;
begin
  if Assigned(ANode) then
  begin
    NodeData := ATree.GetNodeData(ANode);
    if Assigned(NodeData.Data) then
      TfrmProperty.Execute(Self, NodeData.Data);
  end;
end;

procedure TfrmMain.CopyToClipboard;
var
  I: Integer;
  sTemp: string;
  Nodes: TNodeArray;
begin
  sTemp := '';
  if vstList.SelectedCount > 0 then
  begin
    //Convert selected nodes in string
    Nodes := vstList.GetSortedSelection(False);
    for I := 0 to High(Nodes) do
    begin
      if sTemp = '' then
        sTemp := FToDoManager.NodeToString(Nodes[I])
      else
        sTemp := sTemp + LineEnding + FToDoManager.NodeToString(Nodes[I])
    end;
    //Set sTemp in clipboard
    if sTemp <> '' then
      Clipboard.AsText := sTemp;
  end;
end;

procedure TfrmMain.PasteFromClipboard;
var
  I: Integer;
  StringList: TStringList;
begin
  StringList := TStringList.Create;
  try
    StringList.Text := Clipboard.AsText;
    for I := 0 to StringList.Count - 1 do
      FToDoManager.StringToNode(StringList[I]);
  finally
    StringList.Free;
  end;
end;

end.

