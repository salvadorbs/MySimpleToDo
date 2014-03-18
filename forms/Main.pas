unit main;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  StdCtrls, EditBtn, ExtCtrls, Buttons, VirtualTrees, TodoTXTManager;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    mniRemove: TMenuItem;
    mniProperties: TMenuItem;
    mniSeparator1: TMenuItem;
    mniAddItem: TMenuItem;
    pmList: TPopupMenu;
    TrayIcon1: TTrayIcon;
    vstList: TVirtualStringTree;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mniRemoveClick(Sender: TObject);
    procedure mniAddItemClick(Sender: TObject);
    procedure mniPropertiesClick(Sender: TObject);
    procedure vstListChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstListDblClick(Sender: TObject);
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
  vstList.NodeDataSize := SizeOf(TBaseNodeData);
  FToDoManager := TToDoTXTManager.Create('todo.txt', vstList);
  FToDoManager.Load;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FToDoManager.Save;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FToDoManager.Free;
end;

procedure TfrmMain.mniRemoveClick(Sender: TObject);
begin
  if Assigned(vstList.FocusedNode) then
    vstList.DeleteNode(vstList.FocusedNode);
end;

procedure TfrmMain.mniAddItemClick(Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := vstList.AddChild(Nil, TBaseNodeData.Create);
  if Assigned(Node) then
    ShowProperty(vstList, Node);
end;

procedure TfrmMain.mniPropertiesClick(Sender: TObject);
begin
  ShowProperty(vstList, vstList.FocusedNode);
end;

procedure TfrmMain.vstListChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  NodeData: PBaseNodeData;
begin
  NodeData := Sender.GetNodeData(Node);
  if Assigned(NodeData) then
    NodeData.Checked := Sender.CheckState[Node] = csCheckedNormal;
end;

procedure TfrmMain.vstListDblClick(Sender: TObject);
var
  Tree: TVirtualStringTree;
begin
  if Sender is TVirtualStringTree then
  begin
    Tree := TVirtualStringTree(Sender);
    if not(ClickOnButtonTree(Tree)) then
      ShowProperty(Tree, Tree.FocusedNode);
  end;
end;

procedure TfrmMain.vstListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode
  );
var
  NodeData: PBaseNodeData;
begin
  NodeData := Sender.GetNodeData(Node);
  if Assigned(NodeData) then
    NodeData.Free;
end;

procedure TfrmMain.vstListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  NodeData: PBaseNodeData;
begin
  NodeData := Sender.GetNodeData(Node);
  if Assigned(NodeData) then
    CellText := NodeData.Text;
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
  NodeData: PBaseNodeData;
begin
  if Assigned(ANode) then
  begin
    NodeData := ATree.GetNodeData(ANode);
    TfrmProperty.Execute(Self, NodeData);
  end;
end;

end.

