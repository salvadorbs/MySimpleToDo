unit TrayMenu;

{$mode delphi}

interface

uses
  Classes, SysUtils, Menus, VirtualTrees, BaseNodeData, Controls;

type

  { TTrayMenu }

  TTrayMenu = class
  private
    FPopupMenu: TPopupMenu;
    FTree: TBaseVirtualTree;

    procedure CreateToDoList(Sender: TBaseVirtualTree; Node: PVirtualNode;
                             Data: Pointer; var Abort: Boolean);
    function CreateMenuItem(AParentItem: TMenuItem; ACaption: string;
                            AOnClick: TNotifyEvent; AImageIndex: Integer = -1;
                            ADefault: Boolean = False): TMenuItem;
    procedure CreateSeparator(AParentItem: TMenuItem);
    //Events
    procedure ToDoMenuItemClick(Sender: TObject);
    procedure AddNewToDoItem(Sender: TObject);

    procedure Populate(Sender: TObject);
  public
    constructor Create(ATree: TBaseVirtualTree; AImageList: TImageList);
    destructor Destroy; override;

    property PopupMenu: TPopupMenu read FPopupMenu;
  end;

implementation

uses
  Main;

{ TTrayMenu }

constructor TTrayMenu.Create(ATree: TBaseVirtualTree; AImageList: TImageList);
begin
  FPopupMenu := TPopupMenu.Create(nil);
  FPopupMenu.Images  := AImageList;
  FPopupMenu.OnPopup := Populate;
  FTree := ATree;
end;

destructor TTrayMenu.Destroy;
begin
  FPopupMenu.Free;
  inherited Destroy;
end;

procedure TTrayMenu.Populate(Sender: TObject);
begin
  FPopupMenu.Items.Clear;
  //Create TrayMenu's items
  //Header
  CreateMenuItem(FPopupMenu.Items, 'Show MySimpleToDo', frmMain.ShowApp, 7, True);
  CreateMenuItem(FPopupMenu.Items, 'Add a new ToDo item', AddNewToDoItem, 3);
  //List
  CreateSeparator(FPopupMenu.Items);
  FTree.IterateSubtree(nil, CreateToDoList, @FPopupMenu);
  CreateSeparator(FPopupMenu.Items);
  //Footer
  CreateMenuItem(FPopupMenu.Items, 'Exit', frmMain.ExitApp, 8);
end;

procedure TTrayMenu.ToDoMenuItemClick(Sender: TObject);
var
  Node: PVirtualNode;
  NodeData: PTreeNodeData;
begin
  Assert(Assigned(FTree), 'FTree is not assigned!');

  Node := FTree.GetFirst;
  //Find right node to check/uncheck
  while Assigned(Node) do
  begin
    if Cardinal(TMenuItem(Sender).Tag) = Node.Index then
    begin
      NodeData := FTree.GetNodeData(Node);
      if Assigned(NodeData.Data) then
      begin
        //Check/Uncheck item
        if NodeData.Data.Checked then
          FTree.CheckState[Node] := csUncheckedNormal
        else
          FTree.CheckState[Node] := csCheckedNormal;
      end;
      Break;
    end;
    Node := FTree.GetNext(Node);
  end;
end;

procedure TTrayMenu.AddNewToDoItem(Sender: TObject);
begin
  frmMain.AddNewToDoItem(False);
end;

procedure TTrayMenu.CreateToDoList(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var
  PopupMenu : TPopupMenu;
  MenuItem  : TMenuItem;
  NodeData  : PTreeNodeData;
begin
  PopupMenu := TPopupMenu(Data^);
  if Assigned(PopupMenu) then
  begin
    NodeData := Sender.GetNodeData(Node);
    if Assigned(NodeData.Data) then
    begin
      //Create menuitem with ToDo's data and add it in PopupMenu
      MenuItem := CreateMenuItem(PopupMenu.Items, NodeData.Data.Text, ToDoMenuItemClick);
      MenuItem.Checked := NodeData.Data.Checked;
      MenuItem.Tag := Node.Index;
    end;
  end;
end;

function TTrayMenu.CreateMenuItem(AParentItem: TMenuItem; ACaption: string;
  AOnClick: TNotifyEvent; AImageIndex: Integer; ADefault: Boolean): TMenuItem;
begin
  Result := TMenuItem.Create(AParentItem);
  Result.Caption := ACaption;
  Result.OnClick := AOnClick;
  Result.ImageIndex := AImageIndex;
  Result.Default := ADefault;
  AParentItem.Add(Result);
end;

procedure TTrayMenu.CreateSeparator(AParentItem: TMenuItem);
begin
  CreateMenuItem(AParentItem, '-', nil);
end;

end.

