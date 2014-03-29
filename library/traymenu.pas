unit TrayMenu;

{$mode delphi}

interface

uses
  Classes, SysUtils, Menus, VirtualTrees, BaseNodeData;

type

  { TTrayMenu }

  TTrayMenu = class
  private
    FPopupMenu: TPopupMenu;
    FTree: TBaseVirtualTree;

    procedure CreateToDoListInPopupMenu(Sender: TBaseVirtualTree; Node: PVirtualNode;
                                        Data: Pointer; var Abort: Boolean);
    procedure CreateMenuSeparator(AParentItem: TMenuItem);
    procedure CreateMenuHeader(AParentItem: TMenuItem);
    procedure CreateMenuFooter(AParentItem: TMenuItem);
    //Events
    procedure ToDoMenuItemClick(Sender: TObject);
  public
    constructor Create(APopupMenu: TPopupMenu; ATree: TBaseVirtualTree);
    destructor Destroy; override;

    procedure Populate;

    property PopupMenu: TPopupMenu read FPopupMenu;
  end;

implementation

uses
  Main;

{ TTrayMenu }

constructor TTrayMenu.Create(APopupMenu: TPopupMenu; ATree: TBaseVirtualTree);
begin
  FPopupMenu := APopupMenu;
  FTree := ATree;
end;

destructor TTrayMenu.Destroy;
begin
  inherited Destroy;
end;

procedure TTrayMenu.Populate;
begin
  FPopupMenu.Items.Clear;
  //Header
  CreateMenuHeader(FPopupMenu.Items);
  //List
  CreateMenuSeparator(FPopupMenu.Items);
  FTree.IterateSubtree(nil, CreateToDoListInPopupMenu, @FPopupMenu);
  CreateMenuSeparator(FPopupMenu.Items);
  //Footer
  CreateMenuFooter(FPopupMenu.Items);
end;

procedure TTrayMenu.ToDoMenuItemClick(Sender: TObject);
var
  Node: PVirtualNode;
  NodeData: PTreeNodeData;
begin
  Assert(Assigned(FTree), 'FTree is not assigned!');

  Node := FTree.GetFirst;
  while Assigned(Node) do
  begin
    if Cardinal(TMenuItem(Sender).Tag) = Node.Index then
    begin
      NodeData := FTree.GetNodeData(Node);
      if Assigned(NodeData.Data) then
      begin
        //NodeData.Data.Checked := Not(TMenuItem(Sender).Checked);
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

procedure TTrayMenu.CreateToDoListInPopupMenu(Sender: TBaseVirtualTree;
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
      //Create menuitem with ToDo's data
      MenuItem := TMenuItem.Create(PopupMenu);
      MenuItem.Caption := NodeData.Data.Text;
      MenuItem.Checked := NodeData.Data.Checked;
      MenuItem.Tag := Node.Index;
      MenuItem.OnClick := ToDoMenuItemClick;
      //Add menuitem in popupmenu
      PopupMenu.Items.Add(MenuItem);
    end;
  end;
end;

procedure TTrayMenu.CreateMenuSeparator(AParentItem: TMenuItem);
var
  MenuItem: TMenuItem;
begin
  MenuItem := TMenuItem.Create(AParentItem);
  MenuItem.Caption := '-';
  AParentItem.Add(MenuItem);
end;

procedure TTrayMenu.CreateMenuHeader(AParentItem: TMenuItem);
var
  MenuItem: TMenuItem;
begin
  MenuItem := TMenuItem.Create(AParentItem);
  MenuItem.Caption := 'Show MySimpleToDo';
  MenuItem.OnClick := frmMain.ShowApp;
  MenuItem.Default := True;
  AParentItem.Add(MenuItem);
end;

procedure TTrayMenu.CreateMenuFooter(AParentItem: TMenuItem);
var
  MenuItem: TMenuItem;
begin
  MenuItem := TMenuItem.Create(AParentItem);
  MenuItem.Caption := 'Exit';
  MenuItem.OnClick := frmMain.ExitApp;
  AParentItem.Add(MenuItem);
end;

end.

