unit propertynode;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, ButtonPanel, ColorBox, BaseNodeData;

type

  { TfrmProperty }

  TfrmProperty = class(TForm)
    btnContextsAdd: TButton;
    btnContextsRemove: TButton;
    btnContextsReplace: TButton;
    btnPanel: TButtonPanel;
    btnProjectsAdd: TButton;
    btnProjectsRemove: TButton;
    btnProjectsReplace: TButton;
    cbxPriority: TComboBox;
    chkDeadLine: TCheckBox;
    btnColor: TColorButton;
    dtdtDeadLine: TDateEdit;
    edtContexts: TEdit;
    edtProjects: TEdit;
    edtText: TEdit;
    grbContexts: TGroupBox;
    grbItem: TGroupBox;
    grbProjects: TGroupBox;
    lblPriority: TLabel;
    lblColor: TLabel;
    lblText: TLabel;
    lxContexts: TListBox;
    lxProjects: TListBox;
    procedure btnContextsAddClick(Sender: TObject);
    procedure btnContextsRemoveClick(Sender: TObject);
    procedure btnContextsReplaceClick(Sender: TObject);
    procedure btnProjectsAddClick(Sender: TObject);
    procedure btnProjectsRemoveClick(Sender: TObject);
    procedure btnProjectsReplaceClick(Sender: TObject);
    procedure chkDeadLineChange(Sender: TObject);
    procedure edtContextsChange(Sender: TObject);
    procedure edtProjectsChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure lxContextsSelectionChange(Sender: TObject; User: boolean);
    procedure lxProjectsSelectionChange(Sender: TObject; User: boolean);
    procedure OKButtonClick(Sender: TObject);
  private
    { private declarations }
    FNodeData: TBaseNodeData;
    procedure AddItemInListBox(AListBox: TListBox; AItem: string);
    procedure PopulateListBox(AListBox: TListBox; AStringList: TStringList);
    procedure RemoveItemInListBox(AListBox: TListBox);
    procedure ReplaceItemInListBox(AListBox: TListBox; AItem: string);
    procedure SaveListBox(AListBox: TListBox; AStringList: TStringList);
  public
    { public declarations }
    class function Execute(AOwner: TComponent; ANodeData: TBaseNodeData): Boolean;
  end;

var
  frmProperty: TfrmProperty;

implementation

{$R *.lfm}

{ TfrmProperty }

procedure TfrmProperty.FormShow(Sender: TObject);
begin
  if Assigned(FNodeData) then
  begin
    edtText.Text := FNodeData.Text;
    chkDeadLine.Checked   := FNodeData.DateDeadLine <> 0;
    dtdtDeadLine.Enabled  := chkDeadLine.Checked;
    if chkDeadLine.Checked then
      dtdtDeadLine.Date   := FNodeData.DateDeadLine;
    cbxPriority.ItemIndex := cbxPriority.Items.IndexOf(FNodeData.Priority);
    if cbxPriority.ItemIndex = -1 then
      cbxPriority.ItemIndex := 0;
    PopulateListBox(lxProjects, FNodeData.Projects);
    PopulateListBox(lxContexts, FNodeData.Contexts);
    btnColor.ButtonColor := FNodeData.Color;
    //Visual events
    edtContextsChange(Sender);
    edtProjectsChange(Sender);
    lxContextsSelectionChange(Sender, False);
    lxProjectsSelectionChange(Sender, False);
  end;
end;

procedure TfrmProperty.lxContextsSelectionChange(Sender: TObject; User: boolean
  );
begin
  btnContextsRemove.Enabled := lxContexts.ItemIndex <> -1;
end;

procedure TfrmProperty.lxProjectsSelectionChange(Sender: TObject; User: boolean
  );
begin
  btnProjectsRemove.Enabled := lxContexts.ItemIndex <> -1;
end;

procedure TfrmProperty.OKButtonClick(Sender: TObject);
begin
end;

procedure TfrmProperty.PopulateListBox(AListBox: TListBox; AStringList: TStringList);
begin
  AListBox.Clear;
  if AStringList.Count > 0 then
    AListBox.Items.Assign(AStringList);
end;

procedure TfrmProperty.RemoveItemInListBox(AListBox: TListBox);
begin
  if AListBox.ItemIndex <> -1 then
    AListBox.Items.Delete(AListBox.ItemIndex);
end;

procedure TfrmProperty.ReplaceItemInListBox(AListBox: TListBox; AItem: string);
begin
  if (AItem <> '') and (AListBox.ItemIndex <> -1) then
    AListBox.Items[AListBox.ItemIndex] := AItem;
end;

procedure TfrmProperty.AddItemInListBox(AListBox: TListBox; AItem: string);
begin
  if AItem <> '' then
    AListBox.Items.Add(AItem);
end;

procedure TfrmProperty.SaveListBox(AListBox: TListBox; AStringList: TStringList
  );
begin
  AStringList.Clear;
  if AListBox.Count > 0 then
    AStringList.Assign(AListBox.Items);
end;

procedure TfrmProperty.chkDeadLineChange(Sender: TObject);
begin
  dtdtDeadLine.Enabled := chkDeadLine.Checked;
end;

procedure TfrmProperty.edtContextsChange(Sender: TObject);
begin
  btnContextsAdd.Enabled     := edtContexts.Text <> '';
  btnContextsReplace.Enabled := (edtContexts.Text <> '') and (lxContexts.ItemIndex <> -1);
end;

procedure TfrmProperty.edtProjectsChange(Sender: TObject);
begin
  btnProjectsAdd.Enabled     := edtProjects.Text <> '';
  btnProjectsReplace.Enabled := (edtProjects.Text <> '') and (lxProjects.ItemIndex <> -1);
end;

procedure TfrmProperty.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  if ModalResult = mrOk then
  begin
    FNodeData.Text := edtText.Text;
    if chkDeadLine.Checked then
      FNodeData.DateDeadLine := dtdtDeadLine.Date;
    if cbxPriority.ItemIndex <> 0 then
      FNodeData.Priority := cbxPriority.Text
    else
      FNodeData.Priority := '';
    SaveListBox(lxProjects, FNodeData.Projects);
    SaveListBox(lxContexts, FNodeData.Contexts);
    FNodeData.Color := btnColor.ButtonColor;
  end;
end;

procedure TfrmProperty.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := True;
  if ModalResult = mrOK then
  begin
    CanClose := edtText.Text <> '';
    if Not(CanClose) then
      ShowMessage('Field Text is empty. Please type something.');
  end;
end;

procedure TfrmProperty.btnContextsAddClick(Sender: TObject);
begin
  AddItemInListBox(lxContexts, edtContexts.Text);
end;

procedure TfrmProperty.btnContextsRemoveClick(Sender: TObject);
begin
  RemoveItemInListBox(lxContexts);
end;

procedure TfrmProperty.btnContextsReplaceClick(Sender: TObject);
begin
  ReplaceItemInListBox(lxContexts, edtContexts.Text);
end;

procedure TfrmProperty.btnProjectsAddClick(Sender: TObject);
begin
  AddItemInListBox(lxProjects, edtProjects.Text);
end;

procedure TfrmProperty.btnProjectsRemoveClick(Sender: TObject);
begin
  RemoveItemInListBox(lxProjects);
end;

procedure TfrmProperty.btnProjectsReplaceClick(Sender: TObject);
begin
  ReplaceItemInListBox(lxProjects, edtProjects.Text);
end;

class function TfrmProperty.Execute(AOwner: TComponent; ANodeData: TBaseNodeData): Boolean;
var
  frm: TfrmProperty;
begin
  Result := False;
  frm := TfrmProperty.Create(AOwner);
  try
    frm.FNodeData := ANodeData;
    Result := (frm.ShowModal = mrOK);
  finally
    frm.Free;
  end;
end;

end.

