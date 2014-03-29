unit BaseNodeData;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees, Graphics;

type

  { TBaseNodeData }

  TBaseNodeData = class
  private
    FChecked: Boolean;
    FPriority: string;
    FProjects: TStringList;
    FContexts: TStringList;
    FDateCreate: TDate;
    FDateCompleted: TDate;
    FDateDeadLine: TDate;
    FDateThresold: TDate;
    FText: string;
    FColor: TColor;

    procedure SetChecked(AValue: Boolean);
  public
    constructor Create; overload;
    destructor Destroy; override;

    procedure CopyFrom(ASource: TBaseNodeData);

    property Checked: Boolean read FChecked write SetChecked;
    property Priority: string read FPriority write FPriority;
    property Projects: TStringList read FProjects write FProjects;
    property Contexts: TStringList read FContexts write FContexts;
    property DateCreate: TDate read FDateCreate write FDateCreate;
    property DateCompleted: TDate read FDateCompleted write FDateCompleted;
    property DateDeadLine: TDate read FDateDeadLine write FDateDeadLine;
    property DateThresold: TDate read FDateThresold write FDateThresold;
    property Text: string read FText write FText;
    property Color: TColor read FColor write FColor;
  end;
  PBaseNodeData = ^TBaseNodeData;

  rTreeNodeData = record
    Data: TBaseNodeData;
  end;
  PTreeNodeData = ^rTreeNodeData;

  function CreateNodeData: TBaseNodeData;

implementation

function CreateNodeData: TBaseNodeData;
begin
  Result := TBaseNodeData.Create;
end;

procedure TBaseNodeData.SetChecked(AValue: Boolean);
begin
  if FChecked = AValue then Exit;
  FChecked := AValue;
  if AValue then
    FDateCompleted := Now
  else
    FDateCompleted := 0;
end;

constructor TBaseNodeData.Create;
begin
  FChecked       := False;
  FPriority      := '';
  FProjects      := TStringList.Create;
  FContexts      := TStringList.Create;
  FDateCreate    := Now;
  FDateCompleted := 0;
  FDateDeadLine  := 0;
  FDateThresold  := 0;
  FText          := '';
  FColor         := clNone;
end;

destructor TBaseNodeData.Destroy;
begin
  FPriority := '';
  FText     := '';
  FProjects.Free;
  FContexts.Free;
  inherited Destroy;
end;

procedure TBaseNodeData.CopyFrom(ASource: TBaseNodeData);
begin
  Assert(Assigned(ASource), 'ASource is not assigned!');
  //Copy values from ASource
  FChecked       := ASource.Checked;
  FPriority      := ASource.Priority;
  FProjects.Assign(ASource.Projects);
  FContexts.Assign(ASource.Contexts);
  FDateCreate    := ASource.DateCreate;
  FDateCompleted := ASource.DateCompleted;
  FDateDeadLine  := ASource.DateDeadLine;
  FDateThresold  := ASource.DateThresold;
  FText          := ASource.Text;
end;

end.

