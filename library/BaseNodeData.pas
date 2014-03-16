unit BaseNodeData;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees;

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
    //FColor: TColor;

    function GetCompleteString: string;
  public
    constructor Create; overload;
    destructor Destroy; override;

    property Checked: Boolean read FChecked write FChecked;
    property Priority: string read FPriority write FPriority;
    property Projects: TStringList read FProjects write FProjects;
    property Contexts: TStringList read FContexts write FContexts;
    property DateCreate: TDate read FDateCreate write FDateCreate;
    property DateCompleted: TDate read FDateCompleted write FDateCompleted;
    property DateDeadLine: TDate read FDateDeadLine write FDateDeadLine;
    property DateThresold: TDate read FDateThresold write FDateThresold;
    property Text: string read FText write FText;
    //property CompleteString: string read GetCompleteString;
    //property Color: TColor write FColor read FColor;
  end;
  PBaseNodeData = ^TBaseNodeData;

implementation

function TBaseNodeData.GetCompleteString: string;
begin
  Result := '';
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
  //FColor := ;
end;

destructor TBaseNodeData.Destroy;
begin
  FPriority := '';
  FText     := '';
  FProjects.Free;
  FContexts.Free;
  inherited Destroy;
end;

end.

