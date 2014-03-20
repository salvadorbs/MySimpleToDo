unit Utility;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, BaseNodeData, sharedloggerlcl,
  {$IFDEF WINDOWS}Windows, {$ELSE UNIX} LCLIntf, LCLType, LMessages,{$ENDIF} VirtualTrees;

type
  TLogLevel = (llInfo, llWarning, llError, llException);

//Date
function StrToDateEx(const str: string): TDate;
function DateToStrEx(const ADate: TDate): string;

//VirtualTree
function AddVSTNode(ATree: TBaseVirtualTree; AParentNode: PVirtualNode): PVirtualNode;
function CopyVSTNode(ATree: TBaseVirtualTree; ATargetNode, AOldNode: PVirtualNode;
  AAttachMode: TVTNodeAttachMode): PVirtualNode;
function ClickOnButtonTree(Sender: TBaseVirtualTree): Boolean;

//Misc
procedure SetVistaFonts(const AForm: TCustomForm);
function IsWindowsVista: Boolean;
procedure Log(AMessage: string; ALogLevel: TLogLevel; AException: Exception = nil);

const
  VistaFont = 'Segoe UI';
  VistaContentFont = 'Calibri';
  XPContentFont = 'Verdana';
  XPFont = 'Tahoma';

implementation

function StrToDateEx(const str: string): TDate;
var
  FormatSettings: TFormatSettings;
begin
  FormatSettings.ShortDateFormat := 'yyyy-mm-dd';
  FormatSettings.DateSeparator   := '-';
  if Not TryStrToDate(str, Result, FormatSettings) then
    Result := 0;
end;

function DateToStrEx(const ADate: TDate): string;
var
  FormatSettings: TFormatSettings;
begin
  Result := '';
  FormatSettings.ShortDateFormat := 'yyyy-mm-dd';
  FormatSettings.DateSeparator   := '-';
  Result := DateToStr(ADate, FormatSettings);
end;

function AddVSTNode(ATree: TBaseVirtualTree; AParentNode: PVirtualNode): PVirtualNode;
var
  NodeData: PTreeNodeData;
begin
  Result   := ATree.AddChild(AParentNode);
  NodeData := ATree.GetNodeData(Result);
  if Assigned(NodeData) then
    NodeData.Data := TBaseNodeData.Create;
end;

function CopyVSTNode(ATree: TBaseVirtualTree; ATargetNode, AOldNode: PVirtualNode;
  AAttachMode: TVTNodeAttachMode): PVirtualNode;
var
  NodeData, OldNodeData: PTreeNodeData;
begin
  //Copy AOldNode and create a new node
  Result := ATree.CopyTo(AOldNode, ATargetNode, AAttachMode, False);
  //Create new NodeData and copy from old nodedata
  NodeData := ATree.GetNodeData(Result);
  if Assigned(NodeData) then
  begin
    NodeData.Data := CreateNodeData;
    OldNodeData   := ATree.GetNodeData(AOldNode);
    if Assigned(OldNodeData.Data) then
      NodeData.Data.CopyFrom(OldNodeData.Data);
  end;
end;

function ClickOnButtonTree(Sender: TBaseVirtualTree): Boolean;
var
  Point   : TPoint;
  HitInfo : THitInfo;
begin
  Result   := False;
  GetCursorPos(Point);
  Point    := Sender.ScreenToClient(Point);
  Sender.GetHitTestInfoAt(Point.X,Point.Y,true,HitInfo);
  if hiOnItemButton in hitinfo.HitPositions then
    Result := True;
end;

procedure SetVistaFonts(const AForm: TCustomForm);
begin
  if IsWindowsVista and not SameText(AForm.Font.Name, VistaFont) and (Screen.Fonts.IndexOf(VistaFont) >= 0) then
  begin
    if AForm.Font.Size = 0 then
      AForm.Font.Size := AForm.Font.Size + 9
    else
      AForm.Font.Size := AForm.Font.Size + 1;

    AForm.Font.Name := VistaFont;
  end;
end;


function IsWindowsVista: Boolean;
{$IFDEF WINDOWS}
var
  VerInfo: TOSVersioninfo;
begin
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(VerInfo);
  Result := VerInfo.dwMajorVersion >= 6;
{$ELSE}
begin
  Result := False;
{$ENDIF}
end;

procedure Log(AMessage: string; ALogLevel: TLogLevel; AException: Exception = nil);
begin
  case ALogLevel of
    llError     : Logger.SendError(AMessage);
    llException : Logger.SendException(AMessage, AException);
    llInfo      : Logger.Send(AMessage);
    llWarning   : Logger.SendWarning(AMessage);
  end;
end;

end.

