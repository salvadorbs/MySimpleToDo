unit Utility;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, {$IFDEF WINDOWS}Windows, {$ELSE UNIX} LCLIntf, LCLType, LMessages,{$ENDIF} VirtualTrees;

//Date
function StrToDateEx(const str: string): TDate;
function DateToStrEx(const ADate: TDate): string;

//VirtualTree
function ClickOnButtonTree(Sender: TBaseVirtualTree): Boolean;

//Misc
procedure SetVistaFonts(const AForm: TCustomForm);
function IsWindowsVista: Boolean;

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

function ClickOnButtonTree(Sender: TBaseVirtualTree): Boolean;
var
  Point   : TPoint;
  HitInfo : ThitInfo;
begin
  Result   := false;
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

end.

