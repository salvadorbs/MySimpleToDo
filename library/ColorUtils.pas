unit ColorUtils;

interface

uses
  Graphics, Classes, StrUtils, SysUtils, LCLIntf;

type
  TColorArray = array of TColor;

function DarkerColor(const Color : TColor; Percent : Integer) : TColor;
function LighterColor(const Color : TColor; Percent : Integer) : TColor;
function MixColors(const Colors : array of TColor) : TColor;
function GrayColor(Color : TColor) : TColor;

function RGBToHtml(iRGB: Cardinal): string;
function ColorToHtml(Color:TColor): string;
function HtmlToColor(Color: string): TColor;
function InvertColor(const Color: TColor): TColor;

implementation

function DarkerColor(const Color : TColor; Percent : Integer) : TColor;
var
  R, G, B : Byte;
begin
  Result := Color;
  if Percent <= 0 then Exit;
  if Percent > 100 then Percent := 100;
  Result := ColorToRGB(Color);
  R := GetRValue(Result);
  G := GetGValue(Result);
  B := GetBValue(Result);
  R := R - R * Percent div 100;
  G := G - G * Percent div 100;
  B := B - B * Percent div 100;
  Result := RGB(R, G, B);
end;

function LighterColor(const Color : TColor; Percent : Integer) : TColor;
var
  R, G, B : Byte;
begin
  Result := Color;
  if Percent <= 0 then Exit;
  if Percent > 100 then Percent := 100;
  Result := ColorToRGB(Result);
  R := GetRValue(Result);
  G := GetGValue(Result);
  B := GetBValue(Result);
  R := R + (255 - R) * Percent div 100;
  G := G + (255 - G) * Percent div 100;
  B := B + (255 - B) * Percent div 100;
  Result := RGB(R, G, B);
end;

function MixColors(const Colors : array of TColor) : TColor;
var
  R, G, B : Integer;
  i : Integer;
  L : Integer;
begin
  R := 0;
  G := 0;
  B := 0;
  for i := Low(Colors) to High(Colors) do
  begin
    Result := ColorToRGB(Colors[i]);
    R := R + GetRValue(Result);
    G := G + GetGValue(Result);
    B := B + GetBValue(Result);
  end;
  L := Length(Colors);
  Result := RGB(R div L, G div L, B div L);
end;

function GrayColor(Color : TColor) : TColor;
var
  Gray : Byte;
begin
  Result := ColorToRGB(Color);
  Gray := (GetRValue(Result) + GetGValue(Result) + GetBValue(Result)) div 3;
  Result := RGB(Gray, Gray, Gray);
end;

function RGBToHtml(iRGB: Cardinal): string;
begin
  Result:=Format('#%.2x%.2x%.2x',
                 [Byte(iRGB),          //GetRValue(vRGB)
                  Byte(iRGB shr 8),    //GetGValue(vRGB)
                  Byte(iRGB shr 16)]); //GetBValue(vRGB)
end;

function ColorToHtml(Color:TColor): string;
begin
  Result := RGBToHtml(ColorToRGB(Color));
end;

function HtmlToColor(Color: string): TColor;
begin
  Result := StringToColor('$' + Copy(Color, 6, 2) + Copy(Color, 4, 2) + Copy(Color, 2, 2));
end;

function InvertColor(const Color: TColor): TColor;
begin
  if (GetRValue(Color) + GetGValue(Color) + GetBValue(Color)) > 384 then
    result := clBlack
  else
    result := clWhite;
end;

end.
