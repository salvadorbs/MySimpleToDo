{
MySimpleToDo - A simple ToDo Manager for you, based of ToDo.TXT
Copyright (C) 2014 Matteo Salvi

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

unit Settings;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, IniFiles;

type

  { TSettings }

  TSettings = class
  private
    FLogFilePath: string;
    FConfigFilePath: string;
    FConfigDirPath: string;
    FToDoFilePath: string;
    FTrayicon: Boolean;

    procedure SaveFormProps(var IniFile: TIniFile; AForm: TForm);
    procedure LoadFormProps(var IniFile: TIniFile; AForm: TForm);
  public
    constructor Create;
    destructor Destroy; override;

    procedure SaveConfig;
    procedure LoadConfig;

    property LogFilePath: string read FLogFilePath;
    property ConfigFilePath: string read FConfigFilePath;
    property ConfigDirPath: string read FConfigDirPath;
    property ToDoFilePath: string read FToDoFilePath;
    property TrayIcon: Boolean read FTrayicon write FTrayicon;
  end;

implementation

uses
  Main;

{ TSettings }

procedure TSettings.SaveFormProps(var IniFile: TIniFile; AForm: TForm);
begin
  if Assigned(AForm) then
  begin
    IniFile.WriteInteger(AForm.Name, 'Top', AForm.Top);
    IniFile.WriteInteger(AForm.Name, 'Left', AForm.Left);
    IniFile.WriteInteger(AForm.Name, 'Height', AForm.Height);
    IniFile.WriteInteger(AForm.Name, 'Width', AForm.Width);
  end;
end;

procedure TSettings.LoadFormProps(var IniFile: TIniFile; AForm: TForm);
begin
  if Assigned(AForm) then
  begin
    AForm.Position := poDesigned;
    AForm.Top    := IniFile.ReadInteger(AForm.Name, 'Top', AForm.Top);
    AForm.Left   := IniFile.ReadInteger(AForm.Name, 'Left', AForm.Left);
    AForm.Height := IniFile.ReadInteger(AForm.Name, 'Height', AForm.Height);
    AForm.Width  := IniFile.ReadInteger(AForm.Name, 'Width', AForm.Width);
  end;
end;

constructor TSettings.Create;
begin
  //Get user directory for MySimpleToDo
  //Portable
  if FileExists(ApplicationName + '.cfg') then
    FConfigDirPath  := ExtractFilePath(Application.ExeName)
  else begin //Not portable (user directory)
    FConfigDirPath  := GetAppConfigDir(False);
    //Create app directory, if it didn't exists
    ForceDirectories(FConfigDirPath);
  end;
  FConfigFilePath := FConfigDirPath + ApplicationName + '.cfg';
  FLogFilePath := ChangeFileExt(FConfigFilePath, '.log');
end;

destructor TSettings.Destroy;
begin
  FConfigFilePath := '';
  FConfigDirPath  := '';
  inherited Destroy;
end;

procedure TSettings.SaveConfig;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(FConfigFilePath);
  try
    SaveFormProps(IniFile, frmMain);
    IniFile.WriteString('General', 'FileToDo', FToDoFilePath);
  finally
    IniFile.Free;
  end;
end;

procedure TSettings.LoadConfig;
var
  IniFile: TIniFile;
begin
  if FileExists(FConfigFilePath) then
  begin
    IniFile := TIniFile.Create(FConfigFilePath);
    try
      LoadFormProps(IniFile, frmMain);
      FToDoFilePath := IniFile.ReadString('General', 'FileToDo', FConfigDirPath + 'todo.txt');
      FTrayicon := IniFile.ReadBool('General', 'Trayicon', True);
    finally
      IniFile.Free;
    end;
  end;
end;

end.

