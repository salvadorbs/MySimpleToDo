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
  if FileExists(ApplicationName + '.cfg') then
  begin
    FConfigDirPath  := ExtractFilePath(Application.ExeName);
    FConfigFilePath := FConfigDirPath + ApplicationName + '.cfg';
  end
  else begin
    FConfigDirPath  := GetAppConfigDir(False);
    FConfigFilePath := GetAppConfigFile(False);
    //Create app directory, if it didn't exists
    ForceDirectories(FConfigDirPath);
  end;
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
