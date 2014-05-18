{
  Copyright (C) 2013-2014 Tim Sinaeve tim.sinaeve@gmail.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit about;

{$MODE Delphi}

interface

uses
  SysUtils, Forms, StdCtrls, ExtCtrls, Classes, Contnrs, ComCtrls, ActnList,
  Buttons, Clipbrd,

  VirtualTrees, VersionInfo,
  TreeViewPresenter, ColumnDefinitions;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    {$region 'designer controls' /fold}
    aclMain: TActionList;
    actClose: TAction;
    actDonate: TAction;
    actProjectPage: TAction;
    actReportDefect: TAction;
    actHomePage: TAction;
    btnClose: TButton;
    Image1: TImage;
    imgMain: TImage;
    lbBuildDate: TLabel;
    lbLCLVersion: TLabel;
    lblFileVersion: TLabel;
    lblFPCVersion: TLabel;
    lblLegalCopyright: TLabel;
    lblTargetCPU: TLabel;
    lblTargetOS: TLabel;
    lblHomePage: TLabel;
    lblWidgetSet: TLabel;
    pgcMain: TPageControl;
    pnlBuildDate: TPanel;
    pnlFileVersion: TPanel;
    pnlFPCVersion: TPanel;
    pnlLCLVersion: TPanel;
    pnlName: TPanel;
    pnlTargetCPU: TPanel;
    pnlTargetOS: TPanel;
    pnlWidgetSet: TPanel;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    btnDonate: TSpeedButton;
    btnReportIssue: TSpeedButton;
    tsTranslations: TTabSheet;
    tsInfo: TTabSheet;
    tsAbout: TTabSheet;
    tsCredits: TTabSheet;
    {$endregion}

    procedure actCloseExecute(Sender: TObject);
    procedure actDonateExecute(Sender: TObject);
    procedure actProjectPageExecute(Sender: TObject);
    procedure actReportDefectExecute(Sender: TObject);
    procedure actHomePageExecute(Sender: TObject);
    procedure FTVPCreditsDoubleClick(Sender: TObject);
    procedure lblHomePageClick(Sender: TObject);
    procedure lblHomePageDblClick(Sender: TObject);
    procedure lblProjectPageClick(Sender: TObject);
    procedure lblProjectPageDblClick(Sender: TObject);

  private
    FVersionInfo     : TVersionInfo;
    FVSTCredits      : TVirtualStringTree;
    FVSTTranslations : TVirtualStringTree;
    FVSTInfoList     : TVirtualStringTree;
    FTVPCredits      : TTreeViewPresenter;
    FTVPTranslations : TTreeViewPresenter;
    FTVPInfoList     : TTreeViewPresenter;
    FCredits         : TObjectList;
    FTranslations    : TObjectList;
    FInfoList        : TObjectList;

    procedure FillCredits;
    procedure FillTranslations;
    procedure FillInfoList;

    procedure InitializeCredits;
    procedure InitializeTranslations;
    procedure InitializeInfoList;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

procedure ShowAboutDialog;

implementation

{$R *.lfm}

uses
  Controls,

  FileUtil,

  Helpers, Utils,

  LCLIntf;

const
  URL_DONATE =
    'https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=QYASG6DPAYSXW';
  URL_REPORT_ISSUE = 'https://github.com/salvadorbs/MySimpleToDo/issues/new';

resourcestring
  SName         = 'Name';
  SAuthor       = 'Author';
  SLanguageCode = 'Code';
  SLanguageName = 'Language';
  SValue        = 'Value';

procedure ShowAboutDialog;
var
  F : TfrmAbout;
begin
  F := TfrmAbout.Create(Application);
  try
    F.ShowModal;
  finally
    F.Free;
  end;
end;

{$region 'TCredit' /fold}
type
  TCredit = class(TPersistent)
  strict private
    FAuthor : string;
    FName   : string;
    FURL    : string;

  published
    property Name: string
      read FName write FName;

    property Author: string
      read FAuthor write FAuthor;

    property URL: string
      read FURL write FURL;
  end;
{$endregion}

{$region 'TTranslation' /fold}
type
  TTranslation = class(TPersistent)
  strict private
    FAuthor       : string;
    FLanguageCode : string;
    FLanguageName : string;

  published
    property LanguageCode: string
      read FLanguageCode write FLanguageCode;

    property LanguageName: string
      read FLanguageName write FLanguageName;

    property Author: string
      read FAuthor write FAuthor;
  end;
{$endregion}

{$region 'TInfo' /fold}
type
  TInfo = class(TPersistent)
  strict private
    FName  : string;
    FValue : string;

  published
    property Name: string
      read FName write FName;

    property Value: string
      read FValue write FValue;
  end;
{$endregion}

{$region 'construction and destruction' /fold}
procedure TfrmAbout.AfterConstruction;
begin
  inherited AfterConstruction;
  FVersionInfo := TVersionInfo.Create(Self);
  pnlFPCVersion.Caption     := FVersionInfo.FPCVersion;
  pnlLCLVersion.Caption     := FVersionInfo.LCLVersion;
  pnlTargetCPU.Caption      := FVersionInfo.TargetCPU;
  pnlTargetOS.Caption       := FVersionInfo.TargetOS;
  pnlBuildDate.Caption      := FVersionInfo.BuildDate;
  pnlWidgetSet.Caption      := FVersionInfo.WidgetSet;
  pnlFileVersion.Caption    := FVersionInfo.FileVersion;
  pnlName.Caption           := FVersionInfo.ProductName + ' '
    + FVersionInfo.ProductVersion;
  lblHomePage.Caption       := FVersionInfo.HomePage;
  lblLegalCopyright.Caption := FVersionInfo.LegalCopyright;
  pnlName.Color                 := Color;
  lblLegalCopyright.Transparent := True;
  lblHomePage.Transparent       := True;
  InitializeCredits;
  InitializeTranslations;
  InitializeInfoList;
  pgcMain.ActivePageIndex := 0;
end;

procedure TfrmAbout.BeforeDestruction;
begin
  FreeAndNil(FCredits);
  FreeAndNil(FTranslations);
  FreeAndNil(FInfoList);
  FreeAndNil(FTVPCredits);
  FreeAndNil(FTVPTranslations);
  FreeAndNil(FTVPInfoList);
  FreeAndNil(FVersionInfo);
  inherited BeforeDestruction;
end;
{$endregion}

{$region 'event handlers' /fold}
procedure TfrmAbout.lblHomePageClick(Sender: TObject);
begin
  actHomePage.Execute;
end;

procedure TfrmAbout.lblHomePageDblClick(Sender: TObject);
begin
  actHomePage.Execute;
end;

procedure TfrmAbout.lblProjectPageClick(Sender: TObject);
begin
  actProjectPage.Execute;
end;

procedure TfrmAbout.lblProjectPageDblClick(Sender: TObject);
begin
  actProjectPage.Execute;
end;

procedure TfrmAbout.FTVPCreditsDoubleClick(Sender: TObject);
begin
  OpenURL((FTVPCredits.SelectedItem as TCredit).URL);
end;
{$endregion}

{$region 'action handlers' /fold}
procedure TfrmAbout.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmAbout.actDonateExecute(Sender: TObject);
begin
  OpenURL(URL_DONATE);
end;

procedure TfrmAbout.actProjectPageExecute(Sender: TObject);
begin
  OpenURL(FVersionInfo.ProjectPage);
end;

procedure TfrmAbout.actReportDefectExecute(Sender: TObject);
begin
  OpenURL(URL_REPORT_ISSUE);
end;

procedure TfrmAbout.actHomePageExecute(Sender: TObject);
begin
  OpenURL(FVersionInfo.HomePage);
end;
{$endregion}

{$region 'private methods' /fold}
procedure TfrmAbout.FillCredits;

  procedure AddCredit(const AName: string; const AAuthor: string; const AURL: string);
  var
    C: TCredit;
  begin
    C := TCredit.Create;
    C.Name := AName;
    C.Author := AAuthor;
    C.URL := AURL;
    FCredits.Add(C);
  end;

begin
  AddCredit(
    'FPC',
    'Free Pascal development team',
    'http://www.freepascal.org/'
  );
  AddCredit(
    'Lazarus',
    'Lazarus development team',
    'http://www.lazarus.freepascal.org/'
  );
  AddCredit(
    'Virtual TreeView',
    'Mike Lischke, FPC port by Luiz Américo Pereira Câmara',
    'http://www.soft-gems.net/'
  );
  AddCredit(
    'About Form & Code',
    'Tim Sinaeve',
    'https://code.google.com/p/notepas/'
  );
  AddCredit(
    'Lui Pack (MultiLog & UniqueInstance)',
    'Luiz Américo Pereira Câmara',
    'https://code.google.com/p/luipack/'
  );
  AddCredit(
    'Faenza Icons',
    'Matthieu James',
    'https://code.google.com/p/faenza-icon-theme/'
  );
end;

procedure TfrmAbout.FillTranslations;

  procedure AddTranslation(const ACode: string; const AName: string;
    const AAuthor: string);
  var
    T: TTranslation;
  begin
    T := TTranslation.Create;
    T.LanguageCode := ACode;
    T.LanguageName := AName;
    T.Author := AAuthor;
    FTranslations.Add(T);
  end;

begin
  AddTranslation(
    'en',
    'English',
    'Matteo Salvi'
  );
end;

procedure TfrmAbout.FillInfoList;
var
  S : string;

  procedure AddInfo(const AName: string; const AValue: string);
  var
    I: TInfo;
  begin
    I := TInfo.Create;
    I.Name := AName;
    I.Value := AValue;
    FInfoList.Add(I);
  end;
begin
  S := Application.ExeName;
  AddInfo('Application name:', ExtractFileName(S));
  //Todo: Replace GetApplicationPath with FSettings.ConfigDir
  AddInfo('Application path:', GetApplicationPath);
  AddInfo('Application size:', FormatByteText(FileSize(S)));
  AddInfo('Current path:', ExtractFilePath(ParamStr(0)));
  AddInfo('Current user:', FVersionInfo.UserName);
  //
  //AddInfo('Startup parameters', Application.ApplicationType);
  //
end;

procedure TfrmAbout.InitializeCredits;
begin
  FCredits := TObjectList.Create(True);
  FillCredits;
  FVSTCredits := VST.Create(Self, tsCredits);
  FTVPCredits := TTreeViewPresenter.Create(Self);
  FTVPCredits.ColumnDefinitions.AddColumn('Name', SName, dtString, 140);
  FTVPCredits.ColumnDefinitions.AddColumn('Author', SAuthor, dtString, 150,
    100, 220);
  FTVPCredits.OnDoubleClick := FTVPCreditsDoubleClick;
  FTVPCredits.ListMode := True;
  FTVPCredits.ItemsSource := FCredits;
  FTVPCredits.TreeView := FVSTCredits;
end;

procedure TfrmAbout.InitializeTranslations;
begin
  FTranslations := TObjectList.Create(True);
  FillTranslations;
  FVSTTranslations := VST.Create(Self, tsTranslations);
  FTVPTranslations := TTreeViewPresenter.Create(Self);
  FTVPTranslations.ColumnDefinitions.AddColumn('LanguageCode', SLanguageCode, dtString, 50);
  FTVPTranslations.ColumnDefinitions.AddColumn('LanguageName', SLanguageName, dtString, 100);
  FTVPTranslations.ColumnDefinitions.AddColumn('Author', SAuthor, dtString, 150,
    100, 220);
  FTVPTranslations.ListMode := True;
  FTVPTranslations.ItemsSource := FTranslations;
  FTVPTranslations.TreeView := FVSTTranslations;
end;

procedure TfrmAbout.InitializeInfoList;
begin
  FInfoList := TObjectList.Create(True);
  FillInfoList;
  FVSTInfoList := VST.Create(Self, tsInfo);
  FTVPInfoList := TTreeViewPresenter.Create(Self);
  FTVPInfoList.ShowHeader := False;
  FTVPInfoList.ColumnDefinitions.AddColumn('Name', SName, dtString, 110);
  FTVPInfoList.ColumnDefinitions.AddColumn('Value', SValue, dtString, 200, 200, 400);
  FTVPInfoList.ListMode := True;
  FTVPInfoList.ItemsSource := FInfoList;
  FTVPInfoList.TreeView := FVSTInfoList;
end;
{$endregion}

end.

