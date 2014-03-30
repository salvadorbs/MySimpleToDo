program MySimpleToDo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Main, PropertyNode, BaseNodeData, Utility, TodoTXTManager, SubUtils,
  UniParser, ColorUtils, virtualtreeview_package, multiloglaz,
  uniqueinstance_package, luicontrols, TrayMenu;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

