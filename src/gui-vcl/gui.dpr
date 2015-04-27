program gui;

{$I '..\lessrtti.inc'}

{$ifdef debug}
//  {$apptype console}
{$endif}



uses
  Vcl.Forms,
  uFormMain in 'forms\uFormMain.pas' {FormMain},
  uStrings in 'uStrings.pas',
  uListenerMain in 'uListenerMain.pas',
  uMenu in 'uMenu.pas',
  uPlugins in 'uPlugins.pas',
  uFormUISelectItem in 'forms\uFormUISelectItem.pas' {FormUISelectItem},
  uFormFindBytes in 'forms\uFormFindBytes.pas' {FormFindBytes},
  uFormTextInput in 'forms\uFormTextInput.pas' {FormTextInput},
  uFormAbout in 'forms\uFormAbout.pas' {FormAbout},
  uFormTypeLibBrowser in 'forms\uFormTypeLibBrowser.pas' {FormTypeLibBrowser},
  uFormTypeLibBrowserRecord in 'forms\uFormTypeLibBrowserRecord.pas' {FormTypeLibBrowserRecord},
  uFormExportDump in 'forms\uFormExportDump.pas' {FormExportDump},
  uChooseFileName in 'uChooseFileName.pas',
  UFormCopySpecial in 'forms\UFormCopySpecial.pas' {FormCopySpecial},
  uFormTypeEditor in 'forms\uFormTypeEditor.pas' {FormTypeEditor},
  uEditField in 'uEditField.pas',
  uKeyConsts in 'uKeyConsts.pas',
  uDebugger in 'uDebugger.pas',
  uFormDefineString in 'forms\uFormDefineString.pas' {FormDefineString},
  InternalDebugSdk in '..\common\InternalDebugSdk.pas',
  uFrameDisasm in 'frames\uFrameDisasm.pas' {FrameDisasm: TFrame},
  uFrameHex in 'frames\uFrameHex.pas' {FrameHex: TFrame},
  uFrameBaseListView in 'frames\uFrameBaseListView.pas' {FrameBaseListView: TFrame},
  uFrameExports in 'frames\uFrameExports.pas' {FrameExports: TFrame},
  uFrameImports in 'frames\uFrameImports.pas' {FrameImports: TFrame},
  uFrameRefs in 'frames\uFrameRefs.pas' {FrameRefs: TFrame},
  uFrameSections in 'frames\uFrameSections.pas' {FrameSections: TFrame},
  uFrameNames in 'frames\uFrameNames.pas' {FrameNames: TFrame},
  uFrameProblems in 'frames\uFrameProblems.pas' {FrameProblems: TFrame},
  uDockLayouts in 'uDockLayouts.pas',
  uRecentList in 'uRecentList.pas',
  uFormOptions in 'forms\uFormOptions.pas' {FormOptions},
  uFrameOptionsBase in 'frames_options\uFrameOptionsBase.pas' {FrameOptionsBase: TFrame},
  uFrameOptionsDisplayColors in 'frames_options\uFrameOptionsDisplayColors.pas' {FrameDisplayColors: TFrame},
  uFrameBreakpoints in 'frames\uFrameBreakpoints.pas' {FrameBreakpoints: TFrame},
  uFrameWatches in 'frames\uFrameWatches.pas' {FrameWatches: TFrame},
  uFormLiveCall in 'forms\uFormLiveCall.pas' {FormLiveCall},
  uFrameLog in 'frames\uFrameLog.pas' {FrameLog: TFrame},
  uDisasmGraphControl in '..\common\controls\uDisasmGraphControl.pas',
  uDisasmGeometricGraph in 'uDisasmGeometricGraph.pas',
  uFormCalc in 'forms\uFormCalc.pas' {FormCalc},
  uFormIcons in 'uFormIcons.pas',
  uFormStringScan in 'forms\uFormStringScan.pas' {FormStringScan},
  uFrameScanStrings in 'frames\uFrameScanStrings.pas' {FrameScanStrings: TFrame},
  uUiUtils in 'uUiUtils.pas',
  uColorText in '..\common\controls\uColorText.pas',
  uDisasmText in '..\common\controls\uDisasmText.pas',
  uVAColorText in '..\common\controls\uVAColorText.pas',
  DockAdapter in 'DockAdapter.pas';

{$R *.res}


begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormUISelectItem, FormUISelectItem);
  Application.CreateForm(TFormFindBytes, FormFindBytes);
  Application.CreateForm(TFormTextInput, FormTextInput);
  Application.CreateForm(TFormAbout, FormAbout);
  Application.CreateForm(TFormTypeLibBrowser, FormTypeLibBrowser);
  Application.CreateForm(TFormExportDump, FormExportDump);
  Application.CreateForm(TFormCopySpecial, FormCopySpecial);
  Application.CreateForm(TFormTypeEditor, FormTypeEditor);
  Application.CreateForm(TFormDefineString, FormDefineString);
  Application.CreateForm(TFormOptions, FormOptions);
  Application.CreateForm(TFormCalc, FormCalc);
  Application.CreateForm(TFormStringScan, FormStringScan);
  uFormIcons.AssignFormIcons;

  Application.Run;

end.
